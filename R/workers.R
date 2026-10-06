# Background workers and subprocesses.
#
# A worker runs an R function in a separate R process (processx +
# Rscript), so long computations never block the interface and R's
# single-threaded interpreter is never shared. `app$run_process()` runs any
# external program the same way. The event loop polls running workers on
# every tick without blocking; results, errors, progress and output lines
# become events and callbacks. Workers are cancelled when the app stops and
# when the widget that owns them is removed.
#
# Channels: the function's result travels in an RDS file, progress in a
# separate append-only file written by termr_progress(), and stdout /
# stderr stay free for the job's own output (streamed to on_stdout /
# on_stderr). Nothing from the job's output is ever interpreted as protocol.
#
# The function runs in a fresh R session: it must be self-contained. Pass
# data through `args`, and the packages it needs through `packages`.

#' @title Worker handle
#' @description Returned by `app$run_worker()` and `app$run_process()`. See
#'   [run_worker()].
#' @export
Worker <- R6::R6Class(
  "Worker",
  public = list(
    #' @field name Name of the worker (used in events).
    name = NULL,
    #' @field state `"running"`, `"completed"`, `"failed"` or `"cancelled"`.
    state = "running",
    #' @field result The value returned by the function (workers) or the
    #'   exit status (processes).
    result = NULL,
    #' @field error The error message, if it failed.
    error = NULL,
    #' @field timed_out Did the worker fail because its timeout expired?
    timed_out = FALSE,
    #' @field progress Last reported progress value.
    progress = NA,
    #' @field owner Widget that owns the worker, or `NULL`.
    owner = NULL,
    #' @field stdout,stderr The most recent output lines (at most 1000).
    stdout = character(),
    stderr = character(),
    #' @field kind `"worker"` (an R function) or `"process"`.
    kind = "worker",

    #' @description Create a worker (use `app$run_worker()`).
    #' @param name Name.
    #' @param owner Owner widget.
    #' @param callbacks List of callbacks.
    initialize = function(name, owner = NULL, callbacks = list()) {
      self$name <- name
      self$owner <- owner
      private$callbacks <- callbacks
    },

    #' @description Stop the worker: an interrupt is sent first (Unix) and
    #'   the process tree is killed if it has not ended after `grace`
    #'   seconds.
    #' @param grace Seconds to wait before killing.
    cancel = function(grace = 0.3) {
      if (self$state != "running") return(invisible(FALSE))
      private$terminate(grace)
      private$cleanup()
      self$state <- "cancelled"
      private$emit("worker.cancelled", list())
      private$app <- NULL
      invisible(TRUE)
    },

    #' @description Is the worker still running?
    is_running = function() self$state == "running",

    #' @description Process id of the child (or `NA`).
    pid = function() if (is.null(private$process)) NA_integer_ else private$process$get_pid(),

    #' @description Print the worker.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Worker ", self$name, ": ", self$state, ">\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    process = NULL,
    files = character(),
    progress_file = NULL,
    progress_offset = 0,
    callbacks = list(),
    app = NULL,
    started = 0,
    timeout = NULL,
    max_lines = 1000L,
    max_events_per_poll = 200L,

    start_process = function(app, fn, args, packages, timeout) {
      private$app <- app
      private$timeout <- timeout
      job <- tempfile("termr-job-", fileext = ".rds")
      out <- tempfile("termr-result-", fileext = ".rds")
      prog <- tempfile("termr-progress-", fileext = ".log")
      # The child's own temporary files (R / Rscript create some at start-up)
      # go to a directory of ours, so nothing is left behind when it is killed.
      scratch <- tempfile("termr-worker-tmp-")
      dir.create(scratch)
      private$files <- c(job, out, prog, scratch)
      private$progress_file <- prog
      saveRDS(list(fn = fn, args = args, packages = packages), job)
      script <- system.file("helpers", "termr-worker.R", package = "termr")
      if (!nzchar(script)) stop("The termr worker script is missing.", call. = FALSE)
      rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
      # R CMD check exports R_TESTS. Rscript evaluates it at startup even
      # with --vanilla, so a worker would otherwise rerun the test suite
      # before loading its job (and recursively start more workers).
      private$spawn(rscript, c("--vanilla", script, job, out, prog),
                    env = c(R_TESTS = "", TMPDIR = scratch, TMP = scratch, TEMP = scratch))
    },

    start_command = function(app, command, args, wd, env, timeout) {
      private$app <- app
      private$timeout <- timeout
      self$kind <- "process"
      private$spawn(command, args, wd = wd, env = env)
    },

    spawn = function(command, args, wd = NULL, env = NULL) {
      private$started <- now_seconds()
      private$process <- tryCatch(
        processx::process$new(command, args, stdout = "|", stderr = "|", wd = wd,
                              env = if (is.null(env)) NULL else c("current", env),
                              cleanup = TRUE, cleanup_tree = TRUE),
        error = function(e) {
          private$cleanup()
          stop("Could not start `", command, "`: ", conditionMessage(e), call. = FALSE)
        }
      )
      private$emit("worker.started", list())
    },

    run_inline = function(app, fn, args) {
      private$app <- app
      private$emit("worker.started", list())
      res <- tryCatch(list(ok = TRUE, value = do.call(fn, args)),
                      error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      private$finish(res)
    },

    # Called by the app on every tick.
    poll = function() {
      if (self$state != "running" || is.null(private$process)) return(invisible())
      p <- private$process
      alive <- p$is_alive()
      private$drain(p, final = !alive)
      if (self$state != "running") return(invisible())
      if (alive) {
        if (!is.null(private$timeout) && now_seconds() - private$started > private$timeout) {
          private$terminate(0.3)
          self$timed_out <- TRUE
          private$finish(list(ok = FALSE, message = sprintf("Timed out after %s seconds.", format(private$timeout))))
        }
        return(invisible())
      }
      if (self$kind == "process") {
        status <- p$get_exit_status()
        private$finish(if (identical(status, 0L)) list(ok = TRUE, value = status) else
          list(ok = FALSE, message = sprintf("The process exited with status %s.", format(status)), status = status))
        return(invisible())
      }
      out <- private$files[[2]]
      res <- if (file.exists(out)) tryCatch(readRDS(out), error = function(e) NULL)
      if (is.null(res)) {
        err <- paste(utils::tail(self$stderr, 3L), collapse = " ")
        status <- p$get_exit_status()
        status_text <- if (is.null(status)) "unknown" else as.character(status)
        detail <- if (nzchar(trimws(err))) paste0(" Stderr: ", trimws(err)) else ""
        res <- list(ok = FALSE,
                    message = paste0("The worker process exited with status ", status_text,
                                     " without a result.", detail),
                    status = status)
      }
      private$finish(res)
    },

    # Deliver new output lines and progress records.
    drain = function(p, final = FALSE) {
      budget <- private$max_events_per_poll
      for (stream in c("output", "error")) {
        lines <- if (final) {
          if (stream == "output") p$read_all_output_lines() else p$read_all_error_lines()
        } else if (p$poll_io(0L)[[stream]] == "ready") {
          if (stream == "output") p$read_output_lines() else p$read_error_lines()
        } else character()
        if (!length(lines)) next
        private$deliver_lines(lines, if (stream == "output") "stdout" else "stderr")
      }
      private$read_progress()
      invisible()
    },

    deliver_lines = function(lines, stream) {
      lines <- substr(sanitize_text(lines), 1L, 4000L)
      keep <- utils::tail(c(self[[stream]], lines), private$max_lines)
      self[[stream]] <- keep
      cb <- private$callbacks[[paste0("on_", stream)]]
      for (line in lines) {
        private$emit(paste0("worker.", stream), list(line = line))
        if (!is.null(cb)) call_flex(cb, line, private$app)
      }
    },

    # Progress records are lines "P<TAB>value<TAB>message" appended to a
    # file by termr_progress(); only complete lines are consumed.
    read_progress = function() {
      path <- private$progress_file
      if (is.null(path) || !file.exists(path)) return(invisible())
      size <- file.size(path)
      if (is.na(size) || size <= private$progress_offset) return(invisible())
      con <- file(path, "rb")
      on.exit(close(con))
      seek(con, private$progress_offset)
      bytes <- readBin(con, "raw", size - private$progress_offset)
      last_newline <- which(bytes == as.raw(10L))
      if (!length(last_newline)) return(invisible())
      used <- max(last_newline)
      private$progress_offset <- private$progress_offset + used
      text <- rawToChar(bytes[seq_len(used)])
      Encoding(text) <- "UTF-8"
      for (line in strsplit(text, "\n", fixed = TRUE)[[1]]) {
        fields <- strsplit(line, "\t", fixed = TRUE)[[1]]
        if (length(fields) < 2L || fields[[1]] != "P") next
        value <- suppressWarnings(as.numeric(fields[[2]]))
        message <- if (length(fields) >= 3L) sanitize_text(fields[[3]]) else ""
        self$progress <- value
        private$emit("worker.progress", list(value = value, message = message))
        cb <- private$callbacks$on_progress
        if (!is.null(cb)) call_flex(cb, value, message, private$app)
      }
      invisible()
    },

    # Interrupt (Unix), wait up to `grace` seconds, then kill the tree.
    terminate = function(grace) {
      p <- private$process
      if (is.null(p) || !p$is_alive()) return(invisible())
      if (grace > 0 && .Platform$OS.type != "windows") {
        try(p$interrupt(), silent = TRUE)
        try(p$wait(as.integer(grace * 1000)), silent = TRUE)
      }
      if (p$is_alive()) try(p$kill_tree(), silent = TRUE)
      invisible()
    },

    finish = function(res) {
      app <- private$app
      private$cleanup()
      if (isTRUE(res$ok)) {
        self$state <- "completed"
        self$result <- res$value
        private$emit("worker.completed", list(result = res$value))
        cb <- private$callbacks$on_complete
        if (!is.null(cb)) call_flex(cb, res$value, app)
      } else {
        self$state <- "failed"
        self$error <- res$message
        private$emit("worker.failed", list(error = res$message, timed_out = self$timed_out, status = res$status))
        cb <- private$callbacks$on_error
        if (!is.null(cb)) call_flex(cb, res$message, app)
      }
      if (self$kind == "process") {
        cb <- private$callbacks$on_exit
        status <- if (isTRUE(res$ok)) res$value else res$status
        if (!is.null(cb)) call_flex(cb, status, app)
      }
      private$app <- NULL
    },

    cleanup = function() {
      unlink(private$files, recursive = TRUE)
      private$files <- character()
      private$progress_file <- NULL
    },

    emit = function(type, data) {
      app <- private$app
      if (is.null(app)) return(invisible())
      data$worker <- self
      sender <- self$owner
      if (!is.null(sender) && !identical(sender$app, app)) sender <- NULL
      app$post(MessageEvent$new(type, sender = sender, data = data))
    }
  )
)

#' Background workers and subprocesses
#'
#' `app$run_worker()` runs a function in a separate R process and reports
#' back through events and callbacks, so the interface stays responsive
#' during long computations (model fitting, downloads, file processing).
#' `app$run_process()` does the same for an external program.
#'
#' The function runs in a fresh R session (`Rscript --vanilla`): it cannot
#' see your global variables. Pass data through `args` and list the
#' packages it needs in `packages`. Inside the function,
#' `termr_progress(value, message)` reports progress on a channel of its
#' own, so anything the function prints to stdout or stderr is free for
#' `on_stdout` / `on_stderr` (`message()` goes to stderr).
#'
#' `run_process(command, args)` starts a program *without a shell*: the
#' command and its arguments are separate, so nothing is interpolated or
#' interpreted. (To use a shell on purpose, run it yourself:
#' `run_process("sh", c("-c", script))`.) It completes with the exit status
#' as `result`; a non-zero status counts as failure.
#'
#' Events (from the owner widget, or the screen), with `event$data$worker`:
#' `"worker.started"`, `"worker.progress"` (`value`, `message`),
#' `"worker.stdout"` / `"worker.stderr"` (`line`), `"worker.completed"`
#' (`result`), `"worker.failed"` (`error`, `timed_out`, `status`),
#' `"worker.cancelled"`. Output lines are stripped of control characters;
#' at most 200 lines are delivered per event-loop tick.
#'
#' Workers stop when `cancel()` is called, when their owner widget is
#' removed, when `timeout` seconds have passed, and when the app exits. A
#' stopping worker is interrupted first (Unix) and killed, with its child
#' processes, if it does not end within a short grace period.
#'
#' Temporary files (the job, the result and the progress file) have unique
#' names and are removed when the worker ends, however it ends.
#'
#' There is no worker pool: start as many workers as you need; each is an
#' independent process. This API is **experimental**.
#'
#' @section Testing:
#' `inline = TRUE` runs the function in the current R process at once
#' (blocking), which makes tests fast and deterministic. With real
#' processes, `pilot$wait_for_workers()` waits until they finish.
#'
#' @param fn The function to run.
#' @param args A list of arguments.
#' @param on_complete `function(result, app)`.
#' @param on_error `function(message, app)`.
#' @param on_progress `function(value, message, app)`.
#' @param on_stdout,on_stderr `function(line, app)` called for each output
#'   line.
#' @param timeout Seconds after which the worker is stopped and fails
#'   (`NULL`: no limit).
#' @param name Name of the worker.
#' @param owner A widget owning the worker (events come from it, and it is
#'   cancelled when the widget is removed).
#' @param packages Packages to attach in the worker process.
#' @param inline Run in this process instead (for tests).
#' @param app The app (defaults to the running app).
#' @return A [Worker] handle with `cancel()`, `state`, `result`, `error`,
#'   `stdout` and `stderr`.
#' @export
#' @examples
#' \dontrun{
#' app$run_worker(
#'   function(n) {
#'     for (i in seq_len(n)) {
#'       Sys.sleep(0.1)
#'       termr_progress(i / n, paste("step", i))
#'     }
#'     "done"
#'   },
#'   args = list(n = 20),
#'   on_complete = function(result, app) app$notify(result)
#' )
#' app$run_process("ls", c("-l", "/tmp"), on_stdout = function(line, app) message(line))
#' }
run_worker <- function(fn, args = list(), on_complete = NULL, on_error = NULL, on_progress = NULL,
                       name = NULL, owner = NULL, packages = character(), inline = FALSE,
                       on_stdout = NULL, on_stderr = NULL, timeout = NULL, app = current_app()) {
  check_app(app)$run_worker(fn, args = args, on_complete = on_complete, on_error = on_error,
                            on_progress = on_progress, name = name, owner = owner,
                            packages = packages, inline = inline, on_stdout = on_stdout,
                            on_stderr = on_stderr, timeout = timeout)
}

#' @rdname run_worker
#' @param command The program to run (a path or a name found on the `PATH`).
#' @param wd Working directory (`NULL`: the current one).
#' @param env Named character vector of extra environment variables.
#' @param on_exit `function(status, app)` called when the process ends
#'   (whatever the status).
#' @export
run_process <- function(command, args = character(), on_complete = NULL, on_error = NULL,
                        on_stdout = NULL, on_stderr = NULL, on_exit = NULL, timeout = NULL,
                        wd = NULL, env = NULL, name = NULL, owner = NULL, app = current_app()) {
  check_app(app)$run_process(command, args = args, on_complete = on_complete, on_error = on_error,
                             on_stdout = on_stdout, on_stderr = on_stderr, on_exit = on_exit,
                             timeout = timeout, wd = wd, env = env, name = name, owner = owner)
}

# Sequential names that do not touch the user's random number stream.
next_worker_name <- function() {
  termr_env$worker_count <- (termr_env$worker_count %||% 0L) + 1L
  paste0("worker-", termr_env$worker_count)
}

check_worker_options <- function(on_complete, on_error, on_progress, on_stdout, on_stderr, timeout, owner) {
  check_function(on_complete, "on_complete", allow_null = TRUE)
  check_function(on_error, "on_error", allow_null = TRUE)
  check_function(on_progress, "on_progress", allow_null = TRUE)
  check_function(on_stdout, "on_stdout", allow_null = TRUE)
  check_function(on_stderr, "on_stderr", allow_null = TRUE)
  if (!is.null(timeout) && !(is.numeric(timeout) && length(timeout) == 1L && !is.na(timeout) && timeout > 0)) {
    stop("`timeout` must be a positive number of seconds.", call. = FALSE)
  }
  if (!is.null(owner) && !is_widget(owner)) stop("`owner` must be a widget.", call. = FALSE)
}

start_worker <- function(app, fn, args, on_complete, on_error, on_progress, name, owner, packages, inline,
                         on_stdout = NULL, on_stderr = NULL, timeout = NULL) {
  check_function(fn, "fn")
  if (!is.list(args)) stop("`args` must be a list.", call. = FALSE)
  check_worker_options(on_complete, on_error, on_progress, on_stdout, on_stderr, timeout, owner)
  if (!is.character(packages)) stop("`packages` must be a character vector.", call. = FALSE)
  check_flag(inline)
  worker <- Worker$new(
    name %||% next_worker_name(), owner = owner,
    callbacks = list(on_complete = on_complete, on_error = on_error, on_progress = on_progress,
                     on_stdout = on_stdout, on_stderr = on_stderr)
  )
  wp <- worker$.__enclos_env__$private
  if (inline) wp$run_inline(app, fn, args) else wp$start_process(app, fn, args, packages, timeout)
  worker
}

start_command <- function(app, command, args, on_complete, on_error, on_stdout, on_stderr, on_exit, timeout,
                          wd, env, name, owner) {
  check_scalar_character(command, "command")
  if (!is.character(args)) stop("`args` must be a character vector (one element per argument).", call. = FALSE)
  check_function(on_exit, "on_exit", allow_null = TRUE)
  check_worker_options(on_complete, on_error, NULL, on_stdout, on_stderr, timeout, owner)
  if (!is.null(env) && (is.null(names(env)) || !is.character(env))) {
    stop("`env` must be a named character vector.", call. = FALSE)
  }
  worker <- Worker$new(
    name %||% next_worker_name(), owner = owner,
    callbacks = list(
      on_complete = on_complete, on_error = on_error, on_stdout = on_stdout, on_stderr = on_stderr,
      on_exit = on_exit
    )
  )
  worker$.__enclos_env__$private$start_command(app, command, args, wd, env, timeout)
  worker
}
