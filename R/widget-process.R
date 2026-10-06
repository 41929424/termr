#' @title ProcessView widget
#' @description Runs a program and shows its output. See [process_view()].
#' @rdname ProcessView-class
#' @export
ProcessView <- R6::R6Class(
  "ProcessView",
  inherit = Vertical,
  public = list(
    #' @field command,args,wd,env,timeout The process to run.
    command = NULL,
    args = character(),
    wd = NULL,
    env = NULL,
    timeout = NULL,
    #' @field autostart Start when the widget is mounted?
    autostart = TRUE,
    #' @field worker The [Worker] of the current run, or `NULL`.
    worker = NULL,

    #' @description Create a process view. See [process_view()].
    #' @param command,args,wd,env,timeout The process to run.
    #' @param autostart Start when mounted?
    #' @param max_lines Output lines kept.
    #' @param id,classes,style See [Widget].
    initialize = function(command, args = character(), wd = NULL, env = NULL, timeout = NULL,
                          autostart = TRUE, max_lines = 2000L, id = NULL, classes = NULL, style = NULL) {
      check_scalar_character(command, "command")
      if (!is.character(args)) stop("`args` must be a character vector (one element per argument).", call. = FALSE)
      check_flag(autostart)
      private$.status <- Label$new("not started", style = style(foreground = "$muted"))
      private$.log <- LogView$new(max_lines = max_lines, style = style(height = "1fr"))
      super$initialize(private$.status, private$.log, id = id, classes = classes, style = style)
      self$command <- command
      self$args <- args
      self$wd <- wd
      self$env <- env
      self$timeout <- timeout
      self$autostart <- autostart
    },

    #' @description Ctrl+X cancels the process, Ctrl+R restarts it.
    default_bindings = function() {
      list(bind("ctrl+x", "cancel_process", "Cancel process"), bind("ctrl+r", "restart_process", "Restart process"))
    },

    #' @description Start the process when the widget is mounted.
    #' @param event A `MountEvent`.
    on_mount = function(event) {
      if (self$autostart && is.null(self$worker)) self$start()
    },

    #' @description Run the process (output starts afresh).
    start = function() {
      app <- self$app
      if (is.null(app)) stop("The process view is not attached to an app.", call. = FALSE)
      if (self$running) stop("The process is already running.", call. = FALSE)
      private$.log$clear()
      view <- self
      self$worker <- app$run_process(
        self$command, self$args, wd = self$wd, env = self$env, timeout = self$timeout, owner = self,
        on_stdout = function(line, app) view$.__enclos_env__$private$.log$write(line),
        on_stderr = function(line, app) view$.__enclos_env__$private$.log$write(line, level = "error"),
        on_exit = function(status, app) view$.__enclos_env__$private$finished(status)
      )
      private$show_status()
      invisible(self)
    },

    #' @description Stop the process.
    cancel = function() {
      if (!is.null(self$worker) && self$worker$is_running()) {
        self$worker$cancel()
        private$finished(NA_integer_)
      }
      invisible(self)
    },

    #' @description Cancel the process and run it again.
    restart = function() {
      self$cancel()
      self$start()
    },

    #' @description Cancel action.
    action_cancel_process = function() self$cancel(),
    #' @description Restart action.
    action_restart_process = function() self$restart()
  ),
  active = list(
    #' @field running Is the process running?
    running = function(value) {
      if (!missing(value)) read_only("running")
      !is.null(self$worker) && self$worker$is_running()
    },
    #' @field output The output lines so far.
    output = function(value) if (missing(value)) private$.log$lines else read_only("output")
  ),
  private = list(
    .status = NULL,
    .log = NULL,

    finished = function(status) {
      private$show_status()
      w <- self$worker
      self$post_message("process.finished", list(status = status, state = if (!is.null(w)) w$state else NA_character_))
    },

    show_status = function() {
      w <- self$worker
      cmd <- paste(c(self$command, self$args), collapse = " ")
      if (is.null(w)) {
        private$.status$update(span("not started", style(foreground = "$muted")))
        return(invisible())
      }
      private$.status$update(switch(
        w$state,
        running = span(paste0("running: ", cmd), style(foreground = "$accent")),
        completed = span(paste0("finished: ", cmd), style(foreground = "$success")),
        cancelled = span(paste0("cancelled: ", cmd), style(foreground = "$warning")),
        span(paste0(if (isTRUE(w$timed_out)) "timed out" else "failed", " (", w$error %||% "error", "): ", cmd),
             style(foreground = "$error"))
      ))
    }
  )
)

#' Process view
#'
#' Runs an external program (without a shell) and shows its output in a
#' scrolling log: standard output as normal lines and standard error in the
#' error colour, under a one-line status. The process starts when the view
#' is mounted (or call `$start()`), is cancelled when the view is removed
#' or the app exits, and sends `"process.finished"` (`status`, `state`)
#' when it ends. Ctrl+X cancels it and Ctrl+R restarts it while the view
#' has focus. **Experimental.**
#'
#' @param command The program (a path or a name on the `PATH`).
#' @param args Arguments, one element each; never interpreted by a shell.
#' @param wd Working directory (`NULL`: current).
#' @param env Named character vector of extra environment variables.
#' @param timeout Seconds before the process is stopped (`NULL`: no limit).
#' @param autostart Start when mounted?
#' @param max_lines Output lines kept.
#' @param id,classes,style Common widget arguments.
#' @return A `ProcessView` widget with `$start()`, `$cancel()`,
#'   `$restart()`, `$running`, `$output` and `$worker`.
#' @export
#' @examples
#' pv <- process_view(file.path(R.home("bin"), "Rscript"), c("-e", "cat('hi\\n')"),
#'                    autostart = FALSE)
#' pv$running
process_view <- function(command, args = character(), wd = NULL, env = NULL, timeout = NULL,
                         autostart = TRUE, max_lines = 2000L, id = NULL, classes = NULL, style = NULL) {
  ProcessView$new(command, args = args, wd = wd, env = env, timeout = timeout, autostart = autostart,
                  max_lines = max_lines, id = id, classes = classes, style = style)
}
