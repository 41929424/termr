# Driver for Unix terminals.
#
# Raw mode is set with `stty` and restored from the saved state on exit.
# Key input is read by a `cat` child process; R polls its stdout with a
# timeout (processx), so the event loop never blocks longer than needed for
# the next timer. `/dev/tty` is preferred, with a TTY stdin fallback for
# sessions that have no controlling terminal.

posix_path_is_openable <- function(path) {
  con <- suppressWarnings(tryCatch(file(path, open = "rb"), error = function(e) NULL))
  if (is.null(con)) return(FALSE)
  on.exit(close(con), add = TRUE)
  isOpen(con)
}

posix_stdin_is_tty <- function() {
  identical(suppressWarnings(system("test -t 0", ignore.stdout = TRUE, ignore.stderr = TRUE)), 0L)
}

posix_select_terminal_path <- function(tty_available = posix_path_is_openable("/dev/tty"),
                                       stdin_is_tty = posix_stdin_is_tty()) {
  if (isTRUE(tty_available)) return("/dev/tty")
  if (isTRUE(stdin_is_tty)) return("/dev/stdin")
  stop("termr needs an interactive terminal: /dev/tty is unavailable and stdin is not a TTY.", call. = FALSE)
}

PosixDriver <- R6::R6Class(
  "PosixDriver",
  inherit = TerminalDriver,
  public = list(
    size_poll_interval = 0.5,
    # How long an incomplete escape sequence (a lone Escape key, or the start of
    # a fragmented sequence) waits for more input before it is flushed as is.
    escape_timeout_ms = 30L,

    initialize = function(color_mode = detect_color_mode()) {
      self$color_mode <- color_mode
      # Test hook (not a user setting): the PTY integration tests widen the
      # window so that deliberately fragmented sequences cannot be cut by
      # scheduler latency. A lone Escape is still flushed after this long.
      override <- suppressWarnings(as.integer(Sys.getenv("TERMR_ESC_TIMEOUT_MS", "")))
      if (length(override) == 1L && !is.na(override) && override >= 30L && override <= 5000L) {
        self$escape_timeout_ms <- override
      }
      self$capabilities <- terminal_capabilities(overrides = list(colors = color_mode, truecolor = identical(color_mode, "truecolor")))
      private$parser <- KeyParser$new()
    },

    start = function() {
      if (self$started) return(invisible(self))
      private$terminal_path <- posix_select_terminal_path()
      saved <- suppressWarnings(system(paste("stty -g <", shQuote(private$terminal_path), "2>/dev/null"), intern = TRUE))
      if (length(saved) != 1L || !nzchar(saved) || !is.null(attr(saved, "status"))) {
        stop("Cannot read the terminal settings with `stty`.", call. = FALSE)
      }
      private$saved_stty <- saved
      # From here on, stop() must run to restore the terminal.
      self$started <- TRUE
      private$stty("raw -echo")
      private$reader <- tryCatch(
        process$new("cat", stdin = private$terminal_path, stdout = "|", stderr = NULL,
                    encoding = "UTF-8", cleanup = TRUE, cleanup_tree = TRUE),
        error = function(e) {
          # Some process supervisors detach their children from the
          # controlling-terminal session. In that case /dev/tty cannot be
          # reopened by cat, although the parent's stdin still refers to a
          # TTY. Pass that already-attached device through instead.
          if (private$terminal_path == "/dev/tty" && posix_stdin_is_tty()) {
            tryCatch(
              process$new("cat", stdin = "/dev/stdin", stdout = "|", stderr = NULL,
                          encoding = "UTF-8", cleanup = TRUE, cleanup_tree = TRUE),
              error = function(fallback_error) {
                stop("Cannot start the terminal input reader: /dev/tty could not be reopened and the stdin TTY fallback failed: ",
                     conditionMessage(fallback_error), call. = FALSE)
              }
            )
          } else {
            stop("Cannot start the terminal input reader: ", conditionMessage(e), call. = FALSE)
          }
        }
      )
      private$last_size <- self$query_size()
      private$last_size_check <- now_seconds()
      self$write(self$setup_sequence())
      invisible(self)
    },

    stop = function() {
      if (!self$started) return(invisible(self))
      self$started <- FALSE
      try(if (!is.null(private$reader) && private$reader$is_alive()) private$reader$kill_tree(), silent = TRUE)
      private$reader <- NULL
      try(self$write(self$teardown_sequence()), silent = TRUE)
      try(private$stty(private$saved_stty), silent = TRUE)
      invisible(self)
    },

    write = function(text) write_stdout(text),

    size = function() private$last_size %||% self$query_size(),

    query_size = function() {
      out <- suppressWarnings(system(paste("stty size <", shQuote(private$terminal_path), "2>/dev/null"), intern = TRUE))
      nums <- suppressWarnings(as.integer(strsplit(trimws(out[1]), "[[:space:]]+")[[1]]))
      if (length(nums) == 2L && !anyNA(nums) && all(nums > 0L)) {
        return(c(width = nums[[2]], height = nums[[1]]))
      }
      c(
        width = as.integer(Sys.getenv("COLUMNS", "80")),
        height = as.integer(Sys.getenv("LINES", "24"))
      )
    },

    read_events = function(timeout = 0) {
      events <- list()
      wait_ms <- as.integer(round(min(timeout, self$size_poll_interval) * 1000))
      # An incomplete sequence is flushed only after `escape_timeout_ms` of
      # silence since its last byte arrived -- not whenever the event loop
      # happens to poll with a short (or zero) timeout, which would split a
      # fragmented sequence into an Escape key plus characters.
      pending <- private$parser$has_pending()
      elapsed_ms <- if (pending) (now_seconds() - private$pending_since) * 1000 else 0
      if (pending) wait_ms <- as.integer(max(0, min(wait_ms, ceiling(self$escape_timeout_ms - elapsed_ms))))
      status <- private$reader$poll_io(wait_ms)
      if (status[["output"]] == "ready") {
        chunk <- private$reader$read_output()
        if (nzchar(chunk)) {
          events <- private$parser$feed(chunk)
          private$pending_since <- now_seconds()
        }
      } else if (pending && (now_seconds() - private$pending_since) * 1000 >= self$escape_timeout_ms) {
        events <- private$parser$flush()
      }
      if (!private$reader$is_alive() && status[["output"]] != "ready") {
        stop("The terminal input reader stopped unexpectedly.", call. = FALSE)
      }
      if (now_seconds() - private$last_size_check >= self$size_poll_interval) {
        private$last_size_check <- now_seconds()
        size <- self$query_size()
        if (!identical(size, private$last_size)) {
          private$last_size <- size
          events[[length(events) + 1L]] <- ResizeEvent$new(size[["width"]], size[["height"]])
        }
      }
      events
    }
  ),
  private = list(
    saved_stty = NULL,
    reader = NULL,
    parser = NULL,
    terminal_path = NULL,
    last_size = NULL,
    last_size_check = 0,
    pending_since = 0,

    stty = function(args) {
      status <- system(paste("stty", args, "<", shQuote(private$terminal_path)))
      if (status != 0L) stop("`stty ", args, "` failed.", call. = FALSE)
    }
  )
)
