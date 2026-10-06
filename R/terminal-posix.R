# Driver for Unix terminals.
#
# Raw mode is set with `stty` on /dev/tty and restored from the saved
# `stty -g` state on exit. Key input is read by a `cat` child process whose
# stdin is /dev/tty; R polls its stdout with a timeout (processx), so the
# event loop never blocks longer than needed for the next timer. The
# terminal size is polled with `stty size`.

PosixDriver <- R6::R6Class(
  "PosixDriver",
  inherit = TerminalDriver,
  public = list(
    size_poll_interval = 0.5,

    initialize = function(color_mode = detect_color_mode()) {
      self$color_mode <- color_mode
      self$capabilities <- terminal_capabilities(overrides = list(colors = color_mode, truecolor = identical(color_mode, "truecolor")))
      private$parser <- KeyParser$new()
    },

    start = function() {
      if (self$started) return(invisible(self))
      if (!file.exists("/dev/tty")) {
        stop("Cannot open /dev/tty: termr needs an interactive terminal.", call. = FALSE)
      }
      saved <- suppressWarnings(system("stty -g < /dev/tty 2>/dev/null", intern = TRUE))
      if (length(saved) != 1L || !nzchar(saved) || !is.null(attr(saved, "status"))) {
        stop("Cannot read the terminal settings with `stty`.", call. = FALSE)
      }
      private$saved_stty <- saved
      # From here on, stop() must run to restore the terminal.
      self$started <- TRUE
      private$stty("raw -echo")
      private$reader <- process$new(
        "cat", stdin = "/dev/tty", stdout = "|", stderr = NULL,
        cleanup = TRUE, cleanup_tree = TRUE
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
      out <- suppressWarnings(system("stty size < /dev/tty 2>/dev/null", intern = TRUE))
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
      if (private$parser$has_pending()) wait_ms <- min(wait_ms, 30L)
      status <- private$reader$poll_io(wait_ms)
      if (status[["output"]] == "ready") {
        chunk <- private$reader$read_output()
        if (nzchar(chunk)) events <- private$parser$feed(chunk)
      } else if (private$parser$has_pending()) {
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
    last_size = NULL,
    last_size_check = 0,

    stty = function(args) {
      status <- system(paste("stty", args, "< /dev/tty"))
      if (status != 0L) stop("`stty ", args, "` failed.", call. = FALSE)
    }
  )
)
