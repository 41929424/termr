#' Terminal drivers
#'
#' A driver is the only part of termr that talks to the terminal. It
#' switches the terminal into application mode (raw input, alternate
#' screen, hidden cursor), writes output, reports the terminal size and
#' turns input into [Event]s. Everything above the driver is pure R and can
#' be tested without a terminal.
#'
#' * `PosixDriver`: Linux, macOS and other Unix terminals (`stty` raw mode,
#'   input read from `/dev/tty`).
#' * `WindowsDriver`: Windows consoles and Windows Terminal (a small
#'   PowerShell helper reads key events).
#' * `HeadlessDriver`: no terminal at all; output goes to a virtual
#'   terminal and input is fed programmatically. Used by [test_app()].
#'
#' [run()] picks the right driver with `default_driver()`.
#'
#' @keywords internal
#' @name drivers
NULL

TerminalDriver <- R6::R6Class(
  "TerminalDriver",
  public = list(
    color_mode = "truecolor",
    # What the terminal can do (see terminal_capabilities()); detected once.
    capabilities = NULL,
    started = FALSE,
    # Report mouse events? Set by the app before start().
    mouse = FALSE,

    start = function() stop("Not implemented.", call. = FALSE),
    stop = function() invisible(self),
    write = function(text) stop("Not implemented.", call. = FALSE),
    size = function() c(width = 80L, height = 24L),
    read_events = function(timeout) list(),
    clock = function() now_seconds(),

    # Sequences that put the terminal into / out of application mode.
    setup_sequence = function() {
      paste0(
        ansi_alt_screen(TRUE), ansi_cursor_visible(FALSE), ansi_autowrap(FALSE),
        ansi_reset(), ansi_clear_screen(), ansi_cursor_home(),
        if (isTRUE(self$mouse)) ansi_mouse(TRUE) else "",
        if (isTRUE(self$capabilities$bracketed_paste)) ansi_bracketed_paste(TRUE) else ""
      )
    },
    teardown_sequence = function() {
      paste0(
        if (isTRUE(self$capabilities$bracketed_paste)) ansi_bracketed_paste(FALSE) else "",
        if (isTRUE(self$mouse)) ansi_mouse(FALSE) else "",
        ansi_reset(), ansi_autowrap(TRUE), ansi_cursor_visible(TRUE), ansi_alt_screen(FALSE)
      )
    }
  )
)

# Headless ---------------------------------------------------------------------

# In-memory driver used by test_app(): output goes to a VirtualTerminal,
# input is fed programmatically and time is simulated.
HeadlessDriver <- R6::R6Class(
  "HeadlessDriver",
  inherit = TerminalDriver,
  public = list(
    terminal = NULL,
    output = character(),
    time = 0,
    # Simulated seconds without input after which a blocking read raises an
    # error instead of hanging a test.
    max_idle = 60,

    initialize = function(width = 80L, height = 24L, color_mode = "truecolor", ...) {
      self$terminal <- VirtualTerminal$new(width, height)
      self$color_mode <- color_mode
      self$capabilities <- headless_capabilities(color_mode, ...)
    },

    start = function() {
      self$started <- TRUE
      self$write(self$setup_sequence())
      invisible(self)
    },

    stop = function() {
      if (!self$started) return(invisible(self))
      self$write(self$teardown_sequence())
      self$started <- FALSE
      invisible(self)
    },

    write = function(text) {
      self$output <- c(self$output, text)
      self$terminal$feed(text)
      invisible(self)
    },

    size = function() c(width = self$terminal$width, height = self$terminal$height),

    clock = function() self$time,

    advance = function(seconds) {
      self$time <- self$time + seconds
      invisible(self)
    },

    feed = function(...) {
      for (ev in list(...)) private$queue[[length(private$queue) + 1L]] <- ev
      invisible(self)
    },

    press = function(...) {
      for (key in c(...)) self$feed(KeyEvent$new(key))
      invisible(self)
    },

    type_text = function(text) {
      chars <- split_graphemes(sanitize_text(text))
      for (ch in chars) self$feed(KeyEvent$new(if (ch == " ") "space" else ch))
      invisible(self)
    },

    paste_text = function(text) {
      self$feed(PasteEvent$new(text))
      invisible(self)
    },

    resize = function(width, height) {
      self$terminal$resize(width, height)
      self$feed(ResizeEvent$new(width, height))
      invisible(self)
    },

    read_events = function(timeout = 0) {
      events <- private$queue
      private$queue <- list()
      if (length(events) == 0L && timeout > 0) {
        # Simulate waiting: time passes, so timers become due.
        step <- if (is.finite(timeout)) timeout else 1
        self$time <- self$time + step
        private$idle <- private$idle + step
        if (private$idle > self$max_idle) {
          stop("HeadlessDriver: the app is waiting for input but none was provided.", call. = FALSE)
        }
      } else if (length(events)) {
        private$idle <- 0
      }
      events
    },

    screen_text = function() self$terminal$to_text()
  ),
  private = list(queue = list(), idle = 0)
)

# Detection ------------------------------------------------------------------

default_driver <- function() {
  check_terminal()
  if (.Platform$OS.type == "windows") WindowsDriver$new() else PosixDriver$new()
}

check_terminal <- function() {
  gui <- .Platform$GUI
  if (identical(gui, "RStudio") || identical(gui, "Rgui") || identical(Sys.getenv("POSITRON"), "1")) {
    stop(
      "termr apps need a real terminal, but R is running inside ", gui, ".\n",
      "Run the app with Rscript from a terminal, e.g. `Rscript app.R`.",
      call. = FALSE
    )
  }
  if (!isatty(stdout())) {
    stop("termr apps need an interactive terminal: standard output is not a terminal.", call. = FALSE)
  }
  invisible(TRUE)
}

write_stdout <- function(text) {
  cat(text, file = stdout())
  flush(stdout())
}
