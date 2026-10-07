#' Render widgets to a virtual screen
#'
#' Lays out and paints a widget tree into a [ScreenBuffer] without a
#' terminal. Useful for snapshot tests and for checking layouts
#' interactively.
#'
#' @param x A widget, a [Screen] or an [App].
#' @param width,height Screen size in cells.
#' @return A [ScreenBuffer]; use `$to_text()` for plain text.
#' @export
#' @examples
#' buf <- render_widget(vertical(label("a"), label("b")), 10, 2)
#' buf$to_text()
render_widget <- function(x, width = 80L, height = 24L) {
  root <- x
  if (inherits(x, "App")) {
    root <- x$screen
  } else if (!inherits(x, "Screen")) {
    if (!is_widget(x)) stop("`x` must be a widget or an app.", call. = FALSE)
    if (!is.null(x$parent)) {
      stop("Render the root of the tree, not a widget that has a parent.", call. = FALSE)
    }
    root <- Screen$new(x)
    on.exit(x$remove(), add = TRUE)
  }
  compose_frame(root, width, height)
}

# The part of a widget's region that is not clipped by its ancestors.
visible_area <- function(widget) {
  if (is.null(widget$region) || !widget$is_displayed()) return(NULL)
  area <- widget$region
  for (ancestor in widget$ancestors()) {
    if (is.null(ancestor$region)) return(NULL)
    area <- rect_intersect(area, ancestor$child_clip(ancestor$computed_style()))
  }
  area
}

# Layout + paint a screen into a fresh buffer.
compose_frame <- function(screen, width, height) {
  layout_tree(screen, rect(1L, 1L, width, height))
  buffer <- ScreenBuffer$new(width, height)
  paint_tree(screen, buffer)
  buffer
}

#' Test an app without a terminal
#'
#' Starts the app with a headless driver and returns a `Pilot` that
#' simulates the user. Every action runs one tick of the real event loop
#' (input -> events -> timers -> repaint), and the output goes through the
#' real renderer into a virtual terminal, so tests see exactly what a user
#' would see.
#'
#' Pilot methods:
#' * `press(...)`: press keys, e.g. `press("tab", "enter", "ctrl+c")`;
#' * `type(text)`: type characters;
#' * `resize(width, height)`: resize the terminal;
#' * `advance(seconds)`: let simulated time pass (fires timers);
#' * `click(target, y = NULL, button = "left")`: click a widget (a selector
#'   or widget; the centre of its visible part is used) or a position
#'   `click(x, y)`;
#' * `hover(target)`, `scroll(target, direction = "down", times = 1)`:
#'   move the pointer, turn the mouse wheel;
#' * `mouse(action, x, y, button, direction)`: send a raw mouse event;
#' * `drag(from, to, button = "left", steps = 3)`: press, move and release between two
#'   positions (selectors, widgets or `c(x, y)`);
#' * `paste(text)`: deliver a bracketed paste; `system_clipboard()`: text of the last OSC 52 write;
#' * `focus(target)`, `find(selector)`: focus a widget, find one;
#' * `wait_for(condition, timeout = 5)`: step the event loop (simulated time, real time
#'   while workers run) until `condition(app)` is `TRUE`;
#' * `run_worker(fn, ..., wait = TRUE)`: start a worker and wait for it;
#' * `wait_for_workers(timeout)`: wait for background workers to finish;
#' * `screen_text()`: the visible screen as a character vector;
#' * `snapshot()`: print the screen (for [testthat::expect_snapshot()]);
#' * `query_one(selector)`, `query(selector)`: find widgets;
#' * `stop()`: exit the app and restore the (virtual) terminal.
#'
#' Fields: `app`, `driver`, `exited` and `value` (the value passed to
#' `app$exit()`).
#'
#' @param app An [App] or a widget.
#' @param width,height Virtual terminal size.
#' @param color_mode Colour support of the virtual terminal: `"truecolor"`
#'   (default), `"256"`, `"16"` or `"none"` (monochrome, as with `NO_COLOR`).
#' @return A `Pilot` object.
#' @export
#' @examples
#' pilot <- test_app(app(input(id = "name")), width = 30, height = 5)
#' pilot$type("Ada")
#' pilot$query_one("#name")$value
#' pilot$stop()
test_app <- function(app, width = 80L, height = 24L, color_mode = "truecolor") {
  Pilot$new(app, width, height, color_mode)
}

Pilot <- R6::R6Class(
  "Pilot",
  public = list(
    app = NULL,
    driver = NULL,
    exited = FALSE,
    value = NULL,

    initialize = function(app, width = 80L, height = 24L, color_mode = "truecolor") {
      if (is_widget(app)) app <- App$new(app)
      if (!inherits(app, "App")) stop("`app` must be an app or a widget.", call. = FALSE)
      self$app <- app
      self$driver <- HeadlessDriver$new(width, height, color_mode = color_mode)
      private$app_private()$start(self$driver)
      private$check_exit()
    },

    press = function(...) {
      for (key in c(...)) {
        private$ensure_running()
        self$driver$press(key)
        self$step()
      }
      invisible(self)
    },

    type = function(text) {
      private$ensure_running()
      self$driver$type_text(text)
      self$step()
    },

    # Deliver a bracketed paste to the focused widget.
    paste = function(text) {
      private$ensure_running()
      self$driver$paste_text(text)
      self$step()
    },

    # Text of the last OSC 52 clipboard write the app made (NULL if none).
    system_clipboard = function() self$driver$terminal$clipboard_text(),

    resize = function(width, height) {
      private$ensure_running()
      self$driver$resize(width, height)
      self$step()
    },

    advance = function(seconds) {
      private$ensure_running()
      remaining <- seconds
      # Advance in steps so intervals fire once per period.
      repeat {
        step <- min(remaining, private$app_private()$.timers$time_until_next())
        self$driver$advance(step)
        remaining <- remaining - step
        self$step()
        if (remaining <= 0 || self$exited) break
      }
      invisible(self)
    },

    step = function() {
      if (self$exited) return(invisible(self))
      private$app_private()$tick(wait = FALSE)
      private$check_exit()
      invisible(self)
    },

    click = function(target, y = NULL, button = "left") {
      pos <- private$position(target, y)
      self$mouse("down", pos[[1]], pos[[2]], button = button)
      self$mouse("up", pos[[1]], pos[[2]], button = button)
    },

    hover = function(target, y = NULL) {
      pos <- private$position(target, y)
      self$mouse("move", pos[[1]], pos[[2]])
    },

    scroll = function(target, direction = "down", times = 1L, y = NULL) {
      pos <- private$position(target, y)
      for (i in seq_len(times)) self$mouse("scroll", pos[[1]], pos[[2]], direction = direction)
      invisible(self)
    },

    mouse = function(action, x, y, button = "none", direction = NA_character_, ...) {
      private$ensure_running()
      self$driver$feed(MouseEvent$new(action, x, y, button = button, direction = direction, ...))
      self$step()
    },

    # Press, move (in `steps` steps) and release a button between two
    # positions; each is a selector / widget or c(x, y).
    drag = function(from, to, button = "left", steps = 3L) {
      a <- private$position(from, NULL)
      b <- private$position(to, NULL)
      self$mouse("down", a[[1]], a[[2]], button = button)
      for (k in seq_len(steps)) {
        pos <- round(a + (b - a) * k / steps)
        self$mouse("move", pos[[1]], pos[[2]], button = button)
      }
      self$mouse("up", b[[1]], b[[2]], button = button)
    },

    focus = function(target) {
      private$ensure_running()
      widget <- if (is_widget(target)) target else self$app$query_one(target)
      self$app$set_focus(widget)
      self$step()
    },

    find = function(selector) self$app$query_one(selector),

    # Step the event loop until `condition(app)` is TRUE. Time is simulated
    # (timers fire deterministically); while workers run, real time is used.
    wait_for = function(condition, timeout = 5, interval = 0.05) {
      check_function(condition, "condition")
      waited <- 0
      real_deadline <- now_seconds() + timeout
      repeat {
        private$ensure_running()
        if (isTRUE(call_flex(condition, self$app))) return(invisible(self))
        busy <- length(self$app$workers()) > 0L
        if ((busy && now_seconds() > real_deadline) || (!busy && waited >= timeout)) {
          stop(paste0(sprintf("wait_for(): the condition was not met within %s seconds.", format(timeout)),
                      private$worker_diagnostics()), call. = FALSE)
        }
        if (busy) Sys.sleep(0.02)
        self$advance(interval)
        waited <- waited + interval
      }
    },

    run_worker = function(fn, ..., wait = TRUE) {
      private$ensure_running()
      worker <- self$app$run_worker(fn, ...)
      if (wait) self$wait_for_workers()
      worker
    },

    wait_for_workers = function(timeout = 60) {
      deadline <- now_seconds() + timeout
      while (length(self$app$workers()) && !self$exited) {
        if (now_seconds() > deadline) stop(paste0("Workers did not finish in time.",
                                                private$worker_diagnostics()), call. = FALSE)
        Sys.sleep(0.05)
        self$step()
      }
      self$step()
      invisible(self)
    },

    screen_text = function() self$driver$screen_text(),

    snapshot = function() {
      lines <- self$screen_text()
      bar <- strrep("-", self$driver$terminal$width)
      cat("+", bar, "+\n", sep = "")
      cat(paste0("|", lines, "|"), sep = "\n")
      cat("+", bar, "+\n", sep = "")
      invisible(lines)
    },

    query = function(selector) self$app$query(selector),

    query_one = function(selector) self$app$query_one(selector),

    stop = function() {
      if (!self$exited) {
        self$app$exit()
        private$check_exit()
      }
      invisible(self$value)
    }
  ),
  private = list(
    app_private = function() self$app$.__enclos_env__$private,

    worker_diagnostics = function() {
      workers <- self$app$workers()
      if (!length(workers)) return("")
      paste0("\n", paste(vapply(workers, function(w) {
        w$.__enclos_env__$private$diagnostics()
      }, ""), collapse = "\n"))
    },

    check_exit = function() {
      priv <- private$app_private()
      if (priv$.exit_requested && !self$exited) {
        self$exited <- TRUE
        self$value <- priv$.return_value
        priv$shutdown()
      }
    },

    # Screen position of a widget (the centre of its visible part) or of
    # explicit coordinates.
    position = function(target, y) {
      if (is.numeric(target)) {
        if (is.null(y) && length(target) == 2L) return(as.integer(target))
        if (is.null(y)) stop("Give both `x` and `y`, or a selector.", call. = FALSE)
        return(c(as.integer(target), as.integer(y)))
      }
      widget <- if (is_widget(target)) target else self$app$query_one(target)
      area <- visible_area(widget)
      if (is.null(area) || rect_is_empty(area)) {
        stop(sprintf("%s is not visible on the screen.", widget$format()), call. = FALSE)
      }
      c(area$x + (area$width - 1L) %/% 2L, area$y + (area$height - 1L) %/% 2L)
    },

    ensure_running = function() {
      if (self$exited) stop("The app has exited.", call. = FALSE)
    }
  )
)
