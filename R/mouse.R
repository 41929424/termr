# Mouse input.
#
# Drivers turn terminal mouse reports into MouseEvents in screen
# coordinates (1-based columns/rows) with no target. The App then hit-tests
# the widget tree (hit_test()), fills in the target and widget-relative
# coordinates, updates hover and focus, synthesises "click" events and
# dispatches the event like any other (it bubbles).
#
# Event types:
#   "mouse.down"   a button was pressed
#   "mouse.up"     a button was released
#   "mouse.move"   the pointer moved (with or without a button held)
#   "mouse.scroll" the wheel turned (`direction`: up/down/left/right)
#   "click"        down and up on the same widget
#   "drag.start"   the pointer moved with a button held (first time)
#   "drag.move"    ... and on every further move
#   "drag.end"     the button was released after a drag
# While a button is held (or after widget$capture_mouse()) move, up and drag
# events go to the widget where the press happened, even outside its region.

#' @rdname Event
#' @export
MouseEvent <- R6::R6Class(
  "MouseEvent",
  inherit = Event,
  public = list(
    #' @field action `"down"`, `"up"`, `"move"`, `"scroll"` or `"click"`.
    action = NULL,
    #' @field button `"left"`, `"middle"`, `"right"` or `"none"`.
    button = "none",
    #' @field direction Wheel direction for scroll events, else `NA`.
    direction = NA_character_,
    #' @field screen_x,screen_y Pointer position on the screen (1-based).
    screen_x = NULL,
    screen_y = NULL,
    #' @field x,y Pointer position inside the target widget (1-based,
    #'   relative to the widget's region).
    x = NULL,
    y = NULL,
    #' @field origin_x,origin_y Where the button went down (screen
    #'   coordinates) for `drag.*` events, else `NA`.
    origin_x = NA_integer_,
    origin_y = NA_integer_,
    #' @field shift,ctrl,alt Modifier flags.
    shift = FALSE,
    ctrl = FALSE,
    alt = FALSE,

    #' @description Create a mouse event.
    #' @param action Event action.
    #' @param screen_x,screen_y Screen position.
    #' @param button Button name.
    #' @param direction Wheel direction (scroll events).
    #' @param shift,ctrl,alt Modifiers.
    initialize = function(action, screen_x, screen_y, button = "none", direction = NA_character_,
                          shift = FALSE, ctrl = FALSE, alt = FALSE) {
      action <- check_choice(action, c("down", "up", "move", "scroll", "click", "drag_start", "drag_move", "drag_end"), "action")
      super$initialize(switch(action, click = "click", drag_start = "drag.start", drag_move = "drag.move",
                              drag_end = "drag.end", paste0("mouse.", action)))
      self$action <- action
      self$button <- check_choice(button, c("left", "middle", "right", "none"), "button")
      self$direction <- direction
      self$screen_x <- as.integer(screen_x)
      self$screen_y <- as.integer(screen_y)
      self$x <- self$screen_x
      self$y <- self$screen_y
      self$shift <- shift
      self$ctrl <- ctrl
      self$alt <- alt
    },

    #' @description The pointer position relative to another widget.
    #' @param widget A widget with a region.
    #' @return `c(x =, y =)`, 1-based; may be outside the widget.
    offset_in = function(widget) {
      r <- widget$region
      if (is.null(r)) return(c(x = NA_integer_, y = NA_integer_))
      c(x = self$screen_x - r$x + 1L, y = self$screen_y - r$y + 1L)
    }
  ),
  private = list(
    details = function() {
      paste0(
        " ", self$button, " at ", self$screen_x, ",", self$screen_y,
        if (!is.na(self$direction)) paste0(" ", self$direction) else ""
      )
    }
  )
)

# The deepest visible widget at screen position (x, y), respecting the
# clipping of every ancestor (content scrolled out of a viewport cannot be
# hit). Later children are painted on top, so they are tested first.
hit_test <- function(widget, x, y, clip = NULL, inherited = NULL) {
  if (!widget$visible || is.null(widget$region)) return(NULL)
  clip <- clip %||% widget$region
  area <- rect_intersect(widget$region, clip)
  if (rect_is_empty(area) || !rect_contains(area, x, y)) return(NULL)
  st <- widget$computed_style(inherited)
  inner <- rect_intersect(widget$child_clip(st), clip)
  kids <- render_children(widget)
  for (child in rev(kids)) {
    hit <- hit_test(child, x, y, inner, st)
    if (!is.null(hit)) return(hit)
  }
  widget
}

# Parse the parameters of an SGR mouse report ESC [ < b ; x ; y (M|m).
sgr_mouse_event <- function(params, final) {
  nums <- suppressWarnings(as.integer(strsplit(sub("^<", "", params), ";", fixed = TRUE)[[1]]))
  if (length(nums) != 3L || anyNA(nums)) return(NULL)
  code <- nums[[1]]
  shift <- bitwAnd(code, 4L) > 0L
  alt <- bitwAnd(code, 8L) > 0L
  ctrl <- bitwAnd(code, 16L) > 0L
  motion <- bitwAnd(code, 32L) > 0L
  base <- bitwAnd(code, 3L)
  if (bitwAnd(code, 64L) > 0L) {
    direction <- c("up", "down", "left", "right")[[base + 1L]]
    return(MouseEvent$new("scroll", nums[[2]], nums[[3]], direction = direction,
                          shift = shift, ctrl = ctrl, alt = alt))
  }
  button <- c("left", "middle", "right", "none")[[base + 1L]]
  action <- if (motion) "move" else if (final == "m") "up" else "down"
  MouseEvent$new(action, nums[[2]], nums[[3]], button = button, shift = shift, ctrl = ctrl, alt = alt)
}

# Turn a Windows MOUSE_EVENT_RECORD into MouseEvents. `previous` is the
# button state of the last record; presses and releases are derived from
# the change. Coordinates are 1-based screen positions. `buttons` and
# `flags` are unsigned DWORDs represented as doubles, not R integers.
signed_high_word <- function(x) {
  hi <- floor(x / 65536) %% 65536
  if (hi >= 32768) hi <- hi - 65536
  hi
}

windows_mouse_events <- function(x, y, buttons, flags, mods, previous) {
  # R bitw* coerces to signed integers; reduce DWORDs to safe low words.
  flags <- flags %% 65536
  alt <- bitwAnd(mods, 1L) > 0L
  shift <- bitwAnd(mods, 2L) > 0L
  ctrl <- bitwAnd(mods, 4L) > 0L
  make <- function(action, button = "none", direction = NA_character_) {
    MouseEvent$new(action, x, y, button = button, direction = direction, shift = shift, ctrl = ctrl, alt = alt)
  }
  if (bitwAnd(flags, 4L) > 0L || bitwAnd(flags, 8L) > 0L) {
    delta <- signed_high_word(buttons)
    vertical <- bitwAnd(flags, 4L) > 0L
    direction <- if (vertical) (if (delta > 0L) "up" else "down") else (if (delta > 0L) "right" else "left")
    return(list(make("scroll", direction = direction)))
  }
  buttons <- buttons %% 65536
  previous <- previous %% 65536
  names <- c("left", "right", "middle")
  bits <- c(1L, 2L, 4L)
  out <- list()
  for (i in seq_along(bits)) {
    now <- bitwAnd(buttons, bits[[i]]) > 0L
    before <- bitwAnd(previous, bits[[i]]) > 0L
    if (now && !before) out[[length(out) + 1L]] <- make("down", names[[i]])
    if (!now && before) out[[length(out) + 1L]] <- make("up", names[[i]])
  }
  if (length(out) == 0L && bitwAnd(flags, 1L) > 0L) {
    held <- names[vapply(bits, function(b) bitwAnd(buttons, b) > 0L, logical(1))]
    out[[1]] <- make("move", if (length(held)) held[[1]] else "none")
  }
  out
}

ansi_mouse <- function(enable) {
  modes <- c(1000L, 1002L, 1003L, 1006L)
  if (enable) paste0(ansi_csi, "?", modes, "h", collapse = "") else paste0(ansi_csi, "?", rev(modes), "l", collapse = "")
}
