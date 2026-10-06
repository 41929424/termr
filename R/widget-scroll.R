#' @title ScrollView widget
#' @description A container whose content can be larger than the widget.
#'   See [scroll_view()].
#' @rdname ScrollView-class
#' @export
ScrollView <- R6::R6Class(
  "ScrollView",
  inherit = Widget,
  public = list(
    #' @field focusable Scroll views can take focus so they can be scrolled
    #'   with the keyboard even without focusable children.
    focusable = TRUE,
    #' @field direction `"vertical"`, `"horizontal"` or `"both"`.
    direction = "vertical",
    #' @field scrollbars Draw scroll bars when the content overflows?
    scrollbars = TRUE,

    #' @description Create a scroll view. See [scroll_view()].
    #' @param ... Child widgets.
    #' @param direction Scroll direction.
    #' @param scrollbars Show scroll bars?
    #' @param focusable Can the scroll view itself be focused?
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(..., direction = "vertical", scrollbars = TRUE, focusable = TRUE,
                          id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      self$direction <- check_choice(direction, c("vertical", "horizontal", "both"), "direction")
      check_flag(scrollbars)
      check_flag(focusable)
      self$scrollbars <- scrollbars
      self$focusable <- focusable
      super$initialize(..., id = id, classes = classes, style = style, disabled = disabled)
      private$.state$offset_x <- 0L
      private$.state$offset_y <- 0L
    },

    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "1fr", layout = "vertical"),

    #' @description Arrow keys, Page Up/Down and Home/End scroll.
    default_bindings = function() {
      list(
        bind("up", "scroll_up"), bind("down", "scroll_down"),
        bind("left", "scroll_left"), bind("right", "scroll_right"),
        bind("pageup", "page_up"), bind("pagedown", "page_down"),
        bind("home", "scroll_home"), bind("end", "scroll_end")
      )
    },

    #' @description Scroll to an absolute position (in cells). Positions
    #'   are clamped to the scrollable range at the next layout.
    #' @param x,y New offsets; `NULL` keeps the current one.
    scroll_to = function(x = NULL, y = NULL) {
      ox <- private$.state$offset_x
      oy <- private$.state$offset_y
      if (!is.null(x)) private$.state$offset_x <- private$clamp(as.integer(round(x)), "x")
      if (!is.null(y)) private$.state$offset_y <- private$clamp(as.integer(round(y)), "y")
      if (ox != private$.state$offset_x || oy != private$.state$offset_y) {
        # Translation changes layout positions, but not natural extents.
        private$.dirty <- TRUE
        app <- self$app
        if (!is.null(app)) app$request_repaint(self)
        self$post_message("scroll.changed", list(x = private$.state$offset_x, y = private$.state$offset_y))
      }
      invisible(self)
    },

    #' @description Scroll relative to the current position.
    #' @param dx,dy Cells to scroll (negative: up/left).
    scroll_by = function(dx = 0L, dy = 0L) {
      self$scroll_to(private$.state$offset_x + dx, private$.state$offset_y + dy)
    },

    #' @description Scroll to the top (and left).
    scroll_home = function() self$scroll_to(x = 0L, y = 0L),

    #' @description Scroll to the bottom.
    scroll_end = function() self$scroll_to(y = .Machine$integer.max),

    #' @description Scroll so that a descendant is fully visible (as far as
    #'   possible). Uses the last layout; the app re-lays out afterwards.
    #' @param widget A descendant widget.
    #' @return `TRUE` if the offsets changed.
    scroll_into_view = function(widget) {
      r <- widget$region
      vp <- private$.viewport
      if (is.null(r) || is.null(vp) || rect_is_empty(vp)) return(invisible(FALSE))
      ox <- private$.state$offset_x
      oy <- private$.state$offset_y
      new_y <- reveal_offset(r$y - vp$y + oy, r$height, oy, vp$height)
      new_x <- reveal_offset(r$x - vp$x + ox, r$width, ox, vp$width)
      changed <- new_x != ox || new_y != oy
      self$scroll_to(new_x, new_y)
      invisible(changed)
    },

    #' @description The mouse wheel scrolls by three lines. When the view
    #'   cannot scroll further, the event bubbles to outer scroll views.
    #' @param event A `MouseEvent`.
    on_mouse_scroll = function(event) {
      before <- c(private$.state$offset_x, private$.state$offset_y)
      step <- 3L
      switch(
        event$direction,
        up = self$scroll_by(dy = -step),
        down = self$scroll_by(dy = step),
        left = self$scroll_by(dx = -step),
        right = self$scroll_by(dx = step)
      )
      if (!identical(before, c(private$.state$offset_x, private$.state$offset_y))) event$stop()
    },

    #' @description Scroll one line up.
    action_scroll_up = function() self$scroll_by(dy = -1L),
    #' @description Scroll one line down.
    action_scroll_down = function() self$scroll_by(dy = 1L),
    #' @description Scroll one column left.
    action_scroll_left = function() self$scroll_by(dx = -1L),
    #' @description Scroll one column right.
    action_scroll_right = function() self$scroll_by(dx = 1L),
    #' @description Scroll one page up.
    action_page_up = function() self$scroll_by(dy = -max(1L, self$viewport$height - 1L)),
    #' @description Scroll one page down.
    action_page_down = function() self$scroll_by(dy = max(1L, self$viewport$height - 1L)),
    #' @description Scroll to the top.
    action_scroll_home = function() self$scroll_home(),
    #' @description Scroll to the bottom.
    action_scroll_end = function() self$scroll_end(),

    #' @description Lay the children out in a virtual area, shifted by the
    #'   scroll offsets; reserves space for scroll bars when needed.
    #' @param children Visible children.
    #' @param inner Content rectangle.
    #' @param st Computed style.
    arrange_children = function(children, inner, st) {
      measure <- layout_algorithms[[st$layout]]$measure(children, st)
      can_x <- self$direction %in% c("horizontal", "both")
      can_y <- self$direction %in% c("vertical", "both")
      vw <- inner$width
      vh <- inner$height
      size <- function() {
        cw <- if (can_x) max(vw, measure$width()) else vw
        ch <- if (can_y) max(vh, measure$height(cw)) else vh
        c(cw, ch)
      }
      virtual <- size()
      bar_y <- self$scrollbars && can_y && virtual[[2]] > vh
      if (bar_y) {
        vw <- max(0L, vw - 1L)
        virtual <- size()
      }
      bar_x <- self$scrollbars && can_x && virtual[[1]] > vw
      if (bar_x) {
        vh <- max(0L, vh - 1L)
        virtual <- size()
        if (!bar_y && self$scrollbars && can_y && virtual[[2]] > vh) {
          bar_y <- TRUE
          vw <- max(0L, vw - 1L)
          virtual <- size()
        }
      }
      private$.viewport <- rect(inner$x, inner$y, vw, vh)
      private$.virtual <- as.integer(virtual)
      private$.bars <- c(x = bar_x, y = bar_y)
      # Content may have shrunk: keep offsets in range without invalidating.
      private$.state$offset_x <- private$clamp(private$.state$offset_x, "x")
      private$.state$offset_y <- private$clamp(private$.state$offset_y, "y")
      content <- rect(
        inner$x - private$.state$offset_x, inner$y - private$.state$offset_y,
        virtual[[1]], virtual[[2]]
      )
      layout_algorithms[[st$layout]]$arrange(children, content, st)
    },

    #' @description Children entirely outside the viewport are not laid out
    #'   in detail (unless a complete layout is needed, e.g. to scroll a
    #'   focused widget into view).
    #' @param rects Child rectangles.
    layout_descend = function(rects) {
      vp <- private$.viewport
      if (is.null(vp) || isTRUE(termr_env$full_layout)) return(rep(TRUE, length(rects)))
      vapply(rects, function(r) !rect_is_empty(rect_intersect(r, vp)), TRUE)
    },

    #' @description Children are clipped to the viewport.
    #' @param st Computed style.
    child_clip = function(st) private$.viewport %||% content_rect(self$region, st),

    #' @description Paint background, border and scroll bars.
    #' @param buffer A [ScreenBuffer].
    #' @param area Visible part of the region.
    #' @param st Computed style.
    paint = function(buffer, area, st) {
      super$paint(buffer, area, st)
      vp <- private$.viewport
      if (is.null(vp)) return(invisible())
      if (private$.bars[["y"]]) {
        draw_scrollbar(buffer, rect(rect_right(vp) + 1L, vp$y, 1L, vp$height),
                       private$.state$offset_y, private$.virtual[[2]], vp$height, "vertical", st, area)
      }
      if (private$.bars[["x"]]) {
        draw_scrollbar(buffer, rect(vp$x, rect_bottom(vp) + 1L, vp$width, 1L),
                       private$.state$offset_x, private$.virtual[[1]], vp$width, "horizontal", st, area)
      }
      invisible()
    }
  ),
  active = list(
    #' @field offset_x,offset_y Scroll offsets in cells (reactive).
    offset_x = function(value) {
      if (missing(value)) return(private$.state$offset_x)
      self$scroll_to(x = value)
    },
    offset_y = function(value) {
      if (missing(value)) return(private$.state$offset_y)
      self$scroll_to(y = value)
    },
    #' @field viewport The visible content rectangle (after the last layout).
    viewport = function(value) if (missing(value)) private$.viewport %||% rect(1L, 1L, 0L, 0L) else read_only("viewport"),
    #' @field virtual_size Size of the scrollable content: `c(width, height)`.
    virtual_size = function(value) if (missing(value)) structure(private$.virtual %||% c(0L, 0L), names = c("width", "height")) else read_only("virtual_size"),
    #' @field max_scroll Largest offsets: `c(x, y)`.
    max_scroll = function(value) if (missing(value)) c(x = private$max_offset("x"), y = private$max_offset("y")) else read_only("max_scroll")
  ),
  private = list(
    .viewport = NULL,
    .virtual = NULL,
    .bars = c(x = FALSE, y = FALSE),

    max_offset = function(axis) {
      vp <- private$.viewport
      virtual <- private$.virtual
      if (is.null(vp) || is.null(virtual)) return(.Machine$integer.max)
      if (axis == "x") max(0L, virtual[[1]] - vp$width) else max(0L, virtual[[2]] - vp$height)
    },

    clamp = function(value, axis) {
      as.integer(min(max(0L, value), private$max_offset(axis)))
    },

    state_changed = function(name, old, new) {
      if (name %in% c("offset_x", "offset_y")) {
        self$post_message("scroll.changed", list(x = private$.state$offset_x, y = private$.state$offset_y))
      }
    }
  )
)

# The offset that makes the span [pos, pos + size) visible in a viewport of
# `view` cells currently scrolled to `offset`. The start wins when the span
# is larger than the viewport.
reveal_offset <- function(pos, size, offset, view) {
  if (pos < offset) return(as.integer(pos))
  if (pos + size > offset + view) return(as.integer(max(0L, min(pos, pos + size - view))))
  as.integer(offset)
}

draw_scrollbar <- function(buffer, track, offset, virtual, view, orientation, st, clip) {
  if (rect_is_empty(track) || virtual <= view) return(invisible())
  length <- if (orientation == "vertical") track$height else track$width
  thumb <- max(1L, round(length * view / virtual))
  pos <- if (virtual > view) round((length - thumb) * offset / (virtual - view)) else 0L
  ascii <- !unicode_ok()
  track_char <- if (ascii) (if (orientation == "vertical") "|" else "-") else (if (orientation == "vertical") "\u2502" else "\u2500")
  thumb_char <- if (ascii) "#" else "\u2588"
  chars <- rep(track_char, length)
  chars[seq_len(thumb) + pos] <- thumb_char
  fg <- ifelse(chars == thumb_char, "", "$muted")
  for (i in seq_len(length)) {
    x <- if (orientation == "vertical") track$x else track$x + i - 1L
    y <- if (orientation == "vertical") track$y + i - 1L else track$y
    buffer$put_text(x, y, chars[[i]], fg = fg[[i]], bg = st$background, clip = clip)
  }
  invisible()
}

#' Scrollable container
#'
#' Shows content that is larger than the available space. The content is
#' laid out at its natural size and the visible part (the viewport) is
#' shifted by the scroll offsets; everything outside is clipped. Scroll bars
#' appear when the content overflows.
#'
#' Keys (when the scroll view or an unfocusable descendant has focus, or a
#' focused child does not use the key itself): arrows scroll by one cell,
#' Page Up / Page Down by a page, Home / End to the start / end. When focus
#' moves to a widget inside a scroll view, the view scrolls to reveal it.
#' Offset changes send a `"scroll.changed"` message.
#'
#' @param ... Child widgets.
#' @param direction `"vertical"` (default), `"horizontal"` or `"both"`.
#' @param scrollbars Draw scroll bars when the content overflows?
#' @param focusable Can the scroll view take focus itself?
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()]; by default the view fills its parent.
#' @return A `ScrollView` widget with methods `scroll_to(x, y)`,
#'   `scroll_by(dx, dy)`, `scroll_home()`, `scroll_end()` and
#'   `scroll_into_view(widget)`, and fields `offset_x`, `offset_y`,
#'   `viewport`, `virtual_size` and `max_scroll`.
#' @export
#' @examples
#' items <- lapply(1:100, function(i) label(paste("Item", i)))
#' view <- scroll_view(items, id = "list")
#' view$scroll_to(y = 50)
scroll_view <- function(..., direction = "vertical", scrollbars = TRUE, focusable = TRUE,
                        id = NULL, classes = NULL, style = NULL) {
  ScrollView$new(..., direction = direction, scrollbars = scrollbars, focusable = focusable,
                 id = id, classes = classes, style = style)
}
