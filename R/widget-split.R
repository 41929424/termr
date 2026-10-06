#' @title Split handle
#' @description The draggable divider of a [split_pane()].
#' @rdname SplitHandle-class
#' @keywords internal
#' @export
SplitHandle <- R6::R6Class(
  "SplitHandle",
  inherit = Widget,
  public = list(
    #' @field focusable The divider can be moved with the keyboard.
    focusable = TRUE,

    #' @description The built-in style.
    default_style = function() style(width = 1, height = 1, foreground = "$muted", focus = style(foreground = "$accent"),
                                    hover = style(foreground = "$accent")),

    #' @description Arrow keys move the divider (Shift: ten cells).
    default_bindings = function() {
      list(
        bind("left,up", "split_less"), bind("right,down", "split_more"),
        bind("shift+left,shift+up", "split_less_far"), bind("shift+right,shift+down", "split_more_far"),
        bind("home", "split_min"), bind("end", "split_max")
      )
    },

    #' @description Move the divider towards the first pane.
    action_split_less = function() private$pane()$move_divider(-1L),
    #' @description Move the divider towards the second pane.
    action_split_more = function() private$pane()$move_divider(1L),
    #' @description Move the divider ten cells towards the first pane.
    action_split_less_far = function() private$pane()$move_divider(-10L),
    #' @description Move the divider ten cells towards the second pane.
    action_split_more_far = function() private$pane()$move_divider(10L),
    #' @description Give the first pane its smallest size.
    action_split_min = function() private$pane()$set_ratio(0),
    #' @description Give the first pane its largest size.
    action_split_max = function() private$pane()$set_ratio(1),

    #' @description Dragging the divider resizes the panes.
    #' @param event A `MouseEvent`.
    on_drag_move = function(event) {
      pane <- private$pane()
      pane$drag_to(event$screen_x, event$screen_y)
      event$stop()
    },

    #' @description A press on the divider starts a drag.
    #' @param event A `MouseEvent`.
    on_mouse_down = function(event) event$stop(),

    #' @description Announce the new size when a drag ends.
    #' @param event A `MouseEvent`.
    on_drag_end = function(event) private$pane()$announce(),

    #' @description Draw the divider line.
    #' @param buffer A [ScreenBuffer].
    #' @param area Visible part of the region.
    #' @param st Computed style.
    paint = function(buffer, area, st) {
      r <- self$region
      vertical_line <- identical(private$pane()$direction, "horizontal")
      ascii <- !unicode_ok()
      char <- if (vertical_line) (if (ascii) "|" else "\u2502") else (if (ascii) "-" else "\u2500")
      if (self$focused) char <- if (vertical_line) (if (ascii) "#" else "\u2503") else (if (ascii) "=" else "\u2501")
      n <- if (vertical_line) r$height else r$width
      for (i in seq_len(n)) {
        x <- if (vertical_line) r$x else r$x + i - 1L
        y <- if (vertical_line) r$y + i - 1L else r$y
        buffer$put_text(x, y, char, fg = st$foreground, bg = st$background, attrs = st$attrs, clip = area)
      }
    }
  ),
  private = list(
    pane = function() self$parent
  )
)

#' @title SplitPane widget
#' @description Two panes with a draggable divider. See [split_pane()].
#' @rdname SplitPane-class
#' @export
SplitPane <- R6::R6Class(
  "SplitPane",
  inherit = Widget,
  public = list(
    #' @field direction `"horizontal"` (panes side by side) or `"vertical"`
    #'   (stacked).
    direction = "horizontal",
    #' @field min_size Smallest size of a pane in cells (when there is room).
    min_size = 3L,

    #' @description Create a split pane. See [split_pane()].
    #' @param first,second The two panes (widgets).
    #' @param direction,ratio,min_size See [split_pane()].
    #' @param id,classes,style See [Widget].
    initialize = function(first, second, direction = "horizontal", ratio = 0.5, min_size = 3L,
                          id = NULL, classes = NULL, style = NULL) {
      if (!is_widget(first) || !is_widget(second)) stop("`first` and `second` must be widgets.", call. = FALSE)
      super$initialize(id = id, classes = classes, style = style)
      self$direction <- check_choice(direction, c("horizontal", "vertical"), "direction")
      self$min_size <- max(0L, check_count(min_size, "min_size"))
      if (!is.numeric(ratio) || length(ratio) != 1L || is.na(ratio) || ratio < 0 || ratio > 1) {
        stop("`ratio` must be a number between 0 and 1.", call. = FALSE)
      }
      private$.ratio <- ratio
      self$mount(first, SplitHandle$new(), second)
    },

    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "1fr"),

    #' @description Split the content area between the panes and the divider.
    #' @param children Visible children.
    #' @param inner Content rectangle.
    #' @param st Computed style.
    arrange_children = function(children, inner, st) {
      all <- self$children
      horizontal <- self$direction == "horizontal"
      total <- if (horizontal) inner$width else inner$height
      shown <- vapply(all, function(w) w$visible, TRUE)
      rect_of <- function(offset, size) {
        if (horizontal) rect(inner$x + offset, inner$y, size, inner$height)
        else rect(inner$x, inner$y + offset, inner$width, size)
      }
      empty <- rect(inner$x, inner$y, 0L, 0L)
      rects <- list(empty, empty, empty)
      if (!shown[[1]] || !shown[[3]]) {
        # One pane hidden: the other one gets everything, no divider.
        rects[[if (shown[[1]]) 1L else 3L]] <- rect_of(0L, total)
      } else {
        first <- private$first_size(total)
        rects[[1]] <- rect_of(0L, first)
        rects[[2]] <- rect_of(first, min(1L, total))
        rects[[3]] <- rect_of(first + 1L, max(0L, total - first - 1L))
      }
      rects[shown]
    },

    #' @description Move the divider by some cells.
    #' @param cells Positive: towards the second pane.
    move_divider = function(cells) {
      total <- private$axis_total()
      if (total <= 1L) return(invisible(self))
      first <- private$first_size(total) + cells
      private$set_first(first, total)
      private$announce_later()
      invisible(self)
    },

    #' @description Set the share of the first pane.
    #' @param ratio A number between 0 and 1.
    set_ratio = function(ratio) {
      self$ratio <- ratio
      private$announce_later()
      invisible(self)
    },

    #' @description Place the divider under a screen position (drags).
    #' @param x,y Screen position.
    drag_to = function(x, y) {
      r <- self$region
      if (is.null(r)) return(invisible(self))
      st <- self$computed_style()
      inner <- content_rect(r, st)
      total <- private$axis_total()
      if (total <= 1L) return(invisible(self))
      first <- if (self$direction == "horizontal") x - inner$x else y - inner$y
      private$set_first(first, total)
      invisible(self)
    },

    #' @description Send `"splitpane.resized"` with the current ratio.
    announce = function() {
      self$post_message("splitpane.resized", list(ratio = private$.ratio))
    }
  ),
  active = list(
    #' @field ratio Share of the first pane (0 to 1). Assigning moves the
    #'   divider (limited by `min_size`).
    ratio = function(value) {
      if (missing(value)) return(private$.ratio)
      if (!is.numeric(value) || length(value) != 1L || is.na(value)) stop("`ratio` must be a number.", call. = FALSE)
      private$.ratio <- min(1, max(0, value))
      self$invalidate()
    },
    #' @field sizes The current sizes (cells) of the two panes.
    sizes = function(value) {
      if (!missing(value)) read_only("sizes")
      total <- private$axis_total()
      first <- private$first_size(total)
      c(first = first, second = max(0L, total - first - 1L))
    }
  ),
  private = list(
    .ratio = 0.5,

    axis_total = function() {
      r <- self$region
      if (is.null(r)) return(0L)
      inner <- content_rect(r, self$computed_style())
      if (self$direction == "horizontal") inner$width else inner$height
    },

    # Cells of the first pane for a total length (the divider takes one).
    first_size = function(total) {
      avail <- max(0L, total - 1L)
      first <- as.integer(round(private$.ratio * avail))
      lo <- min(self$min_size, avail %/% 2L)
      min(max(first, lo), avail - lo)
    },

    set_first = function(first, total) {
      avail <- max(1L, total - 1L)
      private$.ratio <- min(1, max(0, first / avail))
      self$invalidate()
    },

    announce_later = function() self$announce()
  )
)

#' Split pane
#'
#' Two panes separated by a divider the user can drag with the mouse
#' (press on the divider, move, release) or move with the keyboard (focus
#' the divider with Tab; arrows move it, Shift+arrows ten cells, Home / End
#' to the ends). Typical uses: a tree next to a preview, a table above a
#' detail view, an editor above its output.
#'
#' The panes fill their share of the space regardless of their own size
#' styles. `ratio` is the share of the first pane; `min_size` keeps both
#' panes from collapsing. The pane sends `"splitpane.resized"` (`ratio`)
#' when the user finishes moving the divider. **Experimental.**
#'
#' @param first,second The two panes.
#' @param direction `"horizontal"`: panes side by side, divider vertical;
#'   `"vertical"`: stacked panes.
#' @param ratio Share of the first pane, between 0 and 1.
#' @param min_size Smallest pane size in cells.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `SplitPane` widget with `$ratio` and `$sizes`.
#' @export
#' @examples
#' ui <- split_pane(label("left"), label("right"), ratio = 0.4)
#' render_widget(ui, 20, 2)$to_text()
split_pane <- function(first, second, direction = "horizontal", ratio = 0.5, min_size = 3L,
                       id = NULL, classes = NULL, style = NULL) {
  SplitPane$new(first, second, direction = direction, ratio = ratio, min_size = min_size,
                id = id, classes = classes, style = style)
}
