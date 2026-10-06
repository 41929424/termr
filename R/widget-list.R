# ListBase: a virtualised list of one-line items with an optional cursor.
#
# Subclasses provide item_count() and item_text(i) (and optionally
# item_style(i)); ListBase handles the cursor, scrolling, the scroll bar,
# keyboard and mouse. Only the items inside the viewport are rendered.
# OptionList and LogView are built on it.

ListBase <- R6::R6Class(
  "ListBase",
  inherit = Widget,
  public = list(
    #' @field paint_states Cursor and scroll changes only repaint.
    paint_states = c("cursor", "offset"),
    focusable = TRUE,
    # Without a cursor the arrow keys scroll.
    has_cursor = TRUE,

    initialize = function(..., id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      private$.state$cursor <- 0L
      private$.state$offset <- 0L
    },

    default_style = function() style(width = "1fr", height = "1fr"),

    default_bindings = function() {
      list(
        bind("up", "list_up"), bind("down", "list_down"),
        bind("pageup", "list_page_up"), bind("pagedown", "list_page_down"),
        bind("home", "list_first"), bind("end", "list_last"),
        bind("enter", "list_activate")
      )
    },

    item_count = function() 0L,
    item_text = function(i) "",
    item_style = function(i) NULL,
    # Hooks for subclasses.
    on_highlight = function(i) invisible(),
    on_activate = function(i) invisible(),

    # Move the cursor to item i (clamped) and scroll it into view.
    highlight = function(i) {
      n <- self$item_count()
      if (n == 0L || !self$has_cursor) return(invisible(self))
      i <- as.integer(min(max(1L, i), n))
      changed <- i != private$.state$cursor
      self$set_state("cursor", i)
      private$reveal(i)
      if (changed) self$on_highlight(i)
      invisible(self)
    },

    scroll_to_item = function(i) {
      self$set_state("offset", private$clamp_offset(as.integer(i) - 1L))
      invisible(self)
    },

    scroll_to_end = function() self$scroll_to_item(.Machine$integer.max),

    action_list_up = function() private$step(-1L),
    action_list_down = function() private$step(1L),
    action_list_page_up = function() private$step(-max(1L, private$page() - 1L)),
    action_list_page_down = function() private$step(max(1L, private$page() - 1L)),
    action_list_first = function() private$step(-.Machine$integer.max),
    action_list_last = function() private$step(.Machine$integer.max),
    action_list_activate = function() {
      i <- private$.state$cursor
      if (self$has_cursor && i >= 1L && i <= self$item_count()) self$on_activate(i)
    },

    on_mouse_down = function(event) {
      if (event$button != "left" || is.null(self$region)) return(invisible())
      i <- private$item_at(event$screen_y)
      if (!is.na(i)) {
        self$highlight(i)
        event$stop()
      }
    },

    on_click = function(event) {
      i <- private$item_at(event$screen_y)
      if (!is.na(i) && self$has_cursor && event$button == "left") {
        self$on_activate(i)
        event$stop()
      }
    },

    on_mouse_scroll = function(event) {
      delta <- switch(event$direction, up = -3L, down = 3L, 0L)
      if (delta != 0L) {
        self$set_state("offset", private$clamp_offset(private$.state$offset + delta))
        event$stop()
      }
    },

    content_width = function() {
      n <- self$item_count()
      if (n == 0L) return(0L)
      idx <- sample_rows(n)
      max(vapply(idx, function(i) line_width(text_lines(self$item_text(i))[[1]]), integer(1))) + 1L
    },

    content_height = function(width) min(self$item_count(), 1000L),

    paint = function(buffer, area, st) {
      draw_background(buffer, area, st)
      draw_border(buffer, self$region, st, area, self$border_title)
      inner <- content_rect(self$region, st)
      n <- self$item_count()
      h <- inner$height
      bar <- n > h
      width <- inner$width - bar
      private$.state$offset <- private$clamp_offset(private$.state$offset)
      off <- private$.state$offset
      clip <- rect_intersect(rect(inner$x, inner$y, width, h), area)
      focused <- self$focused
      for (k in seq_len(min(h, n - off))) {
        i <- off + k
        y <- inner$y + k - 1L
        cursor <- self$has_cursor && i == private$.state$cursor
        item_st <- resolve_style(self$item_style(i) %||% new_style(), parent = st) |> keep_background(st)
        if (cursor) {
          item_st <- resolve_style(if (focused) table_styles$cursor else table_styles$cursor_blur, parent = item_st)
        }
        if (!is.null(item_st$background)) {
          buffer$fill(rect_intersect(rect(inner$x, y, width, 1L), clip), " ", bg = item_st$background)
        }
        draw_lines(buffer, rect(inner$x, y, width, 1L), text_lines(self$item_text(i))[1], item_st, clip)
      }
      if (bar) draw_scrollbar(buffer, rect(inner$x + width, inner$y, 1L, h), off, n, h, "vertical", st, area)
      invisible()
    }
  ),
  active = list(
    cursor = function(value) {
      if (missing(value)) return(private$.state$cursor)
      self$highlight(value)
    },
    offset = function(value) if (missing(value)) private$.state$offset else read_only("offset")
  ),
  private = list(
    page = function() {
      if (is.null(self$region)) return(1L)
      max(1L, content_rect(self$region, self$computed_style())$height)
    },
    clamp_offset = function(offset) {
      as.integer(min(max(0L, offset), max(0L, self$item_count() - private$page())))
    },
    reveal = function(i) {
      h <- private$page()
      off <- private$.state$offset
      if (i <= off) off <- i - 1L
      if (i > off + h) off <- i - h
      self$set_state("offset", as.integer(max(0L, off)))
    },
    step = function(delta) {
      if (self$has_cursor) {
        target <- as.numeric(private$.state$cursor) + delta
        self$highlight(max(1, min(self$item_count(), target)))
      } else {
        target <- as.numeric(private$.state$offset) + delta
        self$set_state("offset", private$clamp_offset(min(target, .Machine$integer.max)))
      }
    },
    item_at = function(y) {
      inner <- content_rect(self$region, self$computed_style())
      i <- private$.state$offset + y - inner$y + 1L
      if (i < 1L || i > self$item_count() || y < inner$y || y > rect_bottom(inner)) NA_integer_ else i
    }
  )
)

#' @title OptionList widget
#' @description A list of choices. See [option_list()].
#' @rdname OptionList-class
#' @export
OptionList <- R6::R6Class(
  "OptionList",
  inherit = ListBase,
  public = list(
    #' @description Create an option list. See [option_list()].
    #' @param choices Choices.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(choices, id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      self$set_choices(choices)
    },
    #' @description Replace the choices.
    #' @param choices Character vector (named: names are shown).
    set_choices = function(choices) {
      private$.choices <- as_choices(choices)
      private$.state$cursor <- if (length(private$.choices$values)) 1L else 0L
      private$.state$offset <- 0L
      self$invalidate()
      invisible(self)
    },
    #' @description Number of choices.
    item_count = function() length(private$.choices$values),
    #' @description Label of choice `i`.
    #' @param i Index.
    item_text = function(i) private$.choices$labels[[i]],
    #' @description Sends `"option_list.highlighted"`.
    #' @param i Index.
    on_highlight = function(i) self$post_message("option_list.highlighted", private$choice(i)),
    #' @description Sends `"option_list.selected"`.
    #' @param i Index.
    on_activate = function(i) self$post_message("option_list.selected", private$choice(i))
  ),
  active = list(
    #' @field value Value of the highlighted choice (`NULL` if none).
    value = function(value) {
      if (missing(value)) {
        i <- private$.state$cursor
        return(if (i >= 1L) private$.choices$values[[i]] else NULL)
      }
      i <- match(value, private$.choices$values)
      if (is.na(i)) stop(sprintf("\"%s\" is not one of the choices.", value), call. = FALSE)
      self$highlight(i)
    },
    #' @field choices The choice values.
    choices = function(value) if (missing(value)) private$.choices$values else read_only("choices")
  ),
  private = list(
    .choices = NULL,
    choice = function(i) list(index = i, value = private$.choices$values[[i]], label = private$.choices$labels[[i]])
  )
)

as_choices <- function(choices) {
  if (is.factor(choices)) choices <- as.character(choices)
  if (!is.atomic(choices) || anyNA(choices)) stop("`choices` must be a vector without missing values.", call. = FALSE)
  values <- as.character(choices)
  labels <- names(choices) %||% values
  labels[!nzchar(labels)] <- values[!nzchar(labels)]
  list(values = values, labels = labels)
}

#' Option list
#'
#' A scrollable list of choices. Up/Down/Page Up/Page Down/Home/End move the
#' highlight (sending `"option_list.highlighted"`); Enter or a click selects
#' (`"option_list.selected"`). `event$data` has `index`, `value` and
#' `label`.
#'
#' @param choices A character vector. Names, if present, are shown instead
#'   of the values.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return An `OptionList` widget with field `value`.
#' @export
#' @examples
#' option_list(c(Comma = "csv", JSON = "json", Parquet = "parquet"))
option_list <- function(choices, id = NULL, classes = NULL, style = NULL) {
  OptionList$new(choices, id = id, classes = classes, style = style)
}
