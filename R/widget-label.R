#' @title Label widget
#' @description Displays text. See [label()].
#' @rdname Label-class
#' @export
Label <- R6::R6Class(
  "Label",
  inherit = Widget,
  public = list(
    #' @description Create a label.
    #' @param text A string, character vector (one element per line) or
    #'   [span()]s.
    #' @param id,classes,style,visible See [Widget].
    initialize = function(text = "", id = NULL, classes = NULL, style = NULL, visible = TRUE) {
      super$initialize(id = id, classes = classes, style = style, visible = visible)
      private$.state$text <- check_text(text)
    },

    #' @description The built-in style.
    default_style = function() style(width = "auto", height = "auto"),

    #' @description Replace the text.
    #' @param text New text.
    update = function(text = "") {
      self$text <- text
      invisible(self)
    },

    #' @description The label content.
    render = function() private$.state$text
  ),
  active = list(
    #' @field text The label text (reactive).
    text = function(value) {
      if (missing(value)) return(private$.state$text)
      self$set_state("text", check_text(value))
    }
  ),
  private = list(
    describe = function() {
      txt <- as.character(as_text(private$.state$text))
      encodeString(if (nchar(txt) > 30L) paste0(substr(txt, 1L, 29L), "\u2026") else txt, quote = "\"")
    }
  )
)

#' Text label
#'
#' @param text A string, a character vector (one element per line), a
#'   number, or [span()]s for styled text. A function of no arguments is
#'   re-evaluated whenever the [signal()]s it reads change.
#' @param id Optional identifier (for `#id` selectors).
#' @param classes Optional classes (for `.class` selectors).
#' @param style A [style()].
#' @param wrap Shortcut for `style(wrap = )`: `"none"`, `"word"` or
#'   `"char"`.
#' @return A `Label` widget. Change its text with `$update()` or by
#'   assigning `$text`.
#' @export
#' @examples
#' lbl <- label("Hello", style = style(bold = TRUE))
#' lbl$update("Goodbye")
#' render_widget(lbl, 10, 1)
label <- function(text = "", id = NULL, classes = NULL, style = NULL, wrap = NULL) {
  if (!is.null(wrap)) style <- merge_styles(as_style(style), style(wrap = wrap))
  fn <- if (is.function(text)) text
  w <- Label$new(if (is.null(fn)) text else "", id = id, classes = classes, style = style)
  if (!is.null(fn)) w$bind_reactive("text", fn)
  w
}

check_text <- function(text) {
  if (is.null(text)) return("")
  if (inherits(text, "termr_text")) return(text)
  if (is.character(text) || is.numeric(text) || is.logical(text) || is.factor(text)) {
    return(paste(format_value(text), collapse = "\n"))
  }
  stop("Text must be a character vector, a number or span()s.", call. = FALSE)
}
