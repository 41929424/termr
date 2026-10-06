#' @title Screen widget
#' @description The root of a widget tree. An [App] shows one screen at a
#'   time; the screen always fills the terminal. Bindings on the screen
#'   apply to every widget in it.
#' @export
Screen <- R6::R6Class(
  "Screen",
  inherit = Widget,
  public = list(
    #' @description The built-in style.
    default_style = function() {
      style(width = "1fr", height = "1fr", layout = "vertical", background = "$background")
    }
  )
)

#' @title Vertical container
#' @description Arranges children top to bottom. See [vertical()].
#' @rdname Vertical-class
#' @export
Vertical <- R6::R6Class(
  "Vertical",
  inherit = Widget,
  public = list(
    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "1fr", layout = "vertical")
  )
)

#' @title Horizontal container
#' @description Arranges children left to right. See [horizontal()].
#' @rdname Horizontal-class
#' @export
Horizontal <- R6::R6Class(
  "Horizontal",
  inherit = Widget,
  public = list(
    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "auto", layout = "horizontal")
  )
)

#' @title Grid container
#' @description Arranges children in a grid. See [grid_layout()].
#' @export
Grid <- R6::R6Class(
  "Grid",
  inherit = Widget,
  public = list(
    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "1fr", layout = "grid")
  )
)

#' Grid container
#'
#' Places children in a grid, row by row. Column (and optionally row)
#' tracks are sized like widgets: a number of cells, `"auto"` (natural size
#' of the content), `"<n>fr"` (share of the remaining space) or `"<n>%"`.
#' Rows not listed are `"auto"`. A child can cover several cells with
#' `style(column_span = 2, row_span = 2)`.
#'
#' Named `grid_layout()` because `grid()` would mask `graphics::grid()`.
#'
#' @param ... Child widgets.
#' @param columns A number of equal columns, or a vector of column sizes.
#' @param rows Optional vector of row sizes.
#' @param gap Space between cells: one value, or `c(rows, columns)`.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `Grid` widget.
#' @export
#' @examples
#' ui <- grid_layout(
#'   label("a"), label("b"), label("c"),
#'   label("wide", style = style(column_span = 2)),
#'   columns = c("1fr", "2fr"), gap = 1
#' )
#' render_widget(ui, 20, 5)
grid_layout <- function(..., columns = 2L, rows = NULL, gap = 0L, id = NULL, classes = NULL, style = NULL) {
  grid_style <- style(grid_columns = columns, grid_rows = rows, grid_gap = gap)
  Grid$new(..., id = id, classes = classes, style = merge_styles(grid_style, as_style(style)))
}

#' Layout containers
#'
#' `vertical()` stacks its children top to bottom; `horizontal()` places
#' them side by side. By default a vertical container fills the available
#' space, and a horizontal container is as tall as its tallest child.
#'
#' Children sized `"auto"` take their natural size, fixed sizes are exact,
#' and `"1fr"`, `"2fr"`, ... share the remaining space. Use `style(align =,
#' valign =)` on the container to position children that do not fill it.
#'
#' @param ... Child widgets.
#' @param id Optional identifier (for `#id` selectors).
#' @param classes Optional classes (for `.class` selectors).
#' @param style A [style()].
#' @return A `Vertical` or `Horizontal` widget.
#' @export
#' @examples
#' ui <- vertical(
#'   label("Title"),
#'   horizontal(label("left"), label("right"))
#' )
#' render_widget(ui, 20, 3)
vertical <- function(..., id = NULL, classes = NULL, style = NULL) {
  Vertical$new(..., id = id, classes = classes, style = style)
}

#' @rdname vertical
#' @export
horizontal <- function(..., id = NULL, classes = NULL, style = NULL) {
  Horizontal$new(..., id = id, classes = classes, style = style)
}
