#' Screen regions
#'
#' A region (rectangle) describes a region of the terminal in cells. Coordinates are
#' 1-based, like R indices and ANSI cursor positions: `x` is the column and
#' `y` is the row of the top-left cell.
#'
#' Rectangles are plain values (lists with class `termr_rect`), so they can
#' be compared with [identical()] and are cheap to create during layout.
#'
#' @param x,y Column and row of the top-left cell.
#' @param width,height Size in cells. Negative sizes are clamped to zero.
#' @return An object of class `termr_rect`.
#' @export
#' @examples
#' r <- region(1, 1, 40, 5)
#' r
region <- function(x = 1L, y = 1L, width = 0L, height = 0L) {
  rect(x, y, width, height)
}

# Internal constructor (not exported: `rect` would mask graphics::rect()).
rect <- function(x = 1L, y = 1L, width = 0L, height = 0L) {
  structure(
    list(
      x = as.integer(x),
      y = as.integer(y),
      width = max(0L, as.integer(width)),
      height = max(0L, as.integer(height))
    ),
    class = "termr_rect"
  )
}

#' @export
format.termr_rect <- function(x, ...) {
  sprintf("<rect x=%d y=%d width=%d height=%d>", x$x, x$y, x$width, x$height)
}

#' @export
print.termr_rect <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

rect_right <- function(r) r$x + r$width - 1L

rect_bottom <- function(r) r$y + r$height - 1L

rect_is_empty <- function(r) r$width <= 0L || r$height <= 0L

rect_intersect <- function(a, b) {
  x1 <- max(a$x, b$x)
  y1 <- max(a$y, b$y)
  x2 <- min(rect_right(a), rect_right(b))
  y2 <- min(rect_bottom(a), rect_bottom(b))
  rect(x1, y1, x2 - x1 + 1L, y2 - y1 + 1L)
}

rect_contains <- function(r, x, y) {
  x >= r$x & x <= rect_right(r) & y >= r$y & y <= rect_bottom(r)
}

# Shrink a rectangle by edges c(top, right, bottom, left).
rect_shrink <- function(r, edges) {
  rect(
    r$x + edges[[4]],
    r$y + edges[[1]],
    r$width - edges[[2]] - edges[[4]],
    r$height - edges[[1]] - edges[[3]]
  )
}

# Normalise CSS-like shorthand (1, 2 or 4 values) to c(top, right, bottom, left).
as_edges <- function(x, arg = "edges") {
  if (is.null(x)) return(c(0L, 0L, 0L, 0L))
  if (!is.numeric(x) || anyNA(x) || any(x < 0)) {
    stop(sprintf("`%s` must contain non-negative numbers.", arg), call. = FALSE)
  }
  x <- as.integer(x)
  switch(
    as.character(length(x)),
    "1" = rep(x, 4L),
    "2" = c(x[[1]], x[[2]], x[[1]], x[[2]]),
    "4" = x,
    stop(sprintf("`%s` must have 1, 2 or 4 values.", arg), call. = FALSE)
  )
}
