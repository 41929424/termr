regions_of <- function(widgets) {
  lapply(widgets, function(w) unlist(unclass(w$region)))
}

test_that("distribute splits space exactly", {
  expect_identical(distribute(c(1, 1, 1), 10), c(4L, 3L, 3L))
  expect_identical(distribute(c(1, 2), 9), c(3L, 6L))
  expect_identical(distribute(c(1, 1), 0), c(0L, 0L))
  expect_identical(distribute(numeric(), 5), integer())
})

test_that("vertical layout stacks auto-height children", {
  a <- label("one")
  b <- label("two\nlines")
  root <- vertical(a, b)
  layout_tree(root, rect(1, 1, 20, 10))
  expect_identical(regions_of(list(a, b)), list(
    c(x = 1L, y = 1L, width = 3L, height = 1L),
    c(x = 1L, y = 2L, width = 5L, height = 2L)
  ))
})

test_that("fr children share the remaining height", {
  top <- label("top")
  a <- vertical(id = "a", style = style(height = "1fr"))
  b <- vertical(id = "b", style = style(height = "2fr"))
  root <- vertical(top, a, b)
  layout_tree(root, rect(1, 1, 10, 10))
  expect_identical(a$region$y, 2L)
  expect_identical(a$region$height, 3L)
  expect_identical(b$region$y, 5L)
  expect_identical(b$region$height, 6L)
})

test_that("horizontal layout splits width between fixed, auto and fr", {
  a <- label("abc")
  b <- label("x", style = style(width = 5))
  c <- label("y", style = style(width = "1fr"))
  root <- horizontal(a, b, c)
  layout_tree(root, rect(1, 1, 20, 3))
  expect_identical(vapply(list(a, b, c), function(w) w$region$x, 1L), c(1L, 4L, 9L))
  expect_identical(vapply(list(a, b, c), function(w) w$region$width, 1L), c(3L, 5L, 12L))
  # The horizontal container is as tall as its tallest child.
  expect_identical(natural_height(root, 20), 1L)
})

test_that("padding, border and margin shrink the content box", {
  inner <- label("hi", style = style(margin = c(1, 2)))
  box <- vertical(inner, style = style(border = "round", padding = c(0, 1)))
  layout_tree(box, rect(1, 1, 20, 8))
  expect_identical(unlist(unclass(inner$region)), c(x = 5L, y = 3L, width = 2L, height = 1L))
})

test_that("percent sizes and min/max limits are respected", {
  a <- vertical(style = style(width = "50%", height = 2))
  b <- vertical(style = style(height = "1fr", max_height = 3))
  root <- vertical(a, b)
  layout_tree(root, rect(1, 1, 30, 10))
  expect_identical(a$region$width, 15L)
  expect_identical(b$region$height, 3L)
})

test_that("align and valign position children inside a container", {
  a <- label("abcd")
  root <- vertical(a, style = style(align = "center", valign = "middle"))
  layout_tree(root, rect(1, 1, 10, 5))
  expect_identical(c(a$region$x, a$region$y), c(4L, 3L))
})

test_that("hidden widgets take no space", {
  a <- label("a")
  b <- label("b")
  c <- label("c")
  root <- vertical(a, b, c)
  b$visible <- FALSE
  layout_tree(root, rect(1, 1, 10, 5))
  expect_null(b$region)
  expect_identical(c$region$y, 2L)
})

test_that("auto sizes of nested containers are measured from children", {
  row <- horizontal(label("ab"), label("cdef"))
  col <- vertical(row, label("x\ny\nz"), style = style(width = "auto", height = "auto"))
  expect_identical(natural_width(col), 6L)
  expect_identical(natural_height(col, 6L), 4L)
})
