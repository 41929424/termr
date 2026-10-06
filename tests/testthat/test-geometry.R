test_that("rect clamps negative sizes and formats", {
  r <- rect(2, 3, -1, 4)
  expect_identical(r$width, 0L)
  expect_true(rect_is_empty(r))
  expect_identical(format(rect(1, 1, 40, 5)), "<rect x=1 y=1 width=40 height=5>")
})

test_that("rect_intersect and rect_contains", {
  a <- rect(1, 1, 10, 5)
  b <- rect(6, 3, 10, 10)
  i <- rect_intersect(a, b)
  expect_identical(unclass(i), unclass(rect(6, 3, 5, 3)))
  expect_true(rect_is_empty(rect_intersect(a, rect(20, 20, 2, 2))))
  expect_true(rect_contains(a, 10, 5))
  expect_false(rect_contains(a, 11, 5))
})

test_that("rect_shrink applies top/right/bottom/left edges", {
  r <- rect_shrink(rect(1, 1, 20, 10), c(1L, 2L, 3L, 4L))
  expect_identical(unclass(r), unclass(rect(5, 2, 14, 6)))
})

test_that("as_edges expands CSS-like shorthand", {
  expect_identical(as_edges(1), c(1L, 1L, 1L, 1L))
  expect_identical(as_edges(c(1, 2)), c(1L, 2L, 1L, 2L))
  expect_identical(as_edges(c(1, 2, 3, 4)), 1:4)
  expect_identical(as_edges(NULL), c(0L, 0L, 0L, 0L))
  expect_error(as_edges(c(1, 2, 3)), "1, 2 or 4")
  expect_error(as_edges(-1), "non-negative")
})
