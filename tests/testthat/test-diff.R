test_that("identical buffers produce an empty patch", {
  a <- screen_buffer(10, 3)
  patch <- diff_screen(a, a$copy())
  expect_false(patch$full)
  expect_length(patch$runs, 0)
})

test_that("a single changed cell produces a single one-cell run", {
  a <- screen_buffer(10, 3)
  b <- a$copy()
  b$put_text(5, 2, "x")
  patch <- diff_screen(a, b)
  expect_length(patch$runs, 1)
  run <- patch$runs[[1]]
  expect_identical(c(run$x, run$y), c(5L, 2L))
  expect_identical(run$chars, "x")
})

test_that("style-only changes are detected", {
  a <- screen_buffer(4, 1)
  a$put_text(1, 1, "ab")
  b <- a$copy()
  b$put_text(2, 1, "b", fg = "red")
  patch <- diff_screen(a, b)
  expect_length(patch$runs, 1)
  expect_identical(patch$runs[[1]]$x, 2L)
  expect_identical(patch$runs[[1]]$fg, "red")
})

test_that("nearby changes are merged and distant changes are not", {
  a <- screen_buffer(20, 1)
  b <- a$copy()
  b$put_text(1, 1, "a")
  b$put_text(4, 1, "b")
  b$put_text(15, 1, "c")
  patch <- diff_screen(a, b)
  expect_length(patch$runs, 2)
  expect_identical(patch$runs[[1]]$chars, c("a", " ", " ", "b"))
  expect_identical(patch$runs[[2]]$x, 15L)
  expect_length(diff_screen(a, b, merge_gap = 0)$runs, 3)
})

test_that("runs never split a wide character", {
  a <- screen_buffer(6, 1)
  a$put_text(1, 1, "ab\u4e2d")
  b <- a$copy()
  b$fg[1, 4] <- "red" # change only the continuation cell's style
  patch <- diff_screen(a, b, merge_gap = 0)
  expect_identical(patch$runs[[1]]$x, 3L)
  expect_identical(patch$runs[[1]]$chars, c("\u4e2d", ""))
})

test_that("a missing or resized previous frame forces a full patch", {
  b <- screen_buffer(5, 2)
  b$put_text(1, 1, "hi")
  patch <- diff_screen(NULL, b)
  expect_true(patch$full)
  expect_length(patch$runs, 1)
  expect_true(diff_screen(screen_buffer(4, 2), b)$full)
})
