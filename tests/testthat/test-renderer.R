random_frame <- function(width, height, seed) {
  set.seed(seed)
  buf <- screen_buffer(width, height)
  palette <- c("", "red", "bright_blue", "196", "#123456")
  words <- c("hello", "\u4e2d\u6587", "x", "e\u0301", "termr", "   ")
  for (i in seq_len(8)) {
    buf$put_text(
      sample.int(width, 1) - 2L, sample.int(height, 1), sample(words, 1),
      fg = sample(palette, 1), bg = sample(palette, 1),
      attrs = sample(c(0L, 1L, 8L, 33L), 1)
    )
  }
  buf
}

test_that("applying a patch to the old screen reproduces the new screen", {
  for (seed in 1:25) {
    old <- random_frame(12, 4, seed)
    new <- random_frame(12, 4, seed + 1000)
    vt <- VirtualTerminal$new(12, 4)
    vt$feed(ansi_autowrap(FALSE))
    vt$feed(patch_to_ansi(diff_screen(NULL, old)))
    expect_true(vt$screen$equals(old), info = paste("initial frame, seed", seed))
    vt$feed(patch_to_ansi(diff_screen(old, new)))
    expect_true(vt$screen$equals(new), info = paste("seed", seed))
  }
})

test_that("Renderer writes a full frame first and then only changes", {
  written <- character()
  renderer <- Renderer$new(function(x) written <<- c(written, x), synchronized = FALSE)
  frame <- screen_buffer(20, 5)
  frame$put_text(1, 1, "status: idle")
  renderer$render(frame)
  expect_length(written, 1)
  expect_match(written[[1]], "\033[2J", fixed = TRUE)

  frame2 <- frame$copy()
  frame2$put_text(9, 1, "busy")
  renderer$render(frame2)
  expect_length(written, 2)
  expect_identical(written[[2]], "\033[1;9H\033[0mbusy\033[0m")

  renderer$render(frame2$copy())
  expect_length(written, 2)
  expect_identical(renderer$frames, 3L)
})

test_that("Renderer redraws everything after invalidate()", {
  written <- character()
  renderer <- Renderer$new(function(x) written <<- c(written, x))
  frame <- screen_buffer(5, 1)
  renderer$render(frame)
  renderer$invalidate()
  renderer$render(frame)
  expect_length(written, 2)
  expect_match(written[[2]], "\033[2J", fixed = TRUE)
  expect_match(written[[2]], "\033[?2026h", fixed = TRUE)
})

test_that("VirtualTerminal tracks private modes and SGR", {
  vt <- VirtualTerminal$new(10, 2)
  vt$feed("main")
  vt$feed(ansi_alt_screen(TRUE))
  expect_identical(vt$to_text()[[1]], "          ")
  vt$feed(paste0(ansi_cursor_to(2, 3), "\033[1;38;5;196;44mok"))
  cell <- vt$screen$get_cell(3, 2)
  expect_identical(cell$fg, "196")
  expect_identical(cell$bg, "blue")
  expect_true(attrs_decode(cell$attrs)[["bold"]])
  vt$feed(ansi_alt_screen(FALSE))
  expect_identical(vt$to_text()[[1]], "main      ")
  vt$feed(ansi_cursor_visible(FALSE))
  expect_false(vt$cursor_visible)
})
