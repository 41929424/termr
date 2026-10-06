test_that("ANSI primitives produce the expected sequences", {
  expect_identical(ansi_cursor_to(3, 7), "\033[3;7H")
  expect_identical(ansi_cursor_visible(FALSE), "\033[?25l")
  expect_identical(ansi_alt_screen(TRUE), "\033[?1049h")
  expect_identical(ansi_autowrap(FALSE), "\033[?7l")
  expect_identical(ansi_reset(), "\033[0m")
})

test_that("SGR sequences encode attributes and colours", {
  expect_identical(ansi_sgr("", "", 0L), "\033[0m")
  expect_identical(
    ansi_sgr("red", "blue", attrs_encode(bold = TRUE, underline = TRUE)),
    "\033[0;1;4;31;44m"
  )
  expect_identical(ansi_sgr("#010203", "", 0L), "\033[0;38;2;1;2;3m")
  expect_identical(ansi_sgr(c("red", ""), "", 0L), c("\033[0;31m", "\033[0m"))
})

test_that("patch_to_ansi moves the cursor and only changes style when needed", {
  a <- screen_buffer(10, 2)
  b <- a$copy()
  b$put_text(2, 2, "ab", fg = "red")
  out <- patch_to_ansi(diff_screen(a, b))
  expect_identical(out, "\033[2;2H\033[0;31mab\033[0m")
})

test_that("patch_to_ansi skips continuation cells of wide characters", {
  a <- screen_buffer(4, 1)
  b <- a$copy()
  b$put_text(1, 1, "\u4e2d")
  out <- patch_to_ansi(diff_screen(a, b))
  wide <- "\u4e2d"
  expect_identical(out, paste0("\033[1;1H\033[0m", wide, "\033[0m"))
})

test_that("full patches clear the screen first", {
  b <- screen_buffer(3, 1)
  out <- patch_to_ansi(diff_screen(NULL, b))
  expect_identical(out, "\033[0m\033[2J")
})

test_that("strip_ansi removes escape sequences", {
  expect_identical(strip_ansi("\033[1;31mred\033[0m \033]0;title\007x"), "red x")
})
