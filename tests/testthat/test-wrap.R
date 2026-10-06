lines_text <- function(lines) vapply(lines, function(l) paste(vapply(l, `[[`, "", "text"), collapse = ""), "")

test_that("word wrap breaks at spaces and splits long words", {
  lines <- wrap_text_lines(text_lines("the quick brown fox jumps"), 10, "word")
  expect_identical(lines_text(lines), c("the quick", "brown fox", "jumps"))
  lines <- wrap_text_lines(text_lines("abcdefghijkl xy"), 5, "word")
  expect_identical(lines_text(lines), c("abcde", "fghij", "kl xy"))
})

test_that("char wrap breaks anywhere and keeps newlines", {
  lines <- wrap_text_lines(text_lines("abcdefg\nhi"), 3, "char")
  expect_identical(lines_text(lines), c("abc", "def", "g", "hi"))
  expect_identical(lines_text(wrap_text_lines(text_lines("abc"), 3, "none")), "abc")
})

test_that("wrapping respects display width and graphemes", {
  family <- "\U0001F468\u200d\U0001F469\u200d\U0001F467\u200d\U0001F466"
  lines <- wrap_text_lines(text_lines(paste0("\u4e2d\u6587", family, "ab")), 4, "char")
  expect_identical(lines_text(lines), c("\u4e2d\u6587", paste0(family, "ab")))
})

test_that("wrapping keeps span styles across lines", {
  txt <- c(span("aaa "), span("bbb ccc", style(foreground = "red")))
  lines <- wrap_text_lines(text_lines(txt), 7, "word")
  expect_identical(lines_text(lines), c("aaa bbb", "ccc"))
  expect_identical(lines[[2]][[1]]$style$props$foreground, "red")
  expect_length(lines[[1]], 2)
})

test_that("a wrapped label grows in height and re-wraps on resize", {
  lbl <- label(strrep("word ", 8), wrap = "word")
  a <- app(vertical(lbl, label("below", id = "below")))
  pilot <- test_app(a, 40, 6)
  expect_identical(lbl$region$height, 1L)
  pilot$resize(20, 6)
  expect_identical(lbl$region$height, 2L)
  pilot$resize(10, 6)
  expect_identical(lbl$region$height, 4L)
  expect_identical(pilot$query_one("#below")$region$y, 5L)
  expect_identical(pilot$screen_text()[1:2], c("word word ", "word word "))
})

test_that("wrapped text is aligned per line", {
  buf <- render_widget(label("aa bbbb", style = style(wrap = "word", align = "right", width = 6)), 6, 2)
  expect_identical(buf$to_text(), c("    aa", "  bbbb"))
  expect_error(style(wrap = "lines"), "wrap")
})

test_that("nested spans layer styles and carry links", {
  s <- span(c(span("a"), span("b", style(foreground = "red"))), style(bold = TRUE, foreground = "blue"), link = "https://r-project.org")
  segs <- unclass(s)
  expect_identical(segs[[1]]$style$props$foreground, "blue")
  expect_identical(segs[[2]]$style$props$foreground, "red")
  expect_true(segs[[2]]$style$props$bold)
  expect_identical(segs[[1]]$link, "https://r-project.org")
})
