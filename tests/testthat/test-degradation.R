test_that("colour modes degrade from truecolor to 256, 16 and none", {
  frame <- screen_buffer(4, 1)
  frame$put_text(1, 1, "ab", fg = "#ff8700", bg = "196")
  ansi <- function(mode) patch_to_ansi(diff_screen(NULL, frame), color_mode = mode)
  expect_match(ansi("truecolor"), "38;2;255;135;0", fixed = TRUE)
  expect_match(ansi("256"), "38;5;208", fixed = TRUE)
  expect_match(ansi("16"), "0;33;101m", fixed = TRUE)
  expect_false(grepl("38;|48;|;3[0-9]|;4[0-9]", ansi("none")))
})

test_that("the colour mode is detected from the environment", {
  withr::local_options(termr.color_mode = NULL)
  withr::local_envvar(NO_COLOR = "1")
  expect_identical(detect_color_mode(), "none")
  withr::local_envvar(NO_COLOR = "", COLORTERM = "truecolor")
  expect_identical(detect_color_mode(), "truecolor")
  withr::local_options(termr.color_mode = "16")
  expect_identical(detect_color_mode(), "16")
})

test_that("options(termr.ascii = TRUE) uses plain ASCII symbols", {
  withr::local_options(termr.ascii = TRUE)
  ui <- vertical(
    panel(label("x"), title = "T"),
    checkbox("c"),
    radio_set(radio_button("r"), selected = "r"),
    tree_view(tree_node("root", "leaf", expanded = TRUE), style = style(height = 2)),
    scroll_view(lapply(1:10, function(i) label(i)), style = style(height = 2)),
    progress_bar(0.5),
    sparkline(1:4, style = style(width = 4))
  )
  text <- render_widget(ui, 20, 14)$to_text()
  expect_true(all(is_ascii(text)), info = paste(text, collapse = "\n"))
  expect_match(text[[1]], "^\\+ T -")
})
