test_that("style() stores only the properties that were set", {
  st <- style(width = 20, padding = c(1, 2), bold = TRUE, foreground = "Red")
  expect_s3_class(st, "termr_style")
  expect_setequal(names(st$props), c("width", "padding", "bold", "foreground"))
  expect_identical(st$props$width, size_spec("fixed", 20L))
  expect_identical(st$props$padding, c(1L, 2L, 1L, 2L))
  expect_identical(st$props$foreground, "red")
})

test_that("sizes are parsed from numbers and strings", {
  expect_identical(parse_size(3), size_spec("fixed", 3L))
  expect_identical(parse_size("auto"), size_spec("auto"))
  expect_identical(parse_size("2fr"), size_spec("fr", 2))
  expect_identical(parse_size("50%"), size_spec("percent", 0.5))
  expect_error(style(width = "wide"), "Invalid `width`")
})

test_that("style() validates its arguments", {
  expect_error(style(border = "fancy"), "`border` must be one of")
  expect_error(style(bold = "yes"), "`bold` must be TRUE or FALSE")
  expect_error(style(min_width = -1), "non-negative")
  expect_error(style(background = "nope"), "Unknown colour")
})

test_that("merge_styles layers properties and state styles", {
  a <- style(width = 10, bold = TRUE, focus = style(background = "blue"))
  b <- style(width = 20, focus = style(foreground = "white"))
  m <- merge_styles(a, b)
  expect_identical(m$props$width, size_spec("fixed", 20L))
  expect_true(m$props$bold)
  expect_identical(m$states$focus$props$background, "blue")
  expect_identical(m$states$focus$props$foreground, "white")
})

test_that("resolve_style applies active states and inherits foreground", {
  st <- style(foreground = "red", focus = style(foreground = "green", underline = TRUE))
  parent <- resolve_style(style(foreground = "yellow"))
  plain <- resolve_style(st)
  focused <- resolve_style(st, "focus")
  expect_identical(plain$foreground, "red")
  expect_identical(focused$foreground, "green")
  expect_true(attrs_decode(focused$attrs)[["underline"]])
  child <- resolve_style(style(), parent = parent)
  expect_identical(child$foreground, "yellow")
  expect_null(child$background)
})

test_that("span() builds styled text that can be combined", {
  txt <- c(span("a"), "b\nc", span("d", style(bold = TRUE)))
  expect_identical(as.character(txt), "ab\ncd")
  lines <- text_lines(txt)
  expect_length(lines, 2)
  expect_identical(vapply(lines[[2]], `[[`, "", "text"), c("c", "d"))
  expect_identical(line_width(lines[[1]]), 2L)
})
