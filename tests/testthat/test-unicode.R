test_that("text_cells splits ASCII into single-width cells", {
  cells <- text_cells("abc")
  expect_identical(cells$chars, c("a", "b", "c"))
  expect_identical(cells$widths, c(1L, 1L, 1L))
})

test_that("text_cells reports wide characters", {
  cells <- text_cells("a\u4e2db")
  expect_identical(cells$chars, c("a", "\u4e2d", "b"))
  expect_identical(cells$widths, c(1L, 2L, 1L))
  expect_identical(str_width("a\u4e2db"), 4L)
})

test_that("combining characters stay with their base character", {
  cells <- text_cells("e\u0301x")
  expect_identical(cells$chars, c("e\u0301", "x"))
  expect_identical(cells$widths, c(1L, 1L))
})

test_that("control characters are removed so text cannot inject escapes", {
  expect_identical(sanitize_text("a\033[31mb\tc"), "a[31mb c")
  expect_identical(text_cells("\033")$chars, character())
})

test_that("str_truncate and str_align respect display width", {
  expect_identical(str_truncate("hello", 3), "hel")
  expect_identical(str_truncate("\u4e2d\u6587", 3), "\u4e2d")
  expect_identical(str_align("ab", 6, "center"), "  ab  ")
  expect_identical(str_align("ab", 5, "right"), "   ab")
  expect_identical(str_align("abcdef", 3), "abc")
})

test_that("C1 control characters never reach the framebuffer", {
  csi <- intToUtf8(0x9b)
  expect_identical(sanitize_text(paste0("a", csi, "2Jb")), "a2Jb")
  expect_identical(cell(paste0("\033", csi))$char, " ")
  buf <- render_widget(label(paste0("x", csi, "y")), 5, 1)
  expect_false(any(grepl(csi, buf$chars, fixed = TRUE)))
  expect_identical(sanitize_text("caf\u00e9 \u00a0ok"), "caf\u00e9 \u00a0ok")
})
