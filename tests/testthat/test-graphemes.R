woman_technologist <- "\U0001F469\u200d\U0001F4BB"
family <- "\U0001F468\u200d\U0001F469\u200d\U0001F467\u200d\U0001F466"
rainbow_flag <- "\U0001F3F3\ufe0f\u200d\U0001F308"
red_heart <- "\u2764\ufe0f"
latvia <- "\U0001F1F1\U0001F1FB"
thumbs_medium <- "\U0001F44D\U0001F3FD"

test_that("grapheme clusters are single cells", {
  cases <- list(
    list("e\u0301", 1L),
    list("\U0001F600", 2L),
    list(woman_technologist, 2L),
    list(family, 2L),
    list(rainbow_flag, 2L),
    list(red_heart, 2L),
    list(latvia, 2L),
    list(thumbs_medium, 2L),
    list("\u4e2d", 2L),
    list("\u1100\u1161\u11a8", 2L) # Hangul syllable from jamo
  )
  for (case in cases) {
    cells <- text_cells(case[[1]])
    expect_identical(cells$chars, case[[1]], info = case[[1]])
    expect_identical(cells$widths, case[[2]], info = case[[1]])
  }
})

test_that("text presentation and lone code points keep their width", {
  expect_identical(text_cells("\u2764")$widths, 1L)
  expect_identical(text_cells("\u2764\ufe0e")$widths, 1L)
  # Three regional indicators: one flag plus a lone indicator.
  cells <- text_cells("\U0001F1F1\U0001F1FB\U0001F1FA")
  expect_identical(cells$chars, c(latvia, "\U0001F1FA"))
})

test_that("mixed ASCII, CJK and emoji text is measured per grapheme", {
  x <- paste0("a\u4e2d", woman_technologist, "b", latvia)
  cells <- text_cells(x)
  expect_identical(cells$chars, c("a", "\u4e2d", woman_technologist, "b", latvia))
  expect_identical(cells$widths, c(1L, 2L, 2L, 1L, 2L))
  expect_identical(str_width(x), 8L)
  expect_identical(str_width(c("abc", family, "")), c(3L, 2L, 0L))
  expect_identical(str_truncate(x, 4), "a\u4e2d")
  expect_identical(split_graphemes("ab"), c("a", "b"))
})

test_that("leading combining marks and invisible clusters", {
  cells <- text_cells("\u0301x")
  expect_identical(cells$chars, c(" \u0301", "x"))
  expect_identical(text_cells("a\u200bb")$chars, c("a", "b"))
})

test_that("graphemes are clipped and overwritten as a whole", {
  buf <- screen_buffer(4, 1)
  buf$put_text(1, 1, paste0("a", woman_technologist, "b"), clip = rect(1, 1, 2, 1))
  expect_identical(buf$chars[1, ], c("a", " ", " ", " "))
  buf$put_text(1, 1, family)
  expect_identical(buf$chars[1, 1:2], c(family, ""))
  buf$put_text(2, 1, "x")
  expect_identical(buf$chars[1, 1:2], c(" ", "x"))
  buf$set_cell(3, 1, cell(rainbow_flag))
  expect_identical(buf$chars[1, 3:4], c(rainbow_flag, ""))
})

test_that("the virtual terminal prints clusters and the diff round-trips", {
  old <- screen_buffer(10, 2)
  new <- old$copy()
  new$put_text(1, 1, paste0(family, "x", latvia, "e\u0301"))
  new$put_text(2, 2, paste0(red_heart, thumbs_medium))
  vt <- VirtualTerminal$new(10, 2)
  vt$feed(ansi_autowrap(FALSE))
  vt$feed(patch_to_ansi(diff_screen(old, new)))
  expect_true(vt$screen$equals(new))
  # 2 + 1 + 2 + 1 columns used, 4 left.
  expect_identical(vt$to_text()[[1]], paste0(family, "x", latvia, "e\u0301", strrep(" ", 4)))
})

test_that("Input moves and deletes whole graphemes", {
  inp <- input()
  pilot <- test_app(app(inp), 30, 3)
  pilot$type(paste0("a", woman_technologist, "b"))
  expect_identical(inp$cursor_position, 3L)
  pilot$press("left")
  expect_identical(inp$cursor_position, 2L)
  pilot$press("backspace")
  expect_identical(inp$value, "ab")
  pilot$press("home", "delete")
  expect_identical(inp$value, "b")
})

test_that("ZWJ sequences typed piece by piece merge into one character", {
  inp <- input()
  pilot <- test_app(app(inp), 30, 3)
  inp$insert("\U0001F469")
  inp$insert("\u200d")
  inp$insert("\U0001F4BB")
  expect_identical(inp$value, woman_technologist)
  expect_identical(inp$cursor_position, 1L)
  pilot$step()
  expect_identical(text_cells(pilot$screen_text()[[2]])$chars[[3]], woman_technologist)
})

test_that("a grapheme can be a key", {
  ev <- key_event(woman_technologist)
  expect_identical(ev$key, woman_technologist)
  expect_identical(ev$char, woman_technologist)
  expect_true(ev$is_printable())
})
