test_that("TextBuffer splits and joins lines", {
  b <- TextBuffer$new("one\ntwo\n")
  expect_identical(b$lines, c("one", "two", ""))
  expect_identical(b$text(), "one\ntwo\n")
  expect_identical(TextBuffer$new("")$lines, "")
  expect_identical(TextBuffer$new("a\r\nb\rc")$lines, c("a", "b", "c"))
  expect_identical(TextBuffer$new("a\tb")$lines, "a    b")
  expect_identical(TextBuffer$new("a\u001b[31mb")$lines, "a[31mb")
})

test_that("replace_range inserts and deletes across lines", {
  b <- TextBuffer$new("hello world\nsecond line\nthird")
  e <- b$replace_range(text_pos(1, 5), text_pos(1, 5), ",\nnew")
  expect_identical(b$lines, c("hello,", "new world", "second line", "third"))
  expect_identical(e$end, text_pos(2, 3))
  e2 <- b$replace_range(text_pos(1, 3), text_pos(3, 4), "")
  expect_identical(b$lines, c("helnd line", "third"))
  expect_identical(b$slice(text_pos(1, 0), text_pos(2, 2)), "helnd line\nth")
  # Reversed ranges are accepted.
  expect_identical(b$slice(text_pos(2, 2), text_pos(1, 0)), "helnd line\nth")
})

test_that("positions count graphemes, never splitting a cluster", {
  b <- TextBuffer$new("a\U0001F468\u200d\U0001F469\u200d\U0001F467b e\u0301x")
  expect_identical(b$line_length(1), 6L)
  b$replace_range(text_pos(1, 1), text_pos(1, 2), "")
  expect_identical(b$lines, "ab e\u0301x")
  b$replace_range(text_pos(1, 4), text_pos(1, 5), "")
  expect_identical(b$lines, "ab e\u0301")
  expect_identical(b$line_length(1), 4L)
  expect_identical(grapheme_slice("e\u0301bc", 1, 2), "b")
})

test_that("random edits keep the buffer equal to a string model", {
  set.seed(7)
  model <- "alpha\nbeta \u00e9\ngamma"
  b <- TextBuffer$new(model)
  for (i in 1:200) {
    lines <- b$lines
    r1 <- sample(length(lines), 1)
    r2 <- sample(r1:length(lines), 1)
    c1 <- sample(0:b$line_length(r1), 1)
    c2 <- sample(0:b$line_length(r2), 1)
    if (r1 == r2 && c2 < c1) c2 <- c1
    ins <- sample(c("", "x", "\n", "ab\ncd", "\u00e9"), 1)
    expected_removed <- b$slice(text_pos(r1, c1), text_pos(r2, c2))
    before <- b$text()
    e <- b$replace_range(text_pos(r1, c1), text_pos(r2, c2), ins)
    # undo by replacing the new lines with the old ones
    b$apply(e$first_row, length(e$new), e$old)
    expect_identical(b$text(), before)
    b$replace_range(text_pos(r1, c1), text_pos(r2, c2), ins)
  }
  expect_true(all(!grepl("\n", b$lines, fixed = TRUE)))
})

test_that("UndoStack merges typing, closes groups and is bounded", {
  b <- TextBuffer$new("")
  u <- UndoStack$new(max_entries = 3)
  type <- function(ch) {
    cur <- b$end_pos()
    e <- b$replace_range(cur, cur, ch)
    e$before <- cur
    e$after <- e$end
    e$kind <- "type"
    e$closes <- ch == " "
    u$push(e)
  }
  for (ch in strsplit("ab cd", "")[[1]]) type(ch)
  expect_identical(b$text(), "ab cd")
  expect_identical(u$steps(), 2L)
  edit <- u$undo(b)
  expect_identical(b$text(), "ab ")
  expect_identical(edit$before, text_pos(1, 3))
  u$undo(b)
  expect_identical(b$text(), "")
  expect_false(u$can_undo())
  expect_true(u$can_redo())
  u$redo(b)
  expect_identical(b$text(), "ab ")
  # A new edit clears the redo history.
  type("z")
  expect_false(u$can_redo())
  # Bounded.
  for (i in 1:10) {
    cur <- b$end_pos()
    e <- b$replace_range(cur, cur, "\nx")
    e$before <- cur
    e$after <- e$end
    e$kind <- "paste"
    e$closes <- TRUE
    u$push(e)
  }
  expect_lte(u$steps(), 3L)
})

test_that("search queries validate regexes and find matches in graphemes", {
  expect_error(search_query("a(", regex = TRUE), "Invalid regular expression")
  q <- search_query("an", case_sensitive = FALSE)
  m <- query_line_matches(q, "Banana AN")
  expect_identical(m$start, c(1L, 3L, 7L))
  expect_identical(m$end, c(3L, 5L, 9L))
  expect_identical(nrow(query_line_matches(search_query("an", TRUE), "Banana AN")), 2L)
  # Columns are graphemes even when earlier text has combining marks.
  m <- query_line_matches(search_query("b"), "e\u0301e\u0301b")
  expect_identical(c(m$start, m$end), c(2L, 3L))
  rq <- search_query("^\\d+$", regex = TRUE)
  expect_identical(query_matches(rq, c("12", "a1", "7")), c(TRUE, FALSE, TRUE))
  expect_identical(nrow(query_line_matches(search_query("x*", regex = TRUE), "abc")), 0L)
})

test_that("search_chunks scans lazily in both directions with wraparound", {
  x <- c("a", "b", "needle", "c", "needle", "d")
  q <- search_query("needle")
  calls <- 0L
  get <- function(i) {
    calls <<- calls + length(i)
    x[i]
  }
  expect_identical(search_chunks(q, 6, get, 1, 1), 3L)
  expect_identical(search_chunks(q, 6, get, 4, 1), 5L)
  expect_identical(search_chunks(q, 6, get, 6, 1), 3L)
  expect_identical(search_chunks(q, 6, get, 6, -1), 5L)
  expect_identical(search_chunks(q, 6, get, 2, -1), 5L)
  expect_true(is.na(search_chunks(q, 6, get, 6, 1, wrap = FALSE)))
  expect_true(is.na(search_chunks(search_query("zzz"), 6, get, 1, 1)))
  # Only chunks that are needed are read.
  big <- rep("x", 100000)
  big[[10]] <- "needle"
  read <- 0L
  expect_identical(search_chunks(q, 100000L, function(i) {
    read <<- read + length(i)
    big[i]
  }, 1, 1, chunk = 1000L), 10L)
  expect_identical(read, 1000L)
})
