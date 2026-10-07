describe_events <- function(events) {
  vapply(events, function(e) {
    if (inherits(e, "KeyEvent")) paste0("key:", e$key)
    else if (inherits(e, "PasteEvent")) paste0("paste:", e$text)
    else if (inherits(e, "MouseEvent")) paste("mouse", e$type, e$button, e$screen_x, e$screen_y)
    else class(e)[[1]]
  }, "")
}

parse_stream <- function(chunks) {
  p <- KeyParser$new()
  out <- list()
  for (chunk in chunks) out <- c(out, p$feed(chunk))
  if (p$has_pending()) out <- c(out, p$flush())
  describe_events(out)
}

test_that("splitting the input stream anywhere never changes the events", {
  stream <- paste0(
    "ab\u00e9\U0001F600",
    "\u001b[A\u001b[1;5C\u001b[3~\u001bOP\u001b[15~\u001b[Z",
    "\u001bx",
    "\u001b[<0;12;7M\u001b[<0;12;7m\u001b[<64;3;3M",
    "\u001b[200~pasted\ntext \u00fc\u001b[201~",
    "z\r"
  )
  reference <- parse_stream(stream)
  expect_gt(length(reference), 15)
  chars <- strsplit(stream, "")[[1]]
  set.seed(42)
  for (trial in 1:60) {
    cuts <- sort(sample(seq_len(length(chars) - 1L), sample(1:12, 1)))
    pieces <- split(chars, findInterval(seq_along(chars), cuts + 0.5))
    chunks <- vapply(pieces, paste, "", collapse = "")
    expect_identical(parse_stream(chunks), reference, info = paste(cuts, collapse = ","))
  }
  # One character per read: the worst case.
  expect_identical(parse_stream(chars), reference)
})

test_that("a lone ESC is only reported when no more input follows", {
  p <- KeyParser$new()
  expect_length(p$feed("\u001b"), 0)
  expect_true(p$has_pending())
  expect_identical(describe_events(p$flush()), "key:escape")
  # ESC followed later by [ is one sequence, not escape + text.
  p <- KeyParser$new()
  p$feed("\u001b")
  expect_identical(describe_events(p$feed("[A")), "key:up")
})

test_that("unknown or broken sequences do not wedge the parser", {
  p <- KeyParser$new()
  ev <- p$feed("\u001b[99;99~x\u001b[?9999zy")
  expect_identical(tail(describe_events(ev), 2), c("key:x", "key:y"))
})

test_that("bytes that are not UTF-8 do not break the parser", {
  p <- KeyParser$new()
  bad <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  ev <- expect_no_error(p$feed(bad))
  keys <- describe_events(ev)
  expect_identical(keys[c(1L, 3L)], c("key:a", "key:b"))
})

test_that("an endless CSI sequence is dropped instead of buffered", {
  p <- KeyParser$new()
  p$feed("\u001b[")
  for (i in 1:20) p$feed(strrep("1", 50))
  expect_lt(nchar(p$pending), 100L)
  expect_identical(describe_events(p$feed("\u001b[A")), "key:up")
})
