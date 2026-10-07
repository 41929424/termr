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

test_that("UTF-8 decoding resynchronizes without locale conversions", {
  replacement <- "key:\ufffd"
  for (bad in list(c(0xff), c(0xe2, 0x82), c(0xe2, 0x28),
                   c(0xc0, 0xaf), c(0xed, 0xa0, 0x80), c(0xf4, 0x90, 0x80, 0x80))) {
    bytes <- as.raw(c(0x61, bad, 0x62))
    keys <- parse_stream(list(bytes))
    expect_identical(keys[[1]], "key:a")
    expect_identical(tail(keys, 1), "key:b")
    expect_true(replacement %in% keys)
    # Splitting at any byte preserves both replacement policy and keys.
    for (cut in seq_len(length(bytes) - 1L)) {
      expect_identical(parse_stream(list(bytes[seq_len(cut)], bytes[(cut + 1L):length(bytes)])), keys)
    }
  }
  p <- KeyParser$new()
  expect_length(p$feed(as.raw(c(0xe2, 0x82))), 0L)
  expect_true(p$has_pending())
  expect_identical(describe_events(p$flush()), replacement)
  expect_false(p$has_pending())
  expect_identical(describe_events(p$feed("b")), "key:b")
})

test_that("valid UTF-8 and bracketed paste survive splitting at every byte", {
  text <- "a\u00e9\u20ac\U0001f600\u001b[200~\u20ac\u001b[201~b"
  bytes <- charToRaw(enc2utf8(text))
  reference <- parse_stream(text)
  expect_identical(parse_stream(as.list(bytes)), reference)
  for (cut in seq_len(length(bytes) - 1L)) {
    expect_identical(parse_stream(list(bytes[seq_len(cut)], bytes[(cut + 1L):length(bytes)])), reference)
  }
})
