feed_all <- function(parser, chunks) {
  out <- list()
  for (chunk in chunks) out <- c(out, parser$feed(chunk))
  out
}

test_that("bracketed paste becomes one PasteEvent", {
  p <- KeyParser$new()
  ev <- p$feed("a\u001b[200~hello\nworld\u001b[201~b")
  expect_length(ev, 3)
  expect_identical(ev[[1]]$key, "a")
  expect_s3_class(ev[[2]], "PasteEvent")
  expect_identical(ev[[2]]$text, "hello\nworld")
  expect_identical(ev[[3]]$key, "b")
})

test_that("paste survives arbitrary fragmentation", {
  whole <- "x\u001b[200~line1\r\nline2 \u00e9\U0001F600\u001b[201~y"
  chars <- strsplit(whole, "")[[1]]
  for (size in c(1L, 2L, 3L, 5L)) {
    p <- KeyParser$new()
    chunks <- split(chars, ceiling(seq_along(chars) / size))
    ev <- feed_all(p, vapply(chunks, paste, "", collapse = ""))
    types <- vapply(ev, function(e) class(e)[[1]], "")
    expect_identical(types, c("KeyEvent", "PasteEvent", "KeyEvent"), info = size)
    expect_identical(ev[[2]]$text, "line1\nline2 \u00e9\U0001F600", info = size)
  }
})

test_that("a paste cannot smuggle escape sequences or control characters", {
  p <- KeyParser$new()
  ev <- p$feed("\u001b[200~a\u001b[31mb\u0001c\u001b]52;c;AAAA\u0007\u001b[201~")
  expect_identical(ev[[1]]$text, "a[31mbc]52;c;AAAA")
  expect_false(grepl("\u001b", ev[[1]]$text))
  # An embedded end marker split across reads is still found exactly once.
  p <- KeyParser$new()
  ev <- feed_all(p, c("\u001b[200~ab\u001b[2", "01~cd"))
  expect_length(ev, 3)
  expect_identical(ev[[1]]$text, "ab")
  expect_identical(c(ev[[2]]$key, ev[[3]]$key), c("c", "d"))
})

test_that("an unfinished paste waits for more input", {
  p <- KeyParser$new()
  expect_length(p$feed("\u001b[200~partial"), 0)
  expect_false(p$has_pending())
  ev <- p$feed(" rest\u001b[201~")
  expect_identical(ev[[1]]$text, "partial rest")
})

test_that("large pastes are handled in linear time", {
  p <- KeyParser$new()
  body <- strrep("0123456789abcdef\n", 20000)
  chunks <- c("\u001b[200~", substring(body, seq(1, nchar(body), 4096), pmin(seq(4096, nchar(body) + 4095, 4096), nchar(body))), "\u001b[201~")
  elapsed <- system.time(ev <- feed_all(p, chunks))[["elapsed"]]
  expect_identical(nchar(ev[[1]]$text), nchar(body))
  expect_lt(elapsed, 5)
})

test_that("Input inserts a paste atomically and flattens line breaks", {
  a <- app(input(id = "i"))
  pilot <- test_app(a, 30, 3)
  pilot$type("ab")
  pilot$paste("one\ntwo\r\nthree")
  expect_identical(a$query_one("#i")$value, "abone two three")
  # Control characters are dropped; the cursor ends after the paste.
  pilot$paste("\u0001X\u001b[0m")
  expect_identical(a$query_one("#i")$value, "abone two threeX[0m")
  # Not treated as a submit.
  submitted <- 0
  a$on("input.submitted", function(event, app) submitted <<- submitted + 1)
  pilot$paste("a\nb")
  expect_identical(submitted, 0)
})

test_that("bracketed paste mode is enabled at start and disabled at exit", {
  a <- app(input(id = "i"))
  pilot <- test_app(a, 30, 3)
  expect_true(2004L %in% pilot$driver$terminal$modes)
  pilot$app$exit()
  pilot$step()
  expect_false(2004L %in% pilot$driver$terminal$modes)
})
