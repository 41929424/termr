# The Unix driver cannot run without a real terminal, but its input path can
# be exercised with a fake reader process.
fake_reader <- function(chunks) {
  env <- new.env()
  env$chunks <- chunks
  env$alive <- TRUE
  list(
    # An NA chunk is a poll that finds nothing yet (a gap between fragments).
    poll_io = function(ms) {
      if (length(env$chunks) && is.na(env$chunks[[1]])) {
        env$chunks <- env$chunks[-1]
        return(c(output = "timeout"))
      }
      c(output = if (length(env$chunks)) "ready" else "timeout")
    },
    read_output = function() {
      out <- env$chunks[[1]]
      env$chunks <- env$chunks[-1]
      out
    },
    is_alive = function() env$alive,
    die = function() env$alive <- FALSE
  )
}

posix_with <- function(chunks) {
  driver <- PosixDriver$new(color_mode = "16")
  p <- driver$.__enclos_env__$private
  reader <- fake_reader(chunks)
  p$reader <- reader
  p$last_size <- c(width = 80L, height = 24L)
  p$last_size_check <- now_seconds() + 3600
  list(driver = driver, reader = reader)
}

test_that("the Unix driver turns terminal input into key and mouse events", {
  x <- posix_with(list("ab\033[A", "\033[<0;3;4M\033[<0;3;4m", "\033"))
  keys <- function(evs) vapply(evs, function(e) if (inherits(e, "KeyEvent")) e$key else e$type, "")
  expect_identical(keys(x$driver$read_events(0)), c("a", "b", "up"))
  expect_identical(keys(x$driver$read_events(0)), c("mouse.down", "mouse.up"))
  # A lone ESC is reported once no more input follows for the escape timeout.
  expect_length(x$driver$read_events(0), 0)
  expect_length(x$driver$read_events(0), 0)
  x$driver$.__enclos_env__$private$pending_since <- now_seconds() - 1
  expect_identical(keys(x$driver$read_events(0)), "escape")
})

test_that("a fragmented sequence is not cut by short polls between its fragments", {
  x <- posix_with(list("\033", "[1;", "5C"))
  keys <- function(evs) vapply(evs, function(e) if (inherits(e, "KeyEvent")) e$key else e$type, "")
  got <- character()
  # An event loop with work to do polls with a zero timeout in between.
  for (i in 1:8) got <- c(got, keys(x$driver$read_events(0)))
  expect_identical(got, "ctrl+right")
})

test_that("the escape timeout is measured from the last byte and can be widened for tests", {
  withr::local_envvar(TERMR_ESC_TIMEOUT_MS = "250")
  expect_identical(PosixDriver$new(color_mode = "16")$escape_timeout_ms, 250L)
  withr::local_envvar(TERMR_ESC_TIMEOUT_MS = "5")
  expect_identical(PosixDriver$new(color_mode = "16")$escape_timeout_ms, 30L)
  withr::local_envvar(TERMR_ESC_TIMEOUT_MS = NA)
  expect_identical(PosixDriver$new(color_mode = "16")$escape_timeout_ms, 30L)
})

test_that("the Unix driver reports a dead input reader", {
  x <- posix_with(list())
  x$reader$die()
  expect_error(x$driver$read_events(0), "input reader stopped")
})

test_that("stop() is safe when the driver never started", {
  driver <- PosixDriver$new()
  expect_silent(driver$stop())
  expect_false(driver$started)
})

test_that("the processx input pipe preserves UTF-8 independently of locale", {
  path <- tempfile("termr-input-bytes-")
  bytes <- charToRaw(enc2utf8("a\u00e9\u20ac\U0001f600b"))
  writeBin(bytes, path)
  on.exit(unlink(path), add = TRUE)
  # Copy binary data without R's platform-specific stdout text conversion.
  command <- if (.Platform$OS.type == "windows") Sys.getenv("COMSPEC", "cmd.exe") else "cat"
  args <- if (.Platform$OS.type == "windows") c("/c", "type", normalizePath(path)) else path
  p <- processx::process$new(command, args, stdin = NULL, stdout = "|", stderr = "|", encoding = "UTF-8")
  on.exit(p$kill_tree(), add = TRUE)
  p$wait(5000)
  expect_false(p$is_alive())
  chunk <- p$read_all_output()
  expect_identical(charToRaw(chunk), bytes)
  x <- posix_with(list(chunk))
  events <- x$driver$read_events(0)
  expect_identical(vapply(events, function(e) e$key, ""), c("a", "\u00e9", "\u20ac", "\U0001f600", "b"))
})
