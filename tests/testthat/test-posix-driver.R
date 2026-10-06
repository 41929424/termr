# The Unix driver cannot run without a real terminal, but its input path can
# be exercised with a fake reader process.
fake_reader <- function(chunks) {
  env <- new.env()
  env$chunks <- chunks
  env$alive <- TRUE
  list(
    poll_io = function(ms) c(output = if (length(env$chunks)) "ready" else "timeout"),
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
  # A lone ESC is reported once no more input follows.
  expect_length(x$driver$read_events(0), 0)
  expect_identical(keys(x$driver$read_events(0)), "escape")
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
