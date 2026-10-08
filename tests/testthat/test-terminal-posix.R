test_that("POSIX driver chooses a controlling terminal or safe stdin TTY", {
  skip_on_os("windows")
  expect_identical(posix_select_terminal_path(tty_available = TRUE, stdin_is_tty = FALSE), "/dev/tty")
  expect_identical(posix_select_terminal_path(tty_available = FALSE, stdin_is_tty = TRUE), "/dev/stdin")
  err <- tryCatch(
    posix_select_terminal_path(tty_available = FALSE, stdin_is_tty = FALSE),
    error = identity
  )
  expect_s3_class(err, "error")
  if (inherits(err, "error")) {
    message <- tolower(conditionMessage(err))
    expect_match(message, "termr")
    expect_match(message, "interactive terminal")
    expect_match(message, "stdin is not a tty")
  }
})
