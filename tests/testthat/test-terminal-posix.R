test_that("POSIX driver chooses a controlling terminal or safe stdin TTY", {
  skip_on_os("windows")
  expect_identical(posix_select_terminal_path(tty_available = TRUE, stdin_is_tty = FALSE), "/dev/tty")
  expect_identical(posix_select_terminal_path(tty_available = FALSE, stdin_is_tty = TRUE), "/dev/stdin")
  expect_error(
    posix_select_terminal_path(tty_available = FALSE, stdin_is_tty = FALSE),
    "termr needs an interactive terminal.*stdin is not a TTY"
  )
})
