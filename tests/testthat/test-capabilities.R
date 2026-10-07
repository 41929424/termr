caps <- function(..., windows = FALSE) terminal_capabilities(env = c(...), windows = windows)

test_that("capabilities follow TERM, COLORTERM and NO_COLOR", {
  expect_identical(caps(TERM = "xterm-256color")$colors, "256")
  expect_identical(caps(TERM = "xterm", COLORTERM = "truecolor")$colors, "truecolor")
  expect_true(caps(TERM = "xterm", COLORTERM = "24bit")$truecolor)
  expect_identical(caps(TERM = "xterm-256color", NO_COLOR = "1")$colors, "none")
  # WT_SESSION describes a local Windows Terminal host, not the remote PTY.
  expect_identical(caps(TERM = "xterm-256color", WT_SESSION = "local")$colors, "truecolor")
  expect_identical(caps(TERM = "xterm-256color", WT_SESSION = "local",
                        SSH_CONNECTION = "client server 1234 22")$colors, "256")
  expect_identical(caps(TERM = "xterm-256color", WT_SESSION = "local",
                        SSH_CLIENT = "client 1234 22")$colors, "256")
  expect_identical(caps(TERM = "xterm")$colors, "16")
  expect_identical(caps(TERM = "dumb")$colors, "none")
})

test_that("a dumb terminal gets no interactive extras", {
  d <- caps(TERM = "dumb")
  expect_false(d$mouse)
  expect_false(d$bracketed_paste)
  expect_false(d$osc52)
  expect_false(d$hyperlinks)
})

test_that("common SSH TERM values do not imply newer terminal protocols", {
  terms <- c("xterm", "xterm-256color", "screen", "screen-256color",
             "tmux", "tmux-256color", "linux", "vt100")
  caps_by_term <- lapply(terms, function(term) caps(TERM = term))
  names(caps_by_term) <- terms

  expect_identical(unname(vapply(caps_by_term, `[[`, "", "colors")),
                   c("16", "256", "16", "256", "16", "256", "16", "none"))
  expect_false(caps_by_term$vt100$mouse)
  expect_false(caps_by_term$vt100$sgr_mouse)
  expect_false(caps_by_term$vt100$bracketed_paste)
  expect_false(caps_by_term$vt100$alternate_screen)
  expect_false(caps_by_term$vt100$synchronized_output)
  expect_false(caps_by_term$linux$mouse)
  expect_false(caps_by_term$linux$bracketed_paste)
  expect_false(caps_by_term$linux$synchronized_output)

  # Multiplexer TERM names do not establish support for synchronized output
  # or clipboard forwarding by the outside terminal.
  for (term in c("xterm", "xterm-256color", "screen", "screen-256color",
                 "tmux", "tmux-256color", "linux", "vt100")) {
    expect_false(caps_by_term[[term]]$synchronized_output)
    expect_false(caps_by_term[[term]]$osc52)
  }
  expect_true(caps(TERM = "xterm-256color", TERM_PROGRAM = "WezTerm")$synchronized_output)
  remote_desktop <- caps(TERM = "xterm-256color", TERM_PROGRAM = "iTerm.app",
                         SSH_TTY = "/dev/pts/4")
  expect_false(remote_desktop$synchronized_output)
  expect_false(remote_desktop$hyperlinks)
  expect_false(remote_desktop$osc52)
  tmux_capabilities <- caps(TERM = "tmux-256color", TERM_PROGRAM = "WezTerm")
  expect_false(tmux_capabilities$synchronized_output)
  expect_false(tmux_capabilities$hyperlinks)
  expect_false(tmux_capabilities$osc52)
  expect_true(caps(TERM = "xterm-256color", SSH_TTY = "/dev/pts/4",
                   TERMR_OSC52 = "1")$osc52)
  expect_no_error(caps(TERM = "xterm-256color", VTE_VERSION = "unknown"))
})

test_that("unsupported terminal protocols are omitted from mode changes", {
  driver <- HeadlessDriver$new()
  driver$capabilities <- caps(TERM = "vt100")
  driver$mouse <- TRUE
  expect_false(grepl("?1049h", driver$setup_sequence(), fixed = TRUE))
  expect_false(grepl("?1000h", driver$setup_sequence(), fixed = TRUE))
  expect_false(grepl("?2004h", driver$setup_sequence(), fixed = TRUE))
  expect_false(grepl("?1049l", driver$teardown_sequence(), fixed = TRUE))
  expect_false(grepl("?1000l", driver$teardown_sequence(), fixed = TRUE))
})

test_that("OSC 52 and hyperlinks are only enabled for recognised terminals", {
  expect_false(caps(TERM = "xterm-256color")$osc52)
  expect_true(caps(TERM = "xterm-kitty")$osc52)
  expect_true(caps(TERM = "xterm-256color", TERM_PROGRAM = "iTerm.app")$osc52)
  expect_true(caps(TERM = "xterm-256color", WT_SESSION = "x")$hyperlinks)
  expect_false(caps(TERM = "xterm-kitty", CI = "true")$osc52)
  expect_true(caps(TERM = "xterm-256color", TERMR_OSC52 = "1")$osc52)
  expect_false(caps(TERM = "xterm-kitty", TERMR_OSC52 = "0")$osc52)
  withr_opt <- options(termr.osc52 = TRUE)
  on.exit(options(withr_opt))
  expect_true(caps(TERM = "xterm")$osc52)
})

test_that("capabilities print and accept overrides", {
  x <- terminal_capabilities(env = c(TERM = "xterm"), windows = FALSE, overrides = list(mouse = FALSE))
  expect_false(x$mouse)
  expect_output(print(x), "termr_capabilities")
})

test_that("the headless driver exposes capabilities and applies them", {
  a <- app(label("x"))
  pilot <- test_app(a, 20, 3)
  expect_s3_class(pilot$driver$capabilities, "termr_capabilities")
  expect_true(pilot$driver$capabilities$osc52)
})

test_that("OSC 52 clipboard writes are base64 and bounded", {
  expect_identical(base64_encode(charToRaw("Man")), "TWFu")
  expect_identical(base64_encode(charToRaw("Ma")), "TWE=")
  expect_identical(base64_encode(charToRaw("M")), "TQ==")
  expect_identical(rawToChar(base64_decode("TWFuIQ==")), "Man!")
  text <- "caf\u00e9 \u001b[31m \U0001F600"
  seq <- ansi_osc52(text)
  expect_identical(substr(seq, 1, 7), "\u001b]52;c;")
  # The payload contains no control characters at all.
  payload <- sub("^\u001b\\]52;c;", "", sub("\u0007$", "", seq))
  expect_false(grepl("[^A-Za-z0-9+/=]", payload))
  decoded <- rawToChar(base64_decode(payload))
  Encoding(decoded) <- "UTF-8"
  expect_identical(decoded, text)
  long <- ansi_osc52(strrep("x", 500000))
  expect_lt(nchar(long), 140000)
})

test_that("app$clipboard_write uses OSC 52 when available and degrades otherwise", {
  a <- app(input(id = "i"))
  pilot <- test_app(a, 30, 3)
  expect_true(a$clipboard_write("hello"))
  expect_identical(a$clipboard, "hello")
  expect_identical(pilot$system_clipboard(), "hello")
  expect_false(a$clipboard_write("internal", system = FALSE))
  expect_identical(pilot$system_clipboard(), "hello")

  b <- app(label("x"))
  pb <- test_app(b, 30, 3)
  pb$driver$capabilities$osc52 <- FALSE
  expect_false(b$clipboard_write("quiet"))
  expect_identical(b$clipboard, "quiet")
  expect_null(pb$system_clipboard())
})

test_that("Input copy and cut reach the system clipboard", {
  a <- app(input("secret", id = "i"))
  pilot <- test_app(a, 30, 3)
  pilot$press("ctrl+a", "ctrl+x")
  expect_identical(a$clipboard, "secret")
  expect_identical(pilot$system_clipboard(), "secret")
  expect_identical(a$query_one("#i")$value, "")
})

test_that("hyperlink URLs are validated", {
  expect_true(safe_url("https://example.com/a?b=1"))
  expect_false(safe_url("javascript:alert(1)"))
  expect_false(safe_url("https://x.com/\u001b]52;c;AA"))
  expect_identical(ansi_hyperlink_open("ftp://x"), "")
})
