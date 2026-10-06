keys_of <- function(events) vapply(events, function(e) e$key, character(1))

test_that("key names are normalised", {
  expect_identical(normalize_key("Ctrl+C"), "ctrl+c")
  expect_identical(normalize_key("shift+ctrl+Tab"), "ctrl+shift+tab")
  expect_identical(normalize_key("ESC"), "escape")
  expect_identical(normalize_key("Return"), "enter")
  expect_identical(normalize_key("shift+a"), "A")
  expect_identical(normalize_key("ctrl+A"), "ctrl+a")
  expect_identical(normalize_key("ctrl+shift+a"), "ctrl+shift+a")
  expect_identical(normalize_key("q"), "q")
  expect_identical(normalize_key(" "), "space")
  expect_identical(normalize_key("ctrl++"), "ctrl++")
  expect_identical(normalize_key("+"), "+")
  expect_error(normalize_key("hyper+x"), "unknown modifier")
  expect_error(normalize_key("banana"), "Unknown key")
})

test_that("KeyEvent exposes modifiers and characters", {
  ev <- key_event("ctrl+shift+tab")
  expect_true(ev$ctrl)
  expect_true(ev$shift)
  expect_false(ev$alt)
  expect_identical(ev$char, "")
  expect_identical(key_event("x")$char, "x")
  expect_true(key_event("x")$is_printable())
  expect_identical(key_event("space")$char, " ")
  expect_false(key_event("alt+x")$is_printable())
  expect_identical(split_key("ctrl++")$base, "+")
})

test_that("the terminal input parser decodes keys and escape sequences", {
  p <- KeyParser$new()
  input <- paste0("a", "\r", "\t", "\033[A", "\033[1;5C", "\033[Z", "\033[3~", "\033OP", "\177", "\003", " ", "\u044f")
  expect_identical(
    keys_of(p$feed(input)),
    c("a", "enter", "tab", "up", "ctrl+right", "shift+tab", "delete", "f1", "backspace", "ctrl+c", "space", "\u044f")
  )
})

test_that("alt combinations and lone escape", {
  p <- KeyParser$new()
  expect_identical(
    keys_of(p$feed("\033x\033X\033\033[A")),
    c("alt+x", "alt+shift+x", "escape", "up")
  )
  # A trailing ESC may start a sequence: it waits for more input.
  expect_length(p$feed("\033"), 0)
  expect_true(p$has_pending())
  expect_identical(keys_of(p$flush()), "escape")
})

test_that("escape sequences split across reads are reassembled", {
  p <- KeyParser$new()
  expect_length(p$feed("\033["), 0)
  expect_identical(keys_of(p$feed("15~")), "f5")
})

test_that("Windows console key records map to keys", {
  key <- function(...) windows_key_event(...)$key
  expect_identical(key(65L, 97L, 0L), "a")
  expect_identical(key(81L, 81L, 2L), "Q")
  expect_identical(key(38L, 0L, 0L), "up")
  expect_identical(key(67L, 3L, 4L), "ctrl+c")
  expect_identical(key(9L, 9L, 2L), "shift+tab")
  expect_identical(key(0L, 1103L, 0L), "\u044f")
  expect_identical(key(13L, 13L, 0L), "enter")
  expect_identical(key(39L, 0L, 4L), "ctrl+right")
  expect_identical(key(32L, 32L, 0L), "space")
  expect_identical(key(88L, 120L, 1L), "alt+x")
  expect_identical(key(81L, 64L, 5L), "@") # AltGr
  expect_identical(key(112L, 0L, 0L), "f1")
  expect_null(windows_key_event(16L, 0L, 2L)) # shift alone
})

test_that("bind() validates and normalises keys", {
  b <- bind("q, Escape", "quit")
  expect_identical(b$keys, c("q", "escape"))
  expect_identical(bind(",", "x")$keys, ",")
  expect_error(bind("q", 42), "action")
  expect_identical(find_binding(list(bind("q", "a"), bind("q", "b")), "q")$action, "b")
  expect_null(find_binding(list(bind("q", "a")), "x"))
})
