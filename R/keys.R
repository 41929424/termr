# Keys.
#
# Canonical key names:
# * printable characters are named by the character itself ("a", "A",
#   "?", a Cyrillic letter, ...); shift is implied by the character;
# * the space bar is "space";
# * named keys: enter, tab, escape, backspace, delete, insert, up, down,
#   left, right, home, end, pageup, pagedown, f1 ... f24;
# * modifiers are prefixes in the fixed order ctrl+, alt+, shift+
#   (e.g. "ctrl+c", "shift+tab", "ctrl+alt+delete").

named_keys <- c(
  "enter", "tab", "escape", "backspace", "delete", "insert", "space",
  "up", "down", "left", "right", "home", "end", "pageup", "pagedown",
  paste0("f", 1:24)
)

key_aliases <- c(
  return = "enter", esc = "escape", del = "delete", ins = "insert",
  pgup = "pageup", pgdn = "pagedown", page_up = "pageup", page_down = "pagedown",
  arrowup = "up", arrowdown = "down", arrowleft = "left", arrowright = "right",
  bs = "backspace", plus = "+", comma = ",", control = "ctrl", option = "alt",
  meta = "alt", " " = "space"
)

modifier_order <- c("ctrl", "alt", "shift")

#' Normalise a key name
#'
#' @param key A key description such as `"Ctrl+C"`, `"shift+Tab"`,
#'   `"ESC"` or `"q"`.
#' @return The canonical key name.
#' @keywords internal
#' @noRd
normalize_key <- function(key) {
  check_scalar_character(key, "key")
  if (key == "+") return("+")
  parts <- strsplit(key, "+", fixed = TRUE)[[1]]
  if (endsWith(key, "++")) parts <- c(parts[nzchar(parts)], "+")
  if (length(parts) == 0L || any(!nzchar(parts))) {
    stop(sprintf("Invalid key \"%s\".", key), call. = FALSE)
  }
  base <- parts[[length(parts)]]
  mods <- tolower(parts[-length(parts)])
  mods <- ifelse(mods %in% names(key_aliases), key_aliases[mods], mods)
  if (!all(mods %in% modifier_order)) {
    stop(sprintf("Invalid key \"%s\": unknown modifier.", key), call. = FALSE)
  }
  if (nchar(base) > 1L && !is_grapheme(base)) {
    lower <- tolower(base)
    if (lower %in% names(key_aliases)) lower <- key_aliases[[lower]]
    if (!(lower %in% named_keys) && nchar(lower) > 1L) {
      stop(sprintf("Unknown key \"%s\".", key), call. = FALSE)
    }
    base <- lower
  } else if (base == " ") {
    base <- "space"
  }
  if (nchar(base) == 1L && "shift" %in% mods && !("ctrl" %in% mods) && !("alt" %in% mods)) {
    # "shift+a" is the character "A".
    base <- toupper(base)
    mods <- setdiff(mods, "shift")
  }
  if (nchar(base) == 1L && ("ctrl" %in% mods || "alt" %in% mods)) {
    # With ctrl/alt the letter case does not imply shift: "Ctrl+C" == "ctrl+c".
    base <- tolower(base)
  }
  mods <- modifier_order[modifier_order %in% mods]
  paste(c(mods, base), collapse = "+")
}

# Split a canonical key name into modifiers and base key.
split_key <- function(key) {
  if (key == "+") return(list(mods = character(), base = "+"))
  if (endsWith(key, "++")) {
    mods <- strsplit(substr(key, 1L, nchar(key) - 2L), "+", fixed = TRUE)[[1]]
    return(list(mods = mods, base = "+"))
  }
  parts <- strsplit(key, "+", fixed = TRUE)[[1]]
  list(mods = parts[-length(parts)], base = parts[[length(parts)]])
}

# Printable character produced by a canonical key name ("" if none).
key_char <- function(key) {
  if (key == "space") return(" ")
  if (nchar(key) == 1L || is_grapheme(key)) return(key)
  ""
}

# Is `x` a single non-ASCII grapheme cluster (e.g. an emoji sequence)?
is_grapheme <- function(x) !is_ascii(x) && length(split_graphemes(x)) == 1L

# Build a KeyEvent from a base key and modifier flags.
make_key <- function(base, ctrl = FALSE, alt = FALSE, shift = FALSE, char = NULL) {
  mods <- c(if (ctrl) "ctrl", if (alt) "alt", if (shift) "shift")
  key <- if (base == "+" && length(mods)) paste0(paste(mods, collapse = "+"), "++") else paste(c(mods, base), collapse = "+")
  ev <- KeyEvent$new(key, char = char)
  ev
}

# Windows -------------------------------------------------------------------

# System.ConsoleKey values for non-character keys.
windows_named_keys <- c(
  "8" = "backspace", "9" = "tab", "13" = "enter", "27" = "escape",
  "32" = "space", "33" = "pageup", "34" = "pagedown", "35" = "end",
  "36" = "home", "37" = "left", "38" = "up", "39" = "right", "40" = "down",
  "45" = "insert", "46" = "delete",
  structure(paste0("f", 1:24), names = as.character(112:135))
)

# Convert a Windows console key record (virtual key, UTF-16 code unit,
# ConsoleModifiers: 1 Alt, 2 Shift, 4 Control) to a KeyEvent or NULL.
windows_key_event <- function(vk, code, mods) {
  alt <- bitwAnd(mods, 1L) > 0L
  shift <- bitwAnd(mods, 2L) > 0L
  ctrl <- bitwAnd(mods, 4L) > 0L
  named <- windows_named_keys[as.character(vk)]
  if (!is.na(named)) {
    if (named == "space" && !ctrl && !alt) return(make_key("space"))
    return(make_key(named, ctrl = ctrl, alt = alt, shift = shift))
  }
  if (code >= 32L && code != 127L) {
    char <- intToUtf8(code)
    if (ctrl && alt) return(make_key(char)) # AltGr produces plain characters
    if (ctrl || alt) return(make_key(tolower(char), ctrl = ctrl, alt = alt, shift = shift && !ctrl))
    return(make_key(char))
  }
  if (ctrl && vk >= 65L && vk <= 90L) {
    return(make_key(tolower(intToUtf8(vk)), ctrl = TRUE, alt = alt, shift = shift))
  }
  if (ctrl && vk >= 48L && vk <= 57L) {
    return(make_key(intToUtf8(vk), ctrl = TRUE, alt = alt, shift = shift))
  }
  NULL
}

# POSIX terminals -------------------------------------------------------------

csi_letter_keys <- c(
  A = "up", B = "down", C = "right", D = "left", H = "home", F = "end",
  P = "f1", Q = "f2", R = "f3", S = "f4"
)

csi_tilde_keys <- c(
  "1" = "home", "2" = "insert", "3" = "delete", "4" = "end", "5" = "pageup",
  "6" = "pagedown", "7" = "home", "8" = "end", "11" = "f1", "12" = "f2",
  "13" = "f3", "14" = "f4", "15" = "f5", "17" = "f6", "18" = "f7",
  "19" = "f8", "20" = "f9", "21" = "f10", "23" = "f11", "24" = "f12"
)

# Incremental parser for terminal input (UTF-8 text with escape sequences).
# Incomplete escape sequences are kept until more input arrives; a lone
# ESC is reported as "escape" by flush() when no more input follows.
KeyParser <- R6::R6Class(
  "KeyParser",
  public = list(
    pending = "",

    # Bracketed paste state: while a paste is open its text is collected as
    # raw chunks (not parsed as keys) until the closing marker arrives.
    in_paste = FALSE,
    paste_chunks = character(),
    paste_tail = "",

    feed = function(text) {
      text <- enc2utf8(text)
      events <- list()
      if (self$in_paste) {
        res <- private$feed_paste(text)
        events <- res$events
        if (self$in_paste) return(events)
        text <- res$rest
        if (!nzchar(text)) return(events)
      }
      chars <- c(strsplit(self$pending, "", fixed = TRUE)[[1]], strsplit(text, "", fixed = TRUE)[[1]])
      self$pending <- ""
      i <- 1L
      n <- length(chars)
      while (i <= n) {
        res <- private$parse_at(chars, i)
        if (is.null(res)) {
          self$pending <- paste(chars[i:n], collapse = "")
          break
        }
        if (isTRUE(res$paste_start)) {
          self$in_paste <- TRUE
          self$paste_chunks <- character()
          self$paste_tail <- ""
          rest <- if (res$next_i <= n) paste(chars[res$next_i:n], collapse = "") else ""
          return(c(events, self$feed(rest)))
        }
        if (!is.null(res$event)) events[[length(events) + 1L]] <- res$event
        i <- res$next_i
      }
      events
    },

    has_pending = function() nzchar(self$pending),

    flush = function() {
      pending <- self$pending
      self$pending <- ""
      if (!nzchar(pending)) return(list())
      if (pending == "\033") return(list(make_key("escape")))
      # An incomplete sequence: report ESC, then parse the rest as text.
      c(list(make_key("escape")), self$feed(substring(pending, 2L)))
    }
  ),
  private = list(
    # Collect pasted text; returns the finished PasteEvent (if the closing
    # marker was seen) and the text that follows it.
    feed_paste = function(text) {
      marker <- "\033[201~"
      window <- paste0(self$paste_tail, text)
      pos <- regexpr(marker, window, fixed = TRUE)
      if (pos < 0L) {
        keep <- min(nchar(window), nchar(marker) - 1L)
        cut <- nchar(window) - keep
        if (cut > 0L) self$paste_chunks <- c(self$paste_chunks, substr(window, 1L, cut))
        self$paste_tail <- substr(window, cut + 1L, nchar(window))
        return(list(events = list(), rest = ""))
      }
      body <- paste(c(self$paste_chunks, substr(window, 1L, pos - 1L)), collapse = "")
      rest <- substring(window, pos + nchar(marker))
      self$in_paste <- FALSE
      self$paste_chunks <- character()
      self$paste_tail <- ""
      list(events = list(PasteEvent$new(body)), rest = rest)
    },

    # Parse one key starting at chars[i]. Returns list(event, next_i), or
    # NULL when more input is needed.
    parse_at = function(chars, i) {
      n <- length(chars)
      ch <- chars[[i]]
      if (ch != "\033") return(list(event = private$plain_key(ch), next_i = i + 1L))
      if (i == n) return(NULL)
      nxt <- chars[[i + 1L]]
      if (nxt == "[") return(private$parse_csi(chars, i))
      if (nxt == "O") {
        if (i + 2L > n) return(NULL)
        base <- csi_letter_keys[chars[[i + 2L]]]
        return(list(event = if (!is.na(base)) make_key(base), next_i = i + 3L))
      }
      if (nxt == "\033") return(list(event = make_key("escape"), next_i = i + 1L))
      # ESC + key = alt + key
      inner <- private$plain_key(nxt)
      if (is.null(inner)) return(list(event = NULL, next_i = i + 2L))
      key <- inner$key
      base <- sub("^(ctrl\\+|alt\\+|shift\\+)*", "", key)
      shift <- inner$shift || (nchar(base) == 1L && base != tolower(base))
      ev <- make_key(tolower(base), ctrl = inner$ctrl, alt = TRUE, shift = shift)
      list(event = ev, next_i = i + 2L)
    },

    parse_csi = function(chars, i) {
      n <- length(chars)
      j <- i + 2L
      while (j <= n && grepl("^[0-9;:<=>?]$", chars[[j]])) j <- j + 1L
      if (j > n) return(NULL)
      params <- paste(chars[seq_len(j - i - 2L) + i + 1L], collapse = "")
      final <- chars[[j]]
      if (startsWith(params, "<") && final %in% c("M", "m")) {
        return(list(event = sgr_mouse_event(params, final), next_i = j + 1L))
      }
      if (params == "200" && final == "~") return(list(paste_start = TRUE, next_i = j + 1L))
      if (params == "201" && final == "~") return(list(event = NULL, next_i = j + 1L))
      nums <- if (nzchar(params)) suppressWarnings(as.integer(strsplit(params, ";", fixed = TRUE)[[1]])) else integer()
      mod <- if (length(nums) >= 2L && !is.na(nums[[2]])) nums[[2]] - 1L else 0L
      ctrl <- bitwAnd(mod, 4L) > 0L
      alt <- bitwAnd(mod, 2L) > 0L
      shift <- bitwAnd(mod, 1L) > 0L
      event <- NULL
      if (final == "Z") {
        event <- make_key("tab", shift = TRUE)
      } else if (final == "~") {
        base <- csi_tilde_keys[as.character(if (length(nums)) nums[[1]] else 0L)]
        if (!is.na(base)) event <- make_key(base, ctrl, alt, shift)
      } else {
        base <- csi_letter_keys[final]
        if (!is.na(base)) event <- make_key(base, ctrl, alt, shift)
      }
      list(event = event, next_i = j + 1L)
    },

    plain_key = function(ch) {
      code <- utf8ToInt(ch)
      if (length(code) != 1L || is.na(code)) return(NULL)
      if (code == 13L || code == 10L) return(make_key("enter"))
      if (code == 9L) return(make_key("tab"))
      if (code == 127L || code == 8L) return(make_key("backspace"))
      if (code == 0L) return(make_key("space", ctrl = TRUE))
      if (code >= 1L && code <= 26L) return(make_key(letters[[code]], ctrl = TRUE))
      if (code < 32L) return(NULL)
      if (code == 32L) return(make_key("space"))
      make_key(ch)
    }
  )
)
