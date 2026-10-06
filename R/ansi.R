# ANSI escape sequence primitives.
#
# Pure functions that only build strings. Nothing here performs IO, which
# keeps the renderer testable without a terminal.

ansi_csi <- "\033["

ansi_cursor_to <- function(y, x) paste0(ansi_csi, y, ";", x, "H")

ansi_reset <- function() paste0(ansi_csi, "0m")

ansi_clear_screen <- function() paste0(ansi_csi, "2J")

ansi_cursor_home <- function() paste0(ansi_csi, "H")

ansi_cursor_visible <- function(visible) paste0(ansi_csi, "?25", if (visible) "h" else "l")

ansi_alt_screen <- function(enter) paste0(ansi_csi, "?1049", if (enter) "h" else "l")

ansi_autowrap <- function(enabled) paste0(ansi_csi, "?7", if (enabled) "h" else "l")

# Bracketed paste (mode 2004): pasted text arrives between ESC[200~ and ESC[201~.
ansi_bracketed_paste <- function(enable) paste0(ansi_csi, "?2004", if (enable) "h" else "l")

# Clipboard write (OSC 52). `text` is base64-encoded, so it can never contain
# control characters; it is capped so one sequence stays small.
osc52_max_bytes <- 100000L
ansi_osc52 <- function(text) {
  raw <- charToRaw(enc2utf8(text))
  if (length(raw) > osc52_max_bytes) raw <- raw[seq_len(osc52_max_bytes)]
  paste0("\033]52;c;", base64_encode(raw), "\007")
}

# OSC 8 hyperlink. Only http(s), file and mailto URLs without control
# characters are accepted; anything else yields no link.
ansi_hyperlink_open <- function(url) {
  if (!safe_url(url)) return("")
  paste0("\033]8;;", url, "\033\\")
}
ansi_hyperlink_close <- function() "\033]8;;\033\\"
safe_url <- function(url) {
  is.character(url) && length(url) == 1L && !is.na(url) && nzchar(url) &&
    !grepl("[^ -~]", url) && grepl("^(https?://|file://|mailto:)", url)
}

# Base64 without a dependency (RFC 4648).
base64_encode <- function(raw) {
  if (!length(raw)) return("")
  alphabet <- c(LETTERS, letters, 0:9, "+", "/")
  pad <- (3L - length(raw) %% 3L) %% 3L
  bytes <- c(as.integer(raw), rep(0L, pad))
  m <- matrix(bytes, nrow = 3L)
  n <- m[1, ] * 65536L + m[2, ] * 256L + m[3, ]
  idx <- rbind(n %/% 262144L, (n %/% 4096L) %% 64L, (n %/% 64L) %% 64L, n %% 64L)
  out <- alphabet[idx + 1L]
  if (pad > 0L) out[(length(out) - pad + 1L):length(out)] <- "="
  paste(out, collapse = "")
}

base64_decode <- function(x) {
  x <- gsub("=+$", "", x)
  if (!nzchar(x)) return(raw())
  alphabet <- c(LETTERS, letters, 0:9, "+", "/")
  vals <- match(strsplit(x, "")[[1]], alphabet) - 1L
  bits <- rep(vals, each = 6L) %/% rep(2^(5:0), times = length(vals)) %% 2L
  bytes <- (length(vals) * 6L) %/% 8L
  m <- matrix(bits[seq_len(bytes * 8L)], nrow = 8L)
  as.raw(colSums(m * 2^(7:0)))
}

# Synchronized output (mode 2026): the terminal holds the frame until it is
# complete. Terminals that do not know the mode ignore it.
ansi_sync <- function(begin) paste0(ansi_csi, "?2026", if (begin) "h" else "l")

# Full SGR sequences for vectors of (fg, bg, attrs). Every sequence starts
# with a reset (0) so it does not depend on the previous terminal state.
ansi_sgr <- function(fg, bg, attrs, color_mode = "truecolor") {
  n <- max(length(fg), length(bg), length(attrs))
  fg <- rep_len(fg, n)
  bg <- rep_len(bg, n)
  attrs <- rep_len(as.integer(attrs), n)
  params <- rep("0", n)
  for (name in names(attr_bits)) {
    on <- bitwAnd(attrs, attr_bits[[name]]) > 0L
    params[on] <- paste0(params[on], ";", attr_sgr[[name]])
  }
  fg_codes <- color_sgr(fg, background = FALSE, mode = color_mode)
  bg_codes <- color_sgr(bg, background = TRUE, mode = color_mode)
  has_fg <- nzchar(fg_codes)
  has_bg <- nzchar(bg_codes)
  params[has_fg] <- paste0(params[has_fg], ";", fg_codes[has_fg])
  params[has_bg] <- paste0(params[has_bg], ";", bg_codes[has_bg])
  paste0(ansi_csi, params, "m")
}

#' Remove ANSI escape sequences from a string
#'
#' @param x A character vector.
#' @return `x` without CSI/OSC escape sequences.
#' @export
#' @examples
#' strip_ansi("\033[1mbold\033[0m")
strip_ansi <- function(x) {
  x <- gsub("\033\\[[0-9;?]*[ -/]*[@-~]", "", x, perl = TRUE)
  gsub("\033\\][^\007\033]*(\007|\033\\\\)", "", x, perl = TRUE)
}
