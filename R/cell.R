# Text attributes are stored in the framebuffer as an integer bit mask.
attr_bits <- c(
  bold = 1L, dim = 2L, italic = 4L, underline = 8L,
  blink = 16L, reverse = 32L, strike = 64L
)

# SGR codes that switch each attribute on.
attr_sgr <- c(
  bold = "1", dim = "2", italic = "3", underline = "4",
  blink = "5", reverse = "7", strike = "9"
)

attrs_encode <- function(bold = FALSE, dim = FALSE, italic = FALSE,
                         underline = FALSE, blink = FALSE, reverse = FALSE,
                         strike = FALSE) {
  flags <- c(bold, dim, italic, underline, blink, reverse, strike)
  flags[is.na(flags)] <- FALSE
  as.integer(sum(attr_bits[flags]))
}

attrs_decode <- function(attrs) {
  vapply(attr_bits, function(bit) bitwAnd(attrs, bit) > 0L, logical(1))
}

#' A single terminal cell
#'
#' Cells are the unit of the virtual screen. Each cell holds one character
#' (a wide character such as a CJK ideograph occupies two cells; the second one is a
#' continuation cell whose `char` is `""`), colours and text attributes.
#'
#' The framebuffer stores cells column-wise in matrices for speed; `cell()`
#' is the value-level representation used for inspection and tests.
#'
#' @param char A single character.
#' @param fg,bg Foreground and background colours. `NULL` means the
#'   terminal default. See [style()] for accepted colour formats.
#' @param bold,italic,underline,reverse,dim,strike Text attributes.
#' @return An object of class `termr_cell`.
#' @export
#' @examples
#' cell("A", fg = "red", bold = TRUE)
cell <- function(char = " ", fg = NULL, bg = NULL, bold = FALSE,
                 italic = FALSE, underline = FALSE, reverse = FALSE,
                 dim = FALSE, strike = FALSE) {
  check_scalar_character(char)
  structure(
    list(
      char = char,
      fg = normalize_color(fg),
      bg = normalize_color(bg),
      attrs = attrs_encode(
        bold = bold, dim = dim, italic = italic, underline = underline,
        reverse = reverse, strike = strike
      )
    ),
    class = "termr_cell"
  )
}

#' @export
format.termr_cell <- function(x, ...) {
  flags <- names(attr_bits)[attrs_decode(x$attrs)]
  sprintf(
    "<cell %s fg=%s bg=%s%s>",
    encodeString(x$char, quote = "\""),
    if (nzchar(x$fg)) x$fg else "default",
    if (nzchar(x$bg)) x$bg else "default",
    if (length(flags)) paste0(" ", paste(flags, collapse = ",")) else ""
  )
}

#' @export
print.termr_cell <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}
