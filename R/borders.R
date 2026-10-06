# Border character sets: top-left, top, top-right, left, right,
# bottom-left, bottom, bottom-right.
border_sets <- list(
  none = character(),
  blank = c(" ", " ", " ", " ", " ", " ", " ", " "),
  ascii = c("+", "-", "+", "|", "|", "+", "-", "+"),
  single = c("\u250c", "\u2500", "\u2510", "\u2502", "\u2502", "\u2514", "\u2500", "\u2518"),
  round = c("\u256d", "\u2500", "\u256e", "\u2502", "\u2502", "\u2570", "\u2500", "\u256f"),
  double = c("\u2554", "\u2550", "\u2557", "\u2551", "\u2551", "\u255a", "\u2550", "\u255d"),
  heavy = c("\u250f", "\u2501", "\u2513", "\u2503", "\u2503", "\u2517", "\u2501", "\u251b")
)

border_chars <- function(type) {
  chars <- border_sets[[type]]
  if (type %in% c("none", "blank", "ascii")) return(chars)
  if (!unicode_ok()) {
    return(border_sets$ascii)
  }
  chars
}

border_width <- function(type) if (identical(type, "none")) 0L else 1L
