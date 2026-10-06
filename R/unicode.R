# Unicode helpers.
#
# All text that reaches the screen goes through `text_cells()`, which splits a
# string into terminal cells. A cell holds one *grapheme cluster* (what a
# user perceives as one character): a base character with combining marks,
# an emoji ZWJ sequence such as a family, a flag made of two regional
# indicators, an emoji with a skin tone modifier or variation selector, a
# Hangul syllable built from jamo, ...
#
# Segmentation uses PCRE2's `\X`, which implements Unicode extended
# grapheme clusters (UAX #29) in base R. Display widths of clusters follow
# what modern terminals do (see grapheme_width()). No other part of termr
# splits text into characters, so this is the only place to change when
# the rules evolve.

# Remove control characters so user text can never inject escape sequences.
sanitize_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("\t", " ", x, fixed = TRUE)
  gsub("[\001-\037\177]", "", x, perl = TRUE)
}

is_ascii <- function(x) !grepl("[^\001-\177]", x, useBytes = TRUE)

grapheme_pattern <- "(\\X)"

# Split one string into grapheme clusters. Control characters must already
# be removed (\001 is used as a separator).
split_graphemes <- function(x) {
  if (!nzchar(x)) return(character())
  if (is_ascii(x)) return(strsplit(x, "", fixed = TRUE)[[1]])
  strsplit(gsub(grapheme_pattern, "\\1\001", x, perl = TRUE), "\001", fixed = TRUE)[[1]]
}

# Display width of individual code points (vectorised), 0 to 2.
char_width <- function(chars) {
  w <- nchar(chars, type = "width", allowNA = TRUE)
  w[is.na(w)] <- 1L
  pmin(pmax(as.integer(w), 0L), 2L)
}

# Display width of grapheme clusters (vectorised), 0 to 2.
#
# * single code points: their East Asian width (wide CJK, emoji = 2);
# * emoji sequences (ZWJ, skin tone modifier, emoji presentation selector
#   U+FE0F) and regional indicator pairs (flags) = 2;
# * a text presentation selector U+FE0E forces width 1;
# * otherwise the width of the base character (combining marks add nothing).
grapheme_width <- function(g) {
  w <- rep(1L, length(g))
  if (length(g) == 0L) return(w)
  idx <- which(!is_ascii(g))
  if (length(idx) == 0L) return(w)
  gg <- g[idx]
  out <- char_width(substr(gg, 1L, 1L))
  multi <- nchar(gg, type = "chars") > 1L
  if (any(multi)) {
    emoji <- multi & grepl("[\u200d\ufe0f\U0001f3fb-\U0001f3ff]", gg, perl = TRUE)
    flag <- multi & grepl("^[\U0001f1e6-\U0001f1ff]{2}", gg, perl = TRUE)
    text_style <- multi & grepl("\ufe0e", gg, fixed = TRUE) & !grepl("\u200d", gg, fixed = TRUE)
    out[emoji | flag] <- 2L
    out[text_style] <- 1L
  }
  w[idx] <- out
  w
}

# Display width of whole strings (vectorised).
str_width <- function(x) {
  if (length(x) == 0L) return(integer())
  x <- sanitize_text(x)
  out <- nchar(x, type = "chars")
  complex <- which(!is_ascii(x))
  for (i in complex) out[[i]] <- sum(text_cells(x[[i]])$widths)
  as.integer(out)
}

# Split a string into cells: list(chars = <character>, widths = <integer>).
# Every element of `chars` is one grapheme cluster occupying `widths`
# columns (1 or 2). Invisible clusters are dropped; a combining mark with no
# base character is shown on a space.
text_cells <- function(x) {
  x <- sanitize_text(x)
  if (length(x) != 1L || !nzchar(x)) {
    return(list(chars = character(), widths = integer()))
  }
  if (is_ascii(x)) {
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    return(list(chars = chars, widths = rep(1L, length(chars))))
  }
  chars <- split_graphemes(x)
  widths <- grapheme_width(chars)
  zero <- widths == 0L
  if (any(zero)) {
    mark <- zero & grepl("^\\p{M}", chars, perl = TRUE)
    chars[mark] <- paste0(" ", chars[mark])
    widths[mark] <- 1L
    keep <- widths > 0L
    chars <- chars[keep]
    widths <- widths[keep]
  }
  list(chars = chars, widths = widths)
}

# Truncate a string to at most `width` columns.
str_truncate <- function(x, width) {
  cells <- text_cells(x)
  if (sum(cells$widths) <= width) return(paste(cells$chars, collapse = ""))
  keep <- cumsum(cells$widths) <= width
  paste(cells$chars[keep], collapse = "")
}

# Pad a string to exactly `width` columns according to `align`.
str_align <- function(x, width, align = "left") {
  x <- str_truncate(x, width)
  gap <- width - str_width(x)
  if (gap <= 0L) return(x)
  left <- switch(align, center = gap %/% 2L, right = gap, 0L)
  paste0(strrep(" ", left), x, strrep(" ", gap - left))
}

# Can the terminal show box drawing and other non-ASCII symbols? False in
# non-UTF-8 locales, or when options(termr.ascii = TRUE) asks for plain
# ASCII (for limited terminals and fonts).
unicode_ok <- function() {
  !isTRUE(getOption("termr.ascii", FALSE)) && isTRUE(l10n_info()[["UTF-8"]])
}
