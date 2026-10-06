#' Styled text
#'
#' `span()` creates a piece of text with its own style. Spans can be
#' combined with [c()] and returned from a widget's `render` function or
#' passed to [label()]. Newlines start new lines.
#'
#' Spans nest: `span(c(span("a"), span("b", s1)), s2)` applies `s2` to both
#' pieces, with the inner style taking precedence.
#'
#' @param x Text. A character vector is joined with newlines. May also be
#'   spans, to nest styles.
#' @param style A [style()]; only colours and text attributes are used.
#' @param link Optional link target (a URL or any string). Stored as
#'   metadata on the text for widgets that support it; not rendered yet.
#' @param ... Spans or character strings to combine.
#' @return An object of class `termr_text`.
#' @export
#' @examples
#' c(span("Status: "), span("OK", style(foreground = "green", bold = TRUE)))
#' span(c(span("bold "), span("and red", style(foreground = "red"))), style(bold = TRUE))
span <- function(x = "", style = NULL, link = NULL) {
  check_scalar_character(link, "link", allow_null = TRUE)
  outer <- as_style(style)
  if (inherits(x, "termr_text")) {
    segs <- lapply(unclass(x), function(seg) {
      seg$style <- merge_styles(outer, seg$style)
      if (!is.null(link) && is.null(seg$link)) seg$link <- link
      seg
    })
    return(structure(segs, class = "termr_text"))
  }
  seg <- list(text = paste(as.character(x), collapse = "\n"), style = outer)
  if (!is.null(link)) seg$link <- link
  structure(list(seg), class = "termr_text")
}

#' @rdname span
#' @export
c.termr_text <- function(...) {
  parts <- lapply(list(...), as_text)
  structure(unlist(parts, recursive = FALSE), class = "termr_text")
}

as_text <- function(x) {
  if (inherits(x, "termr_text")) return(x)
  if (is.null(x) || length(x) == 0L) return(structure(list(), class = "termr_text"))
  if (is.list(x)) return(do.call(c.termr_text, unname(x)))
  span(paste(format_value(x), collapse = "\n"))
}

format_value <- function(x) {
  if (is.character(x)) return(x)
  if (is.factor(x)) return(as.character(x))
  format(x)
}

#' @export
as.character.termr_text <- function(x, ...) {
  paste(vapply(unclass(x), function(s) s$text, character(1)), collapse = "")
}

#' @export
format.termr_text <- function(x, ...) {
  paste0("<span ", encodeString(as.character(x), quote = "\""), ">")
}

#' @export
print.termr_text <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

# Split text into lines; each line is a list of segments list(text, style).
text_lines <- function(x) {
  x <- as_text(x)
  lines <- list(list())
  for (seg in unclass(x)) {
    pieces <- strsplit(seg$text, "\n", fixed = TRUE)[[1]]
    if (endsWith(seg$text, "\n")) pieces <- c(pieces, "")
    if (length(pieces) == 0L) next
    for (i in seq_along(pieces)) {
      if (i > 1L) lines[[length(lines) + 1L]] <- list()
      if (nzchar(pieces[[i]])) {
        n <- length(lines)
        piece <- seg
        piece$text <- pieces[[i]]
        lines[[n]][[length(lines[[n]]) + 1L]] <- piece
      }
    }
  }
  lines
}

line_width <- function(line) {
  if (length(line) == 0L) return(0L)
  sum(vapply(line, function(seg) str_width(seg$text), integer(1)))
}

# Wrapping -------------------------------------------------------------------

wrap_modes <- c("none", "word", "char")

# Wrap lines (from text_lines()) to `width` columns.
#   "none": lines are left as they are (the compositor clips them);
#   "word": break at spaces; words longer than the width are split;
#   "char": break at any grapheme.
# Segment styles are kept: a segment split across lines keeps its style.
wrap_text_lines <- function(lines, width, mode = "word") {
  if (mode == "none" || is.na(width)) return(lines)
  width <- max(1L, as.integer(width))
  out <- list()
  for (line in lines) out <- c(out, wrap_line(line, width, mode))
  out
}

wrap_line <- function(line, width, mode) {
  if (length(line) == 0L || line_width(line) <= width) return(list(line))
  cells <- lapply(seq_along(line), function(i) {
    tc <- text_cells(line[[i]]$text)
    list(chars = tc$chars, widths = tc$widths, seg = rep(i, length(tc$chars)))
  })
  chars <- unlist(lapply(cells, `[[`, "chars"))
  widths <- unlist(lapply(cells, `[[`, "widths"))
  segs <- unlist(lapply(cells, `[[`, "seg"))
  breaks <- wrap_breaks(chars, widths, width, mode)
  lapply(breaks, function(idx) cells_to_line(line, chars[idx], segs[idx]))
}

# Index vectors of the cells on each wrapped line.
wrap_breaks <- function(chars, widths, width, mode) {
  n <- length(chars)
  space <- chars == " "
  out <- list()
  start <- 1L
  while (start <= n) {
    # Skip spaces at the start of a continuation line.
    if (length(out) && mode == "word") {
      while (start <= n && space[[start]]) start <- start + 1L
      if (start > n) break
    }
    used <- cumsum(widths[start:n])
    fit <- sum(used <= width)
    if (fit == 0L) fit <- 1L # a single cell wider than the line
    end <- start + fit - 1L
    if (end < n && mode == "word") {
      # Break after the last space that fits, unless the line has none.
      cut <- if (space[[end + 1L]]) end else {
        spaces <- which(space[start:end]) + start - 1L
        if (length(spaces)) max(spaces) else end
      }
      end <- cut
    }
    idx <- seq.int(start, end)
    if (mode == "word") {
      while (length(idx) > 1L && space[[idx[[length(idx)]]]]) idx <- idx[-length(idx)]
    }
    out[[length(out) + 1L]] <- idx
    start <- end + 1L
  }
  out
}

# Rebuild segments from cells, grouping consecutive cells of one segment.
cells_to_line <- function(line, chars, segs) {
  if (length(chars) == 0L) return(list())
  groups <- cumsum(c(TRUE, segs[-1L] != segs[-length(segs)]))
  lapply(split(seq_along(chars), groups), function(i) {
    seg <- line[[segs[[i[[1]]]]]]
    seg$text <- paste(chars[i], collapse = "")
    seg
  }) |> unname()
}
