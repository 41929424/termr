# Text model for multi-line editing.
#
# A TextBuffer stores a document as a character vector of lines. Positions
# are logical: `text_pos(row, col)` with a 1-based row and a 0-based column
# counted in grapheme clusters (col 0 = before the first character, col =
# length of the line = after the last), so a cluster is never split.
#
# All changes go through `replace_range()` / `apply()`, which work on whole
# lines: an edit records the old and the new version of the affected lines,
# which is what the undo stack stores. Single-line edits touch one element
# of the vector; edits that change the number of lines splice the vector
# (cost proportional to the number of lines, a few microseconds for
# thousands of lines).

text_pos <- function(row, col) c(row = as.integer(row), col = as.integer(col))

pos_less <- function(a, b) {
  a[["row"]] < b[["row"]] || (a[["row"]] == b[["row"]] && a[["col"]] < b[["col"]])
}

pos_equal <- function(a, b) a[["row"]] == b[["row"]] && a[["col"]] == b[["col"]]

# Order two positions: list(start, end).
pos_order <- function(a, b) if (pos_less(b, a)) list(start = b, end = a) else list(start = a, end = b)

# Normalise text for the buffer: LF line breaks, tabs expanded to spaces,
# other control characters removed (so no escape sequence can get in).
normalize_text <- function(text, tab_size = 4L) {
  text <- enc2utf8(paste(as.character(text), collapse = "\n"))
  text <- gsub("\r\n?", "\n", text)
  if (grepl("\t", text, fixed = TRUE)) text <- gsub("\t", strrep(" ", tab_size), text, fixed = TRUE)
  gsub("[\001-\011\013\014\016-\037\177]", "", text, perl = TRUE)
}

split_lines <- function(text) {
  if (!nzchar(text)) return("")
  out <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (endsWith(text, "\n")) out <- c(out, "")
  out
}

grapheme_count <- function(s) {
  if (!nzchar(s)) return(0L)
  if (is_ascii(s)) nchar(s, "chars") else length(split_graphemes(s))
}

grapheme_prefix <- function(s, n) {
  if (n <= 0L || !nzchar(s)) return("")
  if (is_ascii(s)) return(substr(s, 1L, n))
  g <- split_graphemes(s)
  paste(g[seq_len(min(n, length(g)))], collapse = "")
}

grapheme_suffix <- function(s, n) {
  if (!nzchar(s)) return("")
  if (n <= 0L) return(s)
  if (is_ascii(s)) return(substring(s, n + 1L))
  g <- split_graphemes(s)
  if (n >= length(g)) "" else paste(g[(n + 1L):length(g)], collapse = "")
}

grapheme_slice <- function(s, from, to) grapheme_prefix(grapheme_suffix(s, from), to - from)

TextBuffer <- R6::R6Class(
  "TextBuffer",
  public = list(
    lines = "",
    # Incremented by every change; caches key on it.
    version = 0L,
    tab_size = 4L,

    initialize = function(text = "", tab_size = 4L) {
      self$tab_size <- tab_size
      self$set_text(text)
    },

    set_text = function(text) {
      self$lines <- split_lines(normalize_text(text, self$tab_size))
      self$version <- self$version + 1L
      invisible(self)
    },

    n_lines = function() length(self$lines),
    line_length = function(row) grapheme_count(self$lines[[row]]),
    text = function() paste(self$lines, collapse = "\n"),
    end_pos = function() text_pos(length(self$lines), self$line_length(length(self$lines))),

    clamp = function(pos) {
      row <- min(max(1L, pos[["row"]]), length(self$lines))
      text_pos(row, min(max(0L, pos[["col"]]), self$line_length(row)))
    },

    # The text between two positions (any order).
    slice = function(a, b) {
      o <- pos_order(self$clamp(a), self$clamp(b))
      s <- o$start
      e <- o$end
      if (s[["row"]] == e[["row"]]) return(grapheme_slice(self$lines[[s[["row"]]]], s[["col"]], e[["col"]]))
      middle <- if (e[["row"]] - s[["row"]] > 1L) self$lines[(s[["row"]] + 1L):(e[["row"]] - 1L)]
      paste(c(grapheme_suffix(self$lines[[s[["row"]]]], s[["col"]]), middle,
              grapheme_prefix(self$lines[[e[["row"]]]], e[["col"]])), collapse = "\n")
    },

    # Replace the text between `start` and `end` by `text`. Returns the
    # edit: list(first_row, old, new, start, end), where `end` is the
    # position after the inserted text.
    replace_range = function(start, end, text) {
      o <- pos_order(self$clamp(start), self$clamp(end))
      start <- o$start
      end <- o$end
      text <- normalize_text(text, self$tab_size)
      first <- start[["row"]]
      last <- end[["row"]]
      prefix <- grapheme_prefix(self$lines[[first]], start[["col"]])
      suffix <- grapheme_suffix(self$lines[[last]], end[["col"]])
      inserted <- split_lines(text)
      n <- length(inserted)
      new <- inserted
      new[[1L]] <- paste0(prefix, new[[1L]])
      new[[n]] <- paste0(new[[n]], suffix)
      old <- self$lines[first:last]
      tail_text <- inserted[[n]]
      end_col <- grapheme_count(if (n == 1L) paste0(prefix, tail_text) else tail_text)
      self$apply(first, length(old), new)
      list(first_row = first, old = old, new = new, start = start, end = text_pos(first + n - 1L, end_col))
    },

    # Replace `n_remove` lines starting at `first_row` by `new_lines`.
    apply = function(first_row, n_remove, new_lines) {
      n <- length(self$lines)
      if (n_remove == length(new_lines)) {
        self$lines[first_row + seq_len(n_remove) - 1L] <- new_lines
      } else {
        keep_before <- if (first_row > 1L) self$lines[seq_len(first_row - 1L)]
        keep_after <- if (first_row + n_remove <= n) self$lines[(first_row + n_remove):n]
        self$lines <- c(keep_before, new_lines, keep_after)
      }
      self$version <- self$version + 1L
      invisible(self)
    }
  )
)

# Undo / redo ------------------------------------------------------------------

# History of edits, independent of any widget. An edit is the list returned
# by TextBuffer$replace_range() plus `before` / `after` (cursor positions),
# `kind` ("type", "delete" or "other") and `closes` (TRUE ends an undo
# group). Consecutive "type" edits on one line, and consecutive "delete"
# edits, are merged into one undo step until a group is closed (a space,
# a line break, a cursor jump). The history is bounded by the number of
# steps and by the total number of characters it keeps.
UndoStack <- R6::R6Class(
  "UndoStack",
  public = list(
    max_entries = 200L,
    max_chars = 2e6,

    initialize = function(max_entries = 200L, max_chars = 2e6) {
      self$max_entries <- max_entries
      self$max_chars <- max_chars
    },

    push = function(edit) {
      edit$size <- sum(nchar(edit$old, "chars")) + sum(nchar(edit$new, "chars"))
      if (edit$size > self$max_chars) {
        private$undo_stack <- list()
        private$redo_stack <- list()
        private$open <- FALSE
        return(invisible(self))
      }
      n <- length(private$undo_stack)
      if (private$open && n > 0L && private$mergeable(private$undo_stack[[n]], edit)) {
        last <- private$undo_stack[[n]]
        last$new <- edit$new
        last$after <- edit$after
        last$end <- edit$end
        last$size <- last$size + nchar(edit$new, "chars") - nchar(edit$old, "chars")
        private$undo_stack[[n]] <- last
      } else {
        private$undo_stack[[n + 1L]] <- edit
      }
      private$open <- !isTRUE(edit$closes)
      private$redo_stack <- list()
      private$trim()
      invisible(self)
    },

    # Close the current group: the next edit starts a new undo step.
    break_group = function() {
      private$open <- FALSE
      invisible(self)
    },

    # Revert the last edit in `buffer`. Returns it (or NULL).
    undo = function(buffer) {
      n <- length(private$undo_stack)
      if (n == 0L) return(NULL)
      edit <- private$undo_stack[[n]]
      private$undo_stack[[n]] <- NULL
      buffer$apply(edit$first_row, length(edit$new), edit$old)
      private$redo_stack[[length(private$redo_stack) + 1L]] <- edit
      private$open <- FALSE
      edit
    },

    redo = function(buffer) {
      n <- length(private$redo_stack)
      if (n == 0L) return(NULL)
      edit <- private$redo_stack[[n]]
      private$redo_stack[[n]] <- NULL
      buffer$apply(edit$first_row, length(edit$old), edit$new)
      private$undo_stack[[length(private$undo_stack) + 1L]] <- edit
      private$open <- FALSE
      edit
    },

    clear = function() {
      private$undo_stack <- list()
      private$redo_stack <- list()
      private$open <- FALSE
      invisible(self)
    },

    can_undo = function() length(private$undo_stack) > 0L,
    can_redo = function() length(private$redo_stack) > 0L,
    steps = function() length(private$undo_stack),
    # Characters currently held by the history.
    size = function() sum(vapply(private$undo_stack, function(e) e$size, 0)) +
      sum(vapply(private$redo_stack, function(e) e$size, 0))
  ),
  private = list(
    undo_stack = list(),
    redo_stack = list(),
    open = FALSE,

    mergeable = function(last, edit) {
      kinds <- c(last$kind, edit$kind)
      identical(kinds[[1]], kinds[[2]]) && kinds[[1]] %in% c("type", "delete") &&
        length(last$new) == 1L && length(last$old) == 1L &&
        length(edit$new) == 1L && length(edit$old) == 1L &&
        edit$first_row == last$first_row && pos_equal(edit$before, last$after)
    },

    trim = function() {
      while (length(private$undo_stack) > self$max_entries ||
             (length(private$undo_stack) > 0L && self$size() > self$max_chars)) {
        private$undo_stack[[1L]] <- NULL
      }
      invisible()
    }
  )
)

# Search ------------------------------------------------------------------------

# A search query: literal text or a regular expression (PCRE).
# Invalid regular expressions raise a friendly error.
search_query <- function(pattern, case_sensitive = FALSE, regex = FALSE) {
  check_scalar_character(pattern, "pattern")
  check_flag(case_sensitive)
  check_flag(regex)
  if (regex) {
    problem <- tryCatch({
      regexpr(pattern, "", perl = TRUE)
      NULL
    }, error = function(e) conditionMessage(e), warning = function(w) conditionMessage(w))
    if (!is.null(problem)) {
      first_line <- strsplit(problem, "\n", fixed = TRUE)[[1]][[1]]
      stop("Invalid regular expression \"", pattern, "\": ", first_line, call. = FALSE)
    }
  }
  structure(list(pattern = pattern, case_sensitive = case_sensitive, regex = regex),
            class = "termr_search_query")
}

# Which of `x` contain a match? (logical vector)
query_matches <- function(query, x) {
  if (!nzchar(query$pattern)) return(rep(FALSE, length(x)))
  if (query$regex) {
    grepl(query$pattern, x, perl = TRUE, ignore.case = !query$case_sensitive)
  } else if (query$case_sensitive) {
    grepl(query$pattern, x, fixed = TRUE)
  } else {
    grepl(tolower(query$pattern), tolower(x), fixed = TRUE)
  }
}

# Matches inside one line, as grapheme columns: data frame (start, end) with
# start inclusive and end exclusive (0-based, like cursor columns).
query_line_matches <- function(query, line) {
  none <- data.frame(start = integer(), end = integer())
  if (!nzchar(query$pattern) || !nzchar(line)) return(none)
  m <- if (query$regex) {
    gregexpr(query$pattern, line, perl = TRUE, ignore.case = !query$case_sensitive)[[1]]
  } else if (query$case_sensitive) {
    gregexpr(query$pattern, line, fixed = TRUE)[[1]]
  } else {
    gregexpr(tolower(query$pattern), tolower(line), fixed = TRUE)[[1]]
  }
  if (m[[1]] == -1L) return(none)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  keep <- lens > 0L
  starts <- starts[keep] - 1L
  ends <- starts + lens[keep]
  if (!length(starts)) return(none)
  if (!is_ascii(line)) {
    # Convert code point offsets to grapheme columns: a match that covers
    # part of a cluster is widened to the whole cluster.
    cum <- cumsum(nchar(split_graphemes(line), "chars"))
    return(data.frame(start = as.integer(findInterval(starts, cum)),
                      end = as.integer(findInterval(ends - 1L, cum) + 1L)))
  }
  data.frame(start = starts, end = ends)
}

# The next index (forward or backward, wrapping) whose element matches.
# `get(i)` returns the strings for the index vector `i`, so the scan reads
# chunks lazily (a data table passes a closure that formats rows on demand).
# Returns NA when nothing matches.
search_chunks <- function(query, n, get, from, direction = 1L, wrap = TRUE, chunk = 5000L) {
  if (n < 1L || !nzchar(query$pattern)) return(NA_integer_)
  order_idx <- function(lo, hi) if (direction > 0L) seq.int(lo, hi) else seq.int(hi, lo)
  scan <- function(lo, hi) {
    if (lo > hi) return(NA_integer_)
    starts <- if (direction > 0L) seq.int(lo, hi, by = chunk) else seq.int(hi, lo, by = -chunk)
    for (s in starts) {
      idx <- if (direction > 0L) seq.int(s, min(s + chunk - 1L, hi)) else seq.int(s, max(s - chunk + 1L, lo))
      hit <- which(query_matches(query, get(idx)))
      if (length(hit)) return(idx[[hit[[1]]]])
    }
    NA_integer_
  }
  from <- min(max(from, 1L), n)
  res <- if (direction > 0L) scan(from, n) else scan(1L, from)
  if (is.na(res) && wrap) res <- if (direction > 0L) scan(1L, from - 1L) else scan(from + 1L, n)
  res
}
