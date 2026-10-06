# A small Markdown renderer for terminal help screens, release notes and
# dialogs. It covers a practical subset: headings, paragraphs (with hard
# breaks), bold, italic, strikethrough, inline code, fenced code blocks,
# bullet and numbered lists (nested), block quotes, horizontal rules, simple
# pipe tables and links (shown underlined; the target is kept as span
# metadata). No HTML; unknown syntax is shown as plain text.
#
# Parsing produces blocks (markdown_blocks()); rendering turns blocks into
# styled lines for a width (markdown_lines()). Text never reaches the
# terminal unfiltered: segments go through the usual cell pipeline, which
# strips control characters.

# Inline ------------------------------------------------------------------------

md_escape_map <- c("\\\\" = "\ufdd0", "\\*" = "\ufdd1", "\\_" = "\ufdd2", "\\`" = "\ufdd3",
                   "\\[" = "\ufdd4", "\\]" = "\ufdd5", "\\(" = "\ufdd6", "\\)" = "\ufdd7",
                   "\\#" = "\ufdd8", "\\~" = "\ufdd9", "\\>" = "\ufdda", "\\|" = "\ufddb",
                   "\\-" = "\ufddc", "\\+" = "\ufddd", "\\!" = "\ufdde", "\\." = "\ufddf")

md_protect <- function(x) {
  for (k in seq_along(md_escape_map)) x <- gsub(names(md_escape_map)[[k]], md_escape_map[[k]], x, fixed = TRUE)
  x
}

md_restore <- function(x) {
  for (k in seq_along(md_escape_map)) x <- gsub(md_escape_map[[k]], substring(names(md_escape_map)[[k]], 2L), x, fixed = TRUE)
  x
}

md_inline_patterns <- list(
  code = "`([^`]+)`",
  link = "\\[([^]]+)\\]\\(([^)[:space:]]+)\\)",
  autolink = "<(https?://[^>[:space:]]+)>",
  bold = "(\\*\\*|__)(?=\\S)(.+?)(?<=\\S)\\1",
  strike = "~~(?=\\S)(.+?)(?<=\\S)~~",
  italic = "(?<![\\w*])(\\*|_)(?=[^\\s*_])(.+?)(?<=[^\\s*_])\\1(?![\\w*])"
)

md_styles <- function() {
  list(
    bold = style(bold = TRUE),
    italic = style(italic = TRUE),
    strike = style(strike = TRUE),
    code = style(foreground = "$accent", background = "$surface"),
    link = style(foreground = "$accent", underline = TRUE)
  )
}

# Split inline markup into segments list(text, style, link).
md_inline <- function(text, base = NULL, styles = md_styles(), link = NULL) {
  if (!nzchar(text)) return(list())
  best <- NULL
  for (kind in names(md_inline_patterns)) {
    m <- regexpr(md_inline_patterns[[kind]], text, perl = TRUE)
    if (m[[1]] > 0L && (is.null(best) || m[[1]] < best$start)) {
      best <- list(kind = kind, start = as.integer(m), length = attr(m, "match.length"))
    }
  }
  seg <- function(txt, st = base, lnk = link) {
    s <- list(text = md_restore(txt), style = merge_styles(base, st))
    if (!is.null(lnk)) s$link <- lnk
    s
  }
  if (is.null(best)) return(list(seg(text)))
  before <- substr(text, 1L, best$start - 1L)
  matched <- substr(text, best$start, best$start + best$length - 1L)
  after <- substring(text, best$start + best$length)
  inner <- function(pattern) {
    parts <- regmatches(matched, regexec(pattern, matched, perl = TRUE))[[1]]
    parts
  }
  merged <- function(st) merge_styles(base, st)
  out <- list()
  if (nzchar(before)) out <- c(out, list(seg(before)))
  middle <- switch(
    best$kind,
    code = {
      parts <- inner(md_inline_patterns$code)
      list(seg(parts[[2]], styles$code))
    },
    link = {
      parts <- inner(md_inline_patterns$link)
      target <- md_restore(parts[[3]])
      md_inline(parts[[2]], merged(styles$link), styles, link = target)
    },
    autolink = {
      parts <- inner(md_inline_patterns$autolink)
      list(seg(parts[[2]], styles$link, parts[[2]]))
    },
    bold = {
      parts <- inner(md_inline_patterns$bold)
      md_inline(parts[[3]], merged(styles$bold), styles, link)
    },
    strike = {
      parts <- inner(md_inline_patterns$strike)
      md_inline(parts[[2]], merged(styles$strike), styles, link)
    },
    italic = {
      parts <- inner(md_inline_patterns$italic)
      md_inline(parts[[3]], merged(styles$italic), styles, link)
    }
  )
  c(out, middle, md_inline(after, base, styles, link))
}

# Blocks ------------------------------------------------------------------------------

md_list_item <- function(line) {
  m <- regmatches(line, regexec("^( *)([-*+]|[0-9]+[.)]) +(.*)$", line))[[1]]
  if (length(m) == 0L) return(NULL)
  list(indent = nchar(m[[2]]), marker = m[[3]], text = m[[4]], ordered = grepl("^[0-9]", m[[3]]))
}

md_is_rule <- function(line) grepl("^ {0,3}([-*_])( *\\1){2,} *$", line, perl = TRUE)

md_table_cells <- function(line) {
  line <- sub("^ *\\|", "", line)
  line <- sub("\\| *$", "", line)
  trimws(strsplit(line, "|", fixed = TRUE)[[1]])
}

md_is_table_sep <- function(line) grepl("^ *\\|? *:?-+:? *(\\| *:?-+:? *)*\\|? *$", line) && grepl("-", line, fixed = TRUE) && grepl("|", line, fixed = TRUE)

# Parse Markdown text into a list of blocks.
markdown_blocks <- function(text) {
  text <- gsub("\r\n?", "\n", paste(as.character(text), collapse = "\n"))
  text <- sanitize_markdown(text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  md_parse_lines(lines)
}

# Control characters other than newline and tab are dropped.
sanitize_markdown <- function(text) {
  text <- enc2utf8(text)
  text <- gsub("\t", "    ", text, fixed = TRUE)
  gsub("[\001-\011\013\014\016-\037\177]", "", text, perl = TRUE)
}

md_parse_lines <- function(lines) {
  blocks <- list()
  n <- length(lines)
  i <- 1L
  para <- character()
  flush_para <- function() {
    if (length(para)) {
      blocks[[length(blocks) + 1L]] <<- list(type = "paragraph", lines = para)
      para <<- character()
    }
  }
  while (i <= n) {
    line <- lines[[i]]
    if (grepl("^ {0,3}(`{3,}|~{3,})", line)) {
      flush_para()
      fence <- regmatches(line, regexpr("`{3,}|~{3,}", line))
      lang <- trimws(sub("^ {0,3}(`{3,}|~{3,})", "", line))
      code <- character()
      i <- i + 1L
      while (i <= n && !startsWith(trimws(lines[[i]]), fence)) {
        code <- c(code, lines[[i]])
        i <- i + 1L
      }
      blocks[[length(blocks) + 1L]] <- list(type = "code", lines = code, lang = lang)
      i <- i + 1L
      next
    }
    if (!nzchar(trimws(line))) {
      flush_para()
      i <- i + 1L
      next
    }
    if (grepl("^ {0,3}#{1,6}( +|$)", line)) {
      flush_para()
      level <- nchar(gsub("[^#]", "", regmatches(line, regexpr("^ {0,3}#{1,6}", line))))
      body <- sub(" +#+ *$", "", sub("^ {0,3}#{1,6} *", "", line))
      blocks[[length(blocks) + 1L]] <- list(type = "heading", level = level, text = body)
      i <- i + 1L
      next
    }
    if (md_is_rule(line)) {
      flush_para()
      blocks[[length(blocks) + 1L]] <- list(type = "rule")
      i <- i + 1L
      next
    }
    if (grepl("^ {0,3}>", line)) {
      flush_para()
      quoted <- character()
      while (i <= n && grepl("^ {0,3}>", lines[[i]])) {
        quoted <- c(quoted, sub("^ {0,3}> ?", "", lines[[i]]))
        i <- i + 1L
      }
      blocks[[length(blocks) + 1L]] <- list(type = "quote", blocks = md_parse_lines(quoted))
      next
    }
    if (i < n && grepl("|", line, fixed = TRUE) && md_is_table_sep(lines[[i + 1L]])) {
      flush_para()
      header <- md_table_cells(line)
      aligns <- vapply(md_table_cells(lines[[i + 1L]]), function(cell) {
        l <- startsWith(cell, ":")
        r <- endsWith(cell, ":")
        if (l && r) "center" else if (r) "right" else "left"
      }, "")
      i <- i + 2L
      rows <- list()
      while (i <= n && nzchar(trimws(lines[[i]])) && grepl("|", lines[[i]], fixed = TRUE)) {
        rows[[length(rows) + 1L]] <- md_table_cells(lines[[i]])
        i <- i + 1L
      }
      blocks[[length(blocks) + 1L]] <- list(type = "table", header = header, aligns = unname(aligns), rows = rows)
      next
    }
    item <- md_list_item(line)
    if (!is.null(item)) {
      flush_para()
      items <- list()
      while (i <= n) {
        cur <- md_list_item(lines[[i]])
        if (!is.null(cur)) {
          items[[length(items) + 1L]] <- cur
          i <- i + 1L
        } else if (nzchar(trimws(lines[[i]])) && grepl("^ +", lines[[i]]) && length(items)) {
          # a continuation line of the previous item
          items[[length(items)]]$text <- paste(items[[length(items)]]$text, trimws(lines[[i]]))
          i <- i + 1L
        } else break
      }
      blocks[[length(blocks) + 1L]] <- list(type = "list", items = items)
      next
    }
    para <- c(para, line)
    i <- i + 1L
  }
  flush_para()
  blocks
}

# Rendering ---------------------------------------------------------------------------------------

md_seg <- function(text, st = NULL) list(text = text, style = as_style(st))

# Lines for blocks at a width: a list of lines, each a list of segments.
markdown_lines <- function(blocks, width, styles = md_styles()) {
  width <- max(1L, as.integer(width))
  utf8 <- unicode_ok()
  out <- list()
  add <- function(lines) out <<- c(out, lines)
  blank <- function() if (length(out) && length(out[[length(out)]])) out[[length(out) + 1L]] <<- list()
  prefix_lines <- function(lines, first, rest) {
    for (k in seq_along(lines)) {
      pre <- if (k == 1L) first else rest
      lines[[k]] <- c(pre, lines[[k]])
    }
    lines
  }
  for (b in blocks) {
    if (b$type == "heading") {
      blank()
      heading_style <- if (b$level <= 2L) style(bold = TRUE, foreground = "$accent") else style(bold = TRUE)
      segs <- md_inline(md_protect(b$text), heading_style, styles)
      lines <- wrap_line(segs, width, "word")
      add(lines)
      if (b$level == 1L) add(list(list(md_seg(strrep(if (utf8) "\u2550" else "=", width), style(foreground = "$accent")))))
      add(list(list()))
    } else if (b$type == "paragraph") {
      text <- md_protect(paste(vapply(seq_along(b$lines), function(k) {
        l <- b$lines[[k]]
        hard <- grepl(" {2,}$", l) || endsWith(l, "\\")
        paste0(trimws(sub("\\\\$", "", l)), if (hard && k < length(b$lines)) "\n" else if (k < length(b$lines)) " " else "")
      }, ""), collapse = ""))
      segs <- md_inline(text, NULL, styles)
      for (line in text_lines(do.call(c.termr_text, lapply(segs, function(s) structure(list(s), class = "termr_text"))))) {
        add(wrap_line(line, width, "word"))
      }
      add(list(list()))
    } else if (b$type == "code") {
      code_st <- style(foreground = "$accent", background = "$surface")
      for (l in (if (length(b$lines)) b$lines else "")) {
        l <- str_truncate(l, max(1L, width - 2L))
        add(list(list(md_seg(str_align(paste0("  ", l), width), code_st))))
      }
      add(list(list()))
    } else if (b$type == "rule") {
      blank()
      add(list(list(md_seg(strrep(if (utf8) "\u2500" else "-", width), style(foreground = "$muted")))))
      add(list(list()))
    } else if (b$type == "quote") {
      inner <- markdown_lines(b$blocks, max(1L, width - 2L), styles)
      while (length(inner) && !length(inner[[length(inner)]])) inner[[length(inner)]] <- NULL
      bar <- md_seg(if (utf8) "\u2502 " else "| ", style(foreground = "$muted"))
      for (line in inner) {
        line <- lapply(line, function(s) {
          s$style <- merge_styles(style(italic = TRUE), s$style)
          s
        })
        out[[length(out) + 1L]] <- c(list(bar), line)
      }
      add(list(list()))
    } else if (b$type == "list") {
      counters <- list()
      kinds <- list()
      for (item in b$items) {
        level <- item$indent %/% 2L
        key <- as.character(level)
        # deeper counters restart when a shallower item appears
        for (k in names(counters)) if (as.integer(k) > level) counters[[k]] <- NULL
        if (!identical(kinds[[key]], item$ordered)) counters[[key]] <- 0L
        kinds[[key]] <- item$ordered
        counters[[key]] <- (counters[[key]] %||% 0L) + 1L
        marker <- if (item$ordered) paste0(counters[[key]], ". ") else if (utf8) (if (level %% 2L == 0L) "\u2022 " else "\u25e6 ") else "- "
        indent <- strrep("  ", level)
        pad <- strrep(" ", nchar(indent) + str_width(marker))
        segs <- md_inline(md_protect(item$text), NULL, styles)
        room <- max(1L, width - nchar(pad))
        lines <- wrap_line(segs, room, "word")
        lines <- prefix_lines(lines, list(md_seg(paste0(indent, marker), style(foreground = "$accent"))), list(md_seg(pad)))
        add(lines)
      }
      add(list(list()))
    } else if (b$type == "table") {
      cols <- length(b$header)
      cells <- lapply(c(list(b$header), b$rows), function(r) {
        r <- c(r, rep("", max(0L, cols - length(r))))[seq_len(cols)]
        vapply(r, function(x) gsub("\\*\\*|__|`", "", sanitize_text(md_restore(md_protect(x)))), "")
      })
      widths <- vapply(seq_len(cols), function(j) max(vapply(cells, function(r) str_width(r[[j]]), 0L)), 0L)
      widths <- pmax(1L, widths)
      # shrink the widest columns when the table is wider than the view
      sep <- if (utf8) " \u2502 " else " | "
      while (sum(widths) + str_width(sep) * (cols - 1L) > width && max(widths) > 3L) {
        k <- which.max(widths)
        widths[[k]] <- widths[[k]] - 1L
      }
      row_text <- function(r, aligns) paste(vapply(seq_len(cols), function(j) str_align(r[[j]], widths[[j]], aligns[[j]]), ""), collapse = sep)
      aligns <- c(b$aligns, rep("left", cols))[seq_len(cols)]
      add(list(list(md_seg(row_text(cells[[1]], aligns), style(bold = TRUE)))))
      add(list(list(md_seg(paste(strrep(if (utf8) "\u2500" else "-", widths), collapse = if (utf8) "\u2500\u253c\u2500" else "-+-"), style(foreground = "$muted")))))
      for (r in cells[-1L]) add(list(list(md_seg(row_text(r, aligns)))))
      add(list(list()))
    }
  }
  while (length(out) && !length(out[[length(out)]])) out[[length(out)]] <- NULL
  out
}

# Widget --------------------------------------------------------------------------------------------

#' @title MarkdownView widget
#' @description A read-only Markdown document. See [markdown_view()].
#' @rdname MarkdownView-class
#' @export
MarkdownView <- R6::R6Class(
  "MarkdownView",
  inherit = Widget,
  public = list(
    #' @description Create a Markdown view.
    #' @param text Markdown source.
    #' @param id,classes,style See [Widget].
    initialize = function(text = "", id = NULL, classes = NULL, style = NULL) {
      super$initialize(id = id, classes = classes, style = style)
      private$.state$text <- paste(as.character(text), collapse = "\n")
      private$.blocks <- markdown_blocks(private$.state$text)
    },

    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "auto"),

    #' @description Replace the document.
    #' @param text Markdown source.
    set_markdown = function(text) {
      private$.state$text <- paste(as.character(text), collapse = "\n")
      private$.blocks <- markdown_blocks(private$.state$text)
      private$.md_cache <- NULL
      self$invalidate()
      invisible(self)
    },

    #' @description The rendered lines for a width.
    #' @param width Content width.
    render_lines = function(width = NA_integer_) {
      w <- if (is.na(width)) 80L else max(1L, width)
      if (is.null(private$.md_cache) || private$.md_cache$width != w) {
        private$.md_cache <- list(width = w, lines = markdown_lines(private$.blocks, w))
      }
      private$.md_cache$lines
    },

    #' @description Natural width.
    content_width = function() 80L,

    #' @description Natural height for a width.
    #' @param width Content width.
    content_height = function(width) length(self$render_lines(width))
  ),
  active = list(
    #' @field markdown The source text (assigning calls `set_markdown()`).
    markdown = function(value) {
      if (missing(value)) return(private$.state$text)
      self$set_markdown(value)
    },
    #' @field links The links in the document: a data frame (`text`, `url`).
    links = function(value) {
      if (!missing(value)) read_only("links")
      md_collect_links(private$.blocks)
    }
  ),
  private = list(.blocks = NULL, .md_cache = NULL)
)

md_collect_links <- function(blocks) {
  out <- data.frame(text = character(), url = character(), stringsAsFactors = FALSE)
  text_of <- function(b) {
    switch(b$type, paragraph = paste(b$lines, collapse = " "), heading = b$text,
           list = vapply(b$items, function(i) i$text, ""), character())
  }
  for (b in blocks) {
    if (b$type == "quote") {
      out <- rbind(out, md_collect_links(b$blocks))
      next
    }
    for (t in text_of(b)) {
      for (m in regmatches(t, gregexpr("\\[[^]]+\\]\\([^)[:space:]]+\\)", t))[[1]]) {
        parts <- regmatches(m, regexec("\\[([^]]+)\\]\\(([^)]+)\\)", m))[[1]]
        out <- rbind(out, data.frame(text = parts[[2]], url = parts[[3]], stringsAsFactors = FALSE))
      }
    }
  }
  out
}

#' Markdown view
#'
#' Shows Markdown text: headings, paragraphs, **bold**, *italic*,
#' ~~strikethrough~~, `inline code`, fenced code blocks, bullet and numbered
#' lists, block quotes, horizontal rules, simple pipe tables and links. It
#' is meant for help screens, release notes and dialogs; there is no HTML
#' and the parser is not CommonMark-complete. Text is wrapped to the width
#' of the view.
#'
#' With `scroll = TRUE` (the default) the view is placed in a
#' [scroll_view()] that fills its parent; use `scroll = FALSE` for short
#' text inside your own layout.
#'
#' @param text Markdown source (a character vector is joined with newlines).
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param scroll Wrap the view in a scrollable container?
#' @return A [scroll_view()] containing a `MarkdownView` (or the
#'   `MarkdownView` itself with `scroll = FALSE`). Change the text with
#'   `$set_markdown()`; the `MarkdownView` is the first child of the scroll
#'   view.
#' @export
#' @examples
#' md <- markdown_view("# Title\n\nSome *emphasis* and `code`.\n\n- one\n- two",
#'                     scroll = FALSE)
#' render_widget(md, 40, 8)$to_text()
markdown_view <- function(text, id = NULL, classes = NULL, style = NULL, scroll = TRUE) {
  check_flag(scroll)
  view <- MarkdownView$new(text, id = if (scroll) NULL else id, classes = if (scroll) NULL else classes,
                           style = if (scroll) NULL else style)
  if (!scroll) return(view)
  scroll_view(view, id = id, classes = classes, style = style)
}
