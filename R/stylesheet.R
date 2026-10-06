# Stylesheets (RTCSS: termr CSS-like stylesheets).
#
# Syntax (a small subset of CSS):
#
#   /* comment */
#   Button { border: round; padding: 0 2; }
#   Button.primary, #save { background: $primary; foreground: $on_primary; }
#   Input:focus { border: heavy $accent; }
#   Vertical > Label { bold: true; }
#
# Selectors are the ones used by query() (type, #id, .class, :pseudo,
# descendant and child combinators, lists). Property names are style()
# arguments; dashes may be used instead of underscores, and `color`,
# `text-align` and `background-color` are accepted as aliases. Values are
# written as in CSS: numbers, keywords, colours (names, #hex, 0-255,
# $theme-colour), several values separated by spaces.
#
# Cascade (lowest to highest priority):
#   1. the widget type's built-in style (with its state styles)
#   2. stylesheet rules, ordered by specificity (ids, then classes and
#      pseudo classes, then types) and, on ties, by source order
#   3. the widget's own style()
#   4. the state styles of the widget's own style()

property_aliases <- c(
  color = "foreground", text_align = "align", background_color = "background",
  vertical_align = "valign", gap = "grid_gap", border_colour = "border_color"
)

#' Stylesheets
#'
#' Parse termr stylesheets (RTCSS, a small CSS-like language) to style many
#' widgets at once by type, id, class and state. Pass them to
#' `app(stylesheet = )` or `app$add_stylesheet()`.
#'
#' ```
#' Button { border: round; padding: 0 2; }
#' Button.danger { background: $error; }
#' #save { width: 20; }
#' Input:focus { border: heavy $accent; }
#' Vertical > Label { bold: true; }
#' ```
#'
#' Properties are the arguments of [style()] (`border-color` and
#' `border_color` both work). `border` also accepts a colour:
#' `border: round cyan`. Colours can refer to the theme: `$primary`.
#' Errors report the line, column and property.
#'
#' Rules apply above a widget type's built-in style and below a widget's
#' own `style`. More specific selectors win (ids over classes and pseudo
#' classes over types); among equally specific rules, later ones win.
#'
#' @param text Stylesheet text.
#' @param path Path to a stylesheet file.
#' @param source Name used in error messages.
#' @return A `termr_stylesheet`.
#' @export
#' @examples
#' sheet <- stylesheet("Button { border: round; } #save { background: $primary; }")
#' sheet
stylesheet <- function(text, source = "<stylesheet>") {
  check_scalar_character(text, "text")
  parse_stylesheet(text, source)
}

#' @rdname stylesheet
#' @export
stylesheet_file <- function(path) {
  if (!file.exists(path)) stop(sprintf("Stylesheet file \"%s\" does not exist.", path), call. = FALSE)
  parse_stylesheet(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), basename(path))
}

as_stylesheet <- function(x) {
  if (inherits(x, "termr_stylesheet")) return(x)
  if (is_scalar_character(x)) {
    if (!grepl("[{]", x) && file.exists(x)) return(stylesheet_file(x))
    return(stylesheet(x))
  }
  stop("A stylesheet must be text, a file path or created with stylesheet().", call. = FALSE)
}

#' @export
format.termr_stylesheet <- function(x, ...) {
  sprintf("<stylesheet %s: %d rule(s)>", attr(x, "source"), length(x))
}

#' @export
print.termr_stylesheet <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  for (rule in unclass(x)) cat("  ", rule$text, " ", format(rule$style), "\n", sep = "")
  invisible(x)
}

stylesheet_error <- function(source, text, offset, message) {
  before <- substr(text, 1L, offset - 1L)
  newlines <- gregexpr("\n", before, fixed = TRUE)[[1]]
  newlines <- newlines[newlines > 0L]
  line <- length(newlines) + 1L
  column <- offset - (if (length(newlines)) max(newlines) else 0L)
  stop(sprintf("%s:%d:%d: %s", source, line, column, message), call. = FALSE)
}

parse_stylesheet <- function(text, source) {
  text <- enc2utf8(text)
  # Blank out comments, keeping offsets (and line numbers) intact.
  comments <- gregexpr("/[*].*?[*]/", text, perl = TRUE)[[1]]
  if (comments[[1]] > 0) {
    lens <- attr(comments, "match.length")
    for (k in seq_along(comments)) {
      piece <- substr(text, comments[[k]], comments[[k]] + lens[[k]] - 1L)
      substr(text, comments[[k]], comments[[k]] + lens[[k]] - 1L) <- gsub("[^\n]", " ", piece)
    }
  }
  if (grepl("/[*]", text)) stylesheet_error(source, text, regexpr("/[*]", text), "unterminated comment")
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  rules <- list()
  order <- 0L
  pos <- 1L
  n <- length(chars)
  while (pos <= n) {
    open <- pos
    while (open <= n && chars[[open]] != "{") {
      if (chars[[open]] == "}") stylesheet_error(source, text, open, "unexpected \"}\"")
      open <- open + 1L
    }
    selector_text <- trimws(paste(chars[seq_len(open - pos) + pos - 1L], collapse = ""))
    if (open > n) {
      if (nzchar(selector_text)) stylesheet_error(source, text, pos, "expected \"{\" after the selector")
      break
    }
    if (!nzchar(selector_text)) stylesheet_error(source, text, open, "missing selector before \"{\"")
    close <- open + 1L
    while (close <= n && chars[[close]] != "}") {
      if (chars[[close]] == "{") stylesheet_error(source, text, close, "nested \"{\" is not allowed")
      close <- close + 1L
    }
    if (close > n) stylesheet_error(source, text, open, "missing \"}\"")
    selector <- tryCatch(parse_selector(selector_text), error = function(e) {
      stylesheet_error(source, text, pos + regexpr("[^[:space:]]", paste(chars[pos:open], collapse = "")) - 1L,
                       sprintf("invalid selector \"%s\"", selector_text))
    })
    body_start <- open + 1L
    body <- paste(chars[seq_len(close - body_start) + body_start - 1L], collapse = "")
    style <- parse_declarations(body, text, body_start, source)
    order <- order + 1L
    for (steps in selector) {
      rules[[length(rules) + 1L]] <- list(
        selector = structure(list(steps), class = "termr_selector"),
        text = selector_text, specificity = selector_specificity(steps),
        order = order, style = style
      )
    }
    pos <- close + 1L
  }
  structure(rules, class = "termr_stylesheet", source = source)
}

parse_declarations <- function(body, text, offset, source) {
  props <- list()
  pieces <- strsplit(body, ";", fixed = TRUE)[[1]]
  starts <- offset + c(0L, cumsum(nchar(pieces) + 1L))[seq_along(pieces)]
  for (k in seq_along(pieces)) {
    decl <- pieces[[k]]
    if (!nzchar(trimws(decl))) next
    at <- starts[[k]] + regexpr("[^[:space:]]", decl) - 1L
    colon <- regexpr(":", decl, fixed = TRUE)
    if (colon < 0) stylesheet_error(source, text, at, sprintf("expected \"property: value\" in \"%s\"", trimws(decl)))
    name <- tolower(gsub("-", "_", trimws(substr(decl, 1L, colon - 1L))))
    value <- trimws(substring(decl, colon + 1L))
    if (name %in% names(property_aliases)) name <- property_aliases[[name]]
    if (!nzchar(value)) stylesheet_error(source, text, at, sprintf("property \"%s\" has no value", name))
    parsed <- tryCatch(css_property(name, value), error = function(e) {
      stylesheet_error(source, text, at, sprintf("property \"%s\": %s", name, conditionMessage(e)))
    })
    props[names(parsed)] <- parsed
  }
  new_style(props)
}

css_kinds <- list(
  size = c("width", "height"),
  count = c("min_width", "max_width", "min_height", "max_height", "column_span", "row_span"),
  numbers = c("margin", "padding", "grid_gap"),
  flag = c("bold", "italic", "underline", "reverse", "dim", "strike"),
  color = c("foreground", "background", "border_color"),
  word = c("align", "valign", "wrap", "layout"),
  tracks = c("grid_columns", "grid_rows")
)

css_kind <- function(name) {
  for (kind in names(css_kinds)) if (name %in% css_kinds[[kind]]) return(kind)
  if (name == "border") return("border")
  stop("unknown property", call. = FALSE)
}

# Convert one declaration to stored style properties (validated by the
# same parsers as style()). Returns a named list (border may set two).
css_property <- function(name, value) {
  tokens <- strsplit(value, "[[:space:]]+")[[1]]
  num <- function(t) {
    v <- suppressWarnings(as.numeric(t))
    if (anyNA(v)) stop(sprintf("expected numbers, got \"%s\"", paste(t, collapse = " ")), call. = FALSE)
    v
  }
  single <- function() {
    if (length(tokens) != 1L) stop(sprintf("expected one value, got \"%s\"", value), call. = FALSE)
    tokens
  }
  kind <- css_kind(name)
  raw <- switch(
    kind,
    size = {
      t <- single()
      if (grepl("^[0-9]+$", t)) as.numeric(t) else t
    },
    count = num(single()),
    numbers = num(tokens),
    flag = {
      t <- tolower(single())
      if (t %in% c("true", "yes", "on")) TRUE else if (t %in% c("false", "no", "off")) FALSE else
        stop(sprintf("expected true or false, got \"%s\"", t), call. = FALSE)
    },
    color = {
      t <- single()
      if (grepl("^[0-9]+$", t)) as.numeric(t) else t
    },
    word = single(),
    tracks = {
      if (length(tokens) == 1L && grepl("^[0-9]+$", tokens)) as.numeric(tokens) else {
        vapply(tokens, function(t) t, "") |> unname()
      }
    },
    border = {
      out <- list(border = style_property_parsers$border(tokens[[1]]))
      if (length(tokens) > 2L) stop("expected a border type and an optional colour", call. = FALSE)
      if (length(tokens) == 2L) {
        col <- if (grepl("^[0-9]+$", tokens[[2]])) as.numeric(tokens[[2]]) else tokens[[2]]
        out$border_color <- style_property_parsers$border_color(col)
      }
      return(out)
    }
  )
  if (kind == "tracks" && is.character(raw)) {
    raw <- lapply(raw, function(t) if (grepl("^[0-9]+$", t)) as.numeric(t) else t)
    out <- list(lapply(raw, parse_size, arg = name))
    if (!length(out[[1]])) stop("expected track sizes", call. = FALSE)
    names(out) <- name
    return(out)
  }
  out <- list(style_property_parsers[[name]](raw))
  names(out) <- name
  out
}

# The merged style of all rules of `sheets` matching `widget` (in its
# current `states`), in cascade order.
stylesheet_style <- function(sheets, widget, states) {
  matched <- list()
  for (k in seq_along(sheets)) {
    for (rule in unclass(sheets[[k]])) {
      if (match_selector(widget, rule$selector, states)) {
        rule$sheet <- k
        matched[[length(matched) + 1L]] <- rule
      }
    }
  }
  if (!length(matched)) return(NULL)
  spec <- t(vapply(matched, function(r) c(r$specificity, r$sheet, r$order), numeric(5)))
  ordered <- matched[order(spec[, 1], spec[, 2], spec[, 3], spec[, 4], spec[, 5])]
  out <- new_style()
  for (rule in ordered) out <- merge_styles(out, rule$style)
  out
}
