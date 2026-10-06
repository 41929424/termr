# CSS-like selectors.
#
# Grammar (a subset of CSS):
#   selector_list := selector ("," selector)*
#   selector      := compound (combinator compound)*
#   combinator    := whitespace (descendant) | ">" (child)
#   compound      := [Type | "*"] ("#" id | "." class | ":" pseudo)*
#
# Pseudo classes match the widget's current states (pseudo_states():
# focus, hover, disabled, and widget-specific ones such as pressed).
#
# Type names match the widget's R6 class or any class it inherits from, so
# "Widget" matches everything and "Button" matches subclasses of Button.
# Parsed selectors are cached. This module is shared with a future
# stylesheet engine.

selector_cache <- new.env(parent = emptyenv())

parse_selector <- function(selector) {
  if (inherits(selector, "termr_selector")) return(selector)
  check_scalar_character(selector, "selector")
  cached <- selector_cache[[selector]]
  if (!is.null(cached)) return(cached)
  alternatives <- strsplit(selector, ",", fixed = TRUE)[[1]]
  parsed <- lapply(alternatives, parse_complex_selector, original = selector)
  out <- structure(parsed, class = "termr_selector", text = selector)
  selector_cache[[selector]] <- out
  out
}

parse_complex_selector <- function(text, original) {
  text <- trimws(gsub(">", " > ", text, fixed = TRUE))
  tokens <- strsplit(text, "[[:space:]]+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  bad <- function() stop(sprintf("Invalid selector \"%s\".", original), call. = FALSE)
  if (length(tokens) == 0L) bad()
  steps <- list()
  combinator <- " "
  for (tok in tokens) {
    if (tok == ">") {
      if (length(steps) == 0L || combinator == ">") bad()
      combinator <- ">"
      next
    }
    compound <- parse_compound(tok)
    if (is.null(compound)) bad()
    compound$combinator <- if (length(steps) == 0L) NA_character_ else combinator
    steps[[length(steps) + 1L]] <- compound
    combinator <- " "
  }
  if (combinator == ">") bad()
  steps
}

parse_compound <- function(token) {
  ident <- "[A-Za-z_][A-Za-z0-9_-]*"
  pattern <- sprintf("^(%s|[*])?([#.:]%s)*$", ident, ident)
  if (!grepl(pattern, token)) return(NULL)
  type <- regmatches(token, regexpr(sprintf("^(%s|[*])", ident), token))
  if (length(type) == 0L) type <- ""
  rest <- substring(token, nchar(type) + 1L)
  parts <- regmatches(rest, gregexpr(sprintf("[#.:]%s", ident), rest))[[1]]
  ids <- substring(parts[startsWith(parts, "#")], 2L)
  if (length(ids) > 1L) return(NULL)
  list(
    type = if (!type %in% c("", "*")) type else NULL,
    id = if (length(ids)) ids else NULL,
    classes = substring(parts[startsWith(parts, ".")], 2L),
    pseudo = substring(parts[startsWith(parts, ":")], 2L)
  )
}

# `states` overrides the widget's own pseudo states (used by the cascade).
match_compound <- function(widget, compound, states = NULL) {
  if (!is.null(compound$type) && !(compound$type %in% class(widget))) return(FALSE)
  if (!is.null(compound$id) && !identical(widget$id, compound$id)) return(FALSE)
  if (!all(compound$classes %in% widget$classes)) return(FALSE)
  if (length(compound$pseudo)) {
    states <- states %||% widget$pseudo_states()
    if (!all(compound$pseudo %in% states)) return(FALSE)
  }
  TRUE
}

# Specificity of one complex selector: c(ids, classes + pseudo, types).
selector_specificity <- function(steps) {
  ids <- sum(vapply(steps, function(s) length(s$id), integer(1)))
  classes <- sum(vapply(steps, function(s) length(s$classes) + length(s$pseudo), integer(1)))
  types <- sum(vapply(steps, function(s) length(s$type), integer(1)))
  c(ids, classes, types)
}

match_selector <- function(widget, selector, states = NULL) {
  for (steps in selector) {
    if (match_steps(widget, steps, length(steps), states)) return(TRUE)
  }
  FALSE
}

# Does `widget` match steps[1..i], with steps[[i]] matched by `widget`?
# `states` applies to the subject only; ancestors use their own states.
match_steps <- function(widget, steps, i, states = NULL) {
  if (!match_compound(widget, steps[[i]], states)) return(FALSE)
  if (i == 1L) return(TRUE)
  combinator <- steps[[i]]$combinator
  node <- widget$parent
  if (combinator == ">") {
    return(!is.null(node) && match_steps(node, steps, i - 1L))
  }
  while (!is.null(node)) {
    if (match_steps(node, steps, i - 1L)) return(TRUE)
    node <- node$parent
  }
  FALSE
}
