#' Widget styles
#'
#' `style()` describes how a widget looks and how much space it takes. Only
#' the properties you set are stored, so styles can be layered: a widget's
#' built-in default style is overridden by the style passed to its
#' constructor, and state styles (`focus`, `disabled`, ...) are applied on
#' top while the widget is in that state.
#'
#' ## Sizes
#' `width` and `height` accept:
#' * a number: a fixed number of cells;
#' * `"auto"`: the natural size of the content;
#' * `"<n>fr"`: a share of the remaining space (e.g. `"1fr"`, `"2fr"`);
#' * `"<n>%"`: a percentage of the parent's content area.
#'
#' ## Colours
#' `foreground`, `background` and `border_color` accept ANSI names
#' (`"red"`, `"bright_blue"`, `"grey"`), integers 0-255 (xterm palette),
#' hex strings (`"#ff8700"`) and R colour names (`"steelblue"`).
#'
#' @param width,height Size, see above.
#' @param min_width,max_width,min_height,max_height Size limits in cells.
#' @param margin,padding Space outside / inside the border. One value (all
#'   sides), two values (vertical, horizontal) or four values (top, right,
#'   bottom, left).
#' @param border Border type: `"none"`, `"ascii"`, `"single"`, `"round"`,
#'   `"double"`, `"heavy"` or `"blank"`.
#' @param border_color Border colour (defaults to the foreground colour).
#' @param align Horizontal alignment of text, or of children inside a
#'   container: `"left"`, `"center"` or `"right"`.
#' @param valign Vertical alignment: `"top"`, `"middle"` or `"bottom"`.
#' @param wrap How text that is wider than the widget is wrapped: `"none"`
#'   (clip, the default), `"word"` or `"char"`. Wrapped text makes
#'   `"auto"` heights grow.
#' @param layout How a container arranges its children: `"vertical"` or
#'   `"horizontal"`.
#' @param foreground,background Text and background colours.
#' @param bold,italic,underline,reverse,dim,strike Text attributes.
#' @param grid_columns,grid_rows Grid tracks for `layout = "grid"`: a number
#'   of equal columns, or a vector of sizes such as `c("auto", "1fr", 20)`.
#'   Rows default to `"auto"` and are added as needed.
#' @param grid_gap Space between grid cells: one value, or `c(rows, columns)`.
#' @param column_span,row_span How many grid cells a child occupies.
#' @param focus,hover,disabled Styles applied while the widget has focus,
#'   is under the mouse pointer, or is disabled.
#' @param states A named list of styles for other widget states, e.g.
#'   `list(pressed = style(reverse = TRUE))` for buttons.
#' @return An object of class `termr_style`.
#' @export
#' @examples
#' style(width = 20, padding = c(0, 1), border = "round",
#'       foreground = "white", background = "blue", bold = TRUE,
#'       focus = style(background = "bright_blue"))
style <- function(width = NULL, height = NULL,
                  min_width = NULL, max_width = NULL,
                  min_height = NULL, max_height = NULL,
                  margin = NULL, padding = NULL,
                  border = NULL, border_color = NULL,
                  align = NULL, valign = NULL, wrap = NULL, layout = NULL,
                  foreground = NULL, background = NULL,
                  bold = NULL, italic = NULL, underline = NULL,
                  reverse = NULL, dim = NULL, strike = NULL,
                  grid_columns = NULL, grid_rows = NULL, grid_gap = NULL,
                  column_span = NULL, row_span = NULL,
                  focus = NULL, hover = NULL, disabled = NULL, states = NULL) {
  values <- list(
    width = width, height = height, min_width = min_width, max_width = max_width,
    min_height = min_height, max_height = max_height, margin = margin, padding = padding,
    border = border, border_color = border_color, align = align, valign = valign,
    wrap = wrap, layout = layout, foreground = foreground, background = background,
    bold = bold, italic = italic, underline = underline, reverse = reverse, dim = dim,
    strike = strike, grid_columns = grid_columns, grid_rows = grid_rows, grid_gap = grid_gap,
    column_span = column_span, row_span = row_span
  )
  props <- parse_style_props(values)
  state_styles <- states %||% list()
  if (length(state_styles) && (is.null(names(state_styles)) || any(!nzchar(names(state_styles))))) {
    stop("`states` must be a named list of styles.", call. = FALSE)
  }
  if (!is.null(focus)) state_styles$focus <- focus
  if (!is.null(hover)) state_styles$hover <- hover
  if (!is.null(disabled)) state_styles$disabled <- disabled
  state_styles <- lapply(state_styles, as_style)
  new_style(props, state_styles)
}

# Validators for every style property: each takes a user value and returns
# the stored value (or signals an error naming the property). Shared by
# style() and the stylesheet parser.
style_property_parsers <- list(
  width = function(v) parse_size(v, "width"),
  height = function(v) parse_size(v, "height"),
  min_width = function(v) check_count(v, "min_width"),
  max_width = function(v) check_count(v, "max_width"),
  min_height = function(v) check_count(v, "min_height"),
  max_height = function(v) check_count(v, "max_height"),
  margin = function(v) as_edges(v, "margin"),
  padding = function(v) as_edges(v, "padding"),
  border = function(v) check_choice(v, names(border_sets), "border"),
  border_color = function(v) normalize_color(v),
  align = function(v) check_choice(v, c("left", "center", "right"), "align"),
  valign = function(v) check_choice(v, c("top", "middle", "bottom"), "valign"),
  wrap = function(v) check_choice(v, wrap_modes, "wrap"),
  layout = function(v) check_choice(v, ls(layout_algorithms, all.names = TRUE), "layout"),
  foreground = function(v) normalize_color(v),
  background = function(v) normalize_color(v),
  bold = function(v) check_optional_flag(v, "bold"),
  italic = function(v) check_optional_flag(v, "italic"),
  underline = function(v) check_optional_flag(v, "underline"),
  reverse = function(v) check_optional_flag(v, "reverse"),
  dim = function(v) check_optional_flag(v, "dim"),
  strike = function(v) check_optional_flag(v, "strike"),
  grid_columns = function(v) parse_tracks(v, "grid_columns"),
  grid_rows = function(v) parse_tracks(v, "grid_rows"),
  grid_gap = function(v) parse_gap(v),
  column_span = function(v) parse_span(v, "column_span"),
  row_span = function(v) parse_span(v, "row_span")
)

parse_style_props <- function(values) {
  values <- values[!vapply(values, is.null, logical(1))]
  props <- lapply(names(values), function(name) style_property_parsers[[name]](values[[name]]))
  names(props) <- names(values)
  props
}

# Grid tracks: a number (that many "1fr" tracks) or a vector of sizes.
parse_tracks <- function(v, arg) {
  if (is_scalar_number(v) && v >= 1 && v == round(v)) return(rep(list(size_spec("fr", 1)), v))
  if (!(is.character(v) || is.numeric(v)) || length(v) == 0L || anyNA(v)) {
    stop(sprintf("`%s` must be a number of tracks or a vector of sizes.", arg), call. = FALSE)
  }
  lapply(as.list(v), parse_size, arg = arg)
}

parse_gap <- function(v) {
  if (!is.numeric(v) || !length(v) %in% 1:2 || anyNA(v) || any(v < 0)) {
    stop("`grid_gap` must be one or two non-negative numbers (rows, columns).", call. = FALSE)
  }
  as.integer(rep_len(v, 2L))
}

parse_span <- function(v, arg) {
  if (!is_scalar_number(v) || v < 1 || v != round(v)) {
    stop(sprintf("`%s` must be a whole number of at least 1.", arg), call. = FALSE)
  }
  as.integer(v)
}

new_style <- function(props = list(), states = list()) {
  structure(list(props = props, states = states), class = "termr_style")
}

is_style <- function(x) inherits(x, "termr_style")

as_style <- function(x) {
  if (is.null(x)) return(new_style())
  if (is_style(x)) return(x)
  if (is.list(x)) return(do.call(style, x))
  stop("A style must be created with style() or be a named list.", call. = FALSE)
}

# Layer `b` over `a`: properties set in `b` win; state styles merge per state.
merge_styles <- function(a, b) {
  if (is.null(b)) return(a)
  if (is.null(a)) return(b)
  props <- a$props
  props[names(b$props)] <- b$props
  states <- a$states
  for (name in names(b$states)) {
    states[[name]] <- merge_styles(states[[name]], b$states[[name]])
  }
  new_style(props, states)
}

#' @export
format.termr_style <- function(x, ...) {
  fmt <- function(v) {
    if (inherits(v, "termr_size")) return(format(v))
    if (is.list(v)) return(paste(vapply(v, format, character(1)), collapse = " "))
    if (is.character(v)) return(encodeString(v, quote = "\""))
    paste(v, collapse = " ")
  }
  props <- vapply(names(x$props), function(n) paste0(n, " = ", fmt(x$props[[n]])), character(1))
  states <- vapply(names(x$states), function(n) paste0(n, " = ", format(x$states[[n]])), character(1))
  paste0("<style ", paste(c(props, states), collapse = ", "), ">")
}

#' @export
print.termr_style <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

# Sizes ---------------------------------------------------------------------

size_spec <- function(type, value = NA_real_) {
  structure(list(type = type, value = value), class = "termr_size")
}

#' @export
format.termr_size <- function(x, ...) {
  switch(
    x$type,
    fixed = as.character(x$value),
    auto = "auto",
    fr = paste0(x$value, "fr"),
    percent = paste0(x$value * 100, "%")
  )
}

parse_size <- function(x, arg = "size") {
  if (is.null(x) || inherits(x, "termr_size")) return(x)
  if (is_scalar_number(x) && x >= 0) return(size_spec("fixed", as.integer(round(x))))
  if (is_scalar_character(x)) {
    s <- tolower(trimws(x))
    num <- "^[0-9]*\\.?[0-9]+"
    if (s == "auto") return(size_spec("auto"))
    if (grepl(paste0(num, "fr$"), s)) return(size_spec("fr", as.numeric(sub("fr$", "", s))))
    if (grepl(paste0(num, "%$"), s)) return(size_spec("percent", as.numeric(sub("%$", "", s)) / 100))
    if (grepl("^[0-9]+$", s)) return(size_spec("fixed", as.integer(s)))
  }
  stop(sprintf(
    "Invalid `%s`: use a number of cells, \"auto\", \"<n>fr\" or \"<n>%%\".", arg
  ), call. = FALSE)
}

# Validation helpers --------------------------------------------------------

check_choice <- function(x, choices, arg) {
  if (is.null(x)) return(NULL)
  if (!is_scalar_character(x) || !(x %in% choices)) {
    stop(sprintf("`%s` must be one of: %s.", arg, paste0("\"", choices, "\"", collapse = ", ")),
         call. = FALSE)
  }
  x
}

check_optional_flag <- function(x, arg) {
  if (is.null(x)) return(NULL)
  check_flag(x, arg)
  x
}

check_count <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (!is_scalar_number(x) || x < 0) {
    stop(sprintf("`%s` must be a non-negative number.", arg), call. = FALSE)
  }
  as.integer(x)
}

# Resolution ----------------------------------------------------------------

# Apply the state styles of the active states and drop the rest. A state
# key may combine states ("focus:hover"); it applies when all of them are
# active, after the single states.
flatten_style <- function(st, active_states = character()) {
  if (is.null(st)) return(new_style())
  if (!length(st$states)) return(st)
  keys <- names(st$states)
  combined <- grepl(":", keys, fixed = TRUE)
  props <- st$props
  for (state in active_states) {
    s <- st$states[[state]]
    if (!is.null(s) && !(state %in% keys[combined])) props[names(s$props)] <- s$props
  }
  for (key in keys[combined]) {
    if (all(strsplit(key, ":", fixed = TRUE)[[1]] %in% active_states)) {
      props[names(st$states[[key]]$props)] <- st$states[[key]]$props
    }
  }
  new_style(props)
}

# Turn a layered style into a complete set of properties, inheriting the
# foreground colour from the parent.
resolve_style <- function(st, active_states = character(), parent = NULL, theme = NULL) {
  st <- flatten_style(st, active_states)
  p <- st$props
  theme <- theme %||% current_theme()
  mono_attrs <- character()
  if (isTRUE(theme$mono)) {
    bgt <- p$background
    fgt <- p$foreground
    if (is.character(bgt) && startsWith(bgt, "$") && substring(bgt, 2L) %in% mono_highlight_tokens) {
      mono_attrs <- c(mono_attrs, "reverse")
      p$background <- NULL
      if (is.character(fgt) && startsWith(fgt, "$")) p$foreground <- NULL
    } else if (is.character(fgt) && startsWith(fgt, "$")) {
      mono_attrs <- c(mono_attrs, switch(substring(fgt, 2L), error = c("bold", "underline"), warning = "bold",
                                         muted = "dim", character()))
    }
  }
  fg <- if (!is.null(p$foreground)) {
    theme_color(p$foreground, theme)
  } else if (!is.null(parent)) {
    parent$foreground
  } else {
    theme_color("$foreground", theme)
  }
  list(
    width = p$width %||% size_spec("fr", 1),
    height = p$height %||% size_spec("auto"),
    min_width = p$min_width,
    max_width = p$max_width,
    min_height = p$min_height,
    max_height = p$max_height,
    margin = p$margin %||% c(0L, 0L, 0L, 0L),
    padding = p$padding %||% c(0L, 0L, 0L, 0L),
    border = p$border %||% "none",
    border_color = theme_color(p$border_color, theme) %||% fg,
    align = p$align %||% "left",
    valign = p$valign %||% "top",
    wrap = p$wrap %||% "none",
    grid_columns = p$grid_columns %||% list(size_spec("fr", 1)),
    grid_rows = p$grid_rows,
    grid_gap = p$grid_gap %||% c(0L, 0L),
    column_span = p$column_span %||% 1L,
    row_span = p$row_span %||% 1L,
    layout = p$layout %||% "vertical",
    foreground = fg,
    background = theme_color(p$background, theme),
    attrs = attrs_encode(
      bold = isTRUE(p$bold) || "bold" %in% mono_attrs, dim = isTRUE(p$dim) || "dim" %in% mono_attrs,
      italic = isTRUE(p$italic),
      underline = isTRUE(p$underline) || "underline" %in% mono_attrs,
      reverse = xor(isTRUE(p$reverse), "reverse" %in% mono_attrs),
      strike = isTRUE(p$strike)
    )
  )
}

# Background tokens that mark something as selected / active / important;
# without colour they become reverse video.
mono_highlight_tokens <- c("primary", "accent", "success", "warning", "error", "information")

# The part of a style that decides sizes and positions. Two styles with the
# same signature lay out identically (colours and text attributes differ
# only in how things are painted).
layout_style_props <- c(
  "width", "height", "min_width", "max_width", "min_height", "max_height", "margin", "padding",
  "border", "align", "valign", "wrap", "layout", "grid_columns", "grid_rows", "grid_gap",
  "column_span", "row_span"
)

style_layout_signature <- function(st) {
  if (is.null(st)) return(NULL)
  list(
    props = st$props[intersect(names(st$props), layout_style_props)],
    states = lapply(st$states, style_layout_signature)
  )
}
