# Themes.
#
# A theme maps semantic colour names to colours. Styles and stylesheets
# refer to them as tokens such as "$primary"; tokens are kept as they are in
# styles and resolved when a widget's style is computed (resolve_style()),
# so switching the theme of a running app recolours everything.
#
# The "default" theme maps tokens to the 16 ANSI colours, so apps follow the
# user's terminal palette; "dark" and "light" use 24-bit colours (downgraded
# automatically on terminals with fewer colours).

theme_color_names <- c(
  "foreground", "background", "surface", "muted", "primary", "on_primary",
  "accent", "success", "warning", "error", "information", "stripe"
)

builtin_theme_colors <- function(name) {
  switch(
    name,
    default = list(
      foreground = "", background = "", surface = "bright_black", muted = "bright_black",
      primary = "blue", on_primary = "bright_white", accent = "bright_cyan",
      success = "bright_green", warning = "bright_yellow", error = "bright_red",
      information = "bright_blue", stripe = "#262626"
    ),
    dark = list(
      foreground = "#e0e0e0", background = "#121212", surface = "#2b2b2b", muted = "#7a7a7a",
      primary = "#0178d4", on_primary = "#ffffff", accent = "#4ec9ff",
      success = "#4ebf71", warning = "#ffa62b", error = "#e05a6e", information = "#59a8f0",
      stripe = "#1c1c1c"
    ),
    light = list(
      foreground = "#1f1f1f", background = "#f5f5f5", surface = "#d9d9d9", muted = "#8a8a8a",
      primary = "#0178d4", on_primary = "#ffffff", accent = "#0060a8",
      success = "#2e7d32", warning = "#b26a00", error = "#c62828", information = "#1565c0",
      stripe = "#ebebeb"
    ),
    "high-contrast" = list(
      foreground = "#ffffff", background = "#000000", surface = "#1c1c1c", muted = "#c8c8c8",
      primary = "#ffff00", on_primary = "#000000", accent = "#00ffff",
      success = "#00ff00", warning = "#ffb000", error = "#ff5f5f", information = "#5fafff",
      stripe = "#101010"
    ),
    NULL
  )
}

#' Themes
#'
#' A theme gives names to colours. Use them in styles and stylesheets as
#' `"$name"`, e.g. `style(background = "$primary")`. Built-in widgets use
#' theme colours, so a theme restyles a whole app:
#' `app(ui, theme = "dark")` or `app$theme <- termr_theme("light", accent = "purple")`.
#'
#' Named `termr_theme()` so that it does not mask `ggplot2::theme()`.
#'
#' Colour names: `foreground`, `background` (screen background), `surface`
#' (headers, panels), `muted` (secondary text, guides, disabled widgets),
#' `primary` and `on_primary` (primary buttons, cursors), `accent` (focus),
#' `success`, `warning`, `error`, `information`, `stripe` (zebra rows).
#' Extra names can be added.
#'
#' Built-in themes: `"default"` (the terminal's own 16-colour palette),
#' `"dark"` and `"light"` (24-bit colours) and `"high-contrast"` (black and
#' white with saturated yellow / cyan highlights and a heavier border around the
#' focused widget).
#'
#' On a terminal without colour (`NO_COLOR`, `TERM=dumb`) selections and
#' cursors are drawn in reverse video, errors in bold underline and
#' secondary text dimmed, so nothing depends on colour alone.
#'
#' @param base Name of a built-in theme to start from.
#' @param ... Colours to set, e.g. `primary = "#8a2be2"`.
#' @param name Name of the theme.
#' @return A `termr_theme`.
#' @export
#' @examples
#' termr_theme("dark", accent = "orange")
termr_theme <- function(base = "default", ..., name = base) {
  colors <- builtin_theme_colors(base)
  if (is.null(colors)) stop(sprintf("Unknown theme \"%s\". Built-in themes: default, dark, light, high-contrast.", base), call. = FALSE)
  extra <- list(...)
  if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra))))) {
    stop("Theme colours must be named.", call. = FALSE)
  }
  bad <- names(extra)[!grepl("^[a-z_][a-z0-9_]*$", names(extra))]
  if (length(bad)) stop(sprintf("Invalid theme colour name \"%s\".", bad[[1]]), call. = FALSE)
  colors[names(extra)] <- extra
  colors <- lapply(colors, function(col) {
    col <- normalize_color(col)
    if (startsWith(col, "$")) stop("Theme colours cannot refer to other theme colours.", call. = FALSE)
    col
  })
  # `strong_focus`: the focused widget also gets a heavier border (not only a
  # colour). `mono`: set by the app on terminals without colour (see
  # resolve_style()).
  structure(list(name = name, colors = colors, mono = FALSE, strong_focus = identical(base, "high-contrast")),
            class = "termr_theme")
}

as_theme <- function(x) {
  if (inherits(x, "termr_theme")) return(x)
  if (is_scalar_character(x)) return(termr_theme(x))
  stop("A theme must be a termr_theme() or the name of a built-in theme.", call. = FALSE)
}

#' @export
format.termr_theme <- function(x, ...) {
  cols <- vapply(names(x$colors), function(n) paste0(n, " = ", if (nzchar(x$colors[[n]])) x$colors[[n]] else "default"), "")
  paste0("<theme ", x$name, ": ", paste(cols, collapse = ", "), ">")
}

#' @export
print.termr_theme <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

# The theme used when no app supplies one.
current_theme <- function() {
  app <- current_app()
  if (!is.null(app)) return(app$theme)
  termr_env$default_theme %||% (termr_env$default_theme <- termr_theme("default"))
}

# Resolve a canonical colour that may be a "$token".
theme_color <- function(color, theme = current_theme()) {
  if (is.null(color) || !startsWith(color, "$")) return(color)
  value <- theme$colors[[substring(color, 2L)]]
  if (is.null(value)) {
    stop(sprintf("Unknown theme colour \"%s\" (theme \"%s\").", color, theme$name), call. = FALSE)
  }
  value
}
