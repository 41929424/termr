#' Terminal capabilities
#'
#' What the terminal can do, detected once by the driver and consulted by
#' everything above it. Detection is conservative: a feature that cannot be
#' recognised from `TERM`, `COLORTERM`, `NO_COLOR`, `CI` or a few
#' terminal-specific variables is reported as unavailable and termr falls
#' back (e.g. to its internal clipboard instead of OSC 52).
#'
#' Fields of the returned list:
#' * `colors`: `"truecolor"`, `"256"`, `"16"` or `"none"`;
#' * `truecolor`, `unicode`, `mouse`, `sgr_mouse`, `bracketed_paste`,
#'   `alternate_screen`, `synchronized_output`, `cursor_shape`,
#'   `hyperlinks` (OSC 8), `osc52` (clipboard write): logical.
#'
#' Overrides (all optional): `options(termr.color_mode = )`,
#' `options(termr.ascii = TRUE)`, `options(termr.osc52 = TRUE/FALSE)`,
#' `options(termr.hyperlinks = TRUE/FALSE)`, or the environment variables
#' `TERMR_OSC52` and `TERMR_HYPERLINKS` set to `1` / `0`.
#'
#' @param env Environment variables (named character vector or list);
#'   defaults to the current ones.
#' @param windows Is this a Windows console?
#' @param overrides Named list of capabilities that replace detected ones.
#' @return An object of class `termr_capabilities`.
#' @examples
#' terminal_capabilities(env = c(TERM = "xterm-256color"), windows = FALSE)
#' terminal_capabilities(env = c(TERM = "dumb"), windows = FALSE)$mouse
#' @export
terminal_capabilities <- function(env = Sys.getenv(), windows = .Platform$OS.type == "windows",
                                  overrides = list()) {
  get <- function(name) env_value(env, name)
  term <- get("TERM")
  program <- get("TERM_PROGRAM")
  dumb <- !windows && term %in% c("dumb", "")
  colors <- detect_color_mode(env, windows)
  known_modern <- nzchar(get("WT_SESSION")) || nzchar(get("KITTY_WINDOW_ID")) ||
    nzchar(get("WEZTERM_EXECUTABLE")) || nzchar(get("ALACRITTY_LOG")) ||
    program %in% c("iTerm.app", "WezTerm", "ghostty", "vscode", "Apple_Terminal") ||
    grepl("kitty|alacritty|foot|wezterm|ghostty|contour", term) ||
    (nzchar(get("VTE_VERSION")) && suppressWarnings(as.integer(get("VTE_VERSION"))) >= 5000L)
  in_ci <- nzchar(get("CI")) && !identical(get("CI"), "false")

  flag <- function(option, var, default) {
    value <- getOption(option)
    if (is.null(value)) {
      v <- get(var)
      value <- if (v %in% c("1", "true", "yes")) TRUE else if (v %in% c("0", "false", "no")) FALSE else NULL
    }
    if (is.null(value)) default else isTRUE(value)
  }
  osc52_default <- known_modern && program != "Apple_Terminal" && !in_ci
  caps <- list(
    colors = colors,
    truecolor = identical(colors, "truecolor"),
    unicode = unicode_ok(),
    mouse = !dumb,
    sgr_mouse = !dumb,
    bracketed_paste = !dumb,
    alternate_screen = !dumb,
    synchronized_output = !dumb,
    cursor_shape = !dumb,
    hyperlinks = flag("termr.hyperlinks", "TERMR_HYPERLINKS", known_modern && !in_ci && !dumb),
    osc52 = flag("termr.osc52", "TERMR_OSC52", osc52_default && !dumb)
  )
  # A Windows console host without VT support is handled by the driver; the
  # capabilities describe the terminal the driver ends up with.
  for (name in names(overrides)) caps[[name]] <- overrides[[name]]
  structure(caps, class = "termr_capabilities")
}

# Capabilities of the headless test terminal: everything on, so tests
# exercise every code path. Pass `overrides` to turn features off.
headless_capabilities <- function(color_mode = "truecolor", ...) {
  caps <- terminal_capabilities(env = c(TERM = "xterm-256color"), windows = FALSE,
                                overrides = list(colors = color_mode, truecolor = identical(color_mode, "truecolor"),
                                                 unicode = TRUE, osc52 = TRUE, hyperlinks = TRUE, ...))
  caps
}

#' @export
print.termr_capabilities <- function(x, ...) {
  cat("<termr_capabilities>\n")
  for (name in names(x)) cat(sprintf("  %-20s %s\n", name, format(x[[name]])))
  invisible(x)
}
