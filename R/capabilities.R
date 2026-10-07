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
  term_lower <- tolower(term)
  program <- get("TERM_PROGRAM")
  dumb <- !windows && term %in% c("dumb", "")
  vt100 <- !windows && grepl("^vt100($|-)", term_lower)
  linux_console <- !windows && identical(term_lower, "linux")
  xterm_family <- grepl("^xterm($|-)", term_lower)
  multiplexer <- grepl("^(screen|tmux)($|-)", term_lower)
  colors <- detect_color_mode(env, windows)
  vte_version <- suppressWarnings(as.integer(get("VTE_VERSION")))
  vte_modern <- length(vte_version) == 1L && !is.na(vte_version) && vte_version >= 5000L
  known_modern <- nzchar(get("WT_SESSION")) || nzchar(get("KITTY_WINDOW_ID")) ||
    nzchar(get("WEZTERM_EXECUTABLE")) || nzchar(get("ALACRITTY_LOG")) ||
    program %in% c("iTerm.app", "WezTerm", "ghostty", "vscode", "Apple_Terminal") ||
    grepl("kitty|alacritty|foot|wezterm|ghostty|contour", term_lower) ||
    vte_modern
  in_ci <- nzchar(get("CI")) && !identical(get("CI"), "false")
  remote_session <- nzchar(get("SSH_TTY")) || nzchar(get("SSH_CONNECTION")) || nzchar(get("SSH_CLIENT"))
  # A local terminal's identifying environment variables do not prove that
  # the corresponding OSC/DEC modes pass through SSH or a multiplexer. A
  # specific TERM entry can still identify the terminal protocol itself.
  direct_modern <- known_modern && !multiplexer &&
    (!remote_session || grepl("kitty|alacritty|foot|wezterm|ghostty|contour", term_lower))
  sgr_mouse <- !dumb && !vt100 && !linux_console &&
    (xterm_family || multiplexer || known_modern)
  bracketed_paste <- !dumb && !vt100 && !linux_console &&
    (xterm_family || multiplexer || known_modern)
  alternate_screen <- !dumb && !vt100 &&
    (xterm_family || multiplexer || linux_console || known_modern)

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
    mouse = sgr_mouse,
    sgr_mouse = sgr_mouse,
    bracketed_paste = bracketed_paste,
    alternate_screen = alternate_screen,
    # DEC private mode 2026 is not implied by a generic xterm-compatible TERM.
    # Enable it only when the terminal itself identifies as a known modern host.
    synchronized_output = direct_modern && !in_ci && !dumb,
    cursor_shape = direct_modern && !in_ci && !dumb,
    hyperlinks = flag("termr.hyperlinks", "TERMR_HYPERLINKS", direct_modern && !in_ci && !dumb),
    osc52 = flag("termr.osc52", "TERMR_OSC52", osc52_default && direct_modern && !dumb)
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
