# Developer tools: widget inspector, event log and debug overlay.

#' Inspect a widget
#'
#' Collects what termr knows about a widget: type, id, classes, region,
#' states, reactive state values, computed style, matching stylesheet
#' rules, bindings, parent and children. Printing the result gives a
#' readable report.
#'
#' @param x An [App] or a widget.
#' @param selector A selector to find the widget when `x` is an app (or a
#'   container).
#' @return A list of class `termr_inspection`.
#' @export
#' @examples
#' ui <- vertical(button("Save", id = "save", classes = "primary"))
#' inspect_widget(ui, "#save")
inspect_widget <- function(x, selector = NULL) {
  widget <- if (!is.null(selector)) x$query_one(selector) else x
  if (!is_widget(widget)) stop("`x` must be a widget, or give a selector.", call. = FALSE)
  st <- widget$computed_style()
  app <- widget$app
  rules <- character()
  if (!is.null(app)) {
    for (sheet in app$stylesheets) {
      for (rule in unclass(sheet)) {
        if (match_selector(widget, rule$selector)) rules <- c(rules, rule$text)
      }
    }
  }
  state <- as.list(widget_private(widget)$.state)
  structure(
    list(
      type = widget$type,
      class_chain = setdiff(class(widget), "R6"),
      id = widget$id,
      classes = widget$classes,
      region = widget$region,
      visible = widget$visible,
      displayed = widget$is_displayed(),
      enabled = widget$is_enabled(),
      focusable = widget$focusable,
      states = widget$pseudo_states(),
      state = state[order(names(state))],
      style = st[c("width", "height", "margin", "padding", "border", "layout", "align", "valign",
                   "wrap", "foreground", "background", "border_color")],
      stylesheet_rules = unique(rules),
      bindings = vapply(widget$bindings(), format, character(1)),
      parent = if (is.null(widget$parent)) NA_character_ else widget$parent$format(),
      children = vapply(widget$children, function(w) w$format(), character(1))
    ),
    class = "termr_inspection"
  )
}

#' @export
print.termr_inspection <- function(x, ...) {
  line <- function(label, value) cat(sprintf("%-11s %s\n", paste0(label, ":"), value))
  show <- function(v) {
    if (is.null(v) || !length(v)) return("-")
    if (inherits(v, "termr_rect")) return(format(v))
    if (inherits(v, "termr_size")) return(format(v))
    if (is.list(v)) return(paste(vapply(v, function(e) paste(format(e), collapse = " "), ""), collapse = ", "))
    paste(format(v), collapse = " ")
  }
  cat("<", x$type, if (!is.null(x$id)) paste0(" #", x$id), ">\n", sep = "")
  line("classes", show(x$classes))
  line("types", paste(x$class_chain, collapse = " < "))
  line("region", show(x$region))
  line("visible", sprintf("%s (displayed: %s), enabled: %s, focusable: %s", x$visible, x$displayed, x$enabled, x$focusable))
  line("states", show(x$states))
  line("parent", x$parent)
  line("children", if (length(x$children)) paste(x$children, collapse = ", ") else "-")
  cat("state:\n")
  for (n in names(x$state)) cat(sprintf("  %-12s %s\n", n, show(x$state[[n]])))
  cat("style:\n")
  for (n in names(x$style)) cat(sprintf("  %-12s %s\n", n, show(x$style[[n]])))
  if (length(x$stylesheet_rules)) cat("rules:      ", paste(x$stylesheet_rules, collapse = " | "), "\n")
  if (length(x$bindings)) cat("bindings:   ", paste(x$bindings, collapse = " "), "\n")
  invisible(x)
}

# The debug overlay: a small panel in the top-right corner with live
# information about the app. It is a separate layer that never takes focus
# or mouse input, refreshed by a timer while it is shown.
DebugLayer <- R6::R6Class(
  "DebugLayer",
  inherit = Screen,
  public = list(
    default_style = function() {
      style(width = "1fr", height = "1fr", layout = "vertical", align = "right", valign = "top", padding = c(1, 1, 0, 0))
    }
  )
)

debug_report <- function(app) {
  p <- app$.__enclos_env__$private
  stats <- app$frame_stats
  widgets <- sum(vapply(app$screens, function(s) length(s$walk()), integer(1)))
  times <- p$.frame_times
  now <- p$clock()
  rate <- sum(times > now - 1)
  last <- p$.last_event
  profile <- app$profile_last_frame
  focused <- app$focused
  hovered <- app$hovered
  list(
    Size = sprintf("%dx%d", app$size[["width"]], app$size[["height"]]),
    Screens = length(app$screens),
    Widgets = widgets,
    Focused = if (is.null(focused)) "-" else focused$format(),
    Hovered = if (is.null(hovered)) "-" else hovered$format(),
    `Last event` = if (is.null(last)) "-" else last,
    Repaints = sprintf("%d full, %d incremental", stats[["full"]], stats[["incremental"]]),
    `Repaints/s` = rate,
    `Last frame ms` = if (is.null(profile)) "profiling off" else profile$frame_ms,
    `Layout/paint/diff ms` = if (is.null(profile)) "-" else paste(round(c(profile$layout_ms, profile$paint_ms, profile$diff_ms), 2), collapse = "/"),
    Workers = length(app$workers()),
    Timers = length(p$.timers$timers)
  )
}

toggle_debug_overlay <- function(app) {
  p <- app$.__enclos_env__$private
  if (!is.null(p$.debug)) {
    if (!is.null(p$.debug_timer)) p$.debug_timer$cancel()
    p$.debug <- NULL
    p$.debug_timer <- NULL
    app$refresh()
    return(invisible(FALSE))
  }
  layer <- DebugLayer$new()
  lp <- widget_private(layer)
  lp$.app <- app
  info <- key_value(debug_report(app), id = "debug-info")
  layer$mount(panel(info, title = "termr debug (F12)", style = style(width = "auto", max_width = 70, background = "default", border_color = "$warning")))
  p$.debug <- layer
  p$.debug_timer <- app$set_interval(0.5, function(app) {
    if (!is.null(p$.debug)) info$data <- debug_report(app)
  })
  app$refresh()
  invisible(TRUE)
}
