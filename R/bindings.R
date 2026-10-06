#' Key bindings
#'
#' `bind()` maps a key (or several keys separated by commas) to an action.
#' Bindings can be attached to the app ([app()]), to the screen, to a widget
#' type ([widget()]) or to a single widget (`widget$bind()`). When a key is
#' pressed, bindings are searched from the focused widget up to the app, so
#' the most local binding wins.
#'
#' An action is either the name of an action or a function. Named actions
#' are looked up on the widget that owns the binding, then on its
#' ancestors, then on the app. Prefix a name with `"app."` to call an app
#' action directly. Built-in app actions are `"quit"`, `"focus_next"`,
#' `"focus_previous"` and `"refresh"`.
#'
#' @param key Key name such as `"q"`, `"ctrl+c"`, `"shift+tab"`, `"f1"`,
#'   or several keys: `"q,escape"`.
#' @param action Action name or a function. Functions bound on the app are
#'   called as `action(app)`; functions bound on a widget as
#'   `action(widget, app)`.
#' @param description Optional human readable description (for help
#'   screens and footers).
#' @return An object of class `termr_binding`.
#' @export
#' @examples
#' bind("q", "quit")
#' bind("ctrl+r", function(app) app$refresh(), "Redraw")
bind <- function(key, action, description = NULL) {
  check_scalar_character(key, "key")
  if (!is_scalar_character(action) && !is.function(action)) {
    stop("`action` must be an action name or a function.", call. = FALSE)
  }
  keys <- if (key == ",") "," else trimws(strsplit(key, ",", fixed = TRUE)[[1]])
  keys <- vapply(keys[nzchar(keys)], normalize_key, character(1), USE.NAMES = FALSE)
  structure(
    list(keys = keys, action = action, description = description),
    class = "termr_binding"
  )
}

#' @export
format.termr_binding <- function(x, ...) {
  action <- if (is.function(x$action)) "<function>" else x$action
  paste0("<binding ", paste(x$keys, collapse = ","), " -> ", action, ">")
}

#' @export
print.termr_binding <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

is_binding <- function(x) inherits(x, "termr_binding")

as_bindings <- function(x) {
  if (is.null(x)) return(list())
  if (is_binding(x)) return(list(x))
  if (!is.list(x) || !all(vapply(x, is_binding, logical(1)))) {
    stop("Bindings must be created with bind().", call. = FALSE)
  }
  x
}

# The binding for `key` in a list of bindings; later bindings win.
find_binding <- function(bindings, key) {
  for (b in rev(bindings)) if (key %in% b$keys) return(b)
  NULL
}

#' Event handlers
#'
#' `on()` creates an event handler that can be passed to [app()] together
#' with the widgets. It is equivalent to calling `app$on()`.
#'
#' The handler runs when an event of the given type reaches the app and,
#' if a selector is given, the widget that sent the event matches it.
#'
#' @param type Event type such as `"button.pressed"`, `"input.submitted"`,
#'   `"key"`, `"resize"`, `"focus"` or `"mount"`. `"*"` matches any type.
#' @param selector Optional selector (e.g. `"#hello"`, `".danger"`,
#'   `"Button"`) matched against the sender (for key and mouse events: the
#'   widget the event was sent to).
#' @param handler `function(event, app)`.
#' @return An object of class `termr_handler`.
#' @export
#' @examples
#' on("button.pressed", "#hello", function(event, app) {
#'   app$query_one("#output")$update("Hello!")
#' })
on <- function(type, selector = NULL, handler) {
  if (missing(handler) && is.function(selector)) {
    handler <- selector
    selector <- NULL
  }
  check_scalar_character(type, "type")
  check_function(handler, "handler")
  if (!is.null(selector)) parse_selector(selector)
  structure(list(type = type, selector = selector, handler = handler), class = "termr_handler")
}

is_handler <- function(x) inherits(x, "termr_handler")

handler_matches <- function(h, event) {
  if (h$type != "*" && h$type != event$type) return(FALSE)
  if (is.null(h$selector)) return(TRUE)
  # Key and mouse events have no sender: match the widget they were sent to.
  widget <- event$sender %||% event$target
  !is.null(widget) && widget$matches(h$selector)
}
