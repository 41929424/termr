#' Define a custom widget type
#'
#' `widget()` creates a new widget type and returns its constructor. All
#' behaviour is given as plain functions that receive the widget as their
#' first argument, `self`.
#'
#' @param name Type name (used by type selectors, e.g. `app$query("Counter")`).
#' @param state A named list of state fields. Values created with
#'   [reactive()] can have a watcher; plain values are reactive too.
#'   Fields are read and assigned as `self$<name>`.
#' @param render `function(self)` returning the content: a string, a
#'   character vector of lines or [span()]s.
#' @param compose `function(self)` returning a list of child widgets.
#' @param bindings A list of [bind()] key bindings.
#' @param actions A named list of `function(self)`; bindings refer to them
#'   by name.
#' @param on A named list of event handlers `function(self, event)`, e.g.
#'   `list(mount = ..., "button.pressed" = ...)`.
#' @param methods A named list of extra methods `function(self, ...)`,
#'   callable as `widget$<name>(...)`.
#' @param style Default [style()] of the type.
#' @param focusable Can widgets of this type receive focus?
#' @param inherit Parent class: [Widget], [Vertical] or [Horizontal].
#' @return A constructor function
#'   `function(..., id = NULL, classes = NULL, style = NULL, disabled = FALSE)`.
#'   Named arguments in `...` set initial state values; unnamed arguments are
#'   child widgets.
#' @export
#' @examples
#' Counter <- widget(
#'   "Counter",
#'   state = list(count = reactive(0L)),
#'   render = function(self) sprintf("Count: %d", self$count),
#'   bindings = list(bind("up", "increment"), bind("down", "decrement")),
#'   actions = list(
#'     increment = function(self) self$count <- self$count + 1L,
#'     decrement = function(self) self$count <- self$count - 1L
#'   ),
#'   focusable = TRUE
#' )
#' counter <- Counter(count = 5L)
#' counter$count
#' render_widget(counter, 12, 1)
widget <- function(name, state = list(), render = NULL, compose = NULL,
                   bindings = list(), actions = list(), on = list(),
                   methods = list(), style = NULL, focusable = FALSE,
                   inherit = Widget) {
  check_scalar_character(name, "name")
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) {
    stop("`name` must start with a letter and contain only letters, digits and '_'.", call. = FALSE)
  }
  if (!inherits(inherit, "R6ClassGenerator") || !generator_inherits(inherit, "Widget")) {
    stop("`inherit` must be Widget or a widget class such as Vertical.", call. = FALSE)
  }
  check_function(render, "render", allow_null = TRUE)
  check_function(compose, "compose", allow_null = TRUE)
  check_flag(focusable)
  state <- check_named_list(state, "state")
  state <- lapply(state, as_reactive)
  check_member_names(names(state), "state")
  actions <- check_named_functions(actions, "actions")
  handlers <- check_named_functions(on, "on")
  methods <- check_named_functions(methods, "methods")
  check_member_names(names(methods), "methods")
  both <- intersect(names(state), names(methods))
  if (length(both)) stop(sprintf("\"%s\" is both a state field and a method.", both[[1]]), call. = FALSE)
  bindings <- as_bindings(bindings)
  type_style <- as_style(style %||% (if (identical(inherit, Widget)) style(width = "1fr", height = "auto")))
  spec <- list(
    state = state, render = render, compose = compose, bindings = bindings,
    actions = actions, handlers = handlers, methods = methods
  )

  public <- list(
    focusable = focusable,
    initialize = function(..., id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      args <- list(...)
      arg_names <- names(args) %||% rep("", length(args))
      values <- args[nzchar(arg_names)]
      unknown <- setdiff(names(values), names(spec$state))
      if (length(unknown)) {
        stop(sprintf("%s has no state field \"%s\".", class(self)[[1]], unknown[[1]]), call. = FALSE)
      }
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      for (field in names(spec$state)) {
        private$.state[[field]] <- if (field %in% names(values)) values[[field]] else spec$state[[field]]$default
      }
      children <- c(if (!is.null(spec$compose)) spec$compose(self), args[!nzchar(arg_names)])
      if (length(children)) self$mount(children)
    },
    default_bindings = function() c(super$default_bindings(), spec$bindings)
  )
  public$default_style <- function() merge_styles(super$default_style(), type_style)
  if (!is.null(render)) public$render <- function() spec$render(self)
  for (action in names(actions)) {
    public[[paste0("action_", action)]] <- make_method(bquote(spec$actions[[.(action)]](self)))
  }
  for (type in names(handlers)) {
    public[[handler_method_name(type)]] <- make_method(
      bquote(spec$handlers[[.(type)]](self, event)), alist(event = )
    )
  }
  for (method in names(methods)) {
    public[[method]] <- make_method(bquote(spec$methods[[.(method)]](self, ...)), alist(... = ))
  }
  active <- lapply(names(state), state_binding)
  names(active) <- names(state)

  generator <- R6::R6Class(
    name,
    inherit = inherit,
    public = public,
    active = if (length(active)) active,
    private = list(
      state_changed = function(name, old, new) {
        super$state_changed(name, old, new)
        watch <- spec$state[[name]]$watch
        if (!is.null(watch)) call_flex(watch, self, new, old)
      }
    )
  )

  constructor <- function(..., id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
    generator$new(..., id = id, classes = classes, style = style, disabled = disabled)
  }
  structure(constructor, class = c("termr_widget_type", "function"), generator = generator, type_name = name)
}

#' @export
print.termr_widget_type <- function(x, ...) {
  gen <- attr(x, "generator")
  fields <- names(gen$active)
  cat("<widget type ", attr(x, "type_name"), ">\n", sep = "")
  if (length(fields)) cat("  state: ", paste(fields, collapse = ", "), "\n", sep = "")
  invisible(x)
}

make_method <- function(body_expr, args = alist()) {
  f <- function() NULL
  formals(f) <- args
  body(f) <- body_expr
  f
}

generator_inherits <- function(gen, classname) {
  while (!is.null(gen)) {
    if (identical(gen$classname, classname)) return(TRUE)
    gen <- gen$get_inherit()
  }
  FALSE
}

check_named_list <- function(x, arg) {
  if (is.null(x)) return(list())
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop(sprintf("`%s` must be a named list.", arg), call. = FALSE)
  }
  if (anyDuplicated(names(x))) stop(sprintf("`%s` has duplicated names.", arg), call. = FALSE)
  x
}

check_named_functions <- function(x, arg) {
  x <- check_named_list(x, arg)
  if (!all(vapply(x, is.function, logical(1)))) {
    stop(sprintf("Every element of `%s` must be a function.", arg), call. = FALSE)
  }
  x
}

# State fields and methods must not hide members of Widget.
check_member_names <- function(names, arg) {
  if (length(names) == 0L) return(invisible())
  bad_syntax <- names[make.names(names) != names | startsWith(names, ".")]
  if (length(bad_syntax)) {
    stop(sprintf("Invalid name in `%s`: \"%s\".", arg, bad_syntax[[1]]), call. = FALSE)
  }
  reserved <- c(
    names(Widget$public_methods), names(Widget$public_fields), names(Widget$active),
    "self", "private", "super", "initialize", "clone"
  )
  clash <- intersect(names, reserved)
  if (length(clash)) {
    stop(sprintf("`%s` cannot use the reserved name \"%s\".", arg, clash[[1]]), call. = FALSE)
  }
  invisible()
}
