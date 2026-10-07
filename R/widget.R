#' Widget base class
#'
#' Every element of a termr interface is a `Widget`. Widgets form a tree:
#' each widget has one parent and an ordered list of children. Most users
#' create widgets with constructor functions such as [label()] or
#' [vertical()], and define new widget types with [widget()]. Subclassing
#' `Widget` with [R6::R6Class()] is also supported for advanced use.
#'
#' A subclass usually overrides some of:
#' * `default_style()`: the built-in [style()] of the widget type;
#' * `render()`: returns the content as a string, a character vector of
#'   lines or [span()]s;
#' * `on_key(event)`: handle a key press (return `TRUE` when handled);
#' * `paint(buffer, area, st)`: full control over drawing.
#'
#' @section State:
#' Widget state lives in reactive fields: assigning a new value invalidates
#' the widget and schedules a repaint. Several assignments in one event
#' loop tick produce a single repaint.
#'
#' @rdname Widget-class
#' @export
Widget <- R6::R6Class(
  "Widget",
  public = list(
    #' @field id Unique identifier used by `#id` selectors, or `NULL`.
    id = NULL,
    #' @field classes Character vector of classes used by `.class` selectors.
    classes = character(),
    #' @field help Optional help text (Markdown) shown on the help screen
    #'   (F1) while this widget or a descendant has focus.
    help = NULL,
    #' @field paint_states Names of state fields whose changes only need a
    #'   repaint (see `invalidate_paint()`); used by `set_state()`.
    paint_states = character(),
    #' @field focusable Can the widget receive keyboard focus?
    focusable = FALSE,
    #' @field region Border box assigned by the layout engine ([region()]).
    region = NULL,

    #' @description Create a widget.
    #' @param ... Child widgets.
    #' @param id Optional identifier.
    #' @param classes Character vector (or space separated string) of classes.
    #' @param style A [style()].
    #' @param disabled Is the widget disabled?
    #' @param visible Is the widget visible?
    initialize = function(..., id = NULL, classes = NULL, style = NULL,
                          disabled = FALSE, visible = TRUE) {
      private$.state <- new.env(parent = emptyenv())
      private$.handlers <- list()
      private$.bindings <- list()
      private$.actions <- list()
      self$id <- validate_id(id)
      self$classes <- parse_classes(classes)
      check_flag(disabled)
      check_flag(visible)
      private$.state$disabled <- disabled
      private$.state$visible <- visible
      private$.default_style <- as_style(self$default_style())
      private$.style <- as_style(style)
      children <- list(...)
      if (length(children)) self$mount(children)
    },

    #' @description The built-in style of this widget type.
    default_style = function() style(width = "1fr", height = "auto"),

    #' @description Content of the widget (string, lines or spans). Containers
    #'   return `NULL`.
    render = function() NULL,

    # Tree ------------------------------------------------------------------

    #' @description Add child widgets, at the end or at a position. A
    #'   widget that already has a parent is moved. Ids must be unique within
    #'   a screen.
    #' @param ... Widgets or lists of widgets.
    #' @param before,after A child widget (or its index) to insert before /
    #'   after.
    mount = function(..., before = NULL, after = NULL) {
      widgets <- flatten_widgets(list(...))
      if (!is.null(before) && !is.null(after)) stop("Use either `before` or `after`, not both.", call. = FALSE)
      position <- length(private$.children)
      if (!is.null(before)) position <- private$child_index(before) - 1L
      if (!is.null(after)) position <- private$child_index(after)
      root <- widget_root(self)
      for (w in widgets) {
        if (identical(w, self) || any(vapply(self$ancestors(), identical, logical(1), w))) {
          stop("A widget cannot be mounted inside itself.", call. = FALSE)
        }
      }
      check_unique_ids_batch(root, widgets)
      if (!length(widgets) || all(vapply(widgets, function(w) is.null(w$parent), TRUE))) {
        count <- length(private$.children)
        before <- if (position > 0L) private$.children[seq_len(position)] else list()
        after <- if (position < count) private$.children[seq.int(position + 1L, count)] else list()
        for (w in widgets) {
          wp <- widget_private(w)
          wp$.parent <- self
        }
        private$.children <- c(before, widgets, after)
        app <- self$app
        if (!is.null(app)) for (w in widgets) app$.__enclos_env__$private$widget_mounted(w)
        bump_epoch()
        self$invalidate()
        return(invisible(self))
      }
      for (w in widgets) {
        if (!is.null(w$parent)) {
          if (identical(w$parent, self) && match(TRUE, vapply(private$.children, identical, logical(1), w)) <= position) {
            position <- position - 1L
          }
          # A move inside the same app keeps the widget's reactive bindings,
          # timers and workers; they end only when it is removed.
          widget_private(w)$detach(dispose = !identical(w$app, self$app))
        }
        wp <- widget_private(w)
        wp$.parent <- self
        private$.children <- append(private$.children, list(w), after = position)
        position <- position + 1L
        app <- self$app
        if (!is.null(app)) app$.__enclos_env__$private$widget_mounted(w)
      }
      bump_epoch()
      self$invalidate()
      invisible(self)
    },

    #' @description Remove all children and mount new ones.
    #' @param ... Widgets.
    replace = function(...) {
      self$remove_children()
      self$mount(...)
    },

    #' @description Remove all children (containers). Some widgets redefine
    #'   `clear()` for their content (e.g. [input()], [log_view()]).
    clear = function() self$remove_children(),

    #' @description Detach this widget (and its children) from its parent.
    remove = function() private$detach(dispose = TRUE),

    #' @description Remove all children.
    remove_children = function() {
      for (child in private$.children) child$remove()
      invisible(self)
    },

    #' @description This widget followed by all descendants (depth first).
    walk = function() {
      children <- private$.children
      c(list(self), if (length(children)) unlist(lapply(children, function(child) child$walk()), recursive = FALSE))
    },

    #' @description Parent, grandparent, ... up to the root.
    ancestors = function() {
      out <- list()
      node <- private$.parent
      while (!is.null(node)) {
        out[[length(out) + 1L]] <- node
        node <- node$parent
      }
      out
    },

    #' @description Find descendants matching a CSS-like selector.
    #' @param selector E.g. `"Button"`, `"#name"`, `".danger"`,
    #'   `"Vertical > Button.primary"`, or several separated by commas.
    #' @return A list of widgets.
    query = function(selector) {
      sel <- parse_selector(selector)
      Filter(function(w) match_selector(w, sel), self$walk()[-1L])
    },

    #' @description The first descendant matching `selector`.
    #' @param selector A selector, see `query()`.
    query_one = function(selector) {
      sel <- parse_selector(selector)
      for (w in self$walk()[-1L]) if (match_selector(w, sel)) return(w)
      stop(sprintf("No widget matches the selector \"%s\".", selector), call. = FALSE)
    },

    #' @description Does this widget match `selector`?
    #' @param selector A selector, see `query()`.
    matches = function(selector) match_selector(self, parse_selector(selector)),

    # Events, bindings and focus ---------------------------------------------

    #' @description Handle events of a type that reach this widget (its own
    #'   events and events bubbling up from descendants).
    #' @param type Event type, or `"*"` for all.
    #' @param handler `function(event, app)`.
    #' @param selector Optional selector matched against the event sender.
    #'   The order of [on()] / `app$on()`, `widget$on(type, selector,
    #'   handler)`, is accepted too.
    on = function(type, handler, selector = NULL) {
      if (is.character(handler) && is.function(selector)) {
        swap <- handler
        handler <- selector
        selector <- swap
      }
      h <- on(type, selector, handler)
      private$.handlers[[length(private$.handlers) + 1L]] <- h
      invisible(self)
    },

    #' @description Add a key binding to this widget, see [bind()].
    #' @param key Key name or a [bind()] binding.
    #' @param action Action name or function.
    #' @param description Optional description.
    bind = function(key, action = NULL, description = NULL) {
      b <- if (is_binding(key)) key else bind(key, action, description)
      private$.bindings[[length(private$.bindings) + 1L]] <- b
      invisible(self)
    },

    #' @description Built-in bindings of this widget type.
    default_bindings = function() list(),

    #' @description All bindings of this widget (type bindings first, so
    #'   instance bindings win).
    bindings = function() c(self$default_bindings(), private$.bindings),

    #' @description Keep a field in sync with a reactive function: `fn` runs
    #'   now and again whenever the [signal()]s it reads change, and its
    #'   result is assigned to `field` (e.g. `"text"` for a label). The
    #'   binding ends when the widget is removed.
    #' @param field Name of a widget field (an active binding such as `text`).
    #' @param fn A function of no arguments.
    bind_reactive = function(field, fn) {
      check_scalar_character(field, "field")
      check_function(fn, "fn")
      self_ref <- self
      handle <- watch(function() {
        value <- fn()
        self_ref[[field]] <- value
      })
      private$.reactive_bindings <- c(private$.reactive_bindings, list(handle))
      invisible(self)
    },

    #' @description Receive all mouse events (also outside this widget's
    #'   region) until `release_mouse()` or the button is released.
    capture_mouse = function() {
      app <- self$app
      if (is.null(app)) stop("The widget is not attached to an app.", call. = FALSE)
      app$capture_mouse(self)
      invisible(self)
    },

    #' @description Stop capturing the mouse.
    release_mouse = function() {
      app <- self$app
      if (!is.null(app)) app$release_mouse()
      invisible(self)
    },

    #' @description Run an action, looked up on this widget, its
    #'   ancestors, then the app.
    #' @param action Action name or function.
    run_action = function(action) {
      app <- self$app
      if (is.null(app)) stop("The widget is not attached to an app.", call. = FALSE)
      app$run_action(action, self)
    },

    #' @description Send a message that bubbles up from this widget to the
    #'   app. Handlers receive a [MessageEvent] whose `sender` is this widget.
    #' @param type Message name, e.g. `"counter.changed"`.
    #' @param data A list of values.
    #' @param bubbles Should the message bubble to ancestors?
    #' @return The event (invisibly), or `NULL` when not attached to an app.
    post_message = function(type, data = list(), bubbles = TRUE) {
      app <- self$app
      if (is.null(app)) return(invisible(NULL))
      app$post(MessageEvent$new(type, sender = self, data = data, bubbles = bubbles))
    },

    #' @description Call `callback(self, app)` once after `delay` seconds.
    #'   The timer is cancelled automatically when the widget is removed.
    #' @param delay Seconds.
    #' @param callback Function.
    #' @return A [Timer].
    set_timeout = function(delay, callback) private$add_timer(delay, callback, repeating = FALSE),

    #' @description Call `callback(self, app)` every `interval` seconds
    #'   while the widget is mounted.
    #' @param interval Seconds.
    #' @param callback Function.
    #' @return A [Timer].
    set_interval = function(interval, callback) private$add_timer(interval, callback, repeating = TRUE),

    #' @description Animate a numeric field of this widget. See [animate()].
    #' @param property Field name.
    #' @param to Target value.
    #' @param ... Other arguments of [animate()].
    animate = function(property, to, ...) animate(self, property, to, ...),

    #' @description Run a background worker owned by this widget (cancelled
    #'   when the widget is removed). See [run_worker()].
    #' @param fn Function to run.
    #' @param ... Other arguments of [run_worker()].
    run_worker = function(fn, ...) {
      app <- self$app
      if (is.null(app)) stop("The widget is not attached to an app.", call. = FALSE)
      app$run_worker(fn, ..., owner = self)
    },

    #' @description Run an external program owned by this widget. See
    #'   [run_process()].
    #' @param command Program to run.
    #' @param ... Other arguments of [run_process()].
    run_process = function(command, ...) {
      app <- self$app
      if (is.null(app)) stop("The widget is not attached to an app.", call. = FALSE)
      app$run_process(command, ..., owner = self)
    },

    #' @description Give keyboard focus to this widget.
    focus = function() {
      app <- self$app
      if (is.null(app)) stop("The widget is not attached to an app.", call. = FALSE)
      invisible(app$set_focus(self))
    },

    #' @description Remove keyboard focus from this widget.
    blur = function() {
      app <- self$app
      if (!is.null(app) && identical(app$focused, self)) app$set_focus(NULL)
      invisible(self)
    },

    # Classes ---------------------------------------------------------------

    #' @description Add classes.
    #' @param ... Class names.
    add_class = function(...) {
      self$classes <- union(self$classes, parse_classes(c(...)))
      bump_epoch()
      self$invalidate()
    },

    #' @description Remove classes.
    #' @param ... Class names.
    remove_class = function(...) {
      self$classes <- setdiff(self$classes, parse_classes(c(...)))
      bump_epoch()
      self$invalidate()
    },

    #' @description Add a class if absent, remove it if present.
    #' @param name Class name.
    toggle_class = function(name) {
      if (self$has_class(name)) self$remove_class(name) else self$add_class(name)
    },

    #' @description Does the widget have this class?
    #' @param name Class name.
    has_class = function(name) name %in% self$classes,

    # State and rendering ---------------------------------------------------

    #' @description Assign one or more fields, e.g.
    #'   `app$query_one("#name")$set(value = "")`. R does not allow
    #'   `app$query_one("#name")$value <- ""`, so use this method when the
    #'   widget comes from a function call.
    #' @param ... Named values.
    #' @return The widget, invisibly (so calls can be chained).
    set = function(...) {
      values <- list(...)
      if (length(values) && (is.null(names(values)) || any(!nzchar(names(values))))) {
        stop("All arguments to set() must be named.", call. = FALSE)
      }
      for (name in names(values)) {
        if (!exists(name, envir = self, inherits = FALSE)) {
          stop(sprintf("%s has no field \"%s\".", self$type, name), call. = FALSE)
        }
        if (!bindingIsActive(name, self) && is.function(self[[name]])) {
          stop(sprintf("\"%s\" is a method of %s, not a field.", name, self$type), call. = FALSE)
        }
        self[[name]] <- values[[name]]
      }
      invisible(self)
    },

    #' @description Read a state field.
    #' @param name Field name.
    get_state = function(name) private$.state[[name]],

    #' @description Write a state field; invalidates the widget when the
    #'   value changes.
    #' @param name Field name.
    #' @param value New value.
    set_state = function(name, value) {
      old <- private$.state[[name]]
      if (identical(old, value)) return(invisible(FALSE))
      before <- self$pseudo_states()
      private$.state[[name]] <- value
      # A change of state classes (disabled, pressed, ...) can restyle this
      # widget and its descendants; it needs a new layout only if the widget's
      # style differs in size-related properties between the two states.
      after <- self$pseudo_states()
      relayout <- FALSE
      if (!identical(before, after)) {
        relayout <- !private$same_layout_in(before, after)
        bump_epoch(layout = relayout)
      }
      if (!relayout && name %in% self$paint_states) self$invalidate_paint() else self$invalidate()
      private$state_changed(name, old, value)
      invisible(TRUE)
    },

    #' @description Would the widget's size-related style differ if it were
    #'   in the pseudo states `with` rather than `without`? Used to decide
    #'   whether a focus / hover change needs a layout pass.
    #' @param with,without Character vectors of states.
    layout_differs = function(with, without) !private$same_layout_in(with, without),

    #' @description Mark the widget as needing a re-render and schedule a
    #'   repaint, including a new layout pass (its size may have changed).
    #'   Repaints are batched per event-loop tick.
    invalidate = function() {
      private$.dirty <- TRUE
      clear_size_caches(self)
      app <- self$app
      if (!is.null(app)) app$request_repaint(self)
      invisible(self)
    },

    #' @description Like `invalidate()` for changes that cannot affect the
    #'   size or position of anything (a cursor moved, a colour changed): the
    #'   widget is repainted but the layout is not recomputed.
    invalidate_paint = function() {
      private$.dirty <- TRUE
      app <- self$app
      if (!is.null(app)) app$request_repaint(self, layout = FALSE)
      invisible(self)
    },

    #' @description Force the widget to re-render on the next repaint.
    refresh = function() self$invalidate(),

    #' @description Rendered content as lines of styled segments (cached
    #'   until the widget is invalidated).
    #' @param width Available content width (may be `NA` while measuring).
    render_lines = function(width = NA_integer_) {
      if (private$.dirty || is.null(private$.render_cache)) {
        private$.render_cache <- text_lines(self$render())
        private$.wrap_cache <- NULL
        private$.dirty <- FALSE
      }
      lines <- private$.render_cache
      mode <- self$computed_style()$wrap
      if (mode == "none" || is.na(width)) return(lines)
      cached <- private$.wrap_cache
      if (!is.null(cached) && identical(cached$key, c(mode, width))) return(cached$lines)
      wrapped <- wrap_text_lines(lines, width, mode)
      private$.wrap_cache <- list(key = c(mode, width), lines = wrapped)
      wrapped
    },

    #' @description Natural width of the content area.
    content_width = function() {
      kids <- visible_children(self)
      if (length(kids)) {
        st <- self$computed_style()
        return(layout_algorithms[[st$layout]]$measure(kids, st)$width())
      }
      lines <- self$render_lines()
      if (length(lines) == 0L) return(0L)
      max(vapply(lines, line_width, integer(1)))
    },

    #' @description Natural height of the content area for a given width.
    #' @param width Content width.
    content_height = function(width) {
      kids <- visible_children(self)
      if (length(kids)) {
        st <- self$computed_style()
        return(layout_algorithms[[st$layout]]$measure(kids, st)$height(width))
      }
      length(self$render_lines(width))
    },

    #' @description Place the visible children inside the content box.
    #'   Containers with special behaviour (scrolling, ...) override this.
    #' @param children Visible child widgets.
    #' @param inner Content rectangle of this widget.
    #' @param st Computed style of this widget.
    #' @return A list with one rectangle per child.
    arrange_children = function(children, inner, st) {
      layout_algorithms[[st$layout]]$arrange(children, inner, st)
    },

    #' @description The rectangle children are clipped to when painting.
    #' @param st Computed style of this widget.
    child_clip = function(st) content_rect(self$region, st),

    #' @description Names of active states used to select state styles.
    pseudo_states = function() {
      c(if (self$focused) "focus", if (self$hovered) "hover", if (!self$is_enabled()) "disabled")
    },

    #' @description The fully resolved style of the widget.
    #' @param inherited The parent's computed style (computed if `NULL`).
    computed_style = function(inherited = NULL) {
      cached <- widget_cache_get(self, "style")
      if (!is.null(cached)) return(cached)
      profile_add("style_resolutions")
      parent <- private$.parent
      if (is.null(inherited) && !is.null(parent)) inherited <- parent$computed_style()
      # Cascade: type defaults < stylesheet rules < own style (see stylesheet.R).
      states <- self$pseudo_states()
      st <- flatten_style(private$.default_style, states)
      app <- self$app
      theme <- NULL
      if (!is.null(app)) {
        st <- merge_styles(st, app$stylesheet_style(self, states))
        theme <- app$theme
      }
      st <- merge_styles(st, flatten_style(private$.style, states))
      resolved <- resolve_style(st, character(), inherited, theme)
      if (isTRUE(theme$strong_focus) && "focus" %in% states && !identical(resolved$border, "none")) {
        resolved$border <- "heavy"
      }
      if (isTRUE(theme$mono) && "focus" %in% states && !identical(resolved$border, "none")) resolved$border <- "heavy"
      widget_cache_set(self, "style", resolved)
    },

    #' @description Which children need their own layout? Containers that
    #'   clip their children (scroll views) skip the ones that are entirely
    #'   out of view; they get their region but not their descendants.
    #' @param rects The rectangles `arrange_children()` returned.
    layout_descend = function(rects) rep(TRUE, length(rects)),

    #' @description Paint the widget (not its children) into a buffer.
    #' @param buffer A [ScreenBuffer].
    #' @param area The visible part of `self$region`.
    #' @param st The computed style.
    paint = function(buffer, area, st) {
      draw_background(buffer, area, st)
      draw_border(buffer, self$region, st, area, self$border_title)
      inner <- content_rect(self$region, st)
      draw_lines(buffer, inner, self$render_lines(inner$width), st, area)
    },

    #' @description Is the widget and every ancestor enabled?
    is_enabled = function() {
      if (private$.state$disabled) return(FALSE)
      parent <- private$.parent
      is.null(parent) || parent$is_enabled()
    },

    #' @description Is the widget and every ancestor visible?
    is_displayed = function() {
      if (!private$.state$visible) return(FALSE)
      parent <- private$.parent
      is.null(parent) || parent$is_displayed()
    },

    #' @description A one-line description used by `print()`.
    #' @param ... Ignored.
    format = function(...) {
      parts <- self$type
      if (!is.null(self$id)) parts <- paste0(parts, " #", self$id)
      if (length(self$classes)) parts <- paste0(parts, paste0(" .", self$classes, collapse = ""))
      if (!private$.state$visible) parts <- paste(parts, "[hidden]")
      if (private$.state$disabled) parts <- paste(parts, "[disabled]")
      desc <- private$describe()
      paste0("<", parts, ">", if (length(desc) && nzchar(desc)) paste0(" ", desc) else "")
    },

    #' @description Print the widget tree.
    #' @param ... Ignored.
    print = function(...) {
      cat(paste(tree_lines(self), collapse = "\n"), "\n", sep = "")
      invisible(self)
    }
  ),

  active = list(
    #' @field parent The parent widget (read-only).
    parent = function(value) {
      if (!missing(value)) stop("`parent` is read-only; use mount() / remove().", call. = FALSE)
      private$.parent
    },
    #' @field children List of child widgets (read-only).
    children = function(value) {
      if (!missing(value)) stop("`children` is read-only; use mount() / remove().", call. = FALSE)
      private$.children
    },
    #' @field app The [App] this widget belongs to, or `NULL`.
    app = function(value) {
      if (!missing(value)) stop("`app` is read-only.", call. = FALSE)
      node <- self
      while (!is.null(node$parent)) node <- node$parent
      widget_private(node)$.app
    },
    #' @field type Widget type name used by type selectors.
    type = function(value) {
      if (!missing(value)) stop("`type` is read-only.", call. = FALSE)
      class(self)[[1]]
    },
    #' @field visible Is the widget shown? Hidden widgets take no space.
    visible = function(value) {
      if (missing(value)) return(private$.state$visible)
      check_flag(value, "visible")
      self$set_state("visible", value)
    },
    #' @field disabled Disabled widgets cannot be focused or used.
    disabled = function(value) {
      if (missing(value)) return(private$.state$disabled)
      check_flag(value, "disabled")
      self$set_state("disabled", value)
    },
    #' @field style The widget's own [style()] (layered over its defaults).
    style = function(value) {
      if (missing(value)) return(private$.style)
      new <- as_style(value)
      same_layout <- identical(style_layout_signature(private$.style), style_layout_signature(new))
      private$.style <- new
      if (same_layout) {
        # Only colours / attributes changed: no new layout pass needed.
        bump_epoch(layout = FALSE)
        self$invalidate_paint()
      } else {
        bump_epoch()
        self$invalidate()
      }
    },
    #' @field border_title Text shown in the top border (reactive), or `NULL`.
    border_title = function(value) {
      if (missing(value)) return(private$.state$border_title)
      check_scalar_character(value, "border_title", allow_null = TRUE)
      self$set_state("border_title", value)
    },
    #' @field hovered Is the mouse pointer over this widget (or a
    #'   descendant)?
    hovered = function(value) {
      if (!missing(value)) stop("`hovered` is read-only.", call. = FALSE)
      app <- self$app
      h <- if (is.null(app)) NULL else app$hovered
      !is.null(h) && (identical(h, self) || any(vapply(h$ancestors(), identical, logical(1), self)))
    },
    #' @field focused Does the widget currently have keyboard focus?
    focused = function(value) {
      if (!missing(value)) stop("Use focus() / blur() to change focus.", call. = FALSE)
      app <- self$app
      !is.null(app) && identical(app$focused, self)
    }
  ),

  private = list(
    .parent = NULL,
    .children = list(),
    .app = NULL,
    .state = NULL,
    .style = NULL,
    .default_style = NULL,
    .dirty = TRUE,
    .render_cache = NULL,
    .wrap_cache = NULL,
    # Per-epoch cache of computed style and natural sizes (see bump_epoch()).
    .cache = NULL,
    .cache_epoch = -1,
    .size_cache = list(),
    .size_epoch = -1,
    .layout_revision = 0,
    .layout_index = NULL,
    .render_children = NULL,
    # Screens: the widget that had focus when another screen covered this one.
    .saved_focus = NULL,
    .handlers = NULL,
    .bindings = NULL,
    .actions = NULL,

    .timers = list(),

    add_timer = function(delay, callback, repeating) {
      check_function(callback, "callback")
      app <- self$app
      if (is.null(app)) stop("The widget is not attached to an app.", call. = FALSE)
      fire <- function(app) call_flex(callback, self, app)
      timer <- if (repeating) app$set_interval(delay, fire) else app$set_timeout(delay, fire)
      private$.timers <- c(Filter(function(t) t$active, private$.timers), list(timer))
      timer
    },

    .reactive_bindings = list(),

    # Same layout properties in the two sets of pseudo states? Stylesheets
    # can have rules for any state, so with one present we say "no".
    same_layout_in = function(a, b) {
      app <- self$app
      if (!is.null(app) && length(app$stylesheets)) return(FALSE)
      sig <- function(states) {
        st <- flatten_style(private$.default_style, states)
        st <- merge_styles(st, flatten_style(private$.style, states))
        style_layout_signature(st)
      }
      identical(sig(a), sig(b))
    },

    # Detach from the parent. `dispose = FALSE` (a move within the same app)
    # keeps reactive bindings, timers and owned workers.
    detach = function(dispose = TRUE) {
      parent <- private$.parent
      if (is.null(parent)) return(invisible(self))
      if (dispose) for (w in self$walk()) widget_private(w)$dispose_bindings()
      app <- self$app
      if (!is.null(app)) app$.__enclos_env__$private$widget_unmounting(self, dispose = dispose)
      pp <- widget_private(parent)
      keep <- !vapply(pp$.children, identical, logical(1), self)
      pp$.children <- pp$.children[keep]
      private$.parent <- NULL
      clear_regions(self)
      bump_epoch()
      parent$invalidate()
      invisible(self)
    },

    dispose_bindings = function() {
      for (h in private$.reactive_bindings) h$dispose()
      private$.reactive_bindings <- list()
      invisible()
    },

    # Called by the app when the widget is unmounted.
    cancel_timers = function() {
      for (t in private$.timers) t$cancel()
      private$.timers <- list()
      invisible()
    },

    child_index = function(child) {
      if (is.numeric(child)) {
        i <- as.integer(child)
        if (length(i) != 1L || is.na(i) || i < 1L || i > length(private$.children)) {
          stop(sprintf("No child at position %s.", format(child)), call. = FALSE)
        }
        return(i)
      }
      i <- match(TRUE, vapply(private$.children, identical, logical(1), child), nomatch = 0L)
      if (i == 0L) stop("`before` / `after` must be a child of this widget.", call. = FALSE)
      i
    },

    # Hook for subclasses: called after a state field changed.
    state_changed = function(name, old, new) invisible(),

    # Hook for subclasses: short description for print().
    describe = function() ""
  )
)

widget_private <- function(w) w$.__enclos_env__$private

# Layout-time caches.
#
# Computed styles depend on the whole tree (inheritance, selectors, states),
# so they are cached per *style epoch*: a global counter bumped by every
# change that can restyle widgets (style and class changes, mount/remove,
# focus, hover, state classes such as disabled, themes, stylesheets).
#
# Natural sizes also depend on content. They are cached per widget and
# cleared by Widget$invalidate() for the widget and its ancestors only (a
# content change cannot change the size of siblings), and also dropped when
# the style epoch changes. A layout pass therefore measures each widget
# once, and a change to one label re-measures only its ancestor chain.
# `layout = FALSE` is for changes that cannot move or resize anything
# (colours, attributes): cached styles are dropped but the app keeps the
# layout of the last pass.
bump_epoch <- function(layout = TRUE) {
  termr_env$epoch <- termr_env$epoch + 1
  if (layout) termr_env$layout_epoch <- termr_env$layout_epoch + 1
  invisible()
}

widget_cache_get <- function(w, key) {
  p <- widget_private(w)
  if (p$.cache_epoch != termr_env$epoch) return(NULL)
  p$.cache[[key]]
}

size_cache_get <- function(w, key) {
  p <- widget_private(w)
  if (p$.size_epoch != termr_env$layout_epoch) return(NULL)
  p$.size_cache[[key]]
}

size_cache_set <- function(w, key, value) {
  p <- widget_private(w)
  if (p$.size_epoch != termr_env$layout_epoch) {
    p$.size_cache <- list()
    p$.size_epoch <- termr_env$layout_epoch
  }
  p$.size_cache[[key]] <- value
  value
}

clear_size_caches <- function(w) {
  node <- w
  while (!is.null(node)) {
    p <- widget_private(node)
    p$.size_cache <- list()
    p$.layout_revision <- p$.layout_revision + 1
    p$.layout_index <- NULL
    node <- p$.parent
  }
  invisible()
}

widget_cache_set <- function(w, key, value) {
  p <- widget_private(w)
  if (p$.cache_epoch != termr_env$epoch) {
    p$.cache <- list()
    p$.cache_epoch <- termr_env$epoch
  }
  p$.cache[[key]] <- value
  value
}

is_widget <- function(x) inherits(x, "Widget")

flatten_widgets <- function(x) {
  items <- lapply(x, function(item) {
    if (is.null(item)) return(list())
    if (is_widget(item)) return(list(item))
    if (is.list(item) && !is.object(item)) return(flatten_widgets(item))
    stop(sprintf("Children must be widgets, not %s.", class(item)[[1]]), call. = FALSE)
  })
  unlist(items, recursive = FALSE)
}

# Ids must be unique within one tree (screen). `widget` may already be in
# the tree (when it is moved).
check_unique_ids <- function(root, widget) {
  new_ids <- unlist(lapply(widget$walk(), function(w) w$id))
  if (!length(new_ids)) return(invisible())
  dup <- new_ids[duplicated(new_ids)]
  if (length(dup)) stop(sprintf("Duplicate widget id \"%s\".", dup[[1]]), call. = FALSE)
  root_ids <- unlist(lapply(root$walk(), function(w) w$id))
  inside <- identical(widget_root(widget), root)
  for (id in new_ids) {
    if (sum(root_ids == id) - inside > 0L) {
      stop(sprintf("Duplicate widget id \"%s\": another widget in this screen already has it.", id), call. = FALSE)
    }
  }
  invisible()
}

check_unique_ids_batch <- function(root, widgets) {
  if (!length(widgets)) return(invisible())
  new_widgets <- unlist(lapply(widgets, function(w) w$walk()), recursive = FALSE)
  new_ids <- unlist(lapply(new_widgets, function(w) w$id), use.names = FALSE)
  if (!length(new_ids)) return(invisible())
  dup <- new_ids[duplicated(new_ids)]
  if (length(dup)) stop(sprintf("Duplicate widget id \"%s\".", dup[[1]]), call. = FALSE)
  root_ids <- unlist(lapply(root$walk(), function(w) w$id), use.names = FALSE)
  moving <- vapply(new_widgets, function(w) identical(widget_root(w), root), TRUE)
  moving_ids <- unlist(lapply(new_widgets[moving], function(w) w$id), use.names = FALSE)
  for (id in new_ids) {
    if (sum(root_ids == id) - sum(moving_ids == id) > 0L) {
      stop(sprintf("Duplicate widget id \"%s\": another widget in this screen already has it.", id), call. = FALSE)
    }
  }
  invisible()
}

validate_id <- function(id) {
  if (is.null(id)) return(NULL)
  check_scalar_character(id, "id")
  if (!grepl("^[A-Za-z_][A-Za-z0-9_-]*$", id)) {
    stop(sprintf("Invalid widget id \"%s\": use letters, digits, '_' and '-'.", id), call. = FALSE)
  }
  id
}

parse_classes <- function(classes) {
  if (is.null(classes) || length(classes) == 0L) return(character())
  if (!is.character(classes)) stop("`classes` must be a character vector.", call. = FALSE)
  out <- unique(unlist(strsplit(classes, "[[:space:]]+")))
  out <- out[nzchar(out)]
  bad <- !grepl("^[A-Za-z_][A-Za-z0-9_-]*$", out)
  if (any(bad)) stop(sprintf("Invalid class name \"%s\".", out[bad][[1]]), call. = FALSE)
  out
}

tree_lines <- function(widget, prefix = "", is_root = TRUE, is_last = TRUE) {
  head <- if (is_root) "" else paste0(prefix, if (is_last) "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 ")
  out <- paste0(head, widget$format())
  kids <- widget$children
  child_prefix <- if (is_root) "" else paste0(prefix, if (is_last) "    " else "\u2502   ")
  for (i in seq_along(kids)) {
    out <- c(out, tree_lines(kids[[i]], child_prefix, FALSE, i == length(kids)))
  }
  out
}
