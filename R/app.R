#' Application
#'
#' An `App` owns the widget tree (through its [Screen]), the event queue,
#' timers, key bindings, focus and the terminal driver. Create one with
#' [app()] and start it with [run()].
#'
#' @section Event loop:
#' Each tick of the loop waits for input (or the next timer), turns input
#' into events, fires due timers, dispatches every queued event and then,
#' if any widget was invalidated, repaints the screen once: the new frame
#' is diffed against the previous one and only changed cells are written.
#'
#' @section Event dispatch:
#' An event goes to its target widget (key events: the focused widget) and
#' bubbles up through the ancestors to the app. At each widget the
#' `on_<type>` method runs, then handlers added with `widget$on()`, then -
#' for key events - the widget's key bindings. At the app, handlers added
#' with `app$on()` / [on()] run, then app bindings. `event$stop()` stops
#' propagation.
#'
#' @rdname App-class
#' @export
App <- R6::R6Class(
  "App",
  public = list(
    #' @field title Optional application title.
    title = NULL,
    #' @field reduce_motion `NULL` (follow the option / environment variable),
    #'   `TRUE` or `FALSE`: reduce decorative animation? See [motion_reduced()].
    reduce_motion = NULL,
    #' @field clipboard Text copied by widgets (Ctrl+C / Ctrl+X in inputs).
    #'   This is the app's own clipboard, not the operating system's.
    clipboard = "",

    #' @description Create an app. See [app()].
    #' @param ... Widgets, [on()] handlers and [bind()] bindings.
    #' @param bindings A list of [bind()] bindings.
    #' @param actions A named list of functions `function(app)`.
    #' @param title Optional title.
    #' @param mouse Enable mouse input?
    #' @param stylesheet A [stylesheet()], stylesheet text or a file path
    #'   (or a list of them).
    #' @param theme A [termr_theme()] or the name of a built-in theme.
    #' @param reduce_motion `TRUE` to turn decorative animation off, `NULL`
    #'   to follow `options(termr.reduce_motion)` / the `TERMR_REDUCE_MOTION`
    #'   environment variable.
    #' @param debug Bind F12 to the debug overlay?
    initialize = function(..., bindings = list(), actions = list(), title = NULL, mouse = TRUE,
                          stylesheet = NULL, theme = "default", debug = FALSE, reduce_motion = NULL) {
      check_flag(debug)
      if (!is.null(reduce_motion)) check_flag(reduce_motion)
      self$reduce_motion <- reduce_motion
      check_flag(mouse)
      private$.theme <- as_theme(theme)
      if (!is.null(stylesheet)) {
        sheets <- if (is.list(stylesheet) && !inherits(stylesheet, "termr_stylesheet")) stylesheet else list(stylesheet)
        for (sheet in sheets) self$add_stylesheet(sheet)
      }
      private$.mouse <- mouse
      private$.queue <- EventQueue$new()
      private$.focus <- FocusManager$new(self)
      private$.timers <- TimerManager$new(function() private$clock())
      private$.screens <- ScreenStack$new(self, Screen$new())
      private$.toasts <- ToastRack$new()
      tp <- widget_private(private$.toasts)
      tp$.app <- self
      private$.bindings <- default_app_bindings()
      check_scalar_character(title, "title", allow_null = TRUE)
      self$title <- title
      if (length(actions) && (is.null(names(actions)) || !all(vapply(actions, is.function, logical(1))))) {
        stop("`actions` must be a named list of functions.", call. = FALSE)
      }
      private$.actions <- actions
      private$add_items(list(...))
      for (b in as_bindings(bindings)) self$bind(b)
      if (debug) self$bind("f12", "toggle_debug", "Debug overlay")
    },

    #' @description Run the app until it exits. Restores the terminal on
    #'   exit, on error and on interrupt.
    #' @param driver A terminal driver; detected automatically by default.
    #' @return The value passed to `exit()`, invisibly.
    run = function(driver = NULL) {
      if (private$.running) stop("This app is already running.", call. = FALSE)
      driver <- driver %||% default_driver()
      on.exit(private$shutdown(), add = TRUE)
      result <- tryCatch(
        {
          private$start(driver)
          while (!private$.exit_requested) private$tick(wait = TRUE)
          NULL
        },
        interrupt = function(e) NULL,
        error = function(e) e
      )
      private$shutdown()
      if (inherits(result, "error")) stop(result)
      invisible(private$.return_value)
    },

    #' @description Ask the app to stop after the current event.
    #' @param value Value returned by `run()`.
    exit = function(value = NULL) {
      private$.return_value <- value
      private$.exit_requested <- TRUE
      invisible(self)
    },

    #' @description Find widgets matching a selector.
    #' @param selector A selector such as `"Button"`, `"#id"`, `".class"`.
    query = function(selector) {
      unlist(lapply(rev(self$screens), function(screen) screen$query(selector)), recursive = FALSE) %||% list()
    },

    #' @description The first widget matching a selector (error if none).
    #' @param selector A selector.
    query_one = function(selector) {
      sel <- parse_selector(selector)
      for (screen in rev(self$screens)) {
        for (w in screen$walk()[-1L]) if (match_selector(w, sel)) return(w)
      }
      stop(sprintf("No widget matches the selector \"%s\".", selector), call. = FALSE)
    },

    #' @description Mount widgets on the screen.
    #' @param ... Widgets.
    mount = function(...) {
      self$screen$mount(...)
      invisible(self)
    },

    #' @description Add an event handler, see [on()].
    #' @param type Event type.
    #' @param selector Optional selector matched against the sender.
    #' @param handler `function(event, app)`.
    on = function(type, selector = NULL, handler) {
      h <- if (is_handler(type)) type else on(type, selector, handler)
      private$.handlers[[length(private$.handlers) + 1L]] <- h
      invisible(self)
    },

    #' @description Add an app-level key binding, see [bind()].
    #' @param key Key name or a binding created by [bind()].
    #' @param action Action name or function.
    #' @param description Optional description.
    bind = function(key, action = NULL, description = NULL) {
      b <- if (is_binding(key)) key else bind(key, action, description)
      private$.bindings[[length(private$.bindings) + 1L]] <- b
      invisible(self)
    },

    #' @description Record dispatched events (for debugging). Off by
    #'   default, so normal apps pay nothing.
    #' @param enable Start (`TRUE`) or stop (`FALSE`) recording.
    #' @param size Number of events kept.
    log_events = function(enable = TRUE, size = 200L) {
      check_flag(enable)
      private$.event_log_size <- check_count(size, "size")
      private$.event_log <- if (enable) {
        data.frame(time = numeric(), type = character(), target = character(), detail = character(),
                   stringsAsFactors = FALSE)
      }
      invisible(self)
    },

    #' @description The recorded events as a data frame (see `log_events()`).
    event_log = function() private$.event_log,

    #' @description Show or hide the debug overlay (also F12 with
    #'   `app(debug = TRUE)`).
    toggle_debug = function() toggle_debug_overlay(self),

    #' @description Add a command to the command palette (Ctrl+P) and the
    #'   help screen (F1). See [command()].
    #' @param label Text shown in the palette, or a [command()] object.
    #' @param action An action name or `function(app)`.
    #' @param category,shortcut,enabled,description See [command()].
    add_command = function(label, action = NULL, category = NULL, shortcut = NULL, enabled = NULL,
                           description = NULL) {
      cmd <- if (is_command(label)) label else
        command(label, action, category = category, shortcut = shortcut, enabled = enabled, description = description)
      private$.commands[[length(private$.commands) + 1L]] <- cmd
      invisible(self)
    },

    #' @description Show the help screen (F1): key bindings of the focused
    #'   widget, its ancestors and the app, and the app's commands.
    show_help = function() open_help(self),

    #' @description All app-level bindings.
    bindings = function() private$.bindings,

    #' @description Queue an event for dispatch.
    #' @param event An [Event].
    post = function(event) {
      if (!inherits(event, "Event")) stop("`event` must be an Event.", call. = FALSE)
      private$.queue$push(event)
      invisible(event)
    },

    #' @description Send a custom message from the app (bubbles from the
    #'   screen).
    #' @param type Message name.
    #' @param data List of values.
    post_message = function(type, data = list()) {
      self$post(MessageEvent$new(type, sender = NULL, data = data))
    },

    #' @description Copy text to the clipboard. The app clipboard
    #'   (`app$clipboard`) is always set; when `system = TRUE` and the terminal
    #'   supports OSC 52 the text is also sent to the system clipboard
    #'   (write-only: termr never reads the system clipboard). Long text is
    #'   truncated to 100 kB for the terminal.
    #' @param text A string.
    #' @param system Also write to the system clipboard?
    #' @return `TRUE` (invisibly) if the text was sent to the terminal.
    clipboard_write = function(text, system = TRUE) {
      text <- paste(as.character(text), collapse = "\n")
      self$clipboard <- text
      sent <- FALSE
      driver <- private$.driver
      if (isTRUE(system) && !is.null(driver) && isTRUE(driver$capabilities$osc52) && nzchar(text)) {
        driver$write(ansi_osc52(text))
        sent <- TRUE
      }
      invisible(sent)
    },

    #' @description Send all mouse events to `widget` (even outside its
    #'   region) until `release_mouse()` or the next button release. Used for
    #'   drags that start elsewhere, e.g. splitters.
    #' @param widget A mounted widget.
    capture_mouse = function(widget) {
      if (!is_widget(widget) || !identical(widget$app, self)) {
        stop("Only a widget of this app can capture the mouse.", call. = FALSE)
      }
      private$.captured <- widget
      invisible(self)
    },

    #' @description Stop capturing the mouse.
    release_mouse = function() {
      private$.captured <- NULL
      invisible(self)
    },

    #' @description Run an action by name (or a function).
    #' @param action Action name such as `"quit"`, or a function.
    #' @param widget Widget where the lookup starts (`NULL`: the app).
    run_action = function(action, widget = NULL) {
      if (is.function(action)) {
        if (is.null(widget)) call_flex(action, self) else call_flex(action, widget, self)
        return(invisible(TRUE))
      }
      check_scalar_character(action, "action")
      name <- action
      if (startsWith(name, "app.")) {
        name <- substring(name, 5L)
        widget <- NULL
      }
      node <- widget
      while (!is.null(node)) {
        method <- node[[paste0("action_", name)]]
        if (is.function(method)) {
          method()
          return(invisible(TRUE))
        }
        node <- node$parent
      }
      fn <- private$.actions[[name]] %||% private$builtin_action(name)
      if (is.null(fn)) stop(sprintf("Unknown action \"%s\".", action), call. = FALSE)
      call_flex(fn, self)
      invisible(TRUE)
    },

    #' @description Schedule a repaint at the end of the current tick.
    #' @param widget The widget that changed, if known. Without it the next
    #'   repaint redraws everything; with it only the rows of changed
    #'   widgets are repainted when the layout did not change.
    #' @param full Force a full repaint?
    #' @param layout Does the change possibly affect sizes or positions? When
    #'   `FALSE` and nothing else changed the layout pass is skipped.
    request_repaint = function(widget = NULL, full = is.null(widget), layout = TRUE) {
      private$.needs_repaint <- TRUE
      if (layout) private$.layout_dirty <- TRUE
      if (!is.null(widget)) {
        if (length(private$.dirty) < 256L) private$.dirty[[length(private$.dirty) + 1L]] <- widget else full <- TRUE
      }
      if (full) private$.full_repaint <- TRUE
      invisible(self)
    },

    #' @description Redraw the whole screen (e.g. after external output).
    refresh = function() {
      if (!is.null(private$.renderer)) private$.renderer$invalidate()
      self$request_repaint()
    },

    #' @description Give focus to a widget (or remove focus with `NULL`).
    #' @param widget A focusable widget in this app, or `NULL`.
    #' @return `TRUE` if focus changed.
    set_focus = function(widget) {
      change <- private$.focus$set(widget)
      if (is.null(change)) return(invisible(FALSE))
      old <- change$old
      changed <- Filter(Negate(is.null), list(old, widget))
      relayout <- any(vapply(changed, function(w) w$layout_differs(union(w$pseudo_states(), "focus"),
                                                                  setdiff(w$pseudo_states(), "focus")), TRUE))
      bump_epoch(layout = relayout)
      repaint <- function(w) if (relayout) w$invalidate() else w$invalidate_paint()
      if (!is.null(old)) {
        repaint(old)
        if (private$.running) self$post(BlurEvent$new(old))
      }
      if (!is.null(widget)) {
        repaint(widget)
        # Only widgets inside a scroll view may need to be scrolled into view.
        if (any(vapply(widget$ancestors(), inherits, logical(1), "ScrollView"))) private$.reveal <- widget
        if (private$.running) self$post(FocusEvent$new(widget))
      }
      private$.needs_repaint <- TRUE
      invisible(TRUE)
    },

    #' @description Add a stylesheet; its rules apply to all screens.
    #' @param sheet A [stylesheet()], stylesheet text or a file path.
    add_stylesheet = function(sheet) {
      private$.stylesheets[[length(private$.stylesheets) + 1L]] <- as_stylesheet(sheet)
      bump_epoch()
      self$request_repaint()
      invisible(self)
    },

    #' @description Remove all stylesheets.
    clear_stylesheets = function() {
      private$.stylesheets <- list()
      bump_epoch()
      self$request_repaint()
      invisible(self)
    },

    #' @description The merged stylesheet rules that apply to a widget.
    #' @param widget A widget.
    #' @param states Its pseudo states.
    stylesheet_style = function(widget, states = widget$pseudo_states()) {
      if (!length(private$.stylesheets)) return(NULL)
      stylesheet_style(private$.stylesheets, widget, states)
    },

    #' @description Show a screen on top of the current one. A widget is
    #'   wrapped in a new [Screen]. Use [modal()] for dialogs.
    #' @param screen A [Screen] (or [ModalScreen]) or a widget.
    #' @param callback Optional `function(result, app)` called when the
    #'   screen is dismissed / popped with a result.
    #' @return The screen, invisibly.
    push_screen = function(screen, callback = NULL) {
      if (is_widget(screen) && !inherits(screen, "Screen")) screen <- Screen$new(screen)
      if (!inherits(screen, "Screen")) stop("`screen` must be a Screen or a widget.", call. = FALSE)
      check_function(callback, "callback", allow_null = TRUE)
      previous <- self$screen
      pp <- widget_private(previous)
      pp$.saved_focus <- self$focused
      private$.screens$push(screen, callback)
      private$.focus$focused <- NULL
      if (private$.running) {
        for (w in screen$walk()) self$post(MountEvent$new(w))
        self$post(Event$new("screen.hide", sender = previous, bubbles = FALSE))
        self$post(Event$new("screen.show", sender = screen, bubbles = FALSE))
      }
      private$ensure_focus(initial = TRUE)
      self$request_repaint()
      invisible(screen)
    },

    #' @description Remove the top screen (or a given screen) and return to
    #'   the one below, restoring its focus.
    #' @param result Passed to the screen's callbacks.
    #' @param screen The screen to remove (default: the top screen).
    pop_screen = function(result = NULL, screen = self$screen) {
      force(result)
      force(screen)
      if (length(private$.screens$entries) <= 1L) stop("Cannot pop the last screen.", call. = FALSE)
      if (!private$.screens$contains(screen)) stop("The screen is not in the stack.", call. = FALSE)
      was_top <- identical(screen, self$screen)
      private$screen_leaving(screen)
      entry <- private$.screens$remove(screen)
      if (was_top) {
        top <- self$screen
        saved <- widget_private(top)$.saved_focus
        private$.focus$focused <- NULL
        if (!is.null(saved) && private$.focus$can_focus(saved)) self$set_focus(saved) else private$ensure_focus(initial = TRUE)
        if (private$.running) self$post(Event$new("screen.show", sender = top, bubbles = FALSE))
      }
      self$request_repaint()
      if (inherits(screen, "ModalScreen") && !is.null(screen$on_dismiss)) call_flex(screen$on_dismiss, result, self)
      if (!is.null(entry$callback)) call_flex(entry$callback, result, self)
      invisible(screen)
    },

    #' @description Replace the top screen.
    #' @param screen A [Screen] or a widget.
    switch_screen = function(screen) {
      old <- self$screen
      self$push_screen(screen)
      private$screen_leaving(old)
      private$.screens$remove(old)
      invisible(screen)
    },

    #' @description Run a function in a background R process. See
    #'   [run_worker()] for the arguments.
    #' @param fn,args,on_complete,on_error,on_progress,name,owner,packages,inline
    #'   See [run_worker()].
    #' @param on_stdout,on_stderr,timeout See [run_worker()].
    run_worker = function(fn, args = list(), on_complete = NULL, on_error = NULL, on_progress = NULL,
                          name = NULL, owner = NULL, packages = character(), inline = FALSE,
                          on_stdout = NULL, on_stderr = NULL, timeout = NULL) {
      worker <- start_worker(self, fn, args, on_complete, on_error, on_progress, name, owner, packages, inline,
                             on_stdout = on_stdout, on_stderr = on_stderr, timeout = timeout)
      if (worker$is_running()) private$.workers[[length(private$.workers) + 1L]] <- worker
      self$request_repaint()
      invisible(worker)
    },

    #' @description Run an external program in the background, without a
    #'   shell. See [run_process()].
    #' @param command,args,on_complete,on_error,on_stdout,on_stderr,on_exit,timeout,wd,env,name,owner
    #'   See [run_process()].
    run_process = function(command, args = character(), on_complete = NULL, on_error = NULL,
                           on_stdout = NULL, on_stderr = NULL, on_exit = NULL, timeout = NULL,
                           wd = NULL, env = NULL, name = NULL, owner = NULL) {
      worker <- start_command(self, command, args, on_complete, on_error, on_stdout, on_stderr, on_exit,
                              timeout, wd, env, name, owner)
      if (worker$is_running()) private$.workers[[length(private$.workers) + 1L]] <- worker
      self$request_repaint()
      invisible(worker)
    },

    #' @description Running workers.
    workers = function() Filter(function(w) w$is_running(), private$.workers),

    #' @description Show a notification (toast) in the bottom-right corner.
    #' @param message Text.
    #' @param title Optional title.
    #' @param severity `"information"`, `"success"`, `"warning"` or `"error"`.
    #' @param timeout Seconds before it disappears (`Inf`: until clicked).
    #' @return The `Toast` widget, invisibly.
    notify = function(message, title = NULL, severity = "information", timeout = 3) {
      toast <- Toast$new(message, title = title, severity = severity)
      rack <- private$.toasts
      rack$mount(toast)
      while (length(rack$children) > rack$max_toasts) rack$children[[1]]$remove()
      if (is.finite(timeout)) {
        fire <- function(app) if (!is.null(toast$parent)) toast$remove()
        tp <- widget_private(toast)
        tp$.timers <- list(self$set_timeout(timeout, fire))
      }
      invisible(toast)
    },

    #' @description Active notifications (toasts).
    notifications = function() private$.toasts$children,

    #' @description Scroll every scroll view containing `widget` so that it
    #'   becomes visible (done at the next repaint).
    #' @param widget A widget of this app.
    scroll_into_view = function(widget) {
      private$.reveal <- widget
      self$request_repaint()
    },

    #' @description Move focus to the next focusable widget.
    focus_next = function() self$set_focus(private$.focus$neighbour(1L)),

    #' @description Move focus to the previous focusable widget.
    focus_previous = function() self$set_focus(private$.focus$neighbour(-1L)),

    #' @description Focusable widgets in focus order.
    focus_chain = function() private$.focus$chain(),

    #' @description Call `callback(app)` once after `delay` seconds.
    #' @param delay Seconds.
    #' @param callback Function.
    set_timeout = function(delay, callback) private$.timers$add(delay, callback, repeating = FALSE),

    #' @description Call `callback(app)` every `interval` seconds.
    #' @param interval Seconds.
    #' @param callback Function.
    set_interval = function(interval, callback) private$.timers$add(interval, callback, repeating = TRUE),

    #' @description Call `callback(app)` at the start of the next tick.
    #' @param callback Function.
    call_later = function(callback) {
      check_function(callback, "callback")
      private$.later[[length(private$.later) + 1L]] <- callback
      invisible(self)
    },

    #' @description Print a summary of the app.
    #' @param ... Ignored.
    print = function(...) {
      cat("<App", if (!is.null(self$title)) paste0(" \"", self$title, "\""),
          if (private$.running) " (running)", ">\n", sep = "")
      print(self$screen)
      invisible(self)
    }
  ),

  active = list(
    #' @field screen The current [Screen].
    screen = function(value) if (missing(value)) private$.screens$top() else read_only("screen"),
    #' @field screens The screen stack, bottom first.
    screens = function(value) if (missing(value)) private$.screens$screens() else read_only("screens"),
    #' @field focused The focused widget, or `NULL`.
    focused = function(value) if (missing(value)) private$.focus$focused else read_only("focused"),
    #' @field theme The [termr_theme()] (assign a theme or a theme name).
    theme = function(value) {
      if (missing(value)) return(private$.theme)
      private$.theme <- as_theme(value)
      if (isTRUE(private$.mono)) private$.theme$mono <- TRUE
      bump_epoch()
      self$refresh()
    },
    #' @field stylesheets The stylesheets of the app.
    stylesheets = function(value) if (missing(value)) private$.stylesheets else read_only("stylesheets"),
    #' @field hovered The widget under the mouse pointer, or `NULL`.
    hovered = function(value) if (missing(value)) private$.hovered else read_only("hovered"),
    #' @field size Terminal size: `c(width =, height =)`.
    size = function(value) if (missing(value)) private$.size else read_only("size"),
    #' @field running Is the app running?
    running = function(value) if (missing(value)) private$.running else read_only("running"),
    #' @field last_paint Instrumentation of the last repaint: screen cells,
    #'   repainted cells, dirty rectangles, whether it was a full repaint and
    #'   the ANSI bytes written. For benchmarks and debugging.
    last_paint = function(value) if (missing(value)) private$.last_paint else read_only("last_paint"),
    #' @field profile_last_frame Optional timings and structural counters;
    #'   enable with `options(termr.profile = TRUE)`. Times are milliseconds.
    profile_last_frame = function(value) if (missing(value)) private$.profile_last_frame else read_only("profile_last_frame"),
    #' @field frame_stats Number of full and incremental repaints and of
    #'   layout passes so far.
    frame_stats = function(value) if (missing(value)) private$.frame_stats else read_only("frame_stats"),
    #' @field frame The last rendered [ScreenBuffer].
    frame = function(value) if (missing(value)) private$.frame else read_only("frame")
  ),

  private = list(
    .screens = NULL,
    .toasts = NULL,
    .queue = NULL,
    .timers = NULL,
    .driver = NULL,
    .renderer = NULL,
    .handlers = list(),
    .bindings = list(),
    .actions = list(),
    .later = list(),
    .focus = NULL,
    .reveal = NULL,
    .debug = NULL,
    .debug_timer = NULL,
    .frame_times = numeric(),
    .last_event = NULL,
    .event_log = NULL,
    .event_log_size = 200L,
    .commands = list(),
    .dirty = list(),
    .full_repaint = TRUE,
    .frame_stats = c(full = 0L, incremental = 0L, layouts = 0L),
    .layout_dirty = TRUE,
    .last_paint = NULL,
    .profile_last_frame = NULL,
    .layout_epoch = -1,
    .layout_size = NULL,
    .snapshot = NULL,
    .layers = NULL,
    .workers = list(),
    .theme = NULL,
    .mono = FALSE,
    .stylesheets = list(),
    .mouse = TRUE,
    .hovered = NULL,
    .mouse_down = NULL,
    .captured = NULL,
    .size = c(width = 80L, height = 24L),
    .frame = NULL,
    .running = FALSE,
    .exit_requested = FALSE,
    .needs_repaint = FALSE,
    .return_value = NULL,
    idle_timeout = 0.5,
    max_events_per_tick = 10000L,

    clock = function() {
      if (is.null(private$.driver)) now_seconds() else private$.driver$clock()
    },

    add_items = function(items) {
      for (item in items) {
        if (is.null(item)) next
        if (is_widget(item)) {
          self$screen$mount(item)
        } else if (is_handler(item)) {
          self$on(item)
        } else if (is_binding(item)) {
          self$bind(item)
        } else if (is.list(item) && !is.object(item)) {
          private$add_items(item)
        } else {
          stop(sprintf(
            "app() accepts widgets, on() handlers and bind() bindings, not %s.",
            class(item)[[1]]
          ), call. = FALSE)
        }
      }
    },

    builtin_action = function(name) {
      switch(
        name,
        quit = function(app) app$exit(),
        focus_next = function(app) app$focus_next(),
        focus_previous = function(app) app$focus_previous(),
        refresh = function(app) app$refresh(),
        command_palette = function(app) open_command_palette(app),
        show_help = function(app) open_help(app),
        toggle_debug = function(app) toggle_debug_overlay(app),
        NULL
      )
    },

    # Lifecycle -------------------------------------------------------------

    start = function(driver) {
      private$.driver <- driver
      private$.exit_requested <- FALSE
      private$.return_value <- NULL
      # Timers created before the app started count from the start.
      start_time <- driver$clock()
      for (t in private$.timers$timers) t$due <- start_time + t$delay
      driver$mouse <- private$.mouse
      private$.mono <- identical(driver$capabilities$colors, "none") || identical(driver$color_mode, "none")
      if (private$.mono) {
        private$.theme$mono <- TRUE
        bump_epoch()
      }
      driver$start()
      private$.size <- driver$size()
      private$.renderer <- Renderer$new(driver$write, color_mode = driver$color_mode,
                                        synchronized = driver$capabilities$synchronized_output %||% TRUE)
      private$.running <- TRUE
      termr_env$apps <- c(list(self), termr_env$apps)
      for (screen in c(self$screens, list(private$.toasts))) {
        for (w in screen$walk()) self$post(MountEvent$new(w))
      }
      private$ensure_focus(initial = TRUE)
      private$.needs_repaint <- TRUE
      private$process_queue()
      private$refresh_screen()
    },

    shutdown = function() {
      driver <- private$.driver
      if (is.null(driver)) return(invisible())
      private$.running <- FALSE
      termr_env$apps <- Filter(function(a) !identical(a, self), termr_env$apps)
      private$.driver <- NULL
      private$.renderer <- NULL
      private$.timers$cancel_all()
      for (w in private$.workers) w$cancel()
      private$.workers <- list()
      if (!is.null(private$.animator)) private$.animator$cancel_all()
      private$.debug <- NULL
      private$.debug_timer <- NULL
      private$.queue$clear()
      private$.later <- list()
      driver$stop()
      invisible()
    },

    # One iteration of the event loop.
    tick = function(wait = TRUE) {
      timeout <- 0
      idle <- private$.queue$is_empty() && !private$.needs_repaint && length(private$.later) == 0L
      if (wait && idle) {
        timeout <- min(private$.timers$time_until_next(), private$idle_timeout)
        if (length(private$.workers)) timeout <- min(timeout, 0.05)
      }
      for (ev in private$.driver$read_events(timeout)) private$post_input(ev)
      private$poll_workers()
      private$.timers$fire_due(function(timer) batch(call_flex(timer$callback, self)))
      private$process_queue()
      if (private$.needs_repaint && private$.running && !private$.exit_requested) {
        private$refresh_screen()
      }
      invisible()
    },

    post_input = function(event) {
      if (inherits(event, "ResizeEvent")) {
        private$.size <- c(width = event$width, height = event$height)
        private$.layout_dirty <- TRUE
        if (!is.null(private$.renderer)) private$.renderer$invalidate()
        private$.needs_repaint <- TRUE
        event$target <- self$screen
      } else if (inherits(event, "KeyEvent") || inherits(event, "PasteEvent")) {
        event$target <- private$.focus$focused %||% self$screen
      } else if (inherits(event, "MouseEvent")) {
        return(private$route_mouse(event))
      }
      self$post(event)
    },

    .animator = NULL,
    animator = function() {
      if (is.null(private$.animator)) private$.animator <- Animator$new(self)
      private$.animator
    },

    record_event = function(event, target) {
      text <- paste0(event$type, if (inherits(event, "KeyEvent")) paste0(" ", event$key) else "",
                     " -> ", target$format())
      private$.last_event <- text
      if (!is.null(private$.event_log)) {
        entry <- data.frame(time = private$clock(), type = event$type, target = target$format(),
                            detail = if (inherits(event, "KeyEvent")) event$key else "", stringsAsFactors = FALSE)
        private$.event_log <- utils::tail(rbind(private$.event_log, entry), private$.event_log_size)
      }
    },

    poll_workers = function() {
      if (!length(private$.workers)) return(invisible())
      for (w in private$.workers) w$.__enclos_env__$private$poll()
      private$.workers <- Filter(function(w) w$is_running(), private$.workers)
      invisible()
    },

    # Mouse -----------------------------------------------------------------

    hit = function(x, y) {
      toast <- hit_test(private$.toasts, x, y)
      if (!is.null(toast) && !identical(toast, private$.toasts)) return(toast)
      hit_test(self$screen, x, y)
    },

    route_mouse = function(event) {
      hit <- private$hit(event$screen_x, event$screen_y)
      private$set_hover(hit)
      captured <- private$captured_target(event)
      if (is.null(captured) && is.null(hit)) {
        if (event$action == "up") private$.mouse_down <- NULL
        return(invisible())
      }
      # Disabled widgets do not receive mouse events; their parent does.
      target <- captured %||% hit
      if (is.null(captured)) while (!is.null(target$parent) && !target$is_enabled()) target <- target$parent
      private$deliver_mouse(event, target)
      if (event$action == "down") {
        if (is.null(captured)) {
          for (w in c(list(target), target$ancestors())) {
            if (private$.focus$can_focus(w)) {
              self$set_focus(w)
              break
            }
          }
        }
        private$.mouse_down <- list(widget = target, button = event$button, x = event$screen_x,
                                    y = event$screen_y, dragging = FALSE)
      }
      invisible()
    },

    # The widget that receives this event instead of the one under the
    # pointer: an explicit capture, or the widget where a held button went
    # down (so drags keep working outside the widget).
    captured_target = function(event) {
      widget <- private$.captured
      if (is.null(widget) && !is.null(private$.mouse_down) && event$action %in% c("move", "up")) {
        widget <- private$.mouse_down$widget
      }
      if (!is.null(widget) && !identical(widget$app, self)) {
        private$.captured <- NULL
        private$.mouse_down <- NULL
        return(NULL)
      }
      widget
    },

    # Post a mouse event to `target`, with the drag and click events that
    # follow from it.
    deliver_mouse = function(event, target) {
      position <- event$offset_in(target)
      event$target <- target
      event$x <- position[["x"]]
      event$y <- position[["y"]]
      down <- private$.mouse_down
      extra <- function(action) {
        ev <- MouseEvent$new(action, event$screen_x, event$screen_y, button = down$button,
                             shift = event$shift, ctrl = event$ctrl, alt = event$alt)
        ev$origin_x <- down$x
        ev$origin_y <- down$y
        ev$target <- target
        pos <- ev$offset_in(target)
        ev$x <- pos[["x"]]
        ev$y <- pos[["y"]]
        ev
      }
      self$post(event)
      if (event$action == "move" && !is.null(down) && event$button != "none" &&
          (event$screen_x != down$x || event$screen_y != down$y || down$dragging)) {
        if (!down$dragging) {
          private$.mouse_down$dragging <- TRUE
          self$post(extra("drag_start"))
        }
        self$post(extra("drag_move"))
      }
      if (event$action == "up") {
        private$.mouse_down <- NULL
        if (!is.null(down) && down$dragging) self$post(extra("drag_end"))
        hit <- private$hit(event$screen_x, event$screen_y)
        private$.captured <- NULL
        if (!is.null(down) && down$button == event$button && !is.null(hit) && identical(hit, target)) {
          click <- MouseEvent$new("click", event$screen_x, event$screen_y, button = event$button,
                                  shift = event$shift, ctrl = event$ctrl, alt = event$alt)
          click$target <- target
          click$x <- event$x
          click$y <- event$y
          self$post(click)
        }
      }
      invisible()
    },

    set_hover = function(widget) {
      old <- private$.hovered
      if (identical(old, widget)) return(invisible())
      private$.hovered <- widget
      touched <- Filter(Negate(is.null), list(old, widget))
      relayout <- any(vapply(touched, function(w) w$layout_differs(union(w$pseudo_states(), "hover"),
                                                                  setdiff(w$pseudo_states(), "hover")), TRUE))
      bump_epoch(layout = relayout)
      for (w in c(if (!is.null(old)) c(list(old), old$ancestors()), if (!is.null(widget)) c(list(widget), widget$ancestors()))) {
        if (relayout) w$invalidate() else w$invalidate_paint()
      }
      invisible()
    },

    process_queue = function() {
      count <- 0L
      repeat {
        if (length(private$.later)) {
          later <- private$.later
          private$.later <- list()
          for (fn in later) call_flex(fn, self)
        }
        event <- private$.queue$pop()
        if (is.null(event)) {
          if (length(private$.later)) next
          break
        }
        batch(private$dispatch(event))
        count <- count + 1L
        if (private$.exit_requested) break
        if (count >= private$max_events_per_tick) {
          warning("termr: too many events in one tick; possible event loop.", call. = FALSE)
          break
        }
      }
      private$ensure_focus()
      invisible(count)
    },

    refresh_screen = function() {
      profiling <- isTRUE(getOption("termr.profile", FALSE))
      if (profiling) {
        previous_profile <- termr_env$profile
        termr_env$profile <- profile_start()
        on.exit(termr_env$profile <- previous_profile, add = TRUE)
        frame_start <- now_seconds()
      }
      size <- private$.size
      bounds <- rect(1L, 1L, size[["width"]], size[["height"]])
      layers <- private$.screens$visible()
      overlays <- c(list(private$.toasts), if (!is.null(private$.debug)) list(private$.debug))
      previous <- private$.frame
      relayout <- private$.layout_dirty || is.null(previous) || !identical(layers, private$.layers) ||
        private$.layout_epoch != termr_env$layout_epoch || !is.null(private$.reveal) ||
        !identical(size, private$.layout_size) || !isTRUE(getOption("termr.skip_layout", TRUE))
      if (relayout) {
        # Widgets inside scroll views are laid out lazily; a widget about to
        # be scrolled into view needs a complete layout first.
        termr_env$full_layout <- !is.null(private$.reveal)
        for (screen in layers) layout_tree(screen, bounds)
        if (!is.null(private$.reveal)) {
          widget <- private$.reveal
          private$.reveal <- NULL
          if (identical(widget$app, self)) {
            top <- self$screen
            reveal_widget(widget, function() layout_tree(top, bounds))
          }
        }
        for (layer in overlays) layout_tree(layer, bounds)
        snapshot <- layout_snapshot(c(layers, overlays))
        changes <- snapshot_changes(private$.snapshot, snapshot)
        private$.layout_dirty <- FALSE
        private$.layout_epoch <- termr_env$layout_epoch
        private$.layout_size <- size
        private$.frame_stats[["layouts"]] <- private$.frame_stats[["layouts"]] + 1L
      } else {
        snapshot <- private$.snapshot
        changes <- list()
      }
      if (profiling) {
        paint_start <- now_seconds()
        profile_add("layout_ms", (paint_start - frame_start) * 1000)
      }
      incremental <- isTRUE(getOption("termr.incremental", TRUE)) && !private$.full_repaint &&
        !is.null(previous) && previous$width == bounds$width && previous$height == bounds$height &&
        !is.null(changes) && identical(layers, private$.layers)
      if (incremental) {
        # Repaint the regions of changed widgets (and of widgets that moved
        # or were resized, old and new place) into a copy of the last frame.
        frame <- previous$copy()
        rects <- dirty_rects(private$.dirty, bounds, extra = changes)
        for (clip in rects) {
          frame$fill(clip)
          paint_layers(frame, layers, overlays, private$.theme, clip)
        }
        repainted <- sum(vapply(rects, rect_area, 0))
      } else {
        frame <- ScreenBuffer$new(size[["width"]], size[["height"]])
        paint_layers(frame, layers, overlays, private$.theme)
        rects <- list(bounds)
        repainted <- rect_area(bounds)
      }
      bytes_before <- private$.renderer$bytes_written
      if (profiling) profile_add("paint_ms", (now_seconds() - paint_start) * 1000)
      private$.renderer$render(frame)
      private$.last_paint <- list(
        screen_cells = rect_area(bounds), repainted_cells = repainted, rects = length(rects),
        full = !incremental, ansi_bytes = private$.renderer$bytes_written - bytes_before
      )
      private$.profile_last_frame <- if (profiling) c(as.list(termr_env$profile), list(
        frame_ms = (now_seconds() - frame_start) * 1000, dirty_rectangles = length(rects),
        dirty_cells = repainted, ansi_bytes = private$.last_paint$ansi_bytes)) else NULL
      private$.frame_times <- utils::tail(c(private$.frame_times, private$clock()), 60L)
      key <- if (incremental) "incremental" else "full"
      private$.frame_stats[[key]] <- private$.frame_stats[[key]] + 1L
      private$.frame <- frame
      private$.snapshot <- snapshot
      private$.layers <- layers
      private$.dirty <- list()
      private$.full_repaint <- FALSE
      private$.needs_repaint <- FALSE
      invisible(frame)
    },

    # Dispatch --------------------------------------------------------------

    dispatch = function(event) {
      target <- event$target %||% self$screen
      if (!is.null(private$.debug) || !is.null(private$.event_log)) private$record_event(event, target)
      path <- if (event$bubbles) c(list(target), target$ancestors()) else list(target)
      for (node in path) {
        event$current <- node
        private$deliver(node, event)
        if (event$stopped) return(invisible())
      }
      event$current <- NULL
      for (h in private$.handlers) {
        if (handler_matches(h, event)) {
          call_flex(h$handler, event, self)
          if (event$stopped) return(invisible())
        }
      }
      if (inherits(event, "KeyEvent")) {
        b <- find_binding(private$.bindings, event$key)
        if (!is.null(b)) {
          event$stop()
          self$run_action(b$action, NULL)
        }
      }
      invisible()
    },

    deliver = function(node, event) {
      method <- node[[handler_method_name(event$type)]]
      if (is.function(method)) {
        method(event)
        if (event$stopped) return(invisible())
      }
      for (h in widget_private(node)$.handlers) {
        if (handler_matches(h, event)) {
          call_flex(h$handler, event, self)
          if (event$stopped) return(invisible())
        }
      }
      if (inherits(event, "KeyEvent")) {
        b <- find_binding(node$bindings(), event$key)
        if (!is.null(b)) {
          event$stop()
          self$run_action(b$action, node)
        }
      }
      invisible()
    },

    # Tree notifications from Widget$mount() / Widget$remove() ---------------

    widget_mounted = function(widget) {
      if (!private$.running) return(invisible())
      for (w in widget$walk()) self$post(MountEvent$new(w))
      self$request_repaint()
    },

    widget_unmounting = function(widget) {
      if (!private$.running) return(invisible())
      private$.focus$release(widget)
      if (!is.null(private$.hovered) && (identical(private$.hovered, widget) ||
          any(vapply(private$.hovered$ancestors(), identical, logical(1), widget)))) {
        private$.hovered <- NULL
      }
      removed <- widget$walk()
      if (!is.null(private$.captured) && any(vapply(removed, identical, logical(1), private$.captured))) {
        private$.captured <- NULL
      }
      for (w in removed) {
        widget_private(w)$cancel_timers()
        self$post(UnmountEvent$new(w))
      }
      for (worker in private$.workers) {
        if (!is.null(worker$owner) && any(vapply(removed, identical, logical(1), worker$owner))) worker$cancel()
      }
      self$request_repaint()
    },

    screen_leaving = function(screen) {
      private$.focus$release(screen)
      private$.focus$lost <- FALSE
      private$.mouse_down <- NULL
      private$.hovered <- NULL
      for (w in screen$walk()) {
        widget_private(w)$cancel_timers()
        widget_private(w)$dispose_bindings()
        if (private$.running) self$post(UnmountEvent$new(w))
      }
      if (private$.running && identical(screen, self$screen)) {
        self$post(Event$new("screen.hide", sender = screen, bubbles = FALSE))
      }
      invisible()
    },

    # Focus -----------------------------------------------------------------

    # Keep focus on a usable widget: at start, and when the focused widget
    # was removed, hidden or disabled, focus the first widget of the chain.
    # After an explicit blur() nothing is focused.
    ensure_focus = function(initial = FALSE) {
      fm <- private$.focus
      if (fm$is_valid()) return(invisible())
      if (initial || !is.null(fm$focused) || fm$lost) {
        fm$lost <- FALSE
        self$set_focus(fm$neighbour(1L))
      }
      invisible()
    }
  )
)

# Package state: the stack of running apps (normally at most one), so
# set_timeout() and friends can be called without passing the app.
termr_env <- new.env(parent = emptyenv())
termr_env$apps <- list()
termr_env$epoch <- 0
termr_env$layout_epoch <- 0

#' The running app
#'
#' @return The [App] that is currently running, or `NULL`.
#' @export
current_app <- function() {
  if (length(termr_env$apps)) termr_env$apps[[1]] else NULL
}

#' Timers
#'
#' Run a function later (`set_timeout()`) or repeatedly
#' (`set_interval()`). Timers are part of the event loop: callbacks run
#' between events, never concurrently, and never block the interface.
#' Widgets have their own `widget$set_timeout()` / `widget$set_interval()`,
#' whose timers are cancelled when the widget is removed.
#'
#' @param delay,interval Seconds.
#' @param callback `function(app)`.
#' @param app The app; defaults to the running app.
#' @return A [Timer]; call `$cancel()` to stop it.
#' @export
#' @examples
#' clock <- app(label("", id = "clock"), bind("q", "quit"))
#' clock$set_interval(1, function(app) {
#'   app$query_one("#clock")$update(format(Sys.time(), "%H:%M:%S"))
#' })
#' if (interactive()) run(clock)
set_timeout <- function(delay, callback, app = current_app()) {
  check_app(app)$set_timeout(delay, callback)
}

#' @rdname set_timeout
#' @export
set_interval <- function(interval, callback, app = current_app()) {
  check_app(app)$set_interval(interval, callback)
}

check_app <- function(app) {
  if (is.null(app)) {
    stop("No app is running. Pass `app`, or call app$set_timeout() before run().", call. = FALSE)
  }
  if (!inherits(app, "App")) stop("`app` must be an App.", call. = FALSE)
  app
}

# Scroll the scroll views around `widget` (innermost first) so it is
# visible, re-running the layout after each change.
reveal_widget <- function(widget, relayout) {
  for (ancestor in widget$ancestors()) {
    if (inherits(ancestor, "ScrollView") && ancestor$scroll_into_view(widget)) relayout()
  }
  invisible()
}

handler_method_name <- function(type) paste0("on_", gsub("[.-]", "_", type))

default_app_bindings <- function() {
  list(
    bind("tab", "focus_next", "Next"),
    bind("shift+tab", "focus_previous", "Previous"),
    bind("ctrl+c", "quit", "Quit"),
    bind("ctrl+p", "command_palette", "Command palette"),
    bind("f1", "show_help", "Help")
  )
}

#' Create an application
#'
#' @param ... Widgets to show, plus optional [on()] event handlers and
#'   [bind()] key bindings.
#' @param bindings A list of [bind()] key bindings.
#' @param actions A named list of action functions `function(app)` that
#'   bindings can refer to by name.
#' @param title Optional title.
#' @param mouse Enable mouse input (clicks, wheel, hover)?
#' @param stylesheet A [stylesheet()], stylesheet text, a file path, or a
#'   list of them.
#' @param reduce_motion Turn decorative animation off? `NULL` follows
#'   [motion_reduced()].
#' @param theme A [termr_theme()] or a built-in theme name (`"default"`,
#'   `"dark"`, `"light"`).
#' @param debug Bind F12 to a debug overlay (focused and hovered widget,
#'   last event, repaint statistics)?
#' @return An [App]. Start it with [run()].
#'
#' Built-in bindings: `tab` / `shift+tab` move focus, `ctrl+c` quits.
#' @export
#' @examples
#' hello <- app(
#'   vertical(
#'     label("What is your name?"),
#'     input(id = "name"),
#'     button("Say hello", id = "hello"),
#'     label("", id = "output")
#'   ),
#'   on("button.pressed", "#hello", function(event, app) {
#'     name <- app$query_one("#name")$value
#'     app$query_one("#output")$update(paste("Hello", name))
#'   }),
#'   bind("escape", "quit")
#' )
#' if (interactive()) run(hello)
app <- function(..., bindings = list(), actions = list(), title = NULL, mouse = TRUE,
                stylesheet = NULL, theme = "default", debug = FALSE, reduce_motion = NULL) {
  App$new(..., bindings = bindings, actions = actions, title = title, mouse = mouse,
          stylesheet = stylesheet, theme = theme, debug = debug, reduce_motion = reduce_motion)
}

#' Run an application
#'
#' Takes over the terminal (alternate screen, raw keyboard input), runs the
#' event loop until the app calls `exit()`, and restores the terminal -
#' also when an error occurs or the user interrupts R.
#'
#' termr apps need a real terminal: run them with `Rscript` (or an
#' interactive R session) in a terminal such as Windows Terminal,
#' iTerm2 or GNOME Terminal. RStudio's console is not a terminal.
#'
#' @param x An [App] created by [app()], or a widget (wrapped in an app).
#' @param ... Passed to `App$run()`, e.g. `driver`.
#' @return The value passed to `app$exit()`, invisibly.
#' @export
run <- function(x, ...) {
  if (inherits(x, "App")) return(x$run(...))
  if (is_widget(x)) return(App$new(x)$run(...))
  stop("`x` must be an app created with app() or a widget.", call. = FALSE)
}
