# Screen stack, modal screens and dialogs.
#
# An App shows a stack of screens. The top screen is active: it receives
# keys, mouse clicks and focus. A *modal* screen is drawn over the screens
# below it (optionally dimming them) instead of replacing them, which is how
# dialogs work. Each screen remembers which widget had focus when another
# screen was pushed over it, and gets that focus back when it is on top
# again.
#
# Lifecycle events (sent to the screen, they do not bubble):
#   "mount" / "unmount"        when the screen enters / leaves the stack
#   "screen.show" / "screen.hide"  when it becomes / stops being the top

ScreenStack <- R6::R6Class(
  "ScreenStack",
  public = list(
    entries = list(),

    initialize = function(app, screen) {
      private$app <- app
      self$push(screen)
    },

    top = function() self$entries[[length(self$entries)]]$screen,

    screens = function() lapply(self$entries, `[[`, "screen"),

    contains = function(screen) any(vapply(self$screens(), identical, logical(1), screen)),

    push = function(screen, callback = NULL) {
      if (self$contains(screen)) stop("This screen is already in the stack.", call. = FALSE)
      if (!is.null(screen$parent)) stop("A screen cannot have a parent widget.", call. = FALSE)
      sp <- widget_private(screen)
      sp$.app <- private$app
      self$entries[[length(self$entries) + 1L]] <- list(screen = screen, callback = callback)
      bump_epoch()
      invisible(screen)
    },

    remove = function(screen) {
      keep <- !vapply(self$screens(), identical, logical(1), screen)
      entry <- self$entries[!keep][[1]]
      self$entries <- self$entries[keep]
      sp <- widget_private(screen)
      sp$.app <- NULL
      bump_epoch()
      entry
    },

    # Screens to paint, bottom to top: from the last non-modal screen up.
    visible = function() {
      screens <- self$screens()
      modal <- vapply(screens, function(s) inherits(s, "ModalScreen"), logical(1))
      base <- max(c(1L, which(!modal)))
      screens[seq.int(base, length(screens))]
    }
  ),
  private = list(app = NULL)
)

#' @title Modal screen
#' @description A screen drawn over the current one. See [modal()].
#' @export
ModalScreen <- R6::R6Class(
  "ModalScreen",
  inherit = Screen,
  public = list(
    #' @field dim Dim the screens below?
    dim = TRUE,
    #' @field dismissable Close with Escape?
    dismissable = TRUE,
    #' @field on_dismiss `NULL` or `function(result, app)` called when the
    #'   screen is dismissed.
    on_dismiss = NULL,

    #' @description Create a modal screen. See [modal()].
    #' @param ... Child widgets.
    #' @param dim,dismissable See fields.
    #' @param on_dismiss Callback.
    #' @param id,classes,style See [Widget].
    initialize = function(..., dim = TRUE, dismissable = TRUE, on_dismiss = NULL,
                          id = NULL, classes = NULL, style = NULL) {
      check_flag(dim)
      check_flag(dismissable)
      check_function(on_dismiss, "on_dismiss", allow_null = TRUE)
      self$dim <- dim
      self$dismissable <- dismissable
      self$on_dismiss <- on_dismiss
      super$initialize(..., id = id, classes = classes, style = style)
    },

    #' @description The built-in style: content centred.
    default_style = function() {
      style(width = "1fr", height = "1fr", layout = "vertical", align = "center", valign = "middle")
    },

    #' @description Escape closes the screen (when dismissable).
    default_bindings = function() list(bind("escape", "dismiss", "Close")),

    #' @description Close the screen, passing `result` to the callbacks.
    #' @param result Any value.
    dismiss = function(result = NULL) {
      # Evaluate now: the result often reads widgets of this screen.
      force(result)
      app <- self$app
      if (is.null(app)) return(invisible(self))
      app$pop_screen(result, screen = self)
    },

    #' @description Action used by the Escape binding.
    action_dismiss = function() {
      if (self$dismissable) self$dismiss(NULL)
    }
  )
)

#' @title Panel widget
#' @description A bordered container with an optional title. See [panel()].
#' @rdname Panel-class
#' @export
Panel <- R6::R6Class(
  "Panel",
  inherit = Vertical,
  public = list(
    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "auto", border = "round", padding = c(0, 1))
  )
)

#' Panel
#'
#' A container with a border and an optional title in the top border. Its
#' height follows its content by default.
#'
#' @param ... Child widgets.
#' @param title Title shown in the top border.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `Panel` widget. Change the title with `$border_title`.
#' @export
#' @examples
#' render_widget(panel(label("CPU 42%"), title = "Metrics"), 20, 3)
panel <- function(..., title = NULL, id = NULL, classes = NULL, style = NULL) {
  p <- Panel$new(..., id = id, classes = classes, style = style)
  p$border_title <- title
  p
}

#' Modal screens and dialogs
#'
#' `modal()` builds a screen that is drawn over the current one and takes
#' the keyboard and mouse until it is closed. Show it with
#' `app$push_screen()`; close it with Escape or `screen$dismiss(result)`.
#' The previously focused widget gets focus back.
#'
#' `confirm_dialog()` and `alert_dialog()` are ready-made dialogs built on
#' `modal()`.
#'
#' @param ... Content widgets (placed in a bordered panel).
#' @param title Panel title.
#' @param width Panel width (cells, `"auto"`, `"50%"`, ...).
#' @param dim Dim the screens below?
#' @param dismissable Can Escape close the dialog?
#' @param on_dismiss `function(result, app)` called when the dialog closes.
#' @param id Optional identifier of the screen.
#' @return A `ModalScreen`.
#' @export
#' @examples
#' dlg <- confirm_dialog("Delete file?", on_confirm = function(app) message("deleted"))
#' a <- app(label("main"))
#' pilot <- test_app(a, 40, 10)
#' a$push_screen(dlg)
#' pilot$press("tab", "enter") # move to OK, press it
#' pilot$stop()
modal <- function(..., title = NULL, width = 50L, dim = TRUE, dismissable = TRUE,
                  on_dismiss = NULL, id = NULL) {
  body <- panel(..., title = title, style = style(width = width, background = "default"))
  ModalScreen$new(body, dim = dim, dismissable = dismissable, on_dismiss = on_dismiss, id = id)
}

#' @rdname modal
#' @param message Message text.
#' @param on_confirm,on_cancel `function(app)` called when the user
#'   confirms, or cancels (Cancel button or Escape).
#' @param confirm_label,cancel_label Button labels.
#' @export
confirm_dialog <- function(message, on_confirm = NULL, on_cancel = NULL, title = "Confirm",
                           confirm_label = "OK", cancel_label = "Cancel", width = 50L) {
  check_function(on_confirm, "on_confirm", allow_null = TRUE)
  check_function(on_cancel, "on_cancel", allow_null = TRUE)
  screen <- modal(
    label(message, wrap = "word", style = style(width = "1fr", margin = c(0, 0, 1, 0))),
    horizontal(
      button(cancel_label, id = "cancel"),
      button(confirm_label, id = "confirm", variant = "primary"),
      style = style(align = "right")
    ),
    title = title, width = width,
    on_dismiss = function(result, app) {
      if (isTRUE(result)) {
        if (!is.null(on_confirm)) call_flex(on_confirm, app)
      } else if (!is.null(on_cancel)) {
        call_flex(on_cancel, app)
      }
    }
  )
  screen$on("button.pressed", function(event, app) {
    event$stop()
    screen$dismiss(identical(event$sender$id, "confirm"))
  })
  screen
}

#' @rdname modal
#' @param button_label Label of the close button.
#' @export
alert_dialog <- function(message, title = "Message", button_label = "OK", width = 50L) {
  screen <- modal(
    label(message, wrap = "word", style = style(width = "1fr", margin = c(0, 0, 1, 0))),
    horizontal(button(button_label, id = "ok", variant = "primary"), style = style(align = "right")),
    title = title, width = width
  )
  screen$on("button.pressed", function(event, app) {
    event$stop()
    screen$dismiss(TRUE)
  })
  screen
}

# Dim everything painted so far (used under modal screens).
dim_buffer <- function(buffer, theme = current_theme(), region = buffer$bounds()) {
  region <- rect_intersect(region, buffer$bounds())
  if (rect_is_empty(region)) return(invisible(buffer))
  rows <- seq.int(region$y, rect_bottom(region))
  cols <- seq.int(region$x, rect_right(region))
  buffer$fg[rows, cols] <- theme_color("$muted", theme)
  buffer$attrs[rows, cols] <- bitwAnd(buffer$attrs[rows, cols], bitwNot(attr_bits[["bold"]]))
  invisible(buffer)
}

widget_root <- function(widget) {
  node <- widget
  while (!is.null(node$parent)) node <- node$parent
  node
}
