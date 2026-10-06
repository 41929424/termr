# Notifications ("toasts").
#
# Toasts live in their own root widget, the toast rack, which is laid out
# over the whole terminal and painted after all screens. It never takes
# focus or keys; clicking a toast closes it. Each toast removes itself after
# its timeout (a timer of the app, so headless tests control the time).

severities <- c("information", "success", "warning", "error")

# Built lazily: style() is not available while the package is loading.
severity_style <- function(severity) {
  style(border_color = paste0("$", severity))
}

ToastRack <- R6::R6Class(
  "ToastRack",
  inherit = Screen,
  public = list(
    max_toasts = 5L,
    default_style = function() {
      style(width = "1fr", height = "1fr", layout = "vertical", align = "right", valign = "bottom",
            padding = c(0, 1, 1, 0))
    }
  )
)

#' @title Toast widget
#' @description A notification shown by `app$notify()`.
#' @export
Toast <- R6::R6Class(
  "Toast",
  inherit = Label,
  public = list(
    #' @field severity `"information"`, `"success"`, `"warning"` or `"error"`.
    severity = "information",

    #' @description Create a toast (use `app$notify()`).
    #' @param message Text.
    #' @param title Optional title.
    #' @param severity Severity.
    initialize = function(message, title = NULL, severity = "information") {
      self$severity <- check_choice(severity, severities, "severity")
      super$initialize(message, classes = paste0("toast-", self$severity))
      self$border_title <- title
    },

    #' @description The built-in style.
    default_style = function() {
      merge_styles(
        style(width = "auto", height = "auto", max_width = 50, min_width = 12, border = "round",
              padding = c(0, 1), wrap = "word", background = "default"),
        severity_style(self$severity)
      )
    },

    #' @description Clicking closes the toast.
    #' @param event A `MouseEvent`.
    on_click = function(event) {
      self$remove()
      event$stop()
    }
  )
)
