button_variants <- list(
  default = style(),
  primary = style(background = "$primary", foreground = "$on_primary"),
  success = style(background = "$success", foreground = "black"),
  warning = style(background = "$warning", foreground = "black"),
  error = style(background = "$error", foreground = "$on_primary")
)

#' @title Button widget
#' @description A focusable button. See [button()].
#' @rdname Button-class
#' @export
Button <- R6::R6Class(
  "Button",
  inherit = Widget,
  public = list(
    #' @field paint_states Pressing only repaints.
    paint_states = "pressed",
    #' @field focusable Buttons can be focused.
    focusable = TRUE,
    #' @field variant Colour variant.
    variant = "default",
    #' @field press_duration Seconds the pressed state stays visible.
    press_duration = 0.15,

    #' @description Create a button.
    #' @param label Button text, or a function of no arguments that returns it
#'   (re-evaluated when the [signal()]s it reads change).
    #' @param id,classes,style,disabled See [Widget].
    #' @param variant One of `"default"`, `"primary"`, `"success"`,
    #'   `"warning"`, `"error"`.
    initialize = function(label = "Button", id = NULL, classes = NULL, style = NULL,
                          disabled = FALSE, variant = "default") {
      self$variant <- check_choice(variant, names(button_variants), "variant")
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      private$.state$label <- check_text(label)
      private$.state$pressed <- FALSE
    },

    #' @description The built-in style.
    default_style = function() {
      merge_styles(
        style(
          width = "auto", height = "auto", min_width = 10, border = "round",
          padding = c(0, 1), align = "center",
          focus = style(bold = TRUE, border_color = "$accent"),
          hover = style(bold = TRUE),
          disabled = style(foreground = "$muted", border_color = "$muted", background = "default"),
          states = list(pressed = style(reverse = TRUE))
        ),
        button_variants[[self$variant]]
      )
    },

    #' @description Enter and space press the button.
    default_bindings = function() list(bind("enter,space", "press", "Press")),

    #' @description The button text.
    render = function() private$.state$label,

    #' @description Press the button: shows the pressed state briefly and
    #'   sends a `"button.pressed"` message. Does nothing when disabled.
    #' @return `TRUE` if the button was pressed.
    press = function() {
      if (!self$is_enabled()) return(invisible(FALSE))
      self$set_state("pressed", TRUE)
      app <- self$app
      if (!is.null(app)) {
        app$set_timeout(self$press_duration, function() self$set_state("pressed", FALSE))
      }
      self$post_message("button.pressed", list(label = as.character(as_text(private$.state$label))))
      invisible(TRUE)
    },

    #' @description A left click presses the button.
    #' @param event A `MouseEvent`.
    on_click = function(event) {
      if (event$button == "left") {
        self$press()
        event$stop()
      }
    },

    #' @description Action used by the default bindings.
    action_press = function() self$press(),

    #' @description Active states: focus, disabled and pressed.
    pseudo_states = function() c(super$pseudo_states(), if (isTRUE(private$.state$pressed)) "pressed")
  ),
  active = list(
    #' @field label The button text (reactive).
    label = function(value) {
      if (missing(value)) return(private$.state$label)
      self$set_state("label", check_text(value))
    },
    #' @field pressed Is the button showing its pressed state?
    pressed = function(value) {
      if (!missing(value)) stop("`pressed` is read-only; call press().", call. = FALSE)
      isTRUE(private$.state$pressed)
    }
  ),
  private = list(
    describe = function() encodeString(as.character(as_text(private$.state$label)), quote = "\"")
  )
)

#' Button
#'
#' A focusable button. Pressing Enter or Space while it has focus sends a
#' `"button.pressed"` message, which you can handle with
#' `on("button.pressed", "#id", function(event, app) ...)`.
#'
#' @param label Button text.
#' @param id Optional identifier (for `#id` selectors).
#' @param classes Optional classes (for `.class` selectors).
#' @param style A [style()]. State styles `focus`, `disabled` and
#'   `states = list(pressed = ...)` are supported.
#' @param disabled Disabled buttons cannot be focused or pressed.
#' @param variant Colour variant: `"default"`, `"primary"`, `"success"`,
#'   `"warning"` or `"error"`.
#' @param on_press Optional `function()` (or `function(event, app)`) called
#'   when the button is pressed.
#' @return A `Button` widget.
#' @export
#' @examples
#' button("Run", id = "run", variant = "primary")
button <- function(label = "Button", id = NULL, classes = NULL, style = NULL,
                   disabled = FALSE, variant = "default", on_press = NULL) {
  check_function(on_press, "on_press", allow_null = TRUE)
  fn <- if (is.function(label)) label
  w <- Button$new(if (is.null(fn)) label else "", id = id, classes = classes, style = style,
                  disabled = disabled, variant = variant)
  if (!is.null(fn)) w$bind_reactive("label", fn)
  if (!is.null(on_press)) w$on("button.pressed", function(event, app) call_flex(on_press, event, app))
  w
}
