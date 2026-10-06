# Form controls: checkbox, radio buttons, select.

#' @title Checkbox widget
#' @description A labelled boolean toggle. See [checkbox()].
#' @rdname Checkbox-class
#' @export
Checkbox <- R6::R6Class(
  "Checkbox",
  inherit = Widget,
  public = list(
    #' @field focusable Checkboxes can be focused.
    focusable = TRUE,
    #' @description Create a checkbox. See [checkbox()].
    #' @param label Label.
    #' @param value Initial state.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(label = "", value = FALSE, id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      check_flag(value)
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      private$.state$label <- check_text(label)
      private$.state$value <- value
    },
    #' @description The built-in style.
    default_style = function() {
      style(width = "auto", height = "auto", focus = style(bold = TRUE), disabled = style(foreground = "$muted"))
    },
    #' @description Space and Enter toggle.
    default_bindings = function() list(bind("space,enter", "toggle", "Toggle")),
    #' @description Toggle the value (sends `"checkbox.changed"`).
    toggle = function() {
      if (self$is_enabled()) self$value <- !private$.state$value
      invisible(self)
    },
    #' @description Action used by the bindings.
    action_toggle = function() self$toggle(),
    #' @description Clicking toggles.
    #' @param event A `MouseEvent`.
    on_click = function(event) {
      if (event$button == "left") {
        self$toggle()
        event$stop()
      }
    },
    #' @description The box and the label.
    render = function() {
      box <- if (private$.state$value) "[x]" else "[ ]"
      mark_style <- if (private$.state$value) style(foreground = "$success", bold = TRUE) else NULL
      c(span(box, mark_style), span(" "), as_text(private$.state$label))
    }
  ),
  active = list(
    #' @field value `TRUE` or `FALSE` (reactive; sends `"checkbox.changed"`).
    value = function(value) {
      if (missing(value)) return(private$.state$value)
      check_flag(value, "value")
      if (self$set_state("value", value)) self$post_message("checkbox.changed", list(value = value))
    },
    #' @field label The label (reactive).
    label = function(value) {
      if (missing(value)) return(private$.state$label)
      self$set_state("label", check_text(value))
    }
  )
)

#' Checkbox
#'
#' A labelled toggle. Space, Enter or a click toggles it and sends
#' `"checkbox.changed"` (`event$data$value`).
#'
#' @param label Label text.
#' @param value Initial state.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param disabled Disabled checkboxes cannot be toggled.
#' @return A `Checkbox` widget with a reactive `value`.
#' @export
#' @examples
#' checkbox("Enable logs", value = TRUE, id = "logs")
checkbox <- function(label = "", value = FALSE, id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
  Checkbox$new(label, value = value, id = id, classes = classes, style = style, disabled = disabled)
}

#' @title RadioButton widget
#' @description One option of a [radio_set()].
#' @export
RadioButton <- R6::R6Class(
  "RadioButton",
  inherit = Widget,
  public = list(
    #' @field focusable Radio buttons can be focused.
    focusable = TRUE,
    #' @field value The value this option stands for.
    value = NULL,
    #' @description Create a radio button. See [radio_button()].
    #' @param label Label.
    #' @param value Value.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(label, value = label, id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      private$.state$label <- check_text(label)
      private$.state$selected <- FALSE
      self$value <- value
    },
    #' @description The built-in style.
    default_style = function() {
      style(width = "auto", height = "auto", focus = style(bold = TRUE), disabled = style(foreground = "$muted"))
    },
    #' @description Space and Enter select the option.
    default_bindings = function() list(bind("space,enter", "choose", "Select")),
    #' @description Select this option in its radio set.
    action_choose = function() {
      set <- self$parent
      if (inherits(set, "RadioSet") && self$is_enabled()) set$select(self)
    },
    #' @description Clicking selects.
    #' @param event A `MouseEvent`.
    on_click = function(event) {
      if (event$button == "left") {
        self$action_choose()
        event$stop()
      }
    },
    #' @description The dot and the label.
    render = function() {
      dot <- if (unicode_ok()) "\u2022" else "*"
      mark <- if (private$.state$selected) paste0("(", dot, ")") else "( )"
      mark_style <- if (private$.state$selected) style(foreground = "$success", bold = TRUE) else NULL
      c(span(mark, mark_style), span(" "), as_text(private$.state$label))
    }
  ),
  active = list(
    #' @field selected Is this option selected? (read-only; use the set)
    selected = function(value) if (missing(value)) private$.state$selected else read_only("selected"),
    #' @field label The label (reactive).
    label = function(value) {
      if (missing(value)) return(private$.state$label)
      self$set_state("label", check_text(value))
    }
  )
)

#' @title RadioSet widget
#' @description A group of radio buttons. See [radio_set()].
#' @rdname RadioSet-class
#' @export
RadioSet <- R6::R6Class(
  "RadioSet",
  inherit = Vertical,
  public = list(
    #' @description Create a radio set. See [radio_set()].
    #' @param ... Radio buttons.
    #' @param selected Value to select initially.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(..., selected = NULL, id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      super$initialize(..., id = id, classes = classes, style = style, disabled = disabled)
      bad <- !vapply(self$children, function(w) inherits(w, "RadioButton"), logical(1))
      if (any(bad)) stop("radio_set() takes radio_button()s.", call. = FALSE)
      if (!is.null(selected)) self$value <- selected
    },
    #' @description The built-in style.
    default_style = function() style(width = "auto", height = "auto", layout = "vertical"),
    #' @description Up/Down move to the previous / next option and select it.
    default_bindings = function() list(bind("up", "previous_option"), bind("down", "next_option")),
    #' @description Select an option (sends `"radio_set.changed"`).
    #' @param button A child radio button.
    select = function(button) {
      changed <- !isTRUE(button$selected)
      for (b in self$children) {
        bp <- widget_private(b)
        if (!identical(bp$.state$selected, identical(b, button))) b$set_state("selected", identical(b, button))
      }
      if (changed) {
        self$post_message("radio_set.changed", list(
          value = button$value, index = match(TRUE, vapply(self$children, identical, logical(1), button)),
          label = as.character(as_text(button$label))
        ))
      }
      invisible(self)
    },
    #' @description Move to the next option.
    action_next_option = function() private$step(1L),
    #' @description Move to the previous option.
    action_previous_option = function() private$step(-1L)
  ),
  active = list(
    #' @field value The selected value (`NULL` if none); assign to select.
    value = function(value) {
      if (missing(value)) {
        for (b in self$children) if (b$selected) return(b$value)
        return(NULL)
      }
      for (b in self$children) {
        if (identical(b$value, value)) return(self$select(b))
      }
      stop(sprintf("No radio button has the value \"%s\".", format(value)), call. = FALSE)
    }
  ),
  private = list(
    step = function(delta) {
      buttons <- Filter(function(b) b$is_enabled(), self$children)
      if (!length(buttons)) return(invisible())
      app <- self$app
      current <- if (!is.null(app)) app$focused else NULL
      i <- match(TRUE, vapply(buttons, identical, logical(1), current), nomatch = 0L)
      target <- buttons[[(i - 1L + delta) %% length(buttons) + 1L]]
      target$focus()
      self$select(target)
    }
  )
)

#' Radio buttons
#'
#' `radio_set()` groups `radio_button()`s so that one can be selected.
#' Space, Enter or a click selects an option; Up/Down move between options.
#' Changes send `"radio_set.changed"` (`event$data`: `value`, `index`,
#' `label`) from the set.
#'
#' @param ... `radio_button()`s.
#' @param selected Value selected initially.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `RadioSet` widget with a `value` field.
#' @export
#' @examples
#' radio_set(radio_button("CSV", "csv"), radio_button("JSON", "json"), selected = "csv")
radio_set <- function(..., selected = NULL, id = NULL, classes = NULL, style = NULL) {
  RadioSet$new(..., selected = selected, id = id, classes = classes, style = style)
}

#' @rdname radio_set
#' @param label Label of the option.
#' @param value Value of the option (defaults to the label).
#' @param disabled Can the option be selected?
#' @export
radio_button <- function(label, value = label, id = NULL, disabled = FALSE) {
  RadioButton$new(label, value = value, id = id, disabled = disabled)
}

# A drop-down list anchored below (or above) a widget.
DropdownScreen <- R6::R6Class(
  "DropdownScreen",
  inherit = ModalScreen,
  public = list(
    anchor = NULL,
    initialize = function(list_widget, anchor) {
      self$anchor <- anchor
      super$initialize(list_widget, dim = FALSE)
    },
    arrange_children = function(children, inner, st) {
      a <- self$anchor
      h <- natural_height(children[[1]], a$width)
      below <- inner$y + inner$height - (rect_bottom(a) + 1L)
      y <- if (below >= h || below >= a$y - inner$y) rect_bottom(a) + 1L else max(inner$y, a$y - h)
      h <- min(h, max(1L, if (y > a$y) below else a$y - inner$y))
      list(rect(a$x, y, a$width, h))
    },
    on_mouse_down = function(event) {
      if (identical(event$target, self)) {
        self$dismiss(NULL)
        event$stop()
      }
    },
    on_option_list_selected = function(event) {
      event$stop()
      self$dismiss(event$data$value)
    }
  )
)

#' @title Select widget
#' @description A drop-down choice. See [dropdown()].
#' @rdname Select-class
#' @export
Select <- R6::R6Class(
  "Select",
  inherit = Widget,
  public = list(
    #' @field focusable Selects can be focused.
    focusable = TRUE,
    #' @field prompt Text shown when nothing is selected.
    prompt = "Select",
    #' @description Create a select. See [dropdown()].
    #' @param choices Choices.
    #' @param value Initial value.
    #' @param prompt Prompt.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(choices, value = NULL, prompt = "Select", id = NULL, classes = NULL,
                          style = NULL, disabled = FALSE) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      check_scalar_character(prompt, "prompt")
      self$prompt <- prompt
      private$.choices <- as_choices(choices)
      private$.state$value <- NULL
      if (!is.null(value)) self$value <- value
    },
    #' @description The built-in style.
    default_style = function() {
      style(width = "1fr", height = "auto", border = "round", padding = c(0, 1), border_color = "$muted",
            focus = style(border_color = "$accent"), disabled = style(foreground = "$muted"))
    },
    #' @description Enter, Space and Down open the list.
    default_bindings = function() list(bind("enter,space,down", "open", "Open")),
    #' @description Open the drop-down list.
    open = function() {
      app <- self$app
      if (is.null(app) || is.null(self$region) || !self$is_enabled()) return(invisible(self))
      list_widget <- OptionList$new(structure(private$.choices$values, names = private$.choices$labels),
                                    style = style(background = "default", border = "round", border_color = "$accent"))
      if (!is.null(private$.state$value)) list_widget$value <- private$.state$value
      region <- self$region
      anchor <- rect(region$x, region$y, region$width, region$height)
      list_rows <- min(max(1L, length(private$.choices$values)), 8L) + 2L
      list_widget$style <- merge_styles(list_widget$style, style(height = list_rows))
      screen <- DropdownScreen$new(list_widget, anchor)
      app$push_screen(screen, callback = function(result, app) {
        if (!is.null(result)) self$value <- result
      })
      invisible(self)
    },
    #' @description Action used by the bindings.
    action_open = function() self$open(),
    #' @description Clicking opens the list.
    #' @param event A `MouseEvent`.
    on_click = function(event) {
      if (event$button == "left") {
        self$open()
        event$stop()
      }
    },
    #' @description The selected label and an arrow.
    render = function() {
      value <- private$.state$value
      arrow <- if (unicode_ok()) "\u25bc" else "v"
      label <- if (is.null(value)) span(self$prompt, style(foreground = "$muted")) else {
        span(private$.choices$labels[[match(value, private$.choices$values)]])
      }
      c(label, span(paste0(" ", arrow), style(foreground = "$muted")))
    }
  ),
  active = list(
    #' @field value The selected value or `NULL` (reactive; sends
    #'   `"dropdown.changed"`).
    value = function(value) {
      if (missing(value)) return(private$.state$value)
      if (!is.null(value) && !(value %in% private$.choices$values)) {
        stop(sprintf("\"%s\" is not one of the choices.", value), call. = FALSE)
      }
      if (self$set_state("value", value)) {
        i <- if (is.null(value)) NA_integer_ else match(value, private$.choices$values)
        self$post_message("dropdown.changed", list(
          value = value, label = if (is.na(i)) NULL else private$.choices$labels[[i]]
        ))
      }
    },
    #' @field choices The choice values.
    choices = function(value) if (missing(value)) private$.choices$values else read_only("choices")
  ),
  private = list(.choices = NULL)
)

#' Drop-down select
#'
#' Shows the selected choice; Enter, Space, Down or a click opens a list
#' below it. Choosing an option sends `"dropdown.changed"`
#' (`event$data`: `value`, `label`); Escape or a click outside closes the
#' list.
#'
#' Named `dropdown()` rather than `select()` so that it does not mask
#' `dplyr::select()`; the widget type (for selectors) is `Select`.
#'
#' @param choices A character vector; names, if present, are shown.
#' @param value Initially selected value, or `NULL`.
#' @param prompt Text shown when nothing is selected.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param disabled Disabled selects cannot be opened.
#' @return A `Select` widget with a reactive `value`.
#' @export
#' @examples
#' dropdown(c("Comma separated" = "csv", "JSON" = "json"), value = "csv", id = "format")
dropdown <- function(choices, value = NULL, prompt = "Select", id = NULL, classes = NULL, style = NULL,
                   disabled = FALSE) {
  Select$new(choices, value = value, prompt = prompt, id = id, classes = classes, style = style,
             disabled = disabled)
}
