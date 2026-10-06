# Focus management.
#
# The focus chain is every widget of the screen that is focusable, visible
# (with all ancestors) and enabled (with all ancestors), in tree order.
# The FocusManager only tracks state; the App turns changes into
# FocusEvent / BlurEvent and repaints.

FocusManager <- R6::R6Class(
  "FocusManager",
  public = list(
    focused = NULL,
    # TRUE when the focused widget was removed; focus moves on.
    lost = FALSE,

    initialize = function(app) {
      private$app <- app
    },

    can_focus = function(widget) {
      isTRUE(widget$focusable) && identical(widget$app, private$app) &&
        identical(widget_root(widget), private$app$screen) &&
        widget$is_displayed() && widget$is_enabled()
    },

    chain = function() Filter(self$can_focus, private$app$screen$walk()),

    is_valid = function() !is.null(self$focused) && self$can_focus(self$focused),

    # Change focus; returns list(old, new) or NULL when nothing changed.
    set = function(widget) {
      if (!is.null(widget) && !self$can_focus(widget)) return(NULL)
      old <- self$focused
      if (identical(old, widget)) return(NULL)
      self$focused <- widget
      list(old = old, new = widget)
    },

    # The widget `direction` steps away from the focused one (wrapping).
    neighbour = function(direction) {
      chain <- self$chain()
      if (length(chain) == 0L) return(NULL)
      current <- self$focused
      idx <- if (is.null(current)) 0L else match(TRUE, vapply(chain, identical, logical(1), current), nomatch = 0L)
      if (idx == 0L) return(chain[[if (direction > 0L) 1L else length(chain)]])
      chain[[(idx - 1L + direction) %% length(chain) + 1L]]
    },

    # Forget the focused widget if it is `widget` or inside it.
    release = function(widget) {
      f <- self$focused
      if (!is.null(f) && (identical(f, widget) || any(vapply(f$ancestors(), identical, logical(1), widget)))) {
        self$focused <- NULL
        self$lost <- TRUE
      }
      invisible(self)
    }
  ),
  private = list(app = NULL)
)
