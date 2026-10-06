#' Reduced motion
#'
#' Decorative animation (`animate()` transitions, indeterminate progress
#' shimmer, spinners) is turned off when motion is reduced: `animate()` jumps
#' to the final value at once, spinners show a still symbol. Timers, events
#' and real progress updates are unaffected. Motion is reduced when
#' `app(reduce_motion = TRUE)`, `options(termr.reduce_motion = TRUE)` or the
#' environment variable `TERMR_REDUCE_MOTION=1` is set (in that order of
#' precedence).
#'
#' @param app An app, or `NULL` to look at the options only.
#' @return `TRUE` or `FALSE`.
#' @export
#' @examples
#' motion_reduced()
motion_reduced <- function(app = NULL) {
  if (!is.null(app) && !is.null(app$reduce_motion)) return(isTRUE(app$reduce_motion))
  opt <- getOption("termr.reduce_motion")
  if (!is.null(opt)) return(isTRUE(opt))
  Sys.getenv("TERMR_REDUCE_MOTION") %in% c("1", "true", "yes")
}

# Animations.
#
# An animation moves a numeric field of a widget (any reactive field, e.g.
# a progress bar's value or a scroll view's offset_y) from one value to
# another over time. All animations of an app share one timer of the event
# loop (30 frames per second), so the frames of many animations are a
# single repaint. The app's clock is used, so headless tests advance
# animations deterministically with pilot$advance().

easings <- list(
  linear = function(t) t,
  in_quad = function(t) t * t,
  out_quad = function(t) t * (2 - t),
  in_out_quad = function(t) ifelse(t < 0.5, 2 * t * t, -1 + (4 - 2 * t) * t),
  in_cubic = function(t) t^3,
  out_cubic = function(t) (t - 1)^3 + 1,
  in_out_cubic = function(t) ifelse(t < 0.5, 4 * t^3, (t - 1) * (2 * t - 2)^2 + 1)
)

#' @title Animation handle
#' @description Returned by [animate()].
#' @export
Animation <- R6::R6Class(
  "Animation",
  public = list(
    #' @field widget,property What is animated.
    widget = NULL,
    property = NULL,
    #' @field from,to Start and end values.
    from = NULL,
    to = NULL,
    #' @field duration Seconds.
    duration = NULL,
    #' @field easing Easing name.
    easing = NULL,
    #' @field start Clock time of the start.
    start = NULL,
    #' @field active Still running?
    active = TRUE,
    #' @field on_complete `function(widget, app)` or `NULL`.
    on_complete = NULL,
    #' @description Create an animation (use [animate()]).
    #' @param widget,property,from,to,duration,easing,start,on_complete See fields.
    initialize = function(widget, property, from, to, duration, easing, start, on_complete) {
      self$widget <- widget
      self$property <- property
      self$from <- from
      self$to <- to
      self$duration <- duration
      self$easing <- easing
      self$start <- start
      self$on_complete <- on_complete
    },
    #' @description Stop the animation where it is.
    cancel = function() {
      self$active <- FALSE
      invisible(self)
    }
  )
)

Animator <- R6::R6Class(
  "Animator",
  public = list(
    animations = list(),
    fps = 30,

    initialize = function(app) {
      private$app <- app
    },

    add = function(animation) {
      # Replace a running animation of the same property.
      for (a in self$animations) {
        if (identical(a$widget, animation$widget) && a$property == animation$property) a$cancel()
      }
      self$animations <- c(Filter(function(a) a$active, self$animations), list(animation))
      if (is.null(private$timer) || !private$timer$active) {
        private$timer <- private$app$set_interval(1 / self$fps, function(app) self$frame())
      }
      invisible(animation)
    },

    frame = function() {
      now <- private$app$.__enclos_env__$private$clock()
      for (a in self$animations) {
        if (!a$active) next
        if (!identical(a$widget$app, private$app)) {
          a$cancel()
          next
        }
        t <- if (a$duration <= 0) 1 else min(1, (now - a$start) / a$duration)
        value <- a$from + (a$to - a$from) * easings[[a$easing]](t)
        if (t >= 1) value <- a$to
        if (is.integer(a$from) && is.integer(a$to)) value <- as.integer(round(value))
        a$widget[[a$property]] <- value
        if (t >= 1) {
          a$active <- FALSE
          if (!is.null(a$on_complete)) call_flex(a$on_complete, a$widget, private$app)
        }
      }
      self$animations <- Filter(function(a) a$active, self$animations)
      if (!length(self$animations) && !is.null(private$timer)) {
        private$timer$cancel()
        private$timer <- NULL
      }
      invisible()
    },

    cancel_all = function() {
      for (a in self$animations) a$cancel()
      self$animations <- list()
      private$timer <- NULL
      invisible()
    }
  ),
  private = list(app = NULL, timer = NULL)
)

#' Animate a widget field
#'
#' Changes a numeric field of a widget (e.g. `value` of a [progress_bar()],
#' `offset_y` of a [scroll_view()], `count` of a custom widget) smoothly
#' from its current value to `to`. Starting a new animation of the same
#' field replaces the running one. Animations stop when the widget is
#' removed or the app exits.
#'
#' @param widget A widget attached to a running app.
#' @param property Name of a numeric field.
#' @param to Target value.
#' @param duration Seconds.
#' @param easing `"linear"`, `"in_quad"`, `"out_quad"`, `"in_out_quad"`,
#'   `"in_cubic"`, `"out_cubic"` (default) or `"in_out_cubic"`.
#' @param from Start value (default: the current value).
#' @param on_complete Optional `function(widget, app)`.
#' @return An `Animation` handle with `cancel()`.
#' @export
#' @examples
#' \dontrun{
#' animate(app$query_one("#progress"), "value", to = 1, duration = 0.5)
#' }
animate <- function(widget, property, to, duration = 0.3, easing = "out_cubic", from = NULL,
                    on_complete = NULL) {
  if (!is_widget(widget)) stop("`widget` must be a widget.", call. = FALSE)
  check_scalar_character(property, "property")
  app <- widget$app
  if (is.null(app)) stop("The widget is not attached to an app.", call. = FALSE)
  if (!exists(property, envir = widget, inherits = FALSE)) {
    stop(sprintf("%s has no field \"%s\".", widget$type, property), call. = FALSE)
  }
  from <- from %||% widget[[property]]
  if (!is_scalar_number(from) || !is_scalar_number(to)) {
    stop("Only numeric fields can be animated (`from` and `to` must be numbers).", call. = FALSE)
  }
  if (!is_scalar_number(duration) || duration < 0) stop("`duration` must be a non-negative number.", call. = FALSE)
  easing <- check_choice(easing, names(easings), "easing")
  check_function(on_complete, "on_complete", allow_null = TRUE)
  if (is.integer(from) && is.numeric(to) && to == round(to)) to <- as.integer(to)
  if (motion_reduced(app)) {
    # Reduced motion: no transition, the final value at once.
    widget[[property]] <- to
    done <- Animation$new(widget, property, from, to, 0, easing, 0, on_complete)
    done$active <- FALSE
    if (!is.null(on_complete)) call_flex(on_complete, widget, app)
    return(invisible(done))
  }
  animation <- Animation$new(widget, property, from, to, duration, easing,
                             app$.__enclos_env__$private$clock(), on_complete)
  app$.__enclos_env__$private$animator()$add(animation)
}
