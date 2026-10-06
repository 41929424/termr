# Timers are owned by the event loop: callbacks run between events, never
# from another thread or a blocking sleep. The clock is injectable so tests
# can advance time deterministically.

#' @title Timer handle
#' @description Returned by [set_timeout()] and [set_interval()].
#' @export
Timer <- R6::R6Class(
  "Timer",
  public = list(
    #' @field delay Delay (or interval) in seconds.
    delay = NULL,
    #' @field repeating Does the timer repeat?
    repeating = FALSE,
    #' @field due Clock time of the next run.
    due = NULL,
    #' @field callback Function called when the timer fires.
    callback = NULL,
    #' @field active Is the timer still scheduled?
    active = TRUE,
    #' @field runs How many times the timer has fired.
    runs = 0L,

    #' @description Create a timer (use [set_timeout()] instead).
    #' @param delay Seconds.
    #' @param callback Function.
    #' @param repeating Logical.
    #' @param now Current clock time.
    initialize = function(delay, callback, repeating, now) {
      if (!is_scalar_number(delay) || delay < 0) {
        stop("`delay` must be a non-negative number of seconds.", call. = FALSE)
      }
      if (repeating && delay <= 0) stop("An interval must be positive.", call. = FALSE)
      check_function(callback, "callback")
      self$delay <- delay
      self$callback <- callback
      self$repeating <- repeating
      self$due <- now + delay
    },

    #' @description Stop the timer.
    cancel = function() {
      self$active <- FALSE
      invisible(self)
    },

    #' @description Print the timer.
    #' @param ... Ignored.
    print = function(...) {
      cat(sprintf(
        "<Timer %s %gs%s, runs: %d>\n",
        if (self$repeating) "interval" else "timeout", self$delay,
        if (self$active) "" else " (cancelled)", self$runs
      ))
      invisible(self)
    }
  )
)

TimerManager <- R6::R6Class(
  "TimerManager",
  public = list(
    timers = list(),

    initialize = function(clock) {
      private$clock <- clock
    },

    add = function(delay, callback, repeating = FALSE) {
      timer <- Timer$new(delay, callback, repeating, private$clock())
      self$timers[[length(self$timers) + 1L]] <- timer
      timer
    },

    # Seconds until the next timer is due (Inf when none).
    time_until_next = function() {
      self$prune()
      if (length(self$timers) == 0L) return(Inf)
      due <- min(vapply(self$timers, function(t) t$due, numeric(1)))
      max(0, due - private$clock())
    },

    # Run every due timer once, in order of due time.
    fire_due = function(run) {
      now <- private$clock()
      due <- Filter(function(t) t$active && t$due <= now, self$timers)
      if (length(due) == 0L) return(invisible(0L))
      due <- due[order(vapply(due, function(t) t$due, numeric(1)))]
      for (timer in due) {
        if (!timer$active) next
        if (timer$repeating) {
          # Skip missed ticks instead of firing a burst.
          next_due <- timer$due + timer$delay
          timer$due <- if (next_due <= now) now + timer$delay else next_due
        } else {
          timer$active <- FALSE
        }
        timer$runs <- timer$runs + 1L
        run(timer)
      }
      self$prune()
      invisible(length(due))
    },

    prune = function() {
      self$timers <- Filter(function(t) t$active, self$timers)
      invisible(self)
    },

    cancel_all = function() {
      for (t in self$timers) t$cancel()
      self$timers <- list()
      invisible(self)
    }
  ),
  private = list(clock = NULL)
)
