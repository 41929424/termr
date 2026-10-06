# First-in first-out queue of pending events.
EventQueue <- R6::R6Class(
  "EventQueue",
  public = list(
    push = function(event) {
      private$items[[length(private$items) + 1L]] <- event
      invisible(self)
    },

    pop = function() {
      if (private$head > length(private$items)) return(NULL)
      item <- private$items[[private$head]]
      private$items[private$head] <- list(NULL)
      private$head <- private$head + 1L
      if (private$head > 64L && private$head > length(private$items) / 2) {
        private$items <- private$items[-seq_len(private$head - 1L)]
        private$head <- 1L
      }
      item
    },

    size = function() length(private$items) - private$head + 1L,

    is_empty = function() self$size() == 0L,

    clear = function() {
      private$items <- list()
      private$head <- 1L
      invisible(self)
    }
  ),
  private = list(items = list(), head = 1L)
)
