log_levels <- c("debug", "info", "success", "warning", "error")

#' @title LogView widget
#' @description See [log_view()].
#' @rdname LogView-class
#' @export
LogView <- R6::R6Class(
  "LogView",
  inherit = ListBase,
  public = list(
    #' @field has_cursor Log views scroll; they have no cursor.
    has_cursor = FALSE,
    #' @field max_lines Lines kept (older lines are dropped).
    max_lines = 1000L,
    #' @field timestamps Prefix lines with the time?
    timestamps = FALSE,
    #' @field auto_scroll Follow new lines while scrolled to the end?
    auto_scroll = TRUE,

    #' @description Create a log view. See [log_view()].
    #' @param max_lines,timestamps,auto_scroll See fields.
    #' @param id,classes,style See [Widget].
    initialize = function(max_lines = 1000L, timestamps = FALSE, auto_scroll = TRUE,
                          id = NULL, classes = NULL, style = NULL) {
      check_flag(timestamps)
      check_flag(auto_scroll)
      super$initialize(id = id, classes = classes, style = style)
      self$max_lines <- check_count(max_lines, "max_lines")
      self$timestamps <- timestamps
      self$auto_scroll <- auto_scroll
      private$.lines <- character()
      private$.levels <- character()
      private$.times <- character()
    },

    #' @description Add lines.
    #' @param ... Text, pasted together; newlines start new lines.
    #' @param level `"debug"`, `"info"`, `"success"`, `"warning"` or `"error"`.
    write = function(..., level = "info") {
      level <- check_choice(level, log_levels, "level")
      text <- strsplit(paste0(..., collapse = ""), "\n", fixed = TRUE)[[1]]
      if (!length(text)) text <- ""
      follow <- self$auto_scroll && !private$.paused && self$at_end()
      private$.lines <- c(private$.lines, text)
      private$.levels <- c(private$.levels, rep(level, length(text)))
      private$.times <- c(private$.times, rep(format(Sys.time(), "%H:%M:%S"), length(text)))
      excess <- length(private$.lines) - self$max_lines
      if (excess > 0L) {
        keep <- -seq_len(excess)
        private$.lines <- private$.lines[keep]
        private$.levels <- private$.levels[keep]
        private$.times <- private$.times[keep]
        private$.state$offset <- max(0L, private$.state$offset - excess)
      }
      if (follow) private$.state$offset <- private$clamp_offset(.Machine$integer.max)
      self$invalidate()
      invisible(self)
    },

    #' @description Remove all lines.
    clear = function() {
      private$.lines <- character()
      private$.levels <- character()
      private$.times <- character()
      private$.state$offset <- 0L
      self$invalidate()
      invisible(self)
    },

    #' @description Stop following new lines.
    pause = function() {
      private$.paused <- TRUE
      invisible(self)
    },

    #' @description Follow new lines again (and jump to the end).
    resume = function() {
      private$.paused <- FALSE
      self$scroll_to_end()
    },

    #' @description Is the view scrolled to the last line?
    at_end = function() {
      private$.state$offset >= length(private$.lines) - private$page()
    },

    #' @description Number of lines.
    item_count = function() length(private$.lines),
    #' @description Text of line `i`.
    #' @param i Index.
    item_text = function(i) {
      if (self$timestamps) {
        c(span(paste0(private$.times[[i]], " "), style(foreground = "$muted")), span(private$.lines[[i]]))
      } else {
        private$.lines[[i]]
      }
    },
    #' @description Style of line `i` (by level).
    #' @param i Index.
    item_style = function(i) {
      switch(
        private$.levels[[i]],
        debug = style(foreground = "$muted"),
        success = style(foreground = "$success"),
        warning = style(foreground = "$warning"),
        error = style(foreground = "$error", bold = TRUE),
        NULL
      )
    }
  ),
  active = list(
    #' @field lines The current lines.
    lines = function(value) if (missing(value)) private$.lines else read_only("lines"),
    #' @field paused Is following paused?
    paused = function(value) if (missing(value)) private$.paused else read_only("paused")
  ),
  private = list(.lines = NULL, .levels = NULL, .times = NULL, .paused = FALSE)
)

#' Log view
#'
#' A scrolling log for long-running work. `write()` appends lines with a
#' level (`"debug"`, `"info"`, `"success"`, `"warning"`, `"error"`, shown
#' in different colours). Only the last `max_lines` lines are kept, so
#' memory stays bounded. While the view is scrolled to the end it follows
#' new lines; scroll up (keys or wheel) to read older lines, or `pause()`.
#'
#' @param max_lines Number of lines kept.
#' @param timestamps Prefix lines with `HH:MM:SS`?
#' @param auto_scroll Follow new lines?
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `LogView` widget with methods `write()`, `clear()`,
#'   `pause()`, `resume()`.
#' @export
#' @examples
#' logs <- log_view(max_lines = 500, id = "log")
#' logs$write("Starting model...")
#' logs$write("Convergence is slow", level = "warning")
log_view <- function(max_lines = 1000L, timestamps = FALSE, auto_scroll = TRUE,
                     id = NULL, classes = NULL, style = NULL) {
  LogView$new(max_lines = max_lines, timestamps = timestamps, auto_scroll = auto_scroll,
              id = id, classes = classes, style = style)
}
