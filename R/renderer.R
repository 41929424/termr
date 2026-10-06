#' Convert a screen patch to ANSI escape sequences
#'
#' Each run becomes a cursor movement followed by the run's characters. An
#' SGR (style) sequence is emitted only when the style differs from the
#' previous cell. The output always ends with a style reset.
#'
#' @param patch A patch from [diff_screen()].
#' @param color_mode One of `"truecolor"`, `"256"`, `"16"` or `"none"`.
#' @return A single string.
#' @export
#' @examples
#' a <- screen_buffer(10, 1)
#' b <- a$copy()
#' b$put_text(1, 1, "hi", fg = "red")
#' patch_to_ansi(diff_screen(a, b))
patch_to_ansi <- function(patch, color_mode = "truecolor") {
  prefix <- if (patch$full) paste0(ansi_reset(), ansi_clear_screen()) else ""
  if (length(patch$runs) == 0L) return(prefix)
  current <- ""
  parts <- character(length(patch$runs))
  for (i in seq_along(patch$runs)) {
    run <- patch$runs[[i]]
    keep <- run$chars != ""
    chars <- run$chars[keep]
    fg <- run$fg[keep]
    bg <- run$bg[keep]
    attrs <- run$attrs[keep]
    n <- length(chars)
    if (n == 0L) next
    keys <- paste(fg, bg, attrs, sep = "\r")
    changed <- keys != c(current, keys[-n])
    sgr <- character(n)
    sgr[changed] <- ansi_sgr(fg[changed], bg[changed], attrs[changed], color_mode)
    parts[[i]] <- paste0(ansi_cursor_to(run$y, run$x), paste0(sgr, chars, collapse = ""))
    current <- keys[[n]]
  }
  paste0(prefix, paste(parts, collapse = ""), ansi_reset())
}

#' Incremental screen renderer
#'
#' Keeps a copy of the frame currently shown by the terminal (the "front"
#' buffer). `$render()` diffs a new frame against it and writes
#' only the changes through the `write` function.
#'
#' @keywords internal
#' @noRd
Renderer <- R6::R6Class(
  "Renderer",
  public = list(
    color_mode = "truecolor",
    synchronized = TRUE,
    front = NULL,
    frames = 0L,
    bytes_written = 0,

    initialize = function(write, color_mode = "truecolor", synchronized = TRUE) {
      check_function(write)
      private$write <- write
      self$color_mode <- color_mode
      self$synchronized <- synchronized
    },

    render = function(buffer) {
      patch <- diff_screen(self$front, buffer)
      if (length(patch$runs) > 0L || patch$full) {
        out <- patch_to_ansi(patch, self$color_mode)
        if (self$synchronized) out <- paste0(ansi_sync(TRUE), out, ansi_sync(FALSE))
        private$write(out)
        self$bytes_written <- self$bytes_written + nchar(out, type = "bytes")
      }
      self$frames <- self$frames + 1L
      self$front <- buffer$copy()
      invisible(patch)
    },

    # Forget what is on screen; the next render redraws everything.
    invalidate = function() {
      self$front <- NULL
      invisible(self)
    }
  ),
  private = list(write = NULL)
)
