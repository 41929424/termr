#' Compare two screen buffers
#'
#' Computes the minimal set of horizontal runs of cells that must be
#' rewritten to turn the screen `old` into `new`. Nearby runs on the same
#' row are merged when rewriting the unchanged cells between them is cheaper
#' than an extra cursor movement.
#'
#' If `old` is `NULL` or has different dimensions, the patch covers every
#' non-blank cell of `new` and is marked `full = TRUE`; the renderer then
#' clears the screen first.
#'
#' @param old,new [ScreenBuffer] objects (`old` may be `NULL`).
#' @param merge_gap Runs separated by at most this many unchanged cells are
#'   merged.
#' @return A `termr_patch`: a list with `full` (logical), `width`, `height`
#'   and `runs`, a list of runs. Each run has `y`, `x` and vectors `chars`,
#'   `fg`, `bg`, `attrs` copied from `new`.
#' @export
#' @examples
#' a <- screen_buffer(10, 2)
#' b <- a$copy()
#' b$put_text(3, 2, "hi")
#' diff_screen(a, b)
diff_screen <- function(old, new, merge_gap = 3L) {
  full <- is.null(old) || old$width != new$width || old$height != new$height
  if (full) {
    old <- ScreenBuffer$new(new$width, new$height)
  }
  changed <- old$chars != new$chars | old$fg != new$fg |
    old$bg != new$bg | old$attrs != new$attrs
  if (!any(changed)) {
    return(new_patch(list(), full, new))
  }

  # Keep wide characters whole: a changed continuation cell pulls in its
  # lead cell, and a changed lead cell pulls in its continuation.
  if (new$width > 1L) {
    cont <- new$chars == ""
    lead_of_changed_cont <- cbind(changed[, -1L, drop = FALSE] & cont[, -1L, drop = FALSE], FALSE)
    cont_of_changed_lead <- cbind(FALSE, changed[, -new$width, drop = FALSE] & cont[, -1L, drop = FALSE])
    changed <- changed | lead_of_changed_cont | cont_of_changed_lead
  }

  pos <- which(changed, arr.ind = TRUE)
  pos <- pos[order(pos[, 1L], pos[, 2L]), , drop = FALSE]
  rows <- pos[, 1L]
  cols <- pos[, 2L]
  n <- length(rows)
  breaks <- c(TRUE, rows[-1L] != rows[-n] | cols[-1L] - cols[-n] > merge_gap + 1L)
  run_id <- cumsum(breaks)
  starts <- which(breaks)
  ends <- c(starts[-1L] - 1L, n)

  runs <- lapply(seq_along(starts), function(i) {
    y <- rows[[starts[[i]]]]
    x1 <- cols[[starts[[i]]]]
    x2 <- cols[[ends[[i]]]]
    span <- seq.int(x1, x2)
    list(
      y = y,
      x = x1,
      chars = new$chars[y, span],
      fg = new$fg[y, span],
      bg = new$bg[y, span],
      attrs = new$attrs[y, span]
    )
  })
  new_patch(runs, full, new)
}

new_patch <- function(runs, full, buffer) {
  structure(
    list(full = full, width = buffer$width, height = buffer$height, runs = runs),
    class = "termr_patch"
  )
}

#' @export
format.termr_patch <- function(x, ...) {
  cells <- sum(vapply(x$runs, function(r) length(r$chars), integer(1)))
  sprintf(
    "<patch %dx%d%s: %d run(s), %d cell(s)>",
    x$width, x$height, if (x$full) " full" else "", length(x$runs), cells
  )
}

#' @export
print.termr_patch <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}
