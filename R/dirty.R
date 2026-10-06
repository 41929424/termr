# Dirty regions.
#
# A repaint after a small change does not redraw the screen: the regions of
# the widgets that changed are collected as rectangles, clipped to the
# screen, widened by one column (so a wide grapheme cut by an edge is always
# repainted whole), merged and painted into a copy of the previous frame.
# These helpers are pure functions on rectangles (see geometry.R).

rect_area <- function(r) as.double(r$width) * as.double(r$height)

# Smallest rectangle containing both.
rect_union <- function(a, b) {
  x1 <- min(a$x, b$x)
  y1 <- min(a$y, b$y)
  x2 <- max(rect_right(a), rect_right(b))
  y2 <- max(rect_bottom(a), rect_bottom(b))
  rect(x1, y1, x2 - x1 + 1L, y2 - y1 + 1L)
}

# Overlapping, or touching with (nearly) no wasted space in the union.
rect_should_merge <- function(a, b, slack = 1.25) {
  overlap <- !rect_is_empty(rect_intersect(a, b))
  if (overlap) return(TRUE)
  rect_area(rect_union(a, b)) <= slack * (rect_area(a) + rect_area(b))
}

# Merge rectangles until no two should be merged. Empty ones are dropped.
merge_rects <- function(rects) {
  rects <- Filter(function(r) !rect_is_empty(r), rects)
  repeat {
    n <- length(rects)
    if (n < 2L) break
    merged <- FALSE
    for (i in seq_len(n - 1L)) {
      for (j in seq.int(i + 1L, n)) {
        if (rect_should_merge(rects[[i]], rects[[j]])) {
          rects[[i]] <- rect_union(rects[[i]], rects[[j]])
          rects[[j]] <- NULL
          merged <- TRUE
          break
        }
      }
      if (merged) break
    }
    if (!merged) break
  }
  rects
}

# Dirty rectangles for widgets: their regions clipped to `bounds`, widened
# by one column each side, merged. At most `max_rects` are kept (more
# than that collapses into their bounding box).
dirty_rects <- function(widgets, bounds, max_rects = 12L, extra = list()) {
  rects <- list()
  for (r in extra) {
    r <- rect_intersect(rect(r$x - 1L, r$y, r$width + 2L, r$height), bounds)
    if (!rect_is_empty(r)) rects[[length(rects) + 1L]] <- r
  }
  for (w in widgets) {
    r <- w$region
    if (is.null(r) || is.null(w$app)) next
    r <- rect_intersect(rect(r$x - 1L, r$y, r$width + 2L, r$height), bounds)
    if (!rect_is_empty(r)) rects[[length(rects) + 1L]] <- r
  }
  rects <- merge_rects(rects)
  if (length(rects) > max_rects) {
    box <- rects[[1]]
    for (r in rects[-1L]) box <- rect_union(box, r)
    rects <- list(box)
  }
  rects
}

# Layout snapshots: the widgets of the visible layers in tree order and
# their regions (a matrix, -1 for hidden widgets). Comparing two snapshots
# tells which widgets moved or changed size, so a layout change can still be
# repainted as a few rectangles (old and new region of each changed widget).
layout_snapshot <- function(roots) {
  widgets <- unlist(lapply(roots, render_walk), recursive = FALSE)
  regions <- matrix(unlist(lapply(widgets, function(w) {
    r <- w$region
    if (is.null(r) || !w$visible) c(-1L, -1L, -1L, -1L) else c(r$x, r$y, r$width, r$height)
  })), ncol = 4L, byrow = TRUE)
  list(widgets = widgets, regions = regions)
}

# Rectangles to repaint because widgets changed region, or NULL when the
# trees differ (mounted / removed widgets: full repaint).
snapshot_changes <- function(old, new, max_changed = 40L) {
  if (is.null(old) || !identical(old$widgets, new$widgets)) return(NULL)
  moved <- which(rowSums(old$regions != new$regions) > 0L)
  if (length(moved) > max_changed) return(NULL)
  rects <- list()
  as_rect <- function(v) if (v[[1]] < 0L) NULL else rect(v[[1]], v[[2]], v[[3]], v[[4]])
  for (i in moved) {
    rects <- c(rects, Filter(Negate(is.null), list(as_rect(old$regions[i, ]), as_rect(new$regions[i, ]))))
  }
  rects
}
