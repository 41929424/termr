# An index of the ordinary vertical layout, not a second sizing algorithm.
# The exact existing arrange method builds relative rectangles once. Sorted
# tops/bottoms let subsequent translations select O(viewport) children.
# Consumers: ScrollView and its ordinary Vertical/Panel/Screen descendants.
# Extents rebuild after a size/content/tree revision or a layout-style epoch.
# Offscreen widgets remain mounted; focus can request a complete layout.

render_children <- function(widget) {
  widget_private(widget)$.render_children %||% widget$children
}

render_walk <- function(widget) {
  c(list(widget), unlist(lapply(render_children(widget), render_walk), recursive = FALSE))
}

# Intersections of half-open integer intervals with [lower, upper).
# Both starts and ends must be sorted; touching an edge is not an overlap.
visible_range <- function(starts, ends, lower, upper) {
  if (!length(starts) || upper <= lower) return(integer())
  first <- findInterval(lower, ends) + 1L
  last <- findInterval(upper - 1, starts)
  if (first > last) integer() else seq.int(first, last)
}

indexed_layout_tree <- function(widget, region, st, viewport) {
  scrolling <- identical(class(widget)[[1]], "ScrollView")
  ordinary <- class(widget)[[1]] %in% c("Vertical", "Panel", "Screen", "Widget")
  if ((!scrolling && (!ordinary || is.null(viewport))) || st$layout != "vertical" ||
      length(widget$children) < 64L || isTRUE(termr_env$full_layout) ||
      !isTRUE(getOption("termr.indexed_layout", TRUE))) return(FALSE)
  p <- widget_private(widget)
  inner <- content_rect(region, st)
  key <- c(inner$width, inner$height, p$.layout_revision, termr_env$layout_epoch,
           if (scrolling) c(widget$direction, widget$scrollbars))
  index <- p$.layout_index
  if (is.null(index) || !identical(index$key, key)) {
    kids <- visible_children(widget)
    for (child in widget$children) if (!child$visible) clear_regions(child)
    rs <- widget$arrange_children(kids, rect(0L, 0L, inner$width, inner$height), st)
    ox <- if (scrolling) widget$offset_x else 0L
    oy <- if (scrolling) widget$offset_y else 0L
    positions <- lapply(rs, function(r) c(r$x + ox, r$y + oy, r$width, r$height))
    positions <- matrix(unlist(positions), ncol = 4L, byrow = TRUE)
    starts <- positions[, 2L]
    ends <- starts + positions[, 4L]
    if (is.unsorted(starts) || is.unsorted(ends)) return(FALSE)
    index <- list(key = key, kids = kids, positions = positions, starts = starts, ends = ends,
                  viewport = if (scrolling) widget$viewport else NULL)
    p$.layout_index <- index
  }
  ox <- if (scrolling) widget$offset_x else 0L
  oy <- if (scrolling) widget$offset_y else 0L
  if (scrolling) {
    vp <- index$viewport
    p$.viewport <- rect(inner$x + vp$x, inner$y + vp$y, vp$width, vp$height)
    viewport <- if (is.null(viewport)) p$.viewport else rect_intersect(viewport, p$.viewport)
  }
  # Two extra rows on each side reduce churn at viewport edges.
  from <- viewport$y - inner$y + oy - 2L
  to <- viewport$y + viewport$height - inner$y + oy + 2L
  selected <- visible_range(index$starts, index$ends, from, to)
  # Only previously arranged roots need clearing. Their descendants cannot
  # paint or receive mouse input through a parent with a NULL region.
  for (child in p$.render_children) child$region <- NULL
  p$.render_children <- index$kids[selected]
  for (i in selected) {
    r <- index$positions[i, ]
    layout_tree(index$kids[[i]], rect(inner$x + r[[1]] - ox, inner$y + r[[2]] - oy,
                                    r[[3]], r[[4]]), viewport)
  }
  TRUE
}
