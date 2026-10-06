# Compositor: paints a laid-out widget tree into a ScreenBuffer.
#
# Layout (layout.R) has already assigned every visible widget a `region`.
# Painting walks the tree in order; children are clipped to their parent's
# content area. Each widget paints itself through `Widget$paint()`, which
# uses the drawing helpers below.

paint_tree <- function(widget, buffer, clip = buffer$bounds(), inherited = NULL) {
  if (!widget$visible || is.null(widget$region)) return(invisible())
  area <- rect_intersect(widget$region, clip)
  if (rect_is_empty(area)) return(invisible())
  st <- widget$computed_style(inherited)
  profile_add("widgets_painted")
  widget$paint(buffer, area, st)
  inner <- rect_intersect(widget$child_clip(st), clip)
  if (rect_is_empty(inner)) return(invisible())
  for (child in render_children(widget)) paint_tree(child, buffer, inner, st)
  invisible()
}

# The content box of a widget: its region minus border and padding.
content_rect <- function(region, st) {
  rect_shrink(region, chrome_edges(st))
}

chrome_edges <- function(st) {
  st$padding + border_width(st$border)
}

draw_background <- function(buffer, area, st) {
  if (!is.null(st$background)) buffer$fill(area, " ", fg = st$foreground, bg = st$background)
}

draw_border <- function(buffer, region, st, clip, title = NULL) {
  chars <- border_chars(st$border)
  if (length(chars) == 0L || region$width < 2L || region$height < 2L) return(invisible())
  fg <- st$border_color
  bg <- st$background
  attrs <- 0L
  inner_w <- region$width - 2L
  top <- paste0(chars[[1]], strrep(chars[[2]], inner_w), chars[[3]])
  bottom <- paste0(chars[[6]], strrep(chars[[7]], inner_w), chars[[8]])
  buffer$put_text(region$x, region$y, top, fg = fg, bg = bg, attrs = attrs, clip = clip)
  buffer$put_text(region$x, rect_bottom(region), bottom, fg = fg, bg = bg, attrs = attrs, clip = clip)
  if (region$height > 2L) {
    for (y in seq.int(region$y + 1L, rect_bottom(region) - 1L)) {
      buffer$put_text(region$x, y, chars[[4]], fg = fg, bg = bg, attrs = attrs, clip = clip)
      buffer$put_text(rect_right(region), y, chars[[5]], fg = fg, bg = bg, attrs = attrs, clip = clip)
    }
  }
  if (!is.null(title) && nzchar(title) && region$width > 4L) {
    text <- str_truncate(paste0(" ", title, " "), region$width - 2L)
    buffer$put_text(region$x + 1L, region$y, text, fg = fg, bg = bg, attrs = attr_bits[["bold"]], clip = clip)
  }
  invisible()
}

# Draw text lines (from text_lines()) inside `area`, aligned per `st`.
draw_lines <- function(buffer, area, lines, st, clip) {
  if (length(lines) == 0L || rect_is_empty(area)) return(invisible())
  clip <- rect_intersect(area, clip)
  if (rect_is_empty(clip)) return(invisible())
  n <- min(length(lines), area$height)
  free_rows <- area$height - n
  top <- area$y + switch(st$valign, middle = free_rows %/% 2L, bottom = free_rows, 0L)
  for (i in seq_len(n)) {
    line <- lines[[i]]
    free_cols <- area$width - line_width(line)
    x <- area$x + max(0L, switch(st$align, center = free_cols %/% 2L, right = free_cols, 0L))
    for (seg in line) {
      seg_st <- resolve_style(seg$style, parent = st)
      attrs <- bitwOr(st$attrs, seg_st$attrs)
      x <- buffer$put_text(
        x, top + i - 1L, seg$text,
        fg = seg_st$foreground, bg = seg_st$background, attrs = attrs, clip = clip
      )
    }
  }
  invisible()
}

# Frames of a running app -------------------------------------------------------
#
# A frame is the screens that are visible (the top non-modal screen and any
# modal screens above it, dimming what is below them) plus the toast layer.
# paint_layers() paints them restricted to `clip`, so the same code renders
# a full frame and a partial (dirty-region) update.

paint_layers <- function(frame, layers, toasts, theme, clip = frame$bounds()) {
  for (screen in layers) {
    if (inherits(screen, "ModalScreen") && screen$dim) dim_buffer(frame, theme, clip)
    paint_tree(screen, frame, clip)
  }
  # `toasts` is the toast layer, or a list of overlay layers.
  overlays <- if (is_widget(toasts)) list(toasts) else toasts
  for (layer in overlays) if (!is.null(layer)) paint_tree(layer, frame, clip)
  invisible(frame)
}
