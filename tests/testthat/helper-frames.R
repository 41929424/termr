# Lay out and paint the app from scratch (what a full repaint produces).
full_frame <- function(a) {
  p <- a$.__enclos_env__$private
  size <- a$size
  bounds <- rect(1L, 1L, size[["width"]], size[["height"]])
  layers <- p$.screens$visible()
  for (s in layers) layout_tree(s, bounds)
  layout_tree(p$.toasts, bounds)
  frame <- ScreenBuffer$new(size[["width"]], size[["height"]])
  paint_layers(frame, layers, p$.toasts, p$.theme)
  frame
}

# One character per cell showing its text attributes (R reverse, B bold,
# U underline, D dim, . none), for snapshot tests of styling without colour.
attr_map <- function(pilot) {
  a <- pilot$driver$terminal$screen$attrs
  flags <- c(R = attrs_encode(reverse = TRUE), B = attrs_encode(bold = TRUE),
             U = attrs_encode(underline = TRUE), D = attrs_encode(dim = TRUE))
  apply(a, 1L, function(row) paste(vapply(row, function(v) {
    hit <- names(flags)[bitwAnd(v, flags) > 0L]
    if (length(hit)) hit[[1]] else "."
  }, ""), collapse = ""))
}
