# Phase 1 demo: the framebuffer + diff renderer without any widgets.
#
# Draws a framed box with a moving marker and a frame counter. Every frame
# is painted into a fresh ScreenBuffer, diffed against the previous frame,
# and only the changed cells are written to the terminal.
#
# Run from a terminal:  Rscript -e 'source(system.file("examples", "buffer-demo.R", package = "termr"))'

library(termr)

width <- 44L
height <- 12L

draw_frame <- function(i) {
  buf <- screen_buffer(width, height)
  box <- region(3, 2, 40, 9)
  buf$fill(box, bg = "blue")
  top <- paste0("\u256d", strrep("\u2500", box$width - 2L), "\u256e")
  bottom <- paste0("\u2570", strrep("\u2500", box$width - 2L), "\u256f")
  buf$put_text(box$x, box$y, top, fg = "bright_white")
  buf$put_text(box$x, box$y + box$height - 1L, bottom, fg = "bright_white")
  for (y in seq.int(box$y + 1L, box$y + box$height - 2L)) {
    buf$put_text(box$x, y, "\u2502", fg = "bright_white")
    buf$put_text(box$x + box$width - 1L, y, "\u2502", fg = "bright_white")
  }
  buf$put_text(6, 3, "termr framebuffer demo", fg = "bright_yellow", attrs = 1L)
  buf$put_text(6, 5, sprintf("frame %3d", i), fg = "bright_white")
  pos <- 6L + (i %% 30L)
  buf$put_text(pos, 7, "\u25cf", fg = "bright_green")
  buf
}

out <- function(x) {
  cat(x)
  flush(stdout())
}

out(paste0("\033[?1049h", "\033[?25l", "\033[?7l"))
on.exit(out(paste0("\033[0m", "\033[?7h", "\033[?25h", "\033[?1049l")), add = TRUE)

previous <- NULL
bytes <- numeric()
for (i in 0:60) {
  frame <- draw_frame(i)
  ansi <- patch_to_ansi(diff_screen(previous, frame))
  out(ansi)
  bytes <- c(bytes, nchar(ansi, type = "bytes"))
  previous <- frame
  Sys.sleep(0.05)
}
Sys.sleep(0.5)
on.exit()
out(paste0("\033[0m", "\033[?7h", "\033[?25h", "\033[?1049l"))
cat(sprintf("first frame: %d bytes\n", bytes[[1]]))
cat(sprintf("later frames: %.1f bytes on average (only changed cells)\n", mean(bytes[-1])))
