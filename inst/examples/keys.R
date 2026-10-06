# Phase 3 demo: the event loop, keyboard input and terminal lifecycle.
#
# Shows every key you press and the terminal size. Press q to quit.
#
#   Rscript -e 'termr::run_example("keys")'

library(termr)

last_key <- label("Press any key...", id = "key", style = style(bold = TRUE))
info <- label("", id = "info", style = style(foreground = "bright_black"))
pressed <- 0L

keys_app <- app(
  vertical(
    label("termr key viewer", style = style(foreground = "bright_cyan", bold = TRUE)),
    label("Press keys to see their names. Press q to quit."),
    last_key,
    info,
    style = style(padding = c(1, 2))
  ),
  on("key", function(event, app) {
    pressed <<- pressed + 1L
    last_key$update(sprintf("key: %s   char: %s", event$key, encodeString(event$char, quote = "\"")))
    info$update(sprintf("%d key(s) pressed, terminal %dx%d", pressed, app$size[["width"]], app$size[["height"]]))
  }),
  on("resize", function(event, app) {
    info$update(sprintf("resized to %dx%d", event$width, event$height))
  }),
  bind("q", "quit")
)

run(keys_app)
cat(sprintf("Bye! You pressed %d key(s).\n", pressed))
