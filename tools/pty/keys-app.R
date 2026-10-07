# Used by pty-check.py: logs every input event to the file named by
# TERMR_PTY_LOG, one line per event, so the harness can check exactly what
# the driver delivered (instead of scraping the screen).
library(termr)
log_file <- Sys.getenv("TERMR_PTY_LOG")
log_line <- function(...) cat(paste0(..., "\n"), file = log_file, append = TRUE)
ticks <- 0L
a <- app(
  vertical(label("keys app", id = "title"), input(id = "box")),
  mouse = TRUE,
  on("*", function(event, app) {
    if (inherits(event, "KeyEvent")) {
      log_line("key ", event$key, if (nzchar(event$char)) paste0(" [", event$char, "]") else "")
    } else if (inherits(event, "PasteEvent")) {
      log_line("paste ", gsub("\n", "<LF>", event$text, fixed = TRUE))
    } else if (inherits(event, "MouseEvent")) {
      log_line("mouse ", event$type, " ", event$button, " ", event$screen_x, ",", event$screen_y,
               if (!is.na(event$direction)) paste0(" ", event$direction) else "")
    } else if (inherits(event, "ResizeEvent")) {
      log_line("resize ", event$width, "x", event$height)
    }
  }),
  bind("ctrl+q", "quit", "Quit")
)
a$set_interval(0.5, function(app) {
  ticks <<- ticks + 1L
  if (ticks <= 3L) log_line("tick ", ticks)
})
a$run_worker(
  function() isatty(stdin()),
  on_complete = function(result, app) log_line("worker stdin is a TTY: ", result)
)
log_line("ready")
run(a)
log_line("exited")
