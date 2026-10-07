# Used by pty-check.py: logs every input event to the file named by
# TERMR_PTY_LOG, one line per event, so the harness can check exactly what
# the driver delivered (instead of scraping the screen).
library(termr)
log_file <- Sys.getenv("TERMR_PTY_LOG")
log_line <- function(...) cat(paste0(..., "\n"), file = log_file, append = TRUE)
ticks <- 0L
a <- app(
  label("keys app", id = "title"),
  mouse = TRUE,
  on("*", function(event, app) {
    if (inherits(event, "KeyEvent")) {
      log_line("key ", event$key, if (nzchar(event$char)) paste0(" [", event$char, "]") else "")
      if (event$key == "ctrl+q") app$exit()
      event$stop() # Keep default bindings (e.g. F1/help) out of this probe.
    } else if (inherits(event, "PasteEvent")) {
      log_line("paste ", gsub("\n", "<LF>", event$text, fixed = TRUE))
      event$stop()
    } else if (inherits(event, "MouseEvent")) {
      log_line("mouse ", event$type, " ", event$button, " ", event$screen_x, ",", event$screen_y,
               if (!is.na(event$direction)) paste0(" ", event$direction) else "")
    } else if (inherits(event, "ResizeEvent")) {
      log_line("resize ", event$width, "x", event$height)
    }
  })
)
a$set_interval(0.5, function(app) {
  ticks <<- ticks + 1L
  if (ticks <= 3L) log_line("tick ", ticks)
})
a$run_worker(
  function() isatty(stdin()),
  on_complete = function(result, app) log_line("worker stdin is a TTY: ", result)
)
a$set_timeout(0, function(app) log_line("ready"))
run(a)
log_line("exited")
