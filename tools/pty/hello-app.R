# Used by pty-check.py: the installed hello demo, with a log of its actions.
#
# The demo itself is unchanged. This only adds a handler that records what the
# button received, so the harness checks that typed text, Tab and Enter
# reached the widgets (the terminal transport) instead of scraping a
# diff-rendered screen for the label text.
library(termr)
log_file <- Sys.getenv("TERMR_PTY_LOG")
log_line <- function(...) cat(paste0(..., "\n"), file = log_file, append = TRUE)
env <- new.env(parent = globalenv())
env$run <- function(a) {
  a$on("button.pressed", "#submit", function(event, app) {
    log_line("pressed name=", app$query_one("#name")$value)
  })
  a$set_timeout(0, function(app) log_line("ready"))
  termr::run(a)
}
sys.source(system.file("examples", "hello.R", package = "termr"), env)
log_line("exited")
