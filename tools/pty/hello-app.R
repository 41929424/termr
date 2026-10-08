# Used by pty-check.py: the installed hello demo, with a log of its actions.
#
# The demo itself is unchanged. Event markers let the harness wait for each
# input stage before sending the next key.
library(termr)
log_file <- Sys.getenv("TERMR_PTY_LOG")
log_line <- function(...) cat(paste0(..., "\n"), file = log_file, append = TRUE)
env <- new.env(parent = globalenv())
env$run <- function(a) {
  a$on("input.changed", "#name", function(event, app) {
    log_line("input value=", event$data$value)
  })
  a$query_one("#submit")$on("focus", function(event, app) {
    log_line("button focused")
  })
  a$on("button.pressed", "#submit", function(event, app) {
    log_line("pressed name=", app$query_one("#name")$value)
  })
  a$set_timeout(0, function(app) log_line("ready"))
  termr::run(a)
}
sys.source(system.file("examples", "hello.R", package = "termr"), env)
log_line("exited")
