# Used by the Windows e2e gate: a worker that fails must not break the terminal.
library(termr)
status <- label("starting", id = "status")
a <- app(status, bind("q", "quit", "Quit"))
a$call_later(function(app) {
  app$run_worker(function() stop("worker blew up"),
                 on_error = function(message, app) status$update(paste("worker failed:", message)))
})
run(a)
