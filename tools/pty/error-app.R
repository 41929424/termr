# Used by pty-check.py: an app whose key handler fails.
library(termr)
run(app(
  label("press x"),
  on("key", function(event, app) if (event$key == "x") stop("boom from handler"))
))
