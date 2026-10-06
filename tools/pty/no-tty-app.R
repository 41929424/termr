library(termr)

tryCatch(
  run(app(label("this should never render"))),
  error = function(e) {
    cat("TERM_ERROR:", conditionMessage(e), "\n")
    quit(save = "no", status = 42L, runLast = FALSE)
  }
)
