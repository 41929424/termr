# No testthat or package-development loader is attached here. The installed
# package and a plain Rscript process exercise the worker serialization path.
library(termr)
cat("standalone worker probe:", R.version.string, "termr", as.character(packageVersion("termr")),
    "at", find.package("termr"), "\n")

probe <- function(name, fn, args = list(), expected_state = "completed", expected = NULL) {
  pilot <- test_app(app(label("worker probe")), width = 20, height = 2)
  on.exit(pilot$stop(), add = TRUE)
  worker <- pilot$app$run_worker(fn, args = args, timeout = 25)
  pilot$wait_for_workers(30)
  diagnostic <- worker$.__enclos_env__$private$last_diagnostics
  cat(name, "state=", worker$state, " result=", if (is.null(worker$result)) "NULL" else
        paste(worker$result, collapse = ","), "\n", diagnostic, "\n", sep = "")
  stopifnot(identical(worker$state, expected_state))
  if (!is.null(expected)) stopifnot(identical(worker$result, expected))
  stopifnot(grepl("job_file_exists.*exists=TRUE size=[1-9][0-9]*", diagnostic),
            grepl("before_read_rds", diagnostic, fixed = TRUE),
            grepl("after_read_rds", diagnostic, fixed = TRUE))
  invisible(worker)
}

probe("literal", function() 42, expected = 42)
probe("identity", base::identity, args = list(17L), expected = 17L)
probe("error", function() stop("boom"), expected_state = "failed")
probe("quit", function() quit(save = "no", status = 0L, runLast = FALSE), expected_state = "failed")
local({
  scalar <- 11L
  probe("scalar closure", function() scalar + 1L, expected = 12L)
})
