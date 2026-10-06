# Developer validation runner; logs and summaries go to the chosen directory.
# Rscript tools/validate-dev.R [filter] [output-directory]
args <- commandArgs(trailingOnly = TRUE)
filter <- if (length(args) && nzchar(args[[1]])) args[[1]] else NULL
out <- if (length(args) >= 2L) args[[2]] else file.path(tempdir(), "termr-validation")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(NOT_CRAN = "true")
result <- testthat::test_local(".", filter = filter, reporter = "summary", stop_on_failure = FALSE)
summary <- as.data.frame(result)
utils::write.csv(summary[setdiff(names(summary), "result")], file.path(out, "tests.csv"), row.names = FALSE)
cat(sprintf("EXPECTATIONS %d; FAILURES %d; ERRORS %d; SKIPS %d; WARNINGS %d\n",
            sum(summary$nb), sum(summary$failed), sum(summary$error), sum(summary$skipped), sum(summary$warning)))
if (any(summary$failed > 0 | summary$error)) quit(status = 1L)
