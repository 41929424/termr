# Developer helper: regenerate docs/NAMESPACE and run the test suite.
#   Rscript tools/dev.R            # document + test
#   Rscript tools/dev.R test       # tests only
#   Rscript tools/dev.R install    # document + install
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[[1]] else "all"
if (mode %in% c("all", "install", "doc")) {
  roxygen2::roxygenise(".")
}
if (mode %in% c("all", "test")) {
  res <- testthat::test_local(".", reporter = testthat::ProgressReporter$new(show_praise = FALSE), stop_on_failure = FALSE)
  df <- as.data.frame(res)
  cat(sprintf("\nTOTAL: %d tests, %d failed, %d errors, %d skipped, %d warnings\n",
              sum(df$nb), sum(df$failed), sum(df$error), sum(df$skipped), sum(df$warning)))
}
if (mode == "install") {
  install.packages(".", repos = NULL, type = "source", quiet = TRUE)
}
