# Worker bootstrap probes against the INSTALLED package, plain Rscript (no
# R CMD check, no R_TESTS). Exit status 1 on any failure.
options(width = 200)
res <- testthat::test_dir("tests/testthat", filter = "worker-bootstrap", package = "termr",
                          load_package = "installed", reporter = "location", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat(sprintf("PLAIN PROBE: %d tests, %d failed, %d errors\n", sum(df$nb), sum(df$failed), sum(df$error)))
if (sum(df$failed) + sum(df$error) > 0) quit(status = 1)
