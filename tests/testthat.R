library(testthat)
library(termr)

# TERMR_TEST_FILTER restricts the run to matching test files (used by the
# dedicated CI worker probe, which runs the bootstrap tests inside R CMD check).
filter <- Sys.getenv("TERMR_TEST_FILTER", unset = "")
test_check("termr", filter = if (nzchar(filter)) filter)
