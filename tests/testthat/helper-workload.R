# Only the measured expensive property/stress loops use this budget. The
# full CI branch returns the original counts/seeds without changing the RNG.
stress_workload <- function(full, cran) {
  if (isTRUE(as.logical(Sys.getenv("NOT_CRAN", unset = "false")))) full else cran
}
