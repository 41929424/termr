# Run an ordered subset of tests preceding test-workers.R in the same
# testthat session, followed by test-workers.R and test-workers2.R.
#
# Examples:
#   Rscript tools/ci/worker-contamination.R --list
#   Rscript tools/ci/worker-contamination.R --indices=none
#   Rscript tools/ci/worker-contamination.R --indices=1-36,40,42
#   Rscript tools/ci/worker-contamination.R --indices=37-72 --filter
# `--filter` prints the exact testthat filter for a targeted R CMD check.

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (!length(script_arg)) stop("Run this script with Rscript.")
root <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg[[1L]])), "../.."))
test_dir <- file.path(root, "tests", "testthat")
files <- basename(testthat:::find_test_scripts(test_dir))
first_worker <- match("test-workers.R", files)
second_worker <- match("test-workers2.R", files)
stopifnot(!is.na(first_worker), !is.na(second_worker), second_worker == first_worker + 1L)
preceding <- files[seq_len(first_worker - 1L)]

if ("--list" %in% args) {
  cat(sprintf("%2d %s\n", seq_along(preceding), preceding), sep = "")
  quit(save = "no")
}

spec <- sub("^--indices=", "", grep("^--indices=", args, value = TRUE))
if (length(spec) != 1L) stop("Pass exactly one --indices=none, --indices=all or --indices=1-36,40,42.")
parse_indices <- function(spec, limit) {
  if (identical(spec, "none")) return(integer())
  if (identical(spec, "all")) return(seq_len(limit))
  parts <- strsplit(spec, ",", fixed = TRUE)[[1L]]
  if (!length(parts) || any(!grepl("^[0-9]+(-[0-9]+)?$", parts))) stop("Invalid --indices specification.")
  values <- unlist(lapply(parts, function(part) {
    ends <- as.integer(strsplit(part, "-", fixed = TRUE)[[1L]])
    if (length(ends) == 1L) ends else {
      if (ends[[1L]] > ends[[2L]]) stop("Ranges must ascend.")
      seq.int(ends[[1L]], ends[[2L]])
    }
  }), use.names = FALSE)
  if (anyNA(values) || any(values < 1L | values > limit)) stop("Index outside preceding test files.")
  sort(unique(values))
}
indices <- parse_indices(spec, length(preceding))
selected <- c(preceding[indices], files[c(first_worker, second_worker)])
contexts <- sub("[.][Rr]$", "", sub("^test[-_]", "", selected))
filter <- paste0("^(", paste(contexts, collapse = "|"), ")$")
if ("--filter" %in% args) {
  cat(filter, "\n", sep = "")
  quit(save = "no")
}

wanted <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[[1L]]
found <- as.character(utils::packageVersion("termr"))
if (!identical(found, wanted)) stop("Install this checkout of termr before the contamination probe.")
cat("WORKER_PRECEDING selected=", length(indices), "/", length(preceding),
    " indices=", spec, " installed=", find.package("termr"), "\n", sep = "")
cat("WORKER_PRECEDING files=", paste(selected, collapse = ","), "\n", sep = "")
Sys.setenv(TERMR_CI_RESOURCE_SNAPSHOT = "1")
Sys.setenv(NOT_CRAN = "true")
result <- testthat::test_dir(test_dir, filter = filter, package = "termr",
                             load_package = "installed", reporter = "summary",
                             stop_on_failure = FALSE, shuffle = FALSE)
counts <- as.data.frame(result)
failed <- sum(counts$failed) + sum(counts$error)
cat("WORKER_CONTAMINATION assertions=", sum(counts$nb), " failed=", failed, "\n", sep = "")
if (failed > 0L) quit(save = "no", status = 1L)
