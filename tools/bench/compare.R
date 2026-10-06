# Compare benchmark results of git revisions.
#
#   Rscript tools/bench/compare.R <ref> [<ref> ...] [-- <bench args>]
#   Rscript tools/bench/compare.R 87b846b HEAD -- datatable
#
# Each revision is checked out into a temporary git worktree, the *current*
# tools/bench/bench.R is copied into it and run there, and the median times
# (and repainted cells, where the revision reports them) are printed side by
# side. "HEAD" is the working tree's last commit. Scenarios that a revision
# cannot run show as NA. Wall times are noisy: look at the ratios, and at
# the repaint columns, which are exact.
args <- commandArgs(trailingOnly = TRUE)
split <- match("--", args)
refs <- if (is.na(split)) args else args[seq_len(split - 1L)]
bench_args <- if (is.na(split)) character() else args[-seq_len(split)]
if (!length(refs)) stop("Give at least one git revision.", call. = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")
bench_src <- normalizePath("tools/bench/bench.R")

run_ref <- function(ref) {
  dir <- tempfile("termr-bench-")
  status <- system2("git", c("worktree", "add", "--detach", "-f", shQuote(dir), ref), stdout = FALSE, stderr = FALSE)
  if (status != 0L) stop("Cannot check out ", ref, call. = FALSE)
  on.exit(system2("git", c("worktree", "remove", "--force", shQuote(dir)), stdout = FALSE, stderr = FALSE))
  dir.create(file.path(dir, "tools", "bench"), recursive = TRUE, showWarnings = FALSE)
  file.copy(bench_src, file.path(dir, "tools", "bench", "bench.R"), overwrite = TRUE)
  out <- tempfile(fileext = ".csv")
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  system2(rscript, c("tools/bench/bench.R", bench_args, "--csv", shQuote(out)), stdout = FALSE, stderr = FALSE)
  if (file.exists(out)) utils::read.csv(out, stringsAsFactors = FALSE) else NULL
}

results <- lapply(refs, run_ref)
names(results) <- refs
base <- results[[1]]
if (is.null(base)) stop("The first revision produced no results.", call. = FALSE)
table <- data.frame(scenario = base$scenario)
for (ref in refs) {
  r <- results[[ref]]
  if (is.null(r)) next
  idx <- match(table$scenario, r$scenario)
  table[[paste0(ref, "_ms")]] <- r$median_ms[idx]
  table[[paste0(ref, "_cells")]] <- r$repaint_cells[idx]
}
if (length(refs) > 1L) {
  first <- table[[paste0(refs[[1]], "_ms")]]
  last <- table[[paste0(refs[[length(refs)]], "_ms")]]
  table$ratio <- round(last / pmax(first, 0.5), 2)
}
old <- options(width = 220)
print(table, row.names = FALSE)
options(old)
