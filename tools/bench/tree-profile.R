# Measure construction and traversal of wide widget trees.
# Rscript tools/bench/tree-profile.R output.csv [100,1000,5000,10000]
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1]] else tempfile(fileext = ".csv")
sizes <- if (length(args) > 1L) as.integer(strsplit(args[[2]], ",", fixed = TRUE)[[1]]) else c(100L, 1000L, 5000L, 10000L)
result <- lapply(sizes, function(n) {
  cat("Widget tree", n, "children\n")
  t <- system.time({
    children <- lapply(seq_len(n), function(i) termr::label(sprintf("item %d", i), id = sprintf("item-%d", i)))
    root <- termr::vertical(children)
  })[["elapsed"]] * 1000
  start <- proc.time()[["elapsed"]]
  count <- length(root$walk())
  walk_ms <- (proc.time()[["elapsed"]] - start) * 1000
  data.frame(children = n, mount_ms = t, walk_ms = walk_ms, widgets = count)
})
result <- do.call(rbind, result)
utils::write.csv(result, output, row.names = FALSE)
print(result, row.names = FALSE)
