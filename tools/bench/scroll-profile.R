# Compare whole event-loop scroll costs, with profiling separate from timing.
# Rscript tools/bench/scroll-profile.R output.csv [100,1000,2000,10000]
pkgload::load_all(".", quiet = TRUE)
ns <- asNamespace("termr")
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1]] else tempfile(fileext = ".csv")
sizes <- if (length(args) > 1L) as.integer(strsplit(args[[2]], ",", fixed = TRUE)[[1]]) else c(100L, 1000L, 2000L, 10000L)
result <- list()
for (n in sizes) {
  cat("ScrollView", n, "children\n")
  items <- lapply(seq_len(n), function(i) label(paste("row", i)))
  ui <- vertical(items)
  view <- scroll_view(ui)
  a <- app(view)
  driver <- ns$HeadlessDriver$new(100L, 30L)
  driver$terminal <- list2env(list(feed = function(text) NULL, width = 100L, height = 30L,
                                 resize = function(w, h) NULL))
  p <- a$.__enclos_env__$private
  initial <- system.time(p$start(driver))[["elapsed"]] * 1000
  step <- function() { view$scroll_by(dy = 1L); p$tick(wait = FALSE) }
  step()
  elapsed <- replicate(3, system.time(for (i in 1:5) step())[["elapsed"]] * 200)
  profile <- paste0(output, "-", n, ".Rprof")
  Rprof(profile, interval = 0.01)
  for (i in 1:5) step()
  Rprof(NULL)
  hot <- summaryRprof(profile)
  write.table(head(hot$by.total, 25), paste0(profile, ".txt"), quote = FALSE)
  old <- options(termr.profile = TRUE)
  step()
  metrics <- a$profile_last_frame
  options(old)
  p$shutdown()
  result[[length(result) + 1L]] <- data.frame(children = n, initial_ms = initial,
                                            scroll_ms = median(elapsed),
                                            measured = if (is.null(metrics)) NA else metrics$widgets_measured,
                                            laid_out = if (is.null(metrics)) NA else metrics$widgets_laid_out,
                                            painted = if (is.null(metrics)) NA else metrics$widgets_painted)
  utils::write.csv(do.call(rbind, result), output, row.names = FALSE)
}
print(do.call(rbind, result), row.names = FALSE)
