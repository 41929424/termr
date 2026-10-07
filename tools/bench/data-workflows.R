# Run from the package root after installing this version of termr.
# Reports construction/render costs for the data workflow widgets.

library(termr)

elapsed <- function(expr) unname(system.time(force(expr))[["elapsed"]])

property_time <- elapsed({
  grid <- property_grid(as.list(stats::setNames(seq_len(1000L), paste0("property_", seq_len(1000L)))))
  invisible(render_widget(grid, 100, 24))
})
record_time <- elapsed({
  record <- record_view(stats::setNames(as.list(seq_len(100L)), paste0("field_", seq_len(100L))))
  invisible(render_widget(record, 100, 24))
})
profile_100k <- elapsed(invisible(data_profile(seq_len(100000L), name = "100k")))
profile_1m <- elapsed(invisible(data_profile(seq_len(1000000L), name = "1m")))

deep <- list(value = TRUE)
for (i in seq_len(100L)) deep <- list(child = deep)
deep_time <- elapsed({
  tree <- json_view(deep)
  node <- tree$root
  for (i in seq_len(100L)) {
    node$expand()
    node <- node$children[[1L]]
  }
})
wide_time <- elapsed({
  tree <- json_view(as.list(seq_len(10000L)), page_size = 100L)
  tree$root$expand()
})

updates <- signal(0L)
bar <- status_bar("benchmark", right = function() updates())
pilot <- test_app(app(bar), 80, 1)
status_time <- elapsed(for (i in seq_len(1000L)) {
  updates(i)
  pilot$step()
})
pilot$stop()

cat("Data workflow widget benchmark (seconds)\n")
print(c(property_grid_1k = property_time, record_view_100 = record_time,
        data_profile_100k = profile_100k, data_profile_1m = profile_1m,
        json_view_deep_100 = deep_time, json_view_wide_10k_first_page = wide_time,
        status_bar_1000_reactive_updates = status_time))
