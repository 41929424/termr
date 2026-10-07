# Run from the package root with DBI and RSQLite installed.
# Compare a fully materialized SQL result with bounded lazy page access.

if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
  stop("Install DBI and RSQLite to run this benchmark.", call. = FALSE)
}

library(termr)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
query <- paste(
  "WITH RECURSIVE seq(x) AS (",
  "SELECT 1 UNION ALL SELECT x + 1 FROM seq WHERE x < 1000000",
  ") SELECT x AS id, x * 2 AS value FROM seq"
)

materialized_time <- system.time(materialized <- DBI::dbGetQuery(con, query))[["elapsed"]]
rm(materialized)
gc()

source_time <- system.time(source <- db_query_source(con, query))[["elapsed"]]
table <- data_table(source, cursor = "none")
initial_time <- system.time(invisible(render_widget(table, 100, 30)))[["elapsed"]]
initial_stats <- table$source_stats()
middle_time <- system.time({
  table$scroll_to_row(500000L)
  invisible(render_widget(table, 100, 30))
})[["elapsed"]]
stats <- table$source_stats()

cat("SQLite query source profile (one million logical rows)\n")
cat(sprintf("Materialized query: %.3f s\n", materialized_time))
cat(sprintf("Lazy source metadata/count: %.3f s\n", source_time))
cat(sprintf("Initial render: %.3f s; %d rows requested\n",
            initial_time, initial_stats$rows_requested))
cat(sprintf("Jump to row 500000: %.3f s; %d total rows requested in %d fetches\n",
            middle_time, stats$rows_requested, stats$fetch_calls))
cat(sprintf("Bounded-fetch ratio: %.5f%% of logical rows\n",
            100 * stats$rows_requested / 1000000))
table$close()
DBI::dbDisconnect(con)
