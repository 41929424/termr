# Run from the package root with DBI and RSQLite installed.
# Measures lazy object listing and explicit table selection/preview work.

if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
  stop("Install DBI and RSQLite to run this benchmark.", call. = FALSE)
}

# Benchmarks measure this checkout, never an installed (possibly older) termr.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

make_catalog <- function(n) {
  for (i in seq_len(n)) {
    DBI::dbExecute(con, sprintf("CREATE TABLE object_%04d (id INTEGER)", i))
  }
  metadata <- db_metadata(con)
  elapsed <- system.time(objects <- metadata$tables())[["elapsed"]]
  data.frame(objects = n, listed = length(objects), list_ms = elapsed * 1000)
}

catalog <- do.call(rbind, lapply(c(100L, 1000L), function(n) {
  # Use a fresh connection for each catalog size because SQLite does not
  # support wildcards in DROP TABLE.
  DBI::dbDisconnect(con)
  con <<- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  make_catalog(n)
}))

DBI::dbExecute(con, "CREATE TABLE preview (id INTEGER, value TEXT)")
DBI::dbExecute(con, paste(
  "WITH RECURSIVE n(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM n WHERE x < 1000000)",
  "INSERT INTO preview SELECT x, printf('row-%d', x) FROM n"
))
source <- db_table_source(con, "preview")
table <- data_table(source)
started <- proc.time()[["elapsed"]]
invisible(render_widget(table, 100, 30))
initial_ms <- (proc.time()[["elapsed"]] - started) * 1000
invisible(table$row_data(500000L))
stats <- table$source_stats()

cat("SQLite database explorer profile\n")
print(catalog, row.names = FALSE)
cat(sprintf("1M-row preview: %.1f ms render; %d fetch calls, %d rows requested, %d cache hits\n",
            initial_ms, stats$fetch_calls, stats$rows_requested, stats$cache_hits))
cat("The 0.6 source benchmark (tools/bench/datatable-source.R) covers logical 10M and 100M row scans.\n")
