# A small SQL editor and results table backed by in-memory SQLite when the
# optional DBI and RSQLite packages are installed.
#
#   Rscript -e 'termr::run_example("sql-workspace")'

library(termr)

db <- NULL
initial <- data.frame()
message <- "Install DBI and RSQLite to enable SQL execution."
if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "mtcars", mtcars)
  db <- db_connection(con, name = "SQLite (in memory)", owned = TRUE)
  initial <- db$query("SELECT * FROM mtcars WHERE mpg > 20 LIMIT 5")
  message <- "Ready \u00b7 Ctrl+Enter runs the selected SQL or the full buffer."
}

results <- data_table(initial, id = "results", cursor = "none", zebra = TRUE)
status <- label(message, id = "sql-status")
editor <- sql_editor(
  "SELECT *\nFROM mtcars\nWHERE mpg > 20\nLIMIT 5",
  id = "query", connection = db, line_numbers = TRUE
)

run(app(
  vertical(
    label("SQL workspace", style = style(bold = TRUE, foreground = "$accent")),
    editor,
    results,
    status,
    style = style(padding = 1)
  ),
  on("sql.query_started", "#query", function(event, app) {
    app$query_one("#sql-status")$update("Running query\u2026")
  }),
  on("sql.query_completed", "#query", function(event, app) {
    app$query_one("#results")$set_data(event$data$result)
    app$query_one("#sql-status")$update(sprintf("%d rows \u00b7 %.1f ms", event$data$rows, event$data$elapsed_ms))
  }),
  on("sql.query_failed", "#query", function(event, app) {
    app$query_one("#sql-status")$update(paste("SQL error:", event$data$message))
  })
))
