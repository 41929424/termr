# A small database browser backed by an in-memory SQLite database.
#
#   Rscript -e 'termr::run_example("database-explorer")'

library(termr)

if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
  message("Install the optional DBI and RSQLite packages to run the database explorer example.")
} else {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "mtcars", mtcars)
  DBI::dbWriteTable(con, "iris", iris)
  DBI::dbExecute(con, "CREATE VIEW cars_over_20_mpg AS SELECT * FROM mtcars WHERE mpg > 20")
  run(app(db_explorer(db_connection(con, name = "SQLite demo", owned = TRUE))))
}
