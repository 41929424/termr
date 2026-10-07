test_that("a table source can turn fetch failures into a visible table error", {
  source <- table_source(
    row_count = 2L,
    column_names = "value",
    get_rows = function(start, count, columns = NULL) stop("fixture fetch failed"),
    on_error = function(error, start, count, columns) {
      data.frame(value = rep(NA_character_, count), stringsAsFactors = FALSE)
    }
  )
  tbl <- data_table(source, cursor = "none")
  out <- render_widget(tbl, 60, 8)
  expect_match(paste(out$to_text(), collapse = "\n"), "Source error: fixture fetch failed")
})

test_that("SQLite query sources wrap one result query and page bounded rows", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "items", data.frame(id = 1:500, value = paste0("v", 1:500)))

  src <- db_query_source(con, "SELECT id, value FROM items WHERE id > ?;  ", params = list(0L))
  expect_true(inherits(src, "termr_table_source"))
  expect_identical(src$query, "SELECT id, value FROM items WHERE id > ?")
  expect_equal(src$row_count(), 500)
  expect_identical(src$column_names(), c("id", "value"))
  expect_equal(src$get_rows(250L, 3L, c("id", "value"))$id, 250:252)
  expect_false(src$capabilities()$sortable)
  expect_false(src$capabilities()$filterable)
  expect_false(src$capabilities()$searchable)

  tbl <- data_table(src, cursor = "none")
  expect_equal(tbl$row_count, 500L)
  expect_lte(tbl$source_stats()$rows_requested, 100L)
  invisible(render_widget(tbl, 40, 8))
  expect_lte(tbl$source_stats()$rows_requested, 100L)
  tbl$scroll_to_row(250L)
  invisible(render_widget(tbl, 40, 8))
  expect_lte(tbl$source_stats()$rows_requested, 200L)
  tbl$close()
  expect_true(DBI::dbIsValid(con))
})

test_that("DBI query source accepts a generic backend pager", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  calls <- new.env(parent = emptyenv())
  calls$rows <- integer()
  adapter <- list(
    column_names = function(connection, query, params) c("id", "value"),
    row_count = function(connection, query, params) 1000000,
    get_rows = function(connection, query, params, start, count, columns) {
      calls$rows <- c(calls$rows, count)
      out <- data.frame(id = seq.int(start, length.out = count),
                        value = seq.int(start, length.out = count) * 2)
      out[columns]
    }
  )
  src <- db_query_source(con, "backend-specific query", adapter = adapter)
  tbl <- data_table(src, cursor = "none")
  expect_equal(tbl$row_count, 1000000L)
  invisible(render_widget(tbl, 40, 8))
  expect_true(sum(tbl$source_stats()$rows_requested) <= 100L)
  tbl$scroll_to_row(500000L)
  invisible(render_widget(tbl, 40, 8))
  expect_true(all(calls$rows <= 100L))
  expect_lt(sum(calls$rows), 300L)
})

test_that("SQLite lazy query source paginates a million-row result", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  query <- paste(
    "WITH RECURSIVE seq(x) AS (",
    "SELECT 1 UNION ALL SELECT x + 1 FROM seq WHERE x < 1000000",
    ") SELECT x AS id, x * 2 AS value FROM seq;"
  )
  src <- db_query_source(con, query)
  tbl <- data_table(src, cursor = "none")
  expect_equal(tbl$row_count, 1000000L)
  invisible(render_widget(tbl, 40, 8))
  expect_lte(tbl$source_stats()$rows_requested, 100L)
  tbl$scroll_to_row(500000L)
  invisible(render_widget(tbl, 40, 8))
  expect_lte(tbl$source_stats()$rows_requested, 200L)
  expect_equal(tbl$row_data(500000L)$id, 500000L)
})

test_that("SQL editor lazy mode is opt-in and preserves materialized events", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "items", data.frame(id = 1:4))
  db <- db_connection(con)

  regular <- sql_editor("SELECT * FROM items", connection = db)
  data_result <- regular$execute()
  expect_true(data_result$ok)
  expect_identical(data_result$result_type, "data")
  expect_s3_class(data_result$result, "data.frame")
  expect_null(data_result$source)

  lazy <- sql_editor("SELECT * FROM items;", connection = db, result_mode = "lazy")
  source_result <- lazy$execute()
  expect_true(source_result$ok)
  expect_identical(source_result$result_type, "source")
  expect_null(source_result$result)
  expect_equal(source_result$rows, 4)
  expect_true(inherits(source_result$source, "termr_table_source"))
  expect_error(sql_editor(connection = db, execution = "execute", result_mode = "lazy"),
               "requires execution")
  expect_true(db$is_valid())
})

test_that("database explorer sends lazy SQL results directly to DataTable", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "items", data.frame(id = 1:250))
  explorer <- db_explorer(db_connection(con))
  expect_identical(explorer$sql$result_mode, "lazy")
  explorer$sql$set_text("SELECT id FROM items;")
  out <- explorer$run_query()
  expect_true(out$ok)
  expect_true(inherits(explorer$query_results$data, "termr_table_source"))
  expect_equal(explorer$query_results$row_count, 250L)
  expect_identical(explorer$query_history()[[1L]]$result_type, "source")
  expect_lte(explorer$query_results$source_stats()$rows_requested, 100L)
})

test_that("lazy query fetch failures are visible and do not abort table paint", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "items", data.frame(id = 1:250))
  src <- db_query_source(con, "SELECT id FROM items")
  tbl <- data_table(src, cursor = "none")
  DBI::dbDisconnect(con)
  tbl$scroll_to_row(200L)
  expect_no_error(render_widget(tbl, 30, 8))
  expect_match(src$last_error(), "no longer valid")
  expect_match(render_widget(tbl, 30, 8)$to_text(), "Source error")
})

test_that("database explorer surfaces lazy query fetch errors without crashing", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "items", data.frame(id = 1:250))
  explorer <- db_explorer(db_connection(con))
  explorer$sql$set_text("SELECT id FROM items")
  expect_true(explorer$run_query()$ok)
  pilot <- test_app(app(explorer), width = 70, height = 20)
  DBI::dbDisconnect(con)
  explorer$query_results$scroll_to_row(200L)
  expect_no_error(pilot$step())
  expect_match(explorer$status$text, "fetch failed")
  expect_match(render_widget(explorer$query_results, 40, 8)$to_text(), "Source error")
  pilot$stop()
})

