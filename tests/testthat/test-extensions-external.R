source(test_path("..", "fixtures", "external_extensions.R"), local = TRUE)

test_that("external widget authors can use state, commands, bindings and events", {
  fixture <- external_app()
  pilot <- termr::test_app(fixture$app, 80, 12)
  on.exit(pilot$stop(), add = TRUE)

  pilot$press("up")
  expect_identical(fixture$counter$count, 1L)
  expect_identical(fixture$seen$count, 1L)
  expect_true(any(grepl("Count: 1", pilot$screen_text(), fixed = TRUE)))

  pilot$press("ctrl+p")
  expect_true(grepl("External action", paste(pilot$screen_text(), collapse = "\n"), fixed = TRUE))
  pilot$type("external action")
  pilot$press("enter")
  expect_identical(fixture$counter$count, 2L)
  expect_identical(fixture$seen$count, 2L)
})

test_that("external layouts and themes work through exported functions", {
  name <- external_stack_layout()
  on.exit(termr::unregister_layout(name), add = TRUE)
  root <- termr::vertical(
    termr::label("first"), termr::label("second"),
    style = termr::style(layout = name, foreground = "$extension_color")
  )
  themed <- termr::app(root, theme = external_theme())
  pilot <- termr::test_app(themed, 16, 3)
  on.exit(pilot$stop(), add = TRUE)
  expect_true(any(grepl("first", pilot$screen_text(), fixed = TRUE)))
  expect_true(any(grepl("second", pilot$screen_text(), fixed = TRUE)))
  expect_identical(external_theme()$colors$extension_color, "#22aa88")

  empty <- termr::vertical(style = termr::style(layout = name))
  expect_no_error(termr::render_widget(empty, 8, 2))
  one <- termr::vertical(termr::label("only"), style = termr::style(layout = name))
  expect_match(termr::render_widget(one, 8, 2)$to_text()[[1L]], "only")
})

test_that("external highlighters can be passed directly to text_area", {
  highlighter <- external_highlighter()
  editor <- termr::text_area("SELECT value", highlighter = highlighter)
  pilot <- termr::test_app(termr::app(editor), 24, 3)
  on.exit(pilot$stop(), add = TRUE)

  expect_gt(attr(highlighter, "calls")$n, 0L)
  expect_true(grepl("SELECT value", paste(pilot$screen_text(), collapse = "\n"), fixed = TRUE))
})

test_that("external table and database adapters use the public source protocol", {
  data <- data.frame(id = 1:3, value = c("a", "b", "c"))
  source <- external_memory_source(data)
  table <- termr::data_table(source)
  expect_identical(table$row_count, 3L)
  expect_identical(table$row_data(2L)$value, "b")
  expect_identical(source$row_key(3L), 3L)

  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbWriteTable(connection, "records", data)

  # db_metadata() is the public portable metadata interface for DBI drivers.
  metadata <- termr::db_metadata(connection)
  expect_true("records" %in% metadata$tables())
  expect_identical(metadata$columns("records"), c("id", "value"))

  # Driver-specific paging can be supplied as an ordinary external adapter.
  db_source <- external_db_source(connection, "records")
  expect_identical(db_source$row_count(), 3L)
  expect_identical(db_source$get_rows(2L, 1L)$value, "b")
})

test_that("external fixtures do not reach into termr internals", {
  fixture <- paste(readLines(test_path("..", "fixtures", "external_extensions.R")), collapse = "\n")
  expect_false(grepl("termr:::", fixture, fixed = TRUE))
})
