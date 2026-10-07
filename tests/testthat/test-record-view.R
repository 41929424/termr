test_that("record view normalizes list, vector and one-row data frame", {
  values <- list(id = 42L, name = "Alice", active = TRUE,
                 created = as.Date("2026-10-07"))
  expect_match(paste(render_widget(record_view(values), 60, 5)$to_text(), collapse = "\n"), "Alice")
  expect_match(paste(render_widget(record_view(c(id = 42, score = 3.5)), 40, 4)$to_text(), collapse = "\n"), "score")
  expect_match(paste(render_widget(record_view(data.frame(id = 1L, label = "one")), 40, 4)$to_text(), collapse = "\n"), "one")
  expect_error(record_view(data.frame(a = 1:12)), "expects one record; received 12 rows")
  expect_error(record_view(c("unnamed")), "named")
})

test_that("record view follows a DataTable row selection through signals", {
  rows <- data.frame(id = 1:3, name = c("Ada", "Lin", "Sam"))
  selected <- signal(as.list(rows[1L, , drop = FALSE]))
  table <- data_table(rows, cursor = "row", id = "records")
  view <- record_view(function() selected(), id = "record")
  ui <- vertical(table, view)
  root <- app(ui, on("datatable.row_selected", "#records", function(event, app) {
    selected(event$data$value)
  }))
  pilot <- test_app(root, 50, 12)
  table$move_cursor(row = 2L)
  pilot$step()
  expect_match(paste(pilot$screen_text(), collapse = "\n"), "Lin")
  expect_equal(selected()$id, 2L)
  pilot$stop()
})

test_that("record view accepts a reactive record", {
  current <- signal(list(name = "first"))
  view <- record_view(function() current())
  current(list(name = "second"))
  expect_match(paste(render_widget(view, 30, 3)$to_text(), collapse = "\n"), "second")
})
