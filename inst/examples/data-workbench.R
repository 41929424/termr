# Select a row in the table to inspect it alongside a compact profile.
rows <- mtcars
selected <- signal(as.list(rows[1L, , drop = FALSE]))
selected_row <- signal(1L)

table <- data_table(rows, cursor = "row", zebra = TRUE, id = "workbench-data")
record <- record_view(function() selected(), id = "workbench-record")
profile <- data_profile(rows$mpg, name = "mpg", id = "workbench-profile")
status <- status_bar(
  left = "mtcars",
  center = function() paste(nrow(rows), "rows"),
  right = function() paste("row", selected_row()),
  id = "workbench-status"
)

ui <- vertical(
  horizontal(
    table,
    vertical(
      panel(record, title = "Selected record", style = style(height = "1fr")),
      panel(profile, title = "Column profile", style = style(height = "1fr")),
      style = style(width = 34, height = "1fr")
    ),
    style = style(height = "1fr")
  ),
  status
)

run(app(
  ui,
  title = "Data workbench",
  on("datatable.row_selected", "#workbench-data", function(event, app) {
    selected(event$data$value)
    selected_row(event$data$row)
  })
))
