# Data tools

## `data_table()`

```r
data_table(
  df,
  id = "table",
  cursor = "row",                          # "row", "cell" or "none"
  columns = list(
    name = column(width = 20),
    score = column(width = 10, align = "right", label = "Score")
  ),
  formatters = list(
    score = function(x) sprintf("%.2f", x),
    active = function(x) ifelse(x, "yes", "no")
  ),
  zebra = TRUE,
  row_style = function(row, data) if (data$score[row] < 0) style(foreground = "$error"),
  cell_style = function(value, row, column) NULL
)
```

* Works with data frames (including tibbles) and matrices.
* **Virtualised**: only visible rows are formatted and painted; widths are
  estimated from the first and last 500 rows. Measured on Windows: a frame
  of a 1,000,000-row table at 120x40 takes about 20 ms to paint, moving
  the cursor about 20 ms per tick (see `tools/bench/bench.R`).
* Keys: arrows, Page Up/Down, Home/End, Enter. Mouse: click, header click,
  wheel.
* `$sort(by, decreasing)`, `$filter(rows)`, `$move_cursor(row, column)`,
  `$selected_row()`, `$row_data(row)`, `$view`.
* Messages: `datatable.row_selected`, `datatable.cell_selected`,
  `datatable.row_activated` (`row`, `position`, `value`, `column`) and
  `datatable.header_selected` (`column`, `index`).

## `browse_data()`

```r
browse_data(mtcars)
viewer <- data_browser(big_df, title = "Sales")   # build without running
```

Metrics, search across all columns (`/`), sorting by the current column
(`s`), column summaries, row details (Enter) and reset (`r`).

## Dashboards

```r
horizontal(
  metric("Rows", nrow(df)),
  metric("Accuracy", 0.943, delta = 0.012),
  panel(sparkline(history, id = "loss"), title = "Loss")
)
```

Use `app$set_interval()` to refresh, `sparkline$push()` for streams,
`log_view()` for logs and `app$run_worker()` for long computations.

## DataTable 2.0

* **Columns.** `column(width, align, formatter, visible, sortable, editable)`;
  `set_column_width(col, n)`, `auto_size_column(col, rows = "sample")` (never
  scans a huge table unless `rows = "all"`), `auto_size_all()`,
  `set_column_visible(col, FALSE)`, `column_info()`. Drag the separator
  between header cells, or Alt+Left/Right, to resize the active column.
  `reorder_column(col, to)` changes display order; drag a header or press
  Ctrl+Alt+Left/Right to move the active column. Data-frame column order is
  unchanged. `column_info()` includes both source and display positions.
* **Frozen columns.** `data_table(df, frozen_columns = 1)`: the first columns
  and the header stay while the rest scrolls; a bar marks the boundary.
* **Sorting.** `sort(c("a", "b"), decreasing = c(FALSE, TRUE))` sorts by
  several keys (stable, `NA` last) and shows `^1` / `v2` markers; the sort
  survives `filter()`. `header_sort = TRUE` sorts on header click (again:
  descending, then off; Shift+click adds a key) and binds `S`. `clear_sort()`.
* **Search.** Ctrl+F opens a find bar over the formatted cells (all columns, or
  the active one with Tab; Ctrl+R regex, Alt+C case); `find(query, column,
  case_sensitive, regex)`, `find_next()`, `find_previous()`, `match_hit`. Large
  tables are scanned in chunks. `goto_row(n)` and Ctrl+G jump to a row.
* **Selection and copy.** Shift+Up/Down extends a row range; Shift+drag does
  the same with the mouse. `select_range(from, to)`, `selected_data()` and
  `visible_data()` expose selected and viewport rows. Ctrl+C copies selected
  rows as TSV with visible headers; `copy_selection(headers = FALSE)` omits
  them. System clipboard output follows the terminal's OSC 52 capability.
* **Filters.** `filter_columns(list(score = table_filter("range", min = 10,
  max = 20), name = table_filter("contains", "r")))` combines column filters
  with AND. Supported types are `equals`, `contains`, `regex`, `range`, and
  `missing`; callers can also pass `function(values) logical`. Predicates run
  in chunks of 5,000 rows. Existing `filter(rows)` subsets compose with the
  column filters.
* **Editing (experimental).** `data_table(df, editable = TRUE, on_edit =
  function(row, column, value) ...)`: Enter or double click opens a small
  dialog; the table edits *its own copy* of the data (`$data`), converts text to
  the column type, runs `on_edit` (return a message to reject) and sends
  `datatable.cell_changed` (`row`, `column`, `old`, `value`). `set_cell()` does
  the same from code.
* More messages: `datatable.sorted`, `datatable.column_resized`,
  `datatable.columns_changed`, `datatable.found`.
