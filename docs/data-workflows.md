# Data and developer workflow widgets

These compact widgets compose existing termr tables, labels, scroll views,
signals, sparklines and tree nodes. Constructors follow the usual `id`,
`classes`, and `style` convention. They use the active theme and do not emit
terminal escape sequences themselves.

## Status bar

`status_bar(left, center, right)` draws one line with left, centered and
right regions. Each region can be a string or a no-argument function that
reads signals. On narrow screens, the left region is preserved first, then
the right region; the center is truncated first. Truncation follows terminal
cell widths and grapheme boundaries.

```r
rows <- signal(127L)
status_bar(
  left = "SQLite :memory:",
  center = function() paste(rows(), "rows"),
  right = "8 ms"
)
```

The default uses the theme's `surface` background and `foreground` text.
Classes and styles can override those defaults; text remains readable without
colour or with the `high-contrast` theme.

## Property grid and record view

`property_grid()` displays a named list/vector or data-frame columns in a
read-only, scrollable two-column layout. Keys share a width capped by
`max_key_width`; values wrap. It handles `NULL`, `NA`, dates, times, vectors
and nested objects compactly. Nested lists and other objects are placeholders
unless a custom `format(name, value)` function is supplied. A function-valued
`data` argument can react to signals.

```r
property_grid(list(
  type = "numeric", missing = 12, unique = 345,
  min = 0, max = 100
))
```

`record_view()` is a semantic wrapper around the same property-grid
renderer. It accepts a named list/vector or exactly one data-frame row; a
multi-row frame raises an error. To follow a DataTable row selection:

```r
selected <- signal(NULL)
table <- data_table(mtcars, cursor = "row", id = "cars")
record <- record_view(function() selected())

ui <- vertical(table, record)
app(ui, on("datatable.row_selected", "#cars", function(event, app) {
  selected(event$data$value)
}))
```

The selection event's `value` is the selected row as a named list. The
DataTable does not depend on `record_view()`; this is ordinary event and
signal composition.

## Data profile

`data_profile()` summarizes atomic vectors, factors, `Date`, and `POSIXct`
values. It reports length, missing and unique counts. Numeric profiles add
mean, sample standard deviation, min, type-7 quartiles, median, max, and an
eight-bin histogram drawn with termr's existing sparkline. Logical profiles
show `TRUE`, `FALSE`, and `NA` counts. Character and factor profiles show up
to `top_n` frequent non-missing values and minimum/maximum terminal-cell
string widths. Date/time profiles show min, median and max in readable date
or timestamp form.

By default, in-memory values are fully scanned. `sample = n` instead examines
deterministic, evenly spaced positions and reports both total length and
analyzed count. Character top-value counting caps its frequency table at
100,000 non-missing values. A `table_source()` is rejected: the widget never
silently fetches a full lazy or remote result. Fetch a column or an explicit
sample before profiling it.

```r
data_profile(mtcars$mpg, name = "mpg")
data_profile(big_vector, sample = 100000)
```

The numeric histogram uses eight equal-width bins over finite values; a
constant vector places its observations in the middle bin. Numeric summaries
exclude `NA`, `NaN`, and infinite values from mean, quantiles and histogram;
the profile reports non-finite numeric values separately.

## JSON-shaped R objects

`json_view()` primarily accepts already-parsed R objects. Named lists and
data frames appear as objects; unnamed lists and vectors appear as arrays.
Object and array children are created only when a node expands. Each
expansion creates at most `page_size` children and a `Load next ...` node for
any remainder, so a wide list does not create thousands of TreeNodes at once.
Nested nodes use TreeView's existing expand/collapse and keyboard behavior.

```r
json_view(list(
  user = list(id = 42, name = "Alice"),
  items = list("a", "b", "c"),
  active = TRUE,
  missing = NA,
  absent = NULL
))
```

Characters are quoted, logical values use lowercase `true`/`false`, and R
`NULL` is shown as `null`. R `NA` is displayed as `NA`, keeping it distinct
from JSON `null`. `json_view(json_text, parse = TRUE)` optionally parses one
JSON string with `jsonlite`; `jsonlite` is an optional Suggests dependency and
is not needed to view R objects.

## Composition example

The bundled `run_example("data-workbench")` combines a DataTable,
signal-backed `record_view()`, `data_profile()`, and reactive `status_bar()`.
Select a row to update the record pane; the table and other widgets remain
independent components.
