# Lazy table sources

`data_table()` accepts a data frame or matrix as before. For data that is too
large to keep in an R data frame, it also accepts an experimental
`table_source()`. The source protocol lets the table render and jump directly
to a viewport without building the full result in memory. It is synchronous,
and the row count must be known.

The source API remains experimental in the 1.0 release candidate and under
the [API stability policy](stability.md). The source protocol itself adds no
runtime dependency; its database adapters use optional DBI/RSQLite packages.

## Custom source

The required callbacks are a row count, column names, and a row-range fetcher:

```r
src <- table_source(
  row_count = function() 10000000,
  column_names = function() c("row", "square"),
  get_rows = function(start, count, columns = NULL) {
    i <- seq.int(start, length.out = count)
    out <- data.frame(row = i, square = as.double(i) * i)
    if (!is.null(columns)) out <- out[columns]
    out
  }
)

tbl <- data_table(src)
```

`get_rows(start, count, columns)` returns a data frame with exactly the
requested column names and the number of rows available in that range. The
table validates every response and reports the requested range when a source
errors or returns malformed data. It requests 100-row chunks for the visible
columns and keeps those chunks in a small per-table cache. Repainting or
scrolling within a cached chunk reuses the values. A jump to a distant row
requests that range directly.

## Capabilities and delegated operations

Sorting, filtering, and searching never scan or materialize a lazy source by
default. Optional callbacks let a backend implement operations in its own
storage engine:

```r
src <- table_source(
  row_count = function() length(state$view),
  column_names = function() c("id", "name"),
  get_rows = fetch_current_view,
  sort = function(spec) { ...; TRUE },
  filter = function(filters) { ...; TRUE },
  search = function(query, columns, start, direction, include_current) { ... },
  row_key = function(rows) ..., 
  set_value = function(row, column, value) { ...; TRUE }
)
src$capabilities()
```

The `sort` callback receives `NULL` to clear sorting or a list with column
names and a `decreasing` vector. The `filter` callback receives the named
`filter_columns()` specifications (or `NULL` to clear them). These callbacks
change the source's current view; `row_count()` and `get_rows()` then describe
that view. A callback can return `FALSE` to reject an operation. Predicate
functions are passed through to the backend and are not automatically
translated into SQL or another query language.

The search callback receives termr's parsed query, selected column names, a
starting view position, direction (`1` or `-1`) and `include_current`. It
returns `NULL` or a list containing a valid `position` and `column`. Without
the callback, find reports that search is unsupported. `src$capabilities()`
reports `sortable`, `filterable`, `searchable`, `editable`, and
`row_count_known`.

The `row_key` callback is optional. Without it, selection reports a
position-based row number in the current view. With it, selection events and
`selected_row()` report the backend key. The cursor itself remains at the same
view position when a delegated sort or filter changes the view, so its key is
looked up again for the row now at that position. `row_data()` and
`set_value()` take a current source position. In a source-backed table,
`row_style(row, data)` receives that position and a one-row data frame; `data`
remains the full frame for ordinary data-frame tables. Editing is disabled
unless the source provides `set_value()`; a successful update clears cached
rows.

## Refresh and resource ownership

`tbl$refresh()` invokes the optional source `refresh()` callback, clears the
row cache, re-reads the row/column metadata and resets the view, sort, and
filters. `tbl$source_stats()` reports fetch calls, requested rows, cache hits
and misses, rendered rows, and rendered cells for profiling. A source can supply `close()`
for explicit cleanup; call `tbl$close()` when the caller is finished with the
source. termr never closes a connection or other resource automatically.

## Data frames and DBI

Existing `data_table(data.frame)` behavior remains in-memory: local sorting,
filters, editing, row names, and selection semantics are unchanged. Lazy
sources use positional view rows, and actions are enabled only when their
callbacks are present.

DBI and RSQLite remain optional dependencies. This release does not generate
generic paginated SQL: safe pagination, ordering and filter syntax vary by
driver and query shape. The optional `db_table_source()` adapter targets
RSQLite tables and uses identifier quoting plus bound values for its supported
filters and `LIMIT`/`OFFSET` slices:

```r
src <- db_table_source(connection, "sales")
app(data_table(src)) |> run()
```

It does not accept arbitrary SQL. Other DBI drivers and query sources can
implement `table_source()` with driver-specific, parameterized queries. Fetch
callbacks run synchronously on the UI thread, so slow remote queries can pause
rendering.

For a read-only query result, `db_query_source()` adds a generic DBI adapter
protocol and a built-in RSQLite pager:

```r
src <- db_query_source(con, "SELECT id, total FROM sales WHERE total > ?",
                       params = list(100))
tbl <- data_table(src)
```

The SQLite pager obtains column names without fetching rows, caches one count,
and uses bound `LIMIT`/`OFFSET` parameters for each 100-row DataTable chunk.
Other drivers can provide `column_names`, `row_count`, and `get_rows` callbacks
through `adapter`; pagination and identifier quoting remain the adapter's
responsibility. Closing a query source never disconnects its supplied
connection. See [SQL workflows](sql.md#lazy-dbi-query-sources) for the limits
on query shape and supported operations.

## Limits

The row count must be available and fit R's integer row indexing. Sources must
return a data frame. The cache is local to one DataTable and invalidates on
sort, filter, edit, and refresh. Lazy sort, filter, and search require source
callbacks; termr does not silently fall back to a full scan. DataTable still
uses the same formatting and viewport rendering path for the rows it fetches.
