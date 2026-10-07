# Database explorer

`db_explorer()` composes a lazy database object tree, a table preview, the
existing SQL editor and DataTable, table details, and an in-memory query
history. DBI and RSQLite are optional; they are not runtime dependencies.

## Getting started

```r
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con, "mtcars", mtcars)

app(db_explorer(db_connection(con, name = "SQLite demo", owned = TRUE))) |>
  run()
```

For an interactive demo, run `run_example("database-explorer")`. It uses an
in-memory SQLite database and includes tables and a view. The example prints
a friendly dependency message if DBI or RSQLite is unavailable.

## Connections

Pass either a `db_connection()` wrapper or a raw DBI connection. A raw
connection is wrapped as externally owned. Set `owned = TRUE` on
`db_connection()` when the explorer should disconnect it during app shutdown;
external connections remain open. `db_metadata()` exposes the portable
metadata functions used by the explorer: schemas, tables, views, fields, and
identifier quoting. The baseline uses `DBI::dbListTables()` and
`DBI::dbListFields()`. SQLite also reports views from `sqlite_master`. Other
drivers currently report an empty view list and do not expose schemas through
the generic baseline.

## Browsing objects and previews

The Tables and Views nodes load their metadata only when expanded. Table
fields are requested when an object is selected and cached until F5 refreshes
the metadata tree. SQLite table and view previews use `db_table_source()`;
the DataTable fetches rows in bounded pages as the viewport moves. Opening a
table requests a row count for that selected object, but the explorer does
not count every table during startup or metadata listing. A schema-aware or
non-SQLite driver can supply richer metadata and a lazy source adapter without
changing DataTable.

The Details tab shows the selected object's name, type, row count when the
SQLite source can provide it, and column names. The generic metadata path does
not make extra catalog queries for types, primary keys, or nullability.

## SQL queries and history

In the SQL tab, Ctrl+Enter runs the selected SQL, or the whole buffer when
there is no selection. Query results are materialized by DBI and displayed in
the adjacent DataTable. Successful and failed statements are retained in
`explorer$query_history()` for the current R session. The History tab shows
the most recent entries; it does not persist SQL to the database. The F5
binding refreshes object metadata, and Ctrl+Shift+O creates a quoted `SELECT
*` statement for the selected object.

## Errors and limitations

Metadata and query errors are reported in the status line; query failures
also produce a notification and a `db_explorer.query_failed` event. If the
connection becomes invalid, the explorer reports a disconnected state and
disables SQL execution. Generic DBI execution is synchronous and cannot be
cancelled through a portable DBI API. Query results are materialized; table
previews are lazy for SQLite. There is no SQL autocomplete, parser,
vendor-specific schema introspection, connection pool, or generic preview
adapter for other drivers yet.
