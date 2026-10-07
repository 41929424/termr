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
the generic baseline. There is no metadata adapter registry yet; other driver
packages can use the `db_metadata()` callback shape as a baseline for their
own composed views.

## Browsing objects and previews

The Tables and Views nodes load their metadata only when expanded. Table
fields are requested when an object is selected and cached until F5 refreshes
the metadata tree. SQLite table and view previews use `db_table_source()`;
the DataTable fetches rows in bounded pages as the viewport moves. Opening a
table requests a row count for that selected object, but the explorer does
not count every table during startup or metadata listing. A schema-aware or
non-SQLite driver can supply richer metadata and a lazy source adapter without
changing DataTable. The explorer currently provides a lazy preview adapter
only for SQLite; another driver's metadata can still be read through DBI and
its SQL can run in the editor.

Type in the object filter above the tree to show case-insensitive substring
matches. Filtering loads and caches the table/view names on demand; clearing
the filter returns the tree to lazy group loading.

The Details tab shows the selected object's name, type, row count when the
SQLite source can provide it, and column names. The generic metadata path does
not make extra catalog queries for types, primary keys, or nullability.

## SQL queries and history

In the SQL tab, Ctrl+Enter runs the selected SQL, or the whole buffer when
there is no selection. Result-set queries use the lazy DBI query source and
the adjacent DataTable fetches bounded pages. SQLite uses a cached count and
bound `LIMIT`/`OFFSET`. Other drivers keep the materialized query behavior;
driver-specific lazy adapters can be configured on a standalone
`sql_editor()`. Successful and failed statements are retained in
`explorer$query_history()` for the current R session. The History tab shows
the most recent entries; it does not persist SQL to the database. The F5
binding refreshes object metadata, and Ctrl+Shift+O creates a quoted `SELECT
*` statement for the selected object. Ctrl+Shift+C copies the selected
object's raw name to the app clipboard, and Ctrl+Shift+I inserts its
driver-quoted name at the SQL cursor.

## Errors and limitations

Metadata and query errors are reported in the status line; query failures
also produce a notification and a `db_explorer.query_failed` event. If the
connection becomes invalid, the explorer reports a disconnected state and
disables SQL execution. Generic DBI execution is synchronous and cannot be
cancelled through a portable DBI API. Lazy query pagination requires a known
row count and does not add SQL sorting, filtering, or search. There is no SQL
autocomplete, parser, vendor-specific schema introspection, connection pool,
or generic table-preview adapter for other drivers yet.
