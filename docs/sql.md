# SQL workflows

termr includes a small SQL editing workflow built on its existing text editor
and data table. It provides token highlighting and explicit database calls;
it does not parse SQL into statements or inspect database schemas.

## SQL highlighting

Pass `sql_highlighter()` to `text_area()`, or use `sql_editor()` which enables
it by default:

```r
text_area(
  "SELECT id, name\nFROM users\nWHERE active = TRUE",
  highlighter = sql_highlighter()
)
```

The tolerant tokenizer highlights common keywords, identifiers, strings,
numbers, operators, punctuation, comments, parameters, and quoted identifiers.
It supports `--` line comments and `/* ... */` block comments across lines.
It accepts incomplete strings and comments while editing. `dialect` currently
accepts `generic`, `postgres`, `sqlite`, `mysql`, or `sqlserver`; these values
share the same common token rules and leave room for later extensions.

## SQL editor and execution

`sql_editor(value, id, connection, execute_key)` is a `TextArea` with SQL
highlighting, line numbers, soft tabs, search, selection, and undo/redo. The
connection is optional. With a connection, the default `Ctrl+Enter` binding
executes selected text when the selection is non-empty, otherwise it executes
the full buffer. It never executes automatically while text changes.

`execution = "query"` (the default) calls `DBI::dbGetQuery()` and emits a
data-frame result. Use `execution = "execute"` for statements that return an
affected-row count through `DBI::dbExecute()`:

```r
db <- db_connection(con, name = "local", owned = FALSE)
editor <- sql_editor("SELECT * FROM mtcars LIMIT 5", connection = db)
```

Query mode preserves that behavior by default. Opt into bounded query-result
pages with `result_mode = "lazy"`:

```r
editor <- sql_editor(
  "SELECT * FROM mtcars;",
  connection = db,
  result_mode = "lazy"
)
answer <- editor$execute()
answer$result_type # "source"
answer$source       # a table_source, ready for data_table()
```

Lazy mode is available for result-set queries only. The built-in pager
supports SQLite; other DBI drivers must supply `query_adapter` callbacks for
column names, row count, and page reads. Query completion keeps the existing
`result` field for materialized results and adds `result_type` plus `source`
for lazy results. Existing handlers that read `event$data$result` continue
to receive a data frame in the default mode.

Calling `editor$execute()` runs the same operation synchronously in the current
R process. The editor emits `sql.query_started`, followed by either
`sql.query_completed` or `sql.query_failed`. Completion data includes `sql`,
`elapsed` (seconds), `elapsed_ms`, `rows`, and `result_type`; it also
includes either `result` (materialized mode) or `source` (lazy mode). Failure
data includes the original `error` condition and its message. Events bubble
through the usual termr event system, so result rendering can stay outside
the editor:

```r
on("sql.query_completed", "#query", function(event, app) {
  app$query_one("#results")$set_data(event$data$result)
})
```

## Lazy DBI query sources

`db_query_source(connection, query, params = NULL, adapter = NULL)` returns
the existing read-only `table_source()` protocol. DataTable reads at most one
100-row chunk per page request and keeps its normal bounded cache. It does not
fetch the complete result into an R data frame.

The default pager accepts RSQLite connections. It discovers field names
without fetching rows, computes `COUNT(*)` once when the source is created,
and requests pages using bound `LIMIT`/`OFFSET` values. Count may scan a
large query and is repeated only when the source is explicitly refreshed. One
trailing semicolon is tolerated. The query must produce one result set;
multiple statements and statements such as DDL are rejected by SQLite with a
query error. There is no SQL parser or automatic sort/filter rewriting, so
lazy query sources advertise sorting, filtering, and searching as unsupported.

Other DBI backends can pass an `adapter` list with `column_names`,
`row_count`, and `get_rows` callbacks. Each callback receives the DBI
connection, original query, and parameters; `get_rows` also receives the
1-based start, row count, and selected columns. The adapter owns dialect
choices such as safe pagination and identifier quoting. A source never owns
or disconnects the supplied connection. Fetch failures are displayed in the
table and sent as `datatable.source_error`; an explorer also reports them in
its status line.

## DBI connections

DBI is optional. Install DBI and a DBI driver such as RSQLite to use database
features. `db_connection(con)` wraps an existing DBI connection and exposes
`query()`, `execute()`, `is_valid()`, and `disconnect()`. Query results are
materialized as data frames in memory; streaming is not currently supported.

Connections are external by default (`owned = FALSE`), so calling
`disconnect()` leaves them open unless `force = TRUE`. With `owned = TRUE`,
the wrapper disconnects when asked and an attached SQL editor disconnects it
when its app shuts down. An editor does not send live DBI connections to
workers; execution is synchronous because most connections cannot safely cross
R process boundaries. Ordinary query mode materializes the full result as a
data frame; `result_mode = "lazy"` opts into the bounded query source described
above.

## Example

`run_example("sql-workspace")` creates an in-memory SQLite workspace when
DBI and RSQLite are installed. Otherwise it opens the same editor and results
layout with database execution disabled and explains which optional packages
are needed.

## Current limitations

The highlighter is a tolerant tokenizer, not a SQL parser. Execution targets
the selected text or entire buffer; it does not infer the statement under the
cursor. Lazy query sources require a row count and fetch pages synchronously;
there is no schema browser, autocomplete, query builder, or async query pool.
