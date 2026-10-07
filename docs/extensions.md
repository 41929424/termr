# Extending termr from another package

This guide describes extension points available to code outside `termr`. The
examples use exported functions and classes only. `tests/fixtures/external_extensions.R`
contains runnable, namespace-qualified examples; the corresponding tests exercise
them without using `termr:::`.

## Widgets, commands, and events

`widget()` creates an R6-backed widget type from state, rendering, bindings,
actions, event handlers, and child composition. It is enough for most custom
controls. A widget can send a typed message with `$post_message()`; applications
can subscribe with `on()`:

```r
Counter <- termr::widget(
  "Counter",
  state = list(count = termr::reactive(0L)),
  render = function(self) sprintf("Count: %d", self$count),
  bindings = list(termr::bind("up", "increment", "Increment")),
  actions = list(increment = function(self) {
    self$count <- self$count + 1L
    self$post_message("counter.changed", list(count = self$count))
  }),
  focusable = TRUE
)

counter <- Counter(id = "counter")
ui <- termr::app(
  counter,
  termr::on("counter.changed", "#counter", function(event, app) {
    message(event$data$count)
  })
)
ui$add_command(termr::command("Increment", function(app) {
  app$query_one("#counter")$run_action("increment")
}))
```

`Widget` is exported for advanced subclasses. `ScreenBuffer` and `region()` are
also public for widgets that need to override `paint()` or arrange their own
children. Such a subclass should use the documented public methods and respect
the supplied clip region. The factory does not currently offer a custom
`paint` callback; use an R6 subclass only when `render()` is insufficient.

## Layouts

`register_layout()` adds a custom container algorithm under a `custom_` name.
The callbacks receive public child widgets and style/region values, and return
one `region()` per child plus natural `width()` / `height(width)` measurements.

```r
termr::register_layout(
  "custom_stack",
  arrange = function(children, inner, parent_style) {
    lapply(seq_along(children), function(i)
      termr::region(inner$x, inner$y + i - 1L, inner$width, 1L))
  },
  measure = function(children, parent_style) list(
    width = function() 1L,
    height = function(width) length(children)
  )
)
container <- termr::vertical(
  termr::label("one"),
  style = termr::style(layout = "custom_stack")
)
```

Remove a registration when an extension unloads with
`termr::unregister_layout("custom_stack")`. Registration and removal are
experimental; registrations are process-wide and names must be unique.

## Themes

`termr_theme()` accepts custom semantic color names in addition to the built-in
tokens. Styles refer to them as `$name`, and an app receives the theme through
`app(theme = ...)`:

```r
theme <- termr::termr_theme("dark", extension_color = "#22aa88")
ui <- termr::app(
  termr::label("colored", style = termr::style(foreground = "$extension_color")),
  theme = theme
)
```

## Highlighters

`text_area(highlighter = fn)` accepts a function of `(lines, state)` that
returns a data frame with `start`, `end`, and `token` columns, or a list of
such frames. Positions are 1-based grapheme ranges. Tokens are `keyword`,
`string`, `comment`, `number`, `constant`, `operator`, `function`,
`identifier`, `punctuation`, `parameter`, and `quoted_identifier`. A regular
highlighter is called with one line; a function marked with
`attr(fn, "termr.contextual") <- TRUE` receives the complete line vector and
may use the supplied state to carry context across lines. This callback
protocol is supported, but remains experimental while the text editor evolves.

## Table sources and database adapters

`table_source()` is the public protocol for lazy, in-memory, remote, or
database-backed table providers. It accepts callbacks for row counts, column
names, page reads, sorting, filtering, search, stable row keys, editing, refresh,
and close. Pass the returned source to `data_table()`:

```r
source <- termr::table_source(
  row_count = nrow(data),
  column_names = names(data),
  get_rows = function(start, count, columns = NULL) {
    rows <- seq.int(start, min(nrow(data), start + count - 1L))
    out <- data[rows, , drop = FALSE]
    if (!is.null(columns)) out <- out[, columns, drop = FALSE]
    out
  }
)
ui <- termr::data_table(source)
```

The table-source protocol and `db_table_source()` are experimental; callback
and capability details may change before 1.0. An extension can implement its
own driver-specific paging by returning `table_source()` callbacks, as the
SQLite fixture does in `tests/fixtures/external_extensions.R`.

For metadata, `db_metadata(connection)` exposes portable DBI callbacks for
schemas, tables, views, columns, and identifier quoting. A third-party DBI
driver can participate through its normal DBI methods; database-specific
source behavior can be layered on `table_source()`. `db_explorer()` currently
uses `db_metadata()` itself and has no metadata-adapter injection or registry.
An extension that needs non-DBI metadata or alternate object types must build
its own widget/view using the public widget and table-source APIs. We leave
this gap as-is rather than adding a registry without a concrete consumer.

## External usage tests

`tests/fixtures/external_extensions.R` is written in third-party style and
qualifies every `termr` call with `termr::`. The tests cover a custom widget,
binding/action/message flow, an app command, custom layout and theme, a custom
highlighter, an in-memory table source, and DBI metadata plus a driver-specific
table source when `DBI` and `RSQLite` are installed. The fixture contains no
`termr:::` access.
