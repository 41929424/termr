# termr

> Modern reactive terminal applications for R, with first-class support for
> data workflows.

termr builds interactive terminal applications from widgets, layouts,
reactive state, keyboard and mouse events, stylesheets and themes. You never
write ANSI escape sequences: every frame is painted into a virtual screen and
only the cells that changed are sent to the terminal. A headless driver runs
the same event loop and renderer without a terminal, so whole applications
can be tested with testthat.

```r
library(termr)

rows <- signal(nrow(mtcars))                   # reactive state

app(
  vertical(
    metric("Rows", function() rows()),         # re-renders when `rows` changes
    data_table(mtcars, id = "data", zebra = TRUE, header_sort = TRUE),
    button("Quit", on_press = function(event, app) app$exit())
  ),
  on("datatable.row_selected", "#data", function(event, app) {
    app$notify(paste("Selected:", rownames(mtcars)[event$data$row]))
  })
) |> run()
```

Or browse a data frame at once: `browse_data(your_data)`.

## Installation

```r
remotes::install_local("path/to/termr")   # from a checkout
```

termr needs R >= 4.1. Its runtime dependencies are R6 and processx; no
compiler is needed. DBI, RSQLite, jsonlite and knitr are optional and only
needed for database, JSON and knitr features.

## Where it runs

Apps need a real terminal. Run them with `Rscript app.R`, or from an
interactive R session started in a terminal:

* **Linux and macOS terminals**, including SSH sessions started with
  `ssh -t` ([SSH guide](docs/ssh.md)).
* **Windows Terminal and the Windows console**. Input goes through a small
  PowerShell helper, so starting an app takes about a second.
* **RStudio's Terminal tab** is a regular shell, so `Rscript app.R` is
  expected to work there, but it is not covered by tests yet. The RStudio and
  Positron **consoles** are not terminals; `run()` stops with an explanation
  instead of drawing.

Without a terminal (knitr, CI, scripts), render a widget tree to text,
Markdown, HTML, SVG or JSON with `render_text()`, `render_html()` and
friends ([export guide](docs/export.md)). See the
[platform matrix](docs/platform-testing.md) for what is tested where.

## What is included

* **Widgets**: labels, buttons, inputs, checkboxes, radio sets, dropdowns,
  option lists, a multi-line editor with syntax highlighting, tree views,
  tabs, split panes, scroll views, panels, Markdown, progress, sparklines,
  metrics, logs, process output, status bars, property grids and JSON trees
  ([widgets](docs/widgets.md)).
* **Layout**: vertical, horizontal and grid layouts, scrolling, split panes,
  custom layouts ([layout](docs/layout.md)).
* **Reactivity**: reactive widget fields and a small signal graph
  (`signal()`, `computed()`, `watch()`, `batch()`)
  ([reactivity](docs/reactivity.md)).
* **Input**: keyboard, mouse (click, drag, wheel, hover), bracketed paste,
  clipboard writes via OSC 52 ([terminal](docs/terminal.md)).
* **Styling**: inline styles, CSS-like stylesheets, themes including
  high-contrast and monochrome (`NO_COLOR`), reduced motion
  ([styling](docs/styling.md)).
* **Screens**: modal dialogs, notifications, a command palette (Ctrl+P) and
  generated help (F1).
* **Background work**: R functions in worker processes and external programs
  without a shell, with streamed output, progress, timeouts and cancellation
  ([workers](docs/workers.md)).

## Data workflows

`data_table()` is virtualised: only visible rows are formatted, so it handles
millions of rows, with sorting, filtering, search, resizable, hidden and
frozen columns, and optional editing. Lazy [table sources](docs/data-sources.md)
let it page through data it never loads whole, including SQLite tables and
query results. `sql_editor()`, `db_connection()` and `db_explorer()` add SQL
editing and database browsing through DBI ([SQL](docs/sql.md),
[database explorer](docs/database-explorer.md)).

```r
run_example()                     # list the bundled examples
run_example("data-explorer")
```

## Testing

```r
test_that("greets", {
  pilot <- test_app(my_app(), width = 40, height = 10)
  pilot$type("Ada")
  pilot$press("tab", "enter")
  expect_match(paste(pilot$screen_text(), collapse = "\n"), "Hello, Ada")
})
```

`test_app()` drives the real event loop and renderer against a virtual
terminal with simulated time ([testing](docs/testing.md)).

## Status

termr is in the 0.9 series, preparing 1.0. Core app, widget, layout, event,
styling and testing APIs are intended to be stable; the reactive graph,
workers, text editor, table sources, database support, custom layouts and
static export are experimental. [API stability](docs/stability.md) lists
every area and the versioning policy; [NEWS](NEWS.md) records changes.

## Documentation

[Getting started](docs/getting-started.md) is the place to begin.
[ARCHITECTURE.md](ARCHITECTURE.md) describes the layers for contributors.
Other guides live in [docs/](docs/): events, custom widgets, extensions,
data, workers, export, SSH, performance and testing.

> **R gotcha.** R cannot assign into the result of a function call, so
> `app$query_one("#name")$value <- ""` is an error. Use
> `app$query_one("#name")$set(value = "")`, or keep the widget in a variable.

## License

MIT
