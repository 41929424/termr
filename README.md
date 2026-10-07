# termr

> termr вЂ” Modern reactive terminal applications for R, with first-class support for data workflows.

termr builds interactive terminal applications from widgets, layouts,
reactive state, keyboard and mouse events, stylesheets and themes. You never
write ANSI escape sequences: every frame is painted into a virtual screen and
only the cells that changed are sent to the terminal. A headless driver lets
you test whole applications without a terminal.

```r
library(termr)

df <- mtcars
rows <- signal(nrow(df))                      # reactive state

app(
  vertical(
    horizontal(
      metric("Rows", function() rows()),      # re-renders when `rows` changes
      metric("Columns", ncol(df))
    ),
    data_table(df, id = "data", zebra = TRUE, header_sort = TRUE),
    button("Quit", on_press = function(event, app) app$exit())
  ),
  on("datatable.row_selected", "#data", function(event, app) {
    app$notify(paste("Selected:", rownames(df)[event$data$row]))
  })
) |> run()
```

Or browse any data frame at once: `browse_data(your_data)`.

Status: **0.3.0, experimental** вЂ” the API may still change; see
[Status](#status).

## Installation

```r
remotes::install_local("path/to/termr")   # from a checkout
```

Runtime dependencies: [R6](https://r6.r-lib.org) and
[processx](https://processx.r-lib.org). No compiler needed. Run apps with
`Rscript my_app.R` in a real terminal (Windows Terminal, the Windows console,
any Unix terminal); the RStudio and Positron consoles are not terminals and
`run()` says so.

## Main capabilities

* **Widgets**: `label()`, `button()`, `input()`, `text_area()`, `checkbox()`,
  `radio_set()`, `dropdown()`, `option_list()`, `data_table()`, `tree_view()`,
  `tabs()`, `split_pane()`, `scroll_view()`, `panel()`, `markdown_view()`,
  `progress_bar()`, `spinner()`, `sparkline()`, `metric()`, `key_value()`,
  `log_view()`, `process_view()` ([docs/widgets.md](docs/widgets.md)).
* **Layout**: vertical, horizontal, grid with spans, scrolling, split panes
  ([docs/layout.md](docs/layout.md)).
* **Reactivity**: widget fields, plus a small standalone graph (`signal()`,
  `computed()`, `watch()`, `batch()`) that widgets can read
  ([docs/reactivity.md](docs/reactivity.md)).
* **Input**: keyboard, mouse (click, drag, wheel), bracketed paste, system
  clipboard writes via OSC 52 ([docs/terminal.md](docs/terminal.md)).
* **Styling**: `style()`, CSS-like stylesheets, themes including
  `"high-contrast"`, monochrome and reduced-motion support
  ([docs/styling.md](docs/styling.md)).
* **Screens and overlays**: modal dialogs, notifications, command palette
  (Ctrl+P), generated help (F1).
* **Background work**: `app$run_worker()` for R functions, `app$run_process()`
  for programs; streamed output, progress, timeouts
  ([docs/workers.md](docs/workers.md)).
* **Performance**: incremental rectangular repaints, layout and style caches,
  virtualised tables ([docs/performance.md](docs/performance.md)).

## Data workflows

`data_table()` shows millions of rows (only visible rows are formatted) and has
resizable, hideable and frozen columns, multi-column sort, search (Ctrl+F),
jump to row, and optional cell editing on its own copy of the data.
`browse_data()` wraps it with metrics, filtering and column summaries.
`data_table()` + `split_pane()` + signals make a data explorer in about a
hundred lines (`run_example("data-explorer")`). See
[docs/data.md](docs/data.md).

For SQL workflows, `sql_editor()` adds SQL highlighting and selection-aware
execution to `text_area()`. Database support is optional through DBI; see
[`docs/sql.md`](docs/sql.md) and `run_example("sql-workspace")`.

```r
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
db <- db_connection(con, owned = TRUE)
app(
  vertical(
    sql_editor("SELECT * FROM mtcars LIMIT 5", connection = db, id = "query"),
    data_table(data.frame(), id = "results")
  ),
  on("sql.query_completed", "#query", function(event, app) {
    app$query_one("#results")$set_data(event$data$result)
  })
)
```

## Testing

`test_app()` drives the real event loop and renderer against a virtual
terminal with simulated time and any colour profile:

```r
test_that("greets", {
  pilot <- test_app(my_app(), width = 40, height = 10)
  pilot$type("Ada")
  pilot$press("tab", "enter")
  pilot$click("#save")
  pilot$drag("#splitter", c(20, 5))
  pilot$paste("pasted\ntext")
  pilot$resize(80, 24)
  pilot$advance(1)                    # timers, animations
  pilot$wait_for(function(app) app$query_one("#status")$text == "Done")
  expect_snapshot(pilot$snapshot())   # what the user sees
})
```

See [docs/testing.md](docs/testing.md).

## Examples

`termr::run_example()` lists them:

| Example              | Shows                                                         |
|----------------------|---------------------------------------------------------------|
| `data-explorer`      | Table, split pane, signals, search/sort, command palette      |
| `task-runner`        | Background workers: progress, logs, cancel, failures          |
| `terminal-dashboard` | Metrics, sparklines, gauges, live table and events            |
| `editor`             | `text_area()`: selection, undo/redo, find, status line        |
| `accessibility-demo` | Themes, monochrome, focus visibility, reduced motion          |
| `kitchen-sink`       | Every widget, tabs, dialogs, notifications                    |
| `dataframe-browser`  | `data_browser()` on 200,000 rows                              |
| `system-monitor`, `model-monitor`, `file-tree`, `counter`, `form`, `hello`, `keys` | Smaller focused demos |

## Status

0.3.0 is a release candidate for an experimental package. Maturity by area:
core widgets, layout, events, testing: stable-ish; `text_area()`, editable
`data_table()`, signals, workers and `run_process()`, `split_pane()`,
`markdown_view()`, stylesheets: experimental. The terminal is always restored
on exit, on errors and on interrupts.

## Limitations

* Verified by the authors in a real Windows console (ConPTY). The Unix driver
  has unit tests and a pseudo-terminal harness (`tools/pty/pty-check.py`, run in
  CI) but has not been exercised by hand on every terminal.
* Starting an app takes about a second on Windows (PowerShell input helper).
* Windows consoles do not send bracketed paste; pasted text arrives as typed
  keys.
* Dirty-region repaints are rectangles of changed widgets; scrolling large
  scroll views re-lays out their visible children each time.
* The system clipboard can only be written (OSC 52), never read.
* Widget content that depends on non-reactive data needs `widget$refresh()`.
* `text_area()` has no syntax highlighting (the `language` argument is
  reserved).

> **R gotcha.** R cannot assign into the result of a function call, so
> `app$query_one("#name")$value <- ""` is an error. Use
> `app$query_one("#name")$set(value = "")`, or keep the widget in a variable.

## Architecture and docs

[ARCHITECTURE.md](ARCHITECTURE.md) describes the layers. Guides live in
[docs/](docs/): getting started, widgets, layout, events, styling, data,
text area, reactivity, workers, terminal and accessibility, performance,
testing, custom widgets, advanced topics.

## License

MIT
