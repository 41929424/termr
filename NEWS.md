# termr 1.0.1

CRAN resubmission following the initial 1.0.0 submission (not published on CRAN).

* Revise DESCRIPTION wording to resolve incoming spell-check notes.
* Bound expensive deterministic stress/property workloads on CRAN to reduce
  check time; `NOT_CRAN=true` retains the full seed and iteration counts.

# termr 1.0.0

First stable release, prepared as the first CRAN submission. This section summarizes the
pre-1.0 work. Stability classifications are defined in
[API stability](https://github.com/41929424/termr/blob/main/docs/stability.md).

The release candidate tag `v1.0.0-rc1` points to
`1846527d3968cdd12fa9900bbe80a9f2a89a1a3e`, whose DESCRIPTION retains
`Version: 0.9.0.9000`. Changes since that tag are limited to documentation,
release metadata and the following:

* `DESCRIPTION` gains `URL` and `BugReports`.
* The `animate()` and `run_worker()` examples now run headlessly with
  `test_app()` instead of being wrapped in `\dontrun{}`.

## Highlights

* Compose modern reactive terminal applications around R data workflows,
  with widgets, layouts, events, themes and deterministic headless testing.
* Inspect data through virtualized tables, lazy sources, SQL editing and
  database browsing, with optional DBI/RSQLite integrations.
* Render incremental terminal frames or export static text, Markdown, HTML,
  SVG and JSON for reports. Advanced subsystems remain experimental.

## Data workflows

* Lazy `table_source()` pages feed DataTable's bounded viewport cache.
  SQLite tables and views support delegated sorting/filtering; read-only
  `db_query_source()` adds bounded query-result paging and driver callbacks.
* `sql_editor()` keeps materialized query results by default; lazy mode is
  opt-in. `db_explorer()` uses lazy SQLite previews and query results,
  portable metadata and an in-memory history. Database calls are synchronous.
* Added `status_bar()`, `property_grid()`, `record_view()`, `data_profile()`,
  and `json_view()` for reactive status, record inspection, vector profiling,
  and paged R-object trees. `record_view()` reuses the property-grid
  renderer; `json_view()` reuses lazy TreeView nodes.
* Added the `data-workbench` composition example. These workflow widgets add
  no mandatory runtime dependency; optional JSON text parsing uses `jsonlite`
  from Suggests.

## Terminal and rendering

* Rectangular dirty regions and paint-only invalidation avoid unnecessary
  layout and full-screen painting. Grapheme-aware rendering preserves
  wide characters, emoji and combining sequences.
* Windows mouse DWORDs use exact unsigned representations and arithmetic
  wheel-delta extraction, including negative wheel values.
* Documented the existing interactive SSH/PTY architecture, clipboard boundary,
  multiplexer limitations, and manual SSH smoke checklist in `docs/ssh.md`.
* Capability detection now leaves synchronized output off for generic xterm and
  multiplexer TERM values, treats `vt100` as monochrome, and avoids enabling
  SGR mouse or bracketed paste for legacy Linux/VT100 terminals.

## Reactivity and background work

* Signals, lazy computed values and watchers connect application state to
  reactive widget values; batching groups updates and disposal ends bindings.
* Background R workers and shell-free subprocesses stream output and progress,
  with timeouts, cancellation, process-tree cleanup and bootstrap diagnostics.
* Worker functions now capture only referenced lexical bindings by value.
  Unrelated enclosing state no longer produces giant payloads for trivial
  jobs. Simple scalar captures and nested/recursive helpers are preserved;
  active bindings and dynamic lexical lookup are rejected before spawn.
  Explicit large values and `args` still serialize. The regression passed
  on Linux, macOS and Windows.

## Testing and portability

* Full CI [37758703750](https://github.com/41929424/termr/actions/runs/37758703750)
  passed on the RC1 source: Ubuntu release, oldrel-1, R 4.1 and devel;
  macOS/Windows release; SQL integration; Linux/macOS PTY; Windows input.
  All six R CMD check jobs reported 0 errors, 0 warnings and 0 notes.
* For RC1, Windows real interactive console and Linux through real `ssh -t`
  smoke were manually reported PASS. macOS real terminal, tmux, screen and RStudio
  Terminal remain NOT RUN. PTY CI is separate from manual console validation.

## Static export

* Added terminal-independent text, Markdown, styled HTML and SVG rendering
  from the virtual screen buffer, plus deterministic screen and widget-tree
  snapshots.
* Added optional JSON serialization (`jsonlite`), explicit knitr/Quarto
  output (`knitr`), and a single `write_rendered()` convenience function.
* Widget-tree snapshots omit arbitrary state and field values. Static exports
  are snapshots; they do not run timers, workers, or an app event loop.

## API changes before 1.0

* `snapshot_json()` is now `screen_snapshot_json()` and `inspect_json()` is
  now `widget_snapshot_json()`, matching `screen_snapshot()` and
  `widget_snapshot()`. Both were new in 0.9; there are no aliases.
  `widget_snapshot()` gains a root `schema_version` (1).
* `db_table_source(own_connection = )` is now `owned = `, as in
  `db_connection(owned = )`.
* `register_layout()` gains `replace = FALSE`; pass `TRUE` to replace a
  registration on purpose, e.g. when a package registering in `.onLoad()`
  is reloaded.
* `datatable.source_error` is posted once per failure rather than once per
  failed page, and carries the root `error` message; `source_stats()` gains
  `fetch_errors`. `db_explorer()` therefore shows one notification for one
  broken connection.

## Fixes from the 1.0 readiness audit

* Terminal input validates UTF-8 bytes before locale conversion and retains
  incomplete characters across reads, including on R 4.1.
* Workers inherit the parent's library search paths while keeping `R_TESTS`
  disabled. Process exit drains output without an unbounded EOF wait,
  cleans up descendants immediately, and reports process diagnostics on
  unexpected exit or a Pilot timeout.
* The `db_explorer()` documentation example now stops its app and closes
  the owned SQLite connection.

* `db_table_source()` works with current RSQLite (3.53 / DBI 1.3), which
  rejects an empty parameter list; previously every unfiltered SQLite source,
  and therefore the `db_explorer()` preview, failed at creation. It now also
  accepts a `db_connection()` wrapper.
* `data_table()` with a table source fetches one page per source call for all
  columns instead of one call per painted column.
* Moving a mounted widget with `mount()` keeps its `bind_reactive()` bindings,
  widget timers and owned workers; only `remove()` ends them.
* `widget$on()` also accepts the argument order of `on()` / `app$on()`:
  `widget$on(type, selector, handler)`.
* Password inputs no longer reveal their value through `print()`, the event
  log, the debug overlay or `inspect_widget()`, and never copy it to a
  clipboard.
* Text and cells drop C1 control characters (U+0080-U+009F) as well as C0
  controls, so untrusted text cannot emit 8-bit CSI/OSC sequences.
* The key parser replaces bytes that are not UTF-8 instead of failing, and
  drops over-long escape sequences instead of buffering them without bound.
* `batch()` and `watch()` restore the reactive graph after an interrupt; an
  interrupted handler previously left every watcher deferred.
* `render_html(trim = TRUE)` trims only the end of each row (inner spaces were
  removed, shifting columns); SVG output preserves runs of spaces.
* Large trees no longer trigger the "too many events in one tick" warning at
  start-up; the guard counts events added during a tick.
* `db_explorer()`: the object filter is case-insensitive as documented, and
  expanding a group after the connection closed no longer errors.

# termr 0.7.0 (development)

## Database workflows

* Added `db_explorer()` with lazy DBI metadata browsing, SQLite table/view
  browsing and lazy table previews, integrated SQL execution and result
  tables, details, status, and in-memory query history.
* Added `db_metadata()` as a small portable metadata interface. DBI and
  RSQLite remain optional dependencies; SQL query results are materialized
  and generic DBI query execution is synchronous.

## Large data sources

* Added the experimental `table_source()` protocol and lazy SQLite table
  source, with viewport-driven pages, delegated sorting/filtering, bounded
  caching and source lifecycle methods.

## SQL workflows

* Added `sql_highlighter()` and the `sql_editor()` text-area subclass with
  selection-aware synchronous query or execute mode and SQL result events.
* Added the optional DBI-backed `db_connection()` wrapper and the
  `sql-workspace` example. DBI and RSQLite are suggested dependencies only.
* Database results are currently materialized in memory; async query execution
  and schema browsing are not included.

# termr 0.3.0

The package was renamed from retui to termr during 0.3 development (no
compatibility aliases; options are `termr.*`, variables `TERMR_*`).

## Rendering and performance

* Incremental repaints are now **rectangles** (changed widgets, plus the old
  and new region of widgets that moved) instead of whole row bands, merged and
  widened so wide graphemes are never cut. `app$last_paint` reports repainted
  cells, rectangles and ANSI bytes.
* Paint-only changes (cursor moves, scrolling in tables and lists, colour
  changes, progress, spinner frames, focus/hover without size differences)
  skip the layout pass (`invalidate_paint()`, `paint_states`).
* Scroll views lay out only the children in view.
* New benchmark suite (`tools/bench/bench.R`, `compare.R`) measuring batches of
  ticks and the repainted area; see [docs/performance.md](https://github.com/41929424/termr/blob/main/docs/performance.md).

## Widgets

* `text_area()`: multi-line editor with selection, undo/redo, find, wrapping,
  line numbers, mouse and paste.
* `split_pane()` with a draggable / keyboard-movable divider.
* `markdown_view()`: headings, lists, quotes, code, tables and links.
* `process_view()`: a program's output with status, cancel and restart.
* Help screen (F1) and a unified `command()` model shared with the command
  palette (categories, shortcuts, enabled state).
* `button(on_press = )`; `label()`, `button()`, `progress_bar()`, `metric()`,
  `sparkline()` and `key_value()` accept reactive functions.

## Data

* `data_table()` 2.0: resizable (mouse and keyboard), hideable and frozen
  columns, multi-column sort with markers, `header_sort`, search (Ctrl+F),
  `goto_row()`, `auto_size_column()` that never scans huge tables, and
  experimental cell editing on the table's own copy (`editable`, `on_edit`,
  `datatable.cell_changed`). Numeric column headers are now right-aligned.
* Reactive graph: `signal()`, `computed()`, `watch()`, `batch()`, `peek()`,
  `untracked()`, `dispose()`.

## Terminal and input

* Terminal capability model (`terminal_capabilities()`).
* Bracketed paste (`PasteEvent`, `on_paste()`), system clipboard writes via
  OSC 52 (`app$clipboard_write()`, write-only).
* Mouse drag events (`drag.start/move/end`) and capture
  (`widget$capture_mouse()`).
* Workers: separate progress channel, streamed `on_stdout` / `on_stderr`,
  `timeout`, interrupt-then-kill cancellation, shell-free `run_process()`.
  The worker progress function is now `termr_progress()`.
* Key parser tested against arbitrary input fragmentation; PTY harness covers
  keys, UTF-8, mouse, paste, resize, timers and SIGINT.

## Accessibility

* `"high-contrast"` theme; monochrome rendering for `NO_COLOR` (reverse video,
  bold/underline, heavier focus border) so state never depends on colour alone.
* Reduced motion (`app(reduce_motion = )`, `options(termr.reduce_motion)`,
  `TERMR_REDUCE_MOTION`).

## Testing

* `test_app(color_mode = )`; pilot methods `paste()`, `drag()`, `focus()`,
  `find()`, `wait_for()`, `run_worker()`, `system_clipboard()`.
* Randomised tests: incremental vs full rendering on rich UIs, extreme resizes,
  focus validity under random tree changes, input fragmentation.

## Examples and docs

* New examples: `data-explorer`, `task-runner`, `terminal-dashboard`, `editor`,
  `accessibility-demo`. New guides: text area, reactivity, workers, terminal and
  accessibility, performance.

## Breaking changes

* Package renamed to **termr**; `retui_progress()` is `termr_progress()`.
* `datatable` Enter on an editable table edits instead of sending
  `datatable.row_activated`; `filter()` keeps an active sort.
* `Ctrl+F`, `Ctrl+G`, `F1`, `F3` and Alt+Left/Right are bound in tables;
  `F1` is bound in every app.
* Worker stdout is no longer used for progress (it is delivered to
  `on_stdout`).

# termr 0.2.0

A large expansion from the 0.1.0 MVP towards a complete TUI framework,
with a focus on data-oriented applications.

## Data

* `data_table()`: virtualised, read-only data frame viewer (millions of
  rows), row/cell/no cursor, row names, `column()` options, formatters,
  zebra stripes, `row_style` / `cell_style` hooks, horizontal and vertical
  scrolling, `sort()` / `filter()` views, mouse support and
  `datatable.*` messages.
* `browse_data()` / `data_browser()`: an interactive data frame browser
  (metrics, search, sorting, column summaries, row details).
* `metric()`, `key_value()`, `sparkline()`, `progress_bar()`, `log_view()`.

## Widgets

* `scroll_view()` with scroll bars, keyboard and wheel scrolling, and
  scroll-into-view on focus changes.
* `tree_view()` / `tree_node()` with lazy loading.
* `tabs()` / `tab()`.
* `checkbox()`, `radio_set()` / `radio_button()`, `dropdown()`,
  `option_list()`, `spinner()`, `rule()`, `panel()` (with border titles).
* `input()`: selection, word movement and deletion, app clipboard
  (`app$clipboard`), `validate =` with `input.valid` / `input.invalid` and
  the `:invalid` state.
* `label(wrap = )` / `style(wrap = "word" | "char")`: text wrapping by
  display width; `"auto"` heights follow wrapped text.

## Layout and rendering

* `grid_layout()` with fixed/auto/fr/percent tracks, gaps and spans.
* Grapheme clusters (UAX #29, via PCRE2 in base R): emoji ZWJ sequences,
  flags, skin tones and combining marks are single characters everywhere.
* Dirty-region repaints: when the layout is unchanged only the rows of
  changed widgets are repainted (a randomised test checks they always
  equal a full repaint). Layout caches are now invalidated per widget
  chain. A label update in a 341-widget tree went from 200 ms to 30 ms per
  tick.

## Application

* Screen stack: `app$push_screen()`, `pop_screen()`, `switch_screen()`
  with focus restore and `screen.show` / `screen.hide` events.
* Modal screens and dialogs: `modal()`, `confirm_dialog()`,
  `alert_dialog()`.
* Notifications: `app$notify()`.
* Command palette (Ctrl+P) with `app$add_command()`.
* Mouse support: clicks, wheel, hover (`:hover`), focus on click, and
  `pilot$click()` / `hover()` / `scroll()` for tests.
* Background workers in separate R processes: `app$run_worker()`,
  `run_worker()`, `termr_progress()`, `worker.*` events.
* Animations: `animate()` / `widget$animate()` with easings.
* Developer tools: `inspect_widget()`, `app$log_events()`, F12 debug
  overlay (`app(debug = TRUE)`).

## Styling

* RTCSS stylesheets: `stylesheet()`, `stylesheet_file()`,
  `app(stylesheet = )` with specificity, source order and pseudo classes
  (`:focus`, `:hover`, `:disabled`, `:pressed`, `:invalid`). Errors report
  line, column and property.
* Themes: `termr_theme()` and `app(theme = )` with `default`, `dark` and
  `light`; colours can refer to the theme (`"$primary"`).
* `style(hover = )`, grid properties, `options(termr.ascii = TRUE)`.

## Changes

* `Ctrl+A` in inputs now selects all (Home still moves to the start).
* `on()` selectors match the target of key and mouse events.
* Read-only fields give a clear error when assigned.
* Widget ids must be unique within a screen.
* The Windows input helper reads console records (keys and mouse); a bug
  that prevented enabling ANSI processing in legacy consoles is fixed.

## Infrastructure

* GitHub Actions: R CMD check on Linux (release, oldrel, devel), macOS and
  Windows, plus a Unix pseudo-terminal integration harness
  (`tools/pty/pty-check.py`).
* `tools/bench/bench.R` rendering benchmarks.

# termr 0.1.0

* First version: widgets (`label()`, `button()`, `input()`, `vertical()`,
  `horizontal()`), layout, reactive state, events and bindings, focus,
  timers, framebuffer diff renderer, Windows/Unix/headless drivers and
  `test_app()`.
