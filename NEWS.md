# termr 0.5.0 (development)

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
  ticks and the repainted area; see [docs/performance.md](docs/performance.md).

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
