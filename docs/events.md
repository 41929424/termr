# Events, bindings and actions

## Dispatch

An event is delivered to its target and, if it bubbles, to each ancestor
and finally to the app. At each widget the `on_<type>` method runs, then
handlers added with `widget$on()`, then (for keys) the widget's bindings.
At the app: handlers from `on()` / `app$on()`, then app bindings.
`event$stop()` ends propagation.

Key events go to the focused widget; mouse events to the widget under the
pointer (respecting scroll clipping; disabled widgets pass them to their
parent).

```r
app$on("button.pressed", "#save", function(event, app) save())
app$on("key", "#search", function(event, app) if (event$key == "escape") ...)
panel$on("button.pressed", function(event, app) event$stop())
```

The selector of a handler is matched against the sender of a message, or
the target of a key or mouse event.

## Event types

| Type | Class | Data |
|------|-------|------|
| `key` | `KeyEvent` | `key`, `char`, `ctrl`, `alt`, `shift` |
| `mouse.down`, `mouse.up`, `mouse.move`, `mouse.scroll`, `click` | `MouseEvent` | `x`, `y` (in the widget), `screen_x`, `screen_y`, `button`, `direction` |
| `drag.start`, `drag.move`, `drag.end` | `MouseEvent` | `screen_x`, `screen_y`, `origin_x`, `origin_y` |
| `paste` | `PasteEvent` | `text` (bracketed paste) |
| `resize` | `ResizeEvent` | `width`, `height` |
| `focus`, `blur`, `mount`, `unmount` | ... | `sender` |
| `screen.show`, `screen.hide` | `Event` | `sender` (the screen) |
| widget messages (`button.pressed`, ...) | `MessageEvent` | `event$data` |

Send your own messages: `self$post_message("cart.updated", list(total = 10))`.

## Widget and subsystem messages

These are all the messages termr itself posts. Each is a `MessageEvent`
whose `sender` is the widget named in the first column; it bubbles from the
sender through its ancestors to the app unless a handler calls
`event$stop()`. Handlers run after the change has happened, so stopping a
message only stops other handlers from seeing it; it does not undo or cancel
the change.

**The names below are a frozen public contract.** They follow different
conventions for historical reasons (`datatable.`, `textarea.` and
`splitpane.` versus `option_list.`, `radio_set.` and `db_explorer.`, short
`tree.` and `scroll.`) and are kept exactly as listed. New fields may be
added to `event$data`; documented fields are not removed or renamed within a
major version.

| Sender | Message | `event$data` fields |
|---|---|---|
| `button()` | `button.pressed` | `label` |
| `checkbox()` | `checkbox.changed` | `value` |
| `radio_set()` | `radio_set.changed` | `value`, `index`, `label` |
| `dropdown()` | `dropdown.changed` | `value`, `label` (`NULL` when cleared) |
| `option_list()` | `option_list.highlighted`, `option_list.selected` | `index`, `value`, `label` |
| `input()` | `input.changed`, `input.submitted` | `value`, `valid` |
| `input()` | `input.valid`, `input.invalid` (when validity changes) | `value`, `error` |
| `text_area()` | `textarea.changed` | `version`, `lines`, `valid` |
| `text_area()` | `textarea.valid`, `textarea.invalid` (when validity changes) | `error` |
| `text_area()` | `textarea.selection_changed` | `empty`, `start_row`, `start_column`, `end_row`, `end_column` (`NULL` when empty) |
| `tabs()` | `tabs.changed` | `index`, `previous`, `id`, `label` |
| `split_pane()` | `splitpane.resized` | `ratio` |
| `scroll_view()` | `scroll.changed` | `x`, `y` (offsets) |
| `tree_view()` | `tree.node_selected`, `tree.node_activated`, `tree.node_expanded`, `tree.node_collapsed` | `node`, `label`, `data`, `id` |
| `data_table()` | `datatable.row_selected` (row cursor), `datatable.cell_selected` (cell cursor), `datatable.row_activated` | `row`, `position`, `value` (row as a list, or the cell value); `column` for cells |
| `data_table()` | `datatable.header_selected` | `column`, `index` |
| `data_table()` | `datatable.sorted` | `columns`, `decreasing` |
| `data_table()` | `datatable.found` | `position`, `row`, `column` |
| `data_table()` | `datatable.column_resized` | `column`, `width` (both `NULL` after auto-sizing every column) |
| `data_table()` | `datatable.columns_changed` | `column`, `visible` |
| `data_table()` | `datatable.columns_reordered` | `column`, `from`, `to` |
| `data_table()` | `datatable.cell_changed` | `row`, `column`, `old`, `value` |
| `data_table()` with a table source | `datatable.source_error` (once per failure, until a fetch succeeds) | `message`, `error` (root cause), `start`, `count` |
| `process_view()` | `process.finished` | `status`, `state` |
| `sql_editor()` | `sql.query_started` | `sql` |
| `sql_editor()` | `sql.query_completed` | `sql`, `elapsed`, `elapsed_ms`, `rows`, `result`, `result_type` (`"data"` or `"source"`), `source` |
| `sql_editor()` | `sql.query_failed` | `sql`, `error` (condition), `message`, and `elapsed`, `elapsed_ms` when a query ran |
| `db_explorer()` | `db_explorer.query_completed` | `sql`, `timestamp`, `elapsed_ms`, `ok`, `rows`, `result_type` |
| `db_explorer()` | `db_explorer.query_failed` | `sql`, `timestamp`, `elapsed_ms`, `ok`, `rows`, `error` |
| worker owner, or none | `worker.started`, `worker.cancelled` | `worker` |
| worker owner, or none | `worker.stdout`, `worker.stderr` | `worker`, `line` |
| worker owner, or none | `worker.progress` | `worker`, `value`, `message` |
| worker owner, or none | `worker.completed` | `worker`, `result` |
| worker owner, or none | `worker.failed` | `worker`, `error`, `timed_out`, `status` |

Worker messages come from the widget that owns the worker, or have no sender
(and start at the screen) when the worker has no owner in the app.
`app$post_message()` posts an application message with no sender.

## Key names

Printable characters are named by themselves (`"a"`, `"A"`, `"?"`, also
emoji); named keys: `enter`, `tab`, `escape`, `backspace`, `delete`,
`insert`, `space`, arrows, `home`, `end`, `pageup`, `pagedown`, `f1`-`f24`;
modifiers: `ctrl+`, `alt+`, `shift+`. Run `termr::run_example("keys")`.

## Bindings and actions

```r
bind("q", "quit")
bind("ctrl+s", "save", "Save")                 # description: shown in Ctrl+P
bind("f5,ctrl+r", function(app) app$refresh())
app(ui, actions = list(save = function(app) ...))
```

Bindings live on the app, a widget type (`widget(bindings = )`) or a
widget (`widget$bind()`); the binding closest to the focused widget wins.
Named actions are looked up on the widget (`action_<name>` methods), its
ancestors, then the app; `"app.quit"` targets the app. Built-in actions:
`quit`, `focus_next`, `focus_previous`, `refresh`, `command_palette`,
`toggle_debug`. Default bindings: Tab, Shift+Tab, Ctrl+C (quit), Ctrl+P
(command palette).

## Command palette

Ctrl+P lists `app$add_command(label, action)` commands, described
bindings of the focused widget and the app, and the app's actions. Type to
filter, Enter to run.

## Timers

```r
app$set_interval(1, function(app) ...)
widget$set_timeout(0.5, function(self, app) ...)   # stops with the widget
```

Timers run inside the event loop; nothing blocks.

## Drag and mouse capture

While a button is held, move, release and `drag.*` events go to the widget where
the press happened, even when the pointer leaves it. `drag.start` fires with the
first move, `drag.move` on every move, `drag.end` on release; they carry the
origin of the drag. `widget$capture_mouse()` sends *all* mouse events to a widget
until `widget$release_mouse()` or the next release. Handle them with
`on_drag_move(event)` methods or `widget$on("drag.move", ...)`.

## Paste

A bracketed paste arrives as one `PasteEvent` for the focused widget
(`on_paste(event)`), see [terminal.md](terminal.md).
