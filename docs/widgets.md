# Widgets

All widgets take `id`, `classes` and `style` arguments. Their state fields
are reactive: assigning a new value repaints the widget.

## Text

| Widget       | Notes                                                                   |
|--------------|-------------------------------------------------------------------------|
| `label(text, wrap = )` | `$update()`, `$text`; text can be `span()`s; `wrap = "word"`/`"char"` |
| `rule(orientation, title)` | horizontal or vertical separator                           |
| `key_value(list(...))` | aligned names and values; `$data`                              |
| `log_view(max_lines, timestamps)` | `$write(..., level =)`, `$clear()`, `$pause()`, `$resume()`; bounded memory |

Styled text: `c(span("CPU "), span("92%", style(foreground = "red", bold = TRUE)))`.
Spans nest and can carry `link =` metadata.

| `markdown_view(text)` | headings, lists, quotes, code, tables, links; `$set_markdown()`, `$links` |
| `process_view(command, args)` | program output with status; see [workers.md](workers.md) |

## Controls

| Widget | Messages (`event$data`) |
|--------|--------------------------|
| `button(label, variant = "primary")` | `button.pressed` (`label`) |
| `input(value, placeholder, password, max_length, validate)` | `input.changed`, `input.submitted` (`value`, `valid`), `input.valid` / `input.invalid` (`value`, `error`) |
| `checkbox(label, value)` | `checkbox.changed` (`value`) |
| `radio_set(radio_button(label, value), ..., selected)` | `radio_set.changed` (`value`, `index`, `label`) |
| `dropdown(choices, value, prompt)` | `dropdown.changed` (`value`, `label`) |
| `option_list(choices)` | `option_list.highlighted`, `option_list.selected` (`index`, `value`, `label`) |

| `text_area(value, line_numbers, wrap, read_only)` | multi-line editor; see [text-area.md](text-area.md); `textarea.changed`, `textarea.selection_changed` |

`input()` and `text_area()` share the validator protocol: `validate =
function(value)` returns `NULL` / `TRUE` (valid), `FALSE` or a message.
`button(on_press = )` takes a callback; `label()`, `button()`,
`progress_bar()`, `metric()`, `sparkline()` and `key_value()` accept a function
that re-evaluates when the [signals](reactivity.md) it reads change.

The data and developer workflow widgets are described in
[data-workflows.md](data-workflows.md), including reactive selection patterns,
profile semantics and lazy JSON expansion.

Input keys: Left/Right/Home/End, Ctrl+Left/Right (words), Shift+... to
select, Ctrl+A select all, Backspace/Delete, Ctrl+Backspace/Ctrl+W and
Ctrl+Delete (words), Ctrl+U/K, Ctrl+C/X/V with the app clipboard.

## Containers

| Widget | Notes |
|--------|-------|
| `vertical(...)`, `horizontal(...)` | stack children |
| `grid_layout(..., columns, rows, gap)` | grid with spans |
| `scroll_view(..., direction)` | `$scroll_to()`, `$scroll_by()`, `$scroll_home()`, `$scroll_end()`, `$scroll_into_view()`; `scroll.changed` |
| `panel(..., title)` | bordered group with a title |
| `split_pane(first, second, direction, ratio)` | draggable divider (mouse or keyboard); `splitpane.resized` |
| `tabs(tab("Label", ...), ...)` | `$activate()`, `$add_tab()`, `$remove_tab()`; `tabs.changed` |

## Data and status

| Widget | Notes |
|--------|-------|
| `data_table(df, ...)` | see [data.md](data.md) |
| `tree_view(tree_node(...))` | lazy `loader =`; `tree.node_selected`, `_expanded`, `_collapsed`, `_activated` |
| `status_bar(left, center, right)` | one-line three-region status display |
| `property_grid(data)` | scrollable, read-only key/value inspector; values wrap |
| `record_view(data)` | named list/vector or one-row data frame via `property_grid()` |
| `data_profile(x)` | R vector summary with a compact numeric distribution |
| `json_view(x)` | collapsible R list/vector tree with paged lazy children |
| `metric(label, value, delta)` | `$update(value, delta)` |
| `sparkline(x)` | `$push(values)` for streams |
| `progress_bar(value, total)` | `NA` for indeterminate; `$advance()` |
| `spinner(label)` | animated by a widget timer |

## Dynamic trees

```r
container$mount(widget)                    # append
container$mount(widget, before = other)    # or after = , or an index
widget$remove()
container$replace(new1, new2)
container$clear()
```

Mount and unmount events are sent, focus moves on when the focused widget
is removed, and the timers and workers owned by removed widgets stop. Ids
must be unique within a screen.
