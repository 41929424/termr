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
