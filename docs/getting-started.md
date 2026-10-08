# Getting started

termr builds reactive terminal applications around R workflows. This tutorial
starts with a form; the same app, layout and event model supports data
explorers and dashboards. See the [README installation instructions](../README.md#installation)
for GitHub, RC1 and local-checkout installation.

## Your first app

Save this as `hello.R` and run `Rscript hello.R` in a terminal:

```r
library(termr)

app(
  vertical(
    label("What is your name?"),
    input(id = "name", placeholder = "Your name"),
    button("Say hello", id = "hello", variant = "primary"),
    label("", id = "output")
  ),
  on("button.pressed", "#hello", function(event, app) {
    name <- app$query_one("#name")$value
    app$query_one("#output")$update(paste("Hello,", name))
  }),
  bind("escape", "quit")
) |>
  run()
```

* `app()` takes widgets, event handlers (`on()`) and key bindings
  (`bind()`).
* `run()` takes over the terminal until the app exits (here: Escape or
  Ctrl+C). Normal exit, handled errors and interrupts run terminal cleanup;
  forced process termination cannot guarantee restoration.
* Tab and Shift+Tab move the focus; Enter or Space press the focused
  button; clicks work too.

## The pieces

| Concept      | Functions                                                      |
|--------------|----------------------------------------------------------------|
| Widgets      | `label()`, `button()`, `input()`, `data_table()`, ...          |
| Layout       | `vertical()`, `horizontal()`, `grid_layout()`, `scroll_view()`  |
| Look         | `style()`, `stylesheet()`, `termr_theme()`                      |
| Events       | `on()`, `widget$on()`, `widget$post_message()`                 |
| Keys         | `bind()`, actions, the command palette (Ctrl+P)                |
| State        | `widget()`, `reactive()`, `widget$set()`                        |
| Time         | `set_timeout()`, `set_interval()`, `animate()`                 |
| Long work    | `app$run_worker()`                                              |
| Screens      | `app$push_screen()`, `modal()`, `confirm_dialog()`, `app$notify()` |
| Tests        | `test_app()`, `render_widget()`                                 |

## Finding and changing widgets

```r
app$query_one("#name")       # by id (error if missing)
app$query("Button")           # all buttons (a list)
app$query(".danger")          # by class
app$query("Horizontal > Button.primary, #cancel")
```

Change widgets through their methods and fields:

```r
lbl <- app$query_one("#output")
lbl$update("Done")                    # or lbl$text <- "Done"
app$query_one("#name")$set(value = "")  # set() when the widget comes from a call
```

R cannot assign into the result of a function call
(`app$query_one("#x")$value <- 1` fails), hence `set()`.

## Where to go next

* [Widgets](widgets.md), [layout](layout.md), [events](events.md),
  [styling](styling.md)
* [Custom widgets and reactive state](custom-widgets.md)
* [Data tools](data.md): `data_table()`, `browse_data()`, metrics
* [Lazy table sources](data-sources.md), [SQL](sql.md) and
  [database explorer](database-explorer.md) for optional database workflows
* [Screens, workers, animation, developer tools](advanced.md)
* [Testing](testing.md)
* [Platform validation](platform-testing.md) and [API stability](stability.md)
