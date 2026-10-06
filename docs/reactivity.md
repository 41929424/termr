# Reactivity

termr has two reactive layers that work together.

## Widget fields

A custom widget declares `reactive()` fields; assigning one repaints the
widget (see [custom-widgets.md](custom-widgets.md)). Built-in widgets expose
reactive fields too (`label$text`, `progress_bar$value`, ...).

## Signals (experimental)

A small graph for application state, independent of widgets:

```r
count  <- signal(0)
double <- computed(function() count() * 2)
watch(function() cat("double is", double(), "\n"))   # prints now
count(5)                                             # prints "double is 10"

app(vertical(
  label(function() paste("Double:", double())),
  button("Add", on_press = function() count(count() + 1))
)) |> run()
```

* `signal(x)`: `s()` reads, `s(value)` writes. Writing an `identical()` value
  does nothing. Collections are replaced as a whole (`items(c(items(), "x"))`);
  changes *inside* an object are not noticed.
* `computed(fn)`: lazy and cached; re-evaluates when read after a dependency
  changed. A computed that depends on itself is an error.
* `watch(fn)`: runs now and after its dependencies changed, in creation order,
  only if a dependency really changed. Writes inside a watcher settle; a loop
  between watchers stops with an error after a bounded number of runs.
* `batch(expr)`: watchers run once, at the end. Event handlers and timers of an
  app run in a batch automatically.
* `peek(x)` / `untracked(expr)`: read without a dependency. `dispose()` ends a
  watcher or computed. `update_signal(x, fn)` applies a function.
* `inspect_signal(x)` shows a node's direct dependencies and subscribers,
  disposed/dirty state, version, and cumulative read/evaluation/run/write
  counters. Give nodes a `name` to make cycle paths readable; errors include
  the named path through the cycle.

`label()`, `button()`, `progress_bar()`, `metric()`, `sparkline()` and
`key_value()` accept a function wherever they take their value; the function
is evaluated again when the signals it reads change, and the binding ends when
the widget is removed. For other widgets use `widget$bind_reactive("field", fn)`.
`button(on_press = )` takes a callback.

Signals hold their subscribers: `dispose()` watchers you no longer need.
