# Extension APIs (experimental)

The widget factory, base widget class, screen buffer, `region()`, events,
bindings, styles, themes, and layout registration are available to extension
authors. APIs marked experimental can still change during 0.4 development.
Built-in widgets may also subclass `Widget` with R6.

## A small custom widget

```r
Counter <- widget(
  "Counter",
  state = list(count = reactive(0L)),
  render = function(self) sprintf("Count: %d", self$count),
  bindings = list(bind("up", "increment", "Increment")),
  actions = list(increment = function(self) self$count <- self$count + 1L),
  style = style(border = "round", width = 16),
  focusable = TRUE
)
counter <- Counter(id = "counter")
```

The factory mounts children returned by `compose()`, installs bindings and
handlers at mount, and disposes timers, workers, processes, and reactive
bindings when removed. Use reactive state for changes that affect painting;
call `self$invalidate()` when a custom method changes non-reactive content.
Messages use `self$post_message()` and bubble through the widget tree.

Override `render()` / `render_lines(width)` for text content, or `paint(buffer,
area, st)` when drawing cells. Respect the supplied clip area and use
`ScreenBuffer$put_text()` so wide characters and clipping stay correct. Use
`region()` for child geometry. Focusable widgets should set `focusable = TRUE`
and participate in the ordinary bindings and focus events.

## Custom layouts

Names start with `custom_` and cannot replace built-ins. Register before
creating a style that names the layout:

```r
stack_arrange <- function(children, inner, parent_style) {
  lapply(seq_along(children), function(i)
    region(inner$x, inner$y + i - 1L, inner$width, 1L))
}
stack_measure <- function(children, parent_style) list(
  width = function() 1L,
  height = function(width) length(children)
)
register_layout("custom_stack", stack_arrange, stack_measure)
```

`arrange()` receives the mounted children, available inner region, and
computed parent style, then returns one `region()` per child. `measure()`
returns `width()` and `height(width)` functions for natural sizing. Remove an
experimental registration with `unregister_layout()` when an extension is
unloaded. Layout functions must not mutate their children.

## Compatibility and testing

Treat APIs documented in the package reference as public. Functions under
`termr:::` and private R6 members are internal. Test custom widgets in a
headless `test_app()`: mount, unmount, focus, keyboard input, resize, and
clipping should all work without a real terminal. Test a custom layout at
zero, one, and several children, and verify that `measure()` agrees with its
arranged geometry.
