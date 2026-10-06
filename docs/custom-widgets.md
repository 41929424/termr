# Custom widgets and reactive state

## `widget()`

```r
Counter <- widget(
  "Counter",
  state = list(
    count = reactive(0L, watch = function(self, value, old) {
      self$post_message("counter.changed", list(value = value))
    })
  ),
  render = function(self) sprintf("Count: %d", self$count),
  bindings = list(bind("up", "increment", "Increment"), bind("down", "decrement")),
  actions = list(
    increment = function(self) self$count <- self$count + 1L,
    decrement = function(self) self$count <- self$count - 1L
  ),
  on = list(mount = function(self, event) self$set_interval(1, function(self) NULL)),
  methods = list(reset = function(self) self$count <- 0L),
  style = style(border = "round", width = 20),
  focusable = TRUE
)

c1 <- Counter(count = 5L, id = "c1")
```

* `state`: reactive fields, read and assigned as `self$count`.
* `render(self)`: content (string, lines or `span()`s).
* `compose(self)`: child widgets (use `inherit = Vertical` for containers).
* `bindings`, `actions`, `on` (event handlers `function(self, event)`),
  `methods`.

## Reactivity and repaints

Assigning a different value invalidates the widget and schedules a
repaint. All changes in one event-loop tick produce one repaint, and when
the layout did not change only the rows of the changed widgets are
repainted. Values that are not reactive need `widget$refresh()`.

## Animations

```r
animate(app$query_one("#progress"), "value", to = 1, duration = 0.5, easing = "out_cubic")
counter$animate("count", 100L, duration = 2)
```

## Subclassing

Advanced widgets can subclass `Widget` with R6 and override
`default_style()`, `default_bindings()`, `render()` /
`render_lines(width)`, `paint(buffer, area, st)`, `content_width()`,
`content_height(width)`, `arrange_children()` and `child_clip()`.
