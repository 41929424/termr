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
