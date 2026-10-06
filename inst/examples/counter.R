# Phase 5 demo: custom widgets with reactive state, messages and timers.
#
# Two counters (Tab switches between them): Up/+ and Down/- change the
# focused counter, r resets it, b adds 100 in one go (100 state changes,
# one repaint). A clock and the total update by themselves. q quits.
#
#   Rscript -e 'termr::run_example("counter")'

library(termr)

Counter <- widget(
  "Counter",
  state = list(
    count = reactive(0L, watch = function(self, value, old) {
      self$post_message("counter.changed", list(value = value, delta = value - old))
    })
  ),
  render = function(self) {
    colour <- if (self$count < 0) "bright_red" else "bright_green"
    c(span("Count: "), span(format(self$count, width = 5), style(foreground = colour, bold = TRUE)))
  },
  bindings = list(
    bind("up,+", "increment", "Increment"),
    bind("down,-", "decrement", "Decrement"),
    bind("r", "reset", "Reset"),
    bind("b", "bulk", "Add 100")
  ),
  actions = list(
    increment = function(self) self$count <- self$count + 1L,
    decrement = function(self) self$count <- self$count - 1L,
    reset = function(self) self$count <- 0L,
    bulk = function(self) for (i in 1:100) self$count <- self$count + 1L
  ),
  style = style(
    width = 22, border = "round", padding = c(0, 1), border_color = "bright_black",
    focus = style(border_color = "bright_cyan")
  ),
  focusable = TRUE
)

ui <- vertical(
  label("termr counter demo", style = style(bold = TRUE, foreground = "bright_cyan")),
  label("Up/Down: change  r: reset  b: +100  Tab: switch  q: quit",
        style = style(foreground = "bright_black")),
  horizontal(
    Counter(id = "left"),
    Counter(id = "right", count = 10L),
    style = style(margin = c(1, 0))
  ),
  label("", id = "total", style = style(bold = TRUE)),
  label("", id = "clock", style = style(foreground = "bright_black")),
  style = style(padding = c(1, 2))
)

show_total <- function(app) {
  total <- sum(vapply(app$query("Counter"), function(w) w$count, integer(1)))
  app$query_one("#total")$update(sprintf("Total: %d", total))
}

counter_app <- app(
  ui,
  on("mount", "#total", function(event, app) show_total(app)),
  on("counter.changed", function(event, app) show_total(app)),
  bind("q", "quit")
)

tick <- function(app) {
  app$query_one("#clock")$update(paste("Time:", format(Sys.time(), "%H:%M:%S")))
}
counter_app$set_interval(1, tick)
counter_app$call_later(tick)

run(counter_app)
