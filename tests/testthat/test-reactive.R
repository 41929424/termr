Counter <- widget(
  "Counter",
  state = list(count = reactive(0L)),
  render = function(self) sprintf("Count: %d", self$count),
  bindings = list(bind("up", "increment"), bind("down", "decrement"), bind("b", "bulk")),
  actions = list(
    increment = function(self) self$count <- self$count + 1L,
    decrement = function(self) self$count <- self$count - 1L,
    bulk = function(self) for (i in 1:50) self$count <- self$count + 1L
  ),
  focusable = TRUE
)

frames <- function(a) a$.__enclos_env__$private$.renderer$frames

test_that("widget() returns a constructor that sets initial state", {
  c1 <- Counter()
  c2 <- Counter(count = 5L, id = "c2")
  expect_s3_class(Counter, "termr_widget_type")
  expect_identical(c1$type, "Counter")
  expect_true(inherits(c1, "Widget"))
  expect_identical(c1$count, 0L)
  expect_identical(c2$count, 5L)
  expect_identical(c2$id, "c2")
  expect_error(Counter(nope = 1), "no state field")
  expect_identical(render_widget(c2, 10, 1)$to_text(), "Count: 5  ")
})

test_that("assigning state invalidates and rerenders the widget", {
  c1 <- Counter()
  expect_identical(c1$render_lines()[[1]][[1]]$text, "Count: 0")
  c1$count <- 3L
  expect_true(widget_private(c1)$.dirty)
  expect_identical(c1$render_lines()[[1]][[1]]$text, "Count: 3")
  # Same value: no invalidation.
  c1$count <- 3L
  expect_false(widget_private(c1)$.dirty)
})

test_that("bindings and actions of the widget type work", {
  counter <- Counter(id = "c")
  pilot <- test_app(app(counter), 20, 1)
  pilot$press("up", "up", "up", "down")
  expect_identical(counter$count, 2L)
  expect_identical(pilot$screen_text(), "Count: 2            ")
})

test_that("many state changes in one tick produce a single repaint", {
  counter <- Counter()
  a <- app(counter)
  pilot <- test_app(a, 20, 1)
  before <- frames(a)
  pilot$press("b")
  expect_identical(counter$count, 50L)
  expect_identical(frames(a) - before, 1L)
  # No change: no repaint.
  pilot$step()
  expect_identical(frames(a) - before, 1L)
})

test_that("only changed cells are sent to the terminal", {
  counter <- Counter()
  a <- app(vertical(label(strrep("static text ", 5)), counter))
  pilot <- test_app(a, 60, 2)
  n <- length(pilot$driver$output)
  pilot$press("up")
  patch <- paste(pilot$driver$output[-seq_len(n)], collapse = "")
  expect_identical(strip_ansi(patch), "1")
})

test_that("watchers run after changes and can post messages", {
  seen <- list()
  Temp <- widget(
    "Temp",
    state = list(celsius = reactive(20, watch = function(self, value, old) {
      self$post_message("temp.changed", list(value = value, old = old))
    })),
    render = function(self) paste0(self$celsius, "C")
  )
  t <- Temp()
  a <- app(t, on("temp.changed", function(event, app) seen[[length(seen) + 1L]] <<- event$data))
  pilot <- test_app(a, 10, 1)
  t$celsius <- 25
  pilot$step()
  expect_identical(seen, list(list(value = 25, old = 20)))
  expect_identical(pilot$screen_text(), "25C       ")
})

test_that("compose, on handlers and methods", {
  log <- character()
  Panel <- widget(
    "Panel",
    state = list(title = "Panel"),
    compose = function(self) list(label(self$title, id = "title"), button("OK", id = "ok")),
    on = list(
      mount = function(self, event) log <<- c(log, "mounted"),
      "button.pressed" = function(self, event) {
        log <<- c(log, paste("pressed", event$sender$id))
        event$stop()
      }
    ),
    methods = list(rename = function(self, title) self$query_one("#title")$update(title)),
    inherit = Vertical
  )
  p <- Panel(title = "Settings")
  a <- app(p, on("button.pressed", function(event, app) log <<- c(log, "app")))
  pilot <- test_app(a, 20, 5)
  expect_identical(p$query_one("#title")$text, "Settings")
  pilot$press("enter")
  p$rename("Options")
  pilot$step()
  expect_identical(log, c("mounted", "pressed ok"))
  expect_match(pilot$screen_text()[[1]], "^Options")
})

test_that("widget() validates its arguments", {
  expect_error(widget("1bad"), "must start with a letter")
  expect_error(widget("W", state = list(1)), "named list")
  expect_error(widget("W", state = list(focus = 1)), "reserved name")
  expect_error(widget("W", actions = list(a = 1)), "must be a function")
  expect_error(widget("W", inherit = list()), "inherit")
  expect_error(widget("W", state = list(x = 1), methods = list(x = function(self) 1)), "both")
})

test_that("widget timers repeat and are cancelled when the widget is removed", {
  Clock <- widget(
    "Clock",
    state = list(ticks = 0L),
    render = function(self) paste("ticks:", self$ticks),
    on = list(mount = function(self, event) {
      self$set_interval(1, function(self) self$ticks <- self$ticks + 1L)
    })
  )
  clock <- Clock()
  a <- app(vertical(clock))
  pilot <- test_app(a, 20, 2)
  pilot$advance(3.5)
  expect_identical(clock$ticks, 3L)
  expect_identical(pilot$screen_text()[[1]], "ticks: 3            ")
  clock$remove()
  pilot$advance(3)
  expect_identical(clock$ticks, 3L)
})

test_that("set_timeout and set_interval use the running app", {
  expect_error(set_timeout(1, function() NULL, app = NULL), "No app is running")
  log <- character()
  a <- app(label("x"))
  pilot <- test_app(a, 10, 1)
  expect_identical(current_app(), a)
  set_timeout(2, function(app) log <<- c(log, "timeout"))
  iv <- set_interval(1, function(app) log <<- c(log, "tick"))
  pilot$advance(1)
  pilot$advance(1)
  iv$cancel()
  pilot$advance(5)
  # At t = 2 both are due; they fire in creation order.
  expect_identical(log, c("tick", "timeout", "tick"))
  pilot$stop()
  expect_false(identical(current_app(), a))
})

test_that("timers created before the app starts count from the start", {
  log <- character()
  a <- app(label("x"))
  a$set_timeout(1, function(app) log <<- c(log, "late"))
  pilot <- test_app(a, 10, 1)
  pilot$advance(0.5)
  expect_identical(log, character())
  pilot$advance(0.6)
  expect_identical(log, "late")
})
