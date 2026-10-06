test_that("app() collects widgets, handlers and bindings", {
  a <- app(
    label("hi", id = "greeting"),
    on("key", function(event, app) NULL),
    bind("q", "quit")
  )
  expect_s3_class(a, "App")
  expect_identical(a$query_one("#greeting")$app, a)
  expect_true(any(vapply(a$bindings(), function(b) "q" %in% b$keys, logical(1))))
  expect_error(app(42), "accepts widgets")
  expect_error(app(actions = list(function(app) NULL)), "named list")
})

test_that("run() exits on a binding and restores the terminal", {
  driver <- HeadlessDriver$new(30, 5)
  driver$press("a", "b", "q")
  seen <- character()
  a <- app(
    label("press q"),
    on("key", function(event, app) seen <<- c(seen, event$key)),
    bind("q", function(app) app$exit("done"))
  )
  value <- run(a, driver = driver)
  expect_identical(value, "done")
  expect_identical(seen, c("a", "b", "q"))
  expect_false(a$running)
  vt <- driver$terminal
  expect_false(vt$alt_screen)
  expect_true(vt$cursor_visible)
  expect_true(vt$autowrap)
  # While running, the app drew into the alternate screen.
  expect_match(paste(driver$output, collapse = ""), "press q", fixed = TRUE)
})

test_that("the terminal is restored when a handler fails", {
  driver <- HeadlessDriver$new(20, 3)
  driver$press("x")
  a <- app(label("x"), on("key", function(event, app) stop("boom")))
  expect_error(run(a, driver = driver), "boom")
  expect_false(driver$started)
  expect_false(driver$terminal$alt_screen)
  expect_true(driver$terminal$cursor_visible)
  expect_false(a$running)
})

test_that("a headless app waiting forever reports missing input", {
  driver <- HeadlessDriver$new(20, 3)
  driver$max_idle <- 2
  expect_error(run(app(label("x")), driver = driver), "waiting for input")
  expect_false(driver$terminal$alt_screen)
})

test_that("events bubble from the target to the app and can be stopped", {
  log <- character()
  inner <- label("inner", id = "inner")
  middle <- vertical(inner, id = "middle")
  a <- app(middle, on("ping", function(event, app) log <<- c(log, "app")))
  middle$on("ping", function(event, app) log <<- c(log, paste("middle saw", event$sender$id)))
  inner$on("ping", function(event) log <<- c(log, "inner"))
  pilot <- test_app(a, 20, 3)
  inner$post_message("ping")
  pilot$step()
  expect_identical(log, c("inner", "middle saw inner", "app"))

  log <- character()
  middle$on("ping", function(event) event$stop())
  inner$post_message("ping")
  pilot$step()
  expect_identical(log, c("inner", "middle saw inner"))
})

test_that("app handlers filter by selector and message data is delivered", {
  got <- list()
  a <- app(
    label("a", id = "a"), label("b", id = "b", classes = "special"),
    on("custom", ".special", function(event, app) got[[length(got) + 1L]] <<- event$data$n)
  )
  pilot <- test_app(a, 10, 2)
  a$query_one("#a")$post_message("custom", list(n = 1))
  a$query_one("#b")$post_message("custom", list(n = 2))
  pilot$step()
  expect_identical(got, list(2))
})

test_that("on_<type> methods handle events on the widget", {
  Pinger <- R6::R6Class("Pinger", inherit = Label, public = list(
    pings = 0L,
    on_ping = function(event) self$pings <- self$pings + 1L,
    on_mount = function(event) self$update("mounted")
  ))
  w <- Pinger$new("x")
  pilot <- test_app(app(w), 10, 1)
  expect_identical(pilot$screen_text(), "mounted   ")
  w$post_message("ping")
  pilot$step()
  expect_identical(w$pings, 1L)
})

test_that("more local bindings win over app bindings", {
  log <- character()
  w <- label("w")
  w$bind("x", function(self, app) log <<- c(log, "widget"))
  a <- app(vertical(w), bind("x", function(app) log <<- c(log, "app")), bind("y", function(app) log <<- c(log, "app-y")))
  pilot <- test_app(a, 10, 2)
  # Key events go to the focused widget (none here), i.e. the screen.
  w$focusable <- TRUE
  w$focus()
  pilot$press("x", "y")
  expect_identical(log, c("widget", "app-y"))
})

test_that("named actions are resolved on widgets, then the app", {
  log <- character()
  Box <- R6::R6Class("Box", inherit = Vertical, public = list(
    action_hello = function() log <<- c(log, "box hello")
  ))
  inner <- label("i")
  inner$focusable <- TRUE
  inner$bind("h", "hello")
  inner$bind("g", "greet")
  inner$bind("Q", "app.quit")
  a <- app(Box$new(inner), actions = list(greet = function(app) log <<- c(log, "app greet")))
  pilot <- test_app(a, 10, 2)
  inner$focus()
  pilot$press("h", "g")
  expect_identical(log, c("box hello", "app greet"))
  expect_error(a$run_action("nope"), "Unknown action")
  pilot$press("Q")
  expect_true(pilot$exited)
})

test_that("resize events update the size and redraw everything", {
  sizes <- list()
  a <- app(label("hello"), on("resize", function(event, app) sizes[[length(sizes) + 1L]] <<- c(event$width, event$height)))
  pilot <- test_app(a, 20, 3)
  pilot$resize(10, 2)
  expect_identical(sizes, list(c(10L, 2L)))
  expect_identical(a$size, c(width = 10L, height = 2L))
  expect_identical(pilot$screen_text(), c("hello     ", "          "))
})

test_that("ctrl+c quits by default and exit() returns a value", {
  pilot <- test_app(app(label("x")), 10, 1)
  pilot$press("ctrl+c")
  expect_true(pilot$exited)
  expect_error(pilot$press("a"), "has exited")

  pilot2 <- test_app(app(label("x"), bind("enter", function(app) app$exit(42))), 10, 1)
  pilot2$press("enter")
  expect_identical(pilot2$value, 42)
})

test_that("call_later runs callbacks on the next tick", {
  a <- app(label("x"))
  pilot <- test_app(a, 10, 1)
  ran <- FALSE
  a$call_later(function(app) ran <<- TRUE)
  expect_false(ran)
  pilot$step()
  expect_true(ran)
})

test_that("run() refuses environments without a terminal", {
  skip_if(isatty(stdout()))
  expect_error(run(app(label("x"))), "terminal")
})
