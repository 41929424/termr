drag_app <- function() {
  log <- character()
  box <- label("drag me", id = "box", style = style(width = 10, height = 3, border = "round"))
  other <- label("other", id = "other")
  a <- app(horizontal(box, other))
  for (type in c("drag.start", "drag.move", "drag.end", "mouse.down", "mouse.up", "click")) {
    local({
      type <- type
      box$on(type, function(event, app) {
        log <<- c(log, sprintf("%s %d,%d o=%s,%s", type, event$screen_x, event$screen_y,
                               event$origin_x, event$origin_y))
      })
    })
  }
  list(app = a, box = box, other = other, log = function() log, reset = function() log <<- character())
}

test_that("a drag sends start, move and end events to the widget where it began", {
  d <- drag_app()
  pilot <- test_app(d$app, 40, 6)
  pilot$drag(c(3, 2), c(30, 5), steps = 3)
  log <- d$log()
  expect_identical(log[[1]], "mouse.down 3,2 o=NA,NA")
  expect_true("drag.start 12,3 o=3,2" %in% log)
  expect_identical(sum(startsWith(log, "drag.move")), 3L)
  # The pointer left the widget, but it still gets the events.
  expect_true(any(startsWith(log, "drag.move 30,5")))
  expect_true("drag.end 30,5 o=3,2" %in% log)
  expect_true(any(startsWith(log, "mouse.up 30,5")))
  # Released over another widget: no click on the origin.
  expect_false(any(startsWith(log, "click")))
})

test_that("a click without movement is a click, not a drag", {
  d <- drag_app()
  pilot <- test_app(d$app, 40, 6)
  pilot$click(3, 2)
  log <- d$log()
  expect_false(any(startsWith(log, "drag")))
  expect_true(any(startsWith(log, "click")))
})

test_that("moving without a button held is not a drag", {
  d <- drag_app()
  pilot <- test_app(d$app, 40, 6)
  pilot$hover(3, 2)
  pilot$hover(5, 3)
  expect_false(any(startsWith(d$log(), "drag")))
})

test_that("a drag released over its original target also emits click", {
  d <- drag_app()
  pilot <- test_app(d$app, 40, 6)
  on.exit(pilot$stop(), add = TRUE)
  pilot$drag(c(3, 2), c(5, 2), steps = 2)
  expect_identical(tail(sub(" .*", "", d$log()), 3L), c("mouse.up", "drag.end", "click"))
})

test_that("capture_mouse routes events to a widget outside its region until release", {
  d <- drag_app()
  seen <- character()
  d$other$on("mouse.move", function(event, app) seen <<- c(seen, "other-move"))
  d$box$on("mouse.move", function(event, app) seen <<- c(seen, "box-move"))
  pilot <- test_app(d$app, 40, 6)
  d$box$capture_mouse()
  pilot$mouse("move", 30, 3)
  pilot$mouse("move", 31, 3)
  expect_identical(seen, c("box-move", "box-move"))
  d$box$release_mouse()
  pilot$mouse("move", 12, 1)
  expect_identical(tail(seen, 1), "other-move")
})

test_that("a captured widget that is removed releases the capture", {
  d <- drag_app()
  pilot <- test_app(d$app, 40, 6)
  d$box$capture_mouse()
  d$box$remove()
  pilot$step()
  expect_no_error(pilot$mouse("move", 5, 2))
})

test_that("Pilot$wait_for steps simulated time until a condition holds", {
  ticks <- 0L
  a <- app(label("x", id = "x"))
  pilot <- test_app(a, 20, 3)
  a$set_interval(1, function(app) ticks <<- ticks + 1L)
  t0 <- now_seconds()
  pilot$wait_for(function(app) ticks >= 5L, timeout = 10)
  expect_identical(ticks, 5L)
  expect_lt(now_seconds() - t0, 2)
  expect_error(pilot$wait_for(function(app) FALSE, timeout = 1), "not met")
})
