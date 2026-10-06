test_that("easing functions start at 0 and end at 1", {
  for (name in names(easings)) {
    f <- easings[[name]]
    expect_equal(f(0), 0, info = name)
    expect_equal(f(1), 1, info = name)
    expect_true(all(diff(f(seq(0, 1, by = 0.1))) >= 0), info = name)
  }
})

test_that("animations interpolate a field over simulated time", {
  pb <- progress_bar(0, id = "pb")
  a <- app(pb)
  pilot <- test_app(a, 20, 1)
  done <- FALSE
  pb$animate("value", 1, duration = 1, easing = "linear", on_complete = function(widget, app) done <<- TRUE)
  pilot$advance(0.5)
  expect_equal(pb$value, 0.5, tolerance = 0.05)
  expect_false(done)
  pilot$advance(0.6)
  expect_identical(pb$value, 1)
  expect_true(done)
  # Integer fields stay integer.
  view <- scroll_view(lapply(1:50, function(i) label(paste(i))))
  a2 <- app(view)
  pilot2 <- test_app(a2, 10, 5)
  animate(view, "offset_y", 20, duration = 0.3)
  pilot2$advance(0.15)
  expect_true(is.integer(view$offset_y))
  pilot2$advance(0.3)
  expect_identical(view$offset_y, 20L)
})

test_that("a new animation of the same field replaces the old one; cancel stops it", {
  pb <- progress_bar(0)
  pilot <- test_app(app(pb), 20, 1)
  first <- pb$animate("value", 1, duration = 1)
  pilot$advance(0.2)
  second <- pb$animate("value", 0, duration = 0.2)
  expect_false(first$active)
  pilot$advance(0.3)
  expect_identical(pb$value, 0)
  third <- pb$animate("value", 1, duration = 1)
  pilot$advance(0.3)
  third$cancel()
  v <- pb$value
  pilot$advance(1)
  expect_identical(pb$value, v)
})

test_that("animations stop when the widget is removed and validate input", {
  pb <- progress_bar(0)
  holder <- vertical(pb)
  pilot <- test_app(app(holder), 20, 2)
  anim <- pb$animate("value", 1, duration = 1)
  pb$remove()
  pilot$advance(0.5)
  expect_false(anim$active)
  expect_error(animate(label("x"), "text", 1), "not attached")
  inp <- input()
  pilot2 <- test_app(app(inp), 20, 3)
  expect_error(animate(inp, "value", 1), "numeric")
  expect_error(animate(inp, "nope", 1), "no field")
  expect_error(animate(progress_bar(0) -> p2, "value", 1), "not attached")
})
