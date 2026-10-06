sp_app <- function(...) {
  left <- vertical(label("LEFT", id = "l"), id = "left")
  right <- vertical(label("RIGHT", id = "r"), id = "right")
  pane <- split_pane(left, right, id = "sp", ...)
  list(pane = pane, left = left, right = right, app = app(pane))
}

test_that("panes share the space with a one-cell divider", {
  s <- sp_app(ratio = 0.5)
  pilot <- test_app(s$app, 21, 4)
  expect_identical(s$left$region$width, 10L)
  expect_identical(s$right$region$width, 10L)
  expect_identical(s$right$region$x, 12L)
  expect_identical(unname(s$pane$sizes), c(10L, 10L))
  text <- pilot$screen_text()
  expect_match(substr(text[[1]], 1, 21), "^LEFT      [\u2502\u2503]RIGHT     $")
  expect_true(all(substr(text, 11, 11) %in% c("\u2502", "\u2503")))
})

test_that("ratio and min_size are respected", {
  s <- sp_app(ratio = 0.25)
  pilot <- test_app(s$app, 41, 3)
  expect_identical(unname(s$pane$sizes), c(10L, 30L))
  s$pane$ratio <- 0
  pilot$step()
  expect_identical(unname(s$pane$sizes), c(3L, 37L))
  s$pane$ratio <- 1
  pilot$step()
  expect_identical(unname(s$pane$sizes), c(37L, 3L))
  expect_error(split_pane(label("a"), label("b"), ratio = 2), "between 0 and 1")
  expect_error(split_pane(label("a"), "b"), "widgets")
})

test_that("dragging the divider moves it and reports the new ratio", {
  s <- sp_app(ratio = 0.5)
  got <- NULL
  s$app$on("splitpane.resized", function(event, app) got <<- event$data$ratio)
  pilot <- test_app(s$app, 41, 4)
  handle_x <- s$pane$children[[2]]$region$x
  expect_identical(handle_x, 21L)
  pilot$drag(c(handle_x, 2), c(11, 2), steps = 4)
  expect_identical(unname(s$pane$sizes), c(10L, 30L))
  expect_equal(got, 0.25, tolerance = 0.03)
  # The drag may leave the pane: positions are clamped by min_size.
  pilot$drag(c(11, 2), c(200, 2), steps = 2)
  expect_identical(unname(s$pane$sizes), c(37L, 3L))
})

test_that("the divider can be moved with the keyboard", {
  s <- sp_app(ratio = 0.5)
  pilot <- test_app(s$app, 21, 3)
  pilot$focus(s$pane$children[[2]])
  pilot$press("left", "left")
  expect_identical(unname(s$pane$sizes), c(8L, 12L))
  pilot$press("right")
  expect_identical(unname(s$pane$sizes), c(9L, 11L))
  pilot$press("shift+left")
  expect_identical(unname(s$pane$sizes), c(3L, 17L))
  pilot$press("end")
  expect_identical(unname(s$pane$sizes), c(17L, 3L))
  pilot$press("home")
  expect_identical(unname(s$pane$sizes), c(3L, 17L))
})

test_that("vertical split stacks the panes", {
  s <- sp_app(direction = "vertical", ratio = 0.5)
  pilot <- test_app(s$app, 10, 9)
  expect_identical(unname(s$pane$sizes), c(4L, 4L))
  text <- pilot$screen_text()
  expect_match(text[[5]], "^[\u2500\u2501]+$")
  expect_match(text[[1]], "^LEFT")
  expect_match(text[[6]], "^RIGHT")
  pilot$drag(c(3, 5), c(3, 3))
  expect_identical(unname(s$pane$sizes), c(3L, 5L))
})

test_that("tiny areas and nested splits do not break", {
  inner <- split_pane(label("a"), label("b"), direction = "vertical")
  outer <- split_pane(label("c"), inner, ratio = 0.3)
  a <- app(outer)
  pilot <- test_app(a, 30, 10)
  for (size in list(c(1, 1), c(2, 2), c(5, 2), c(3, 12), c(80, 30), c(30, 10))) {
    pilot$resize(size[[1]], size[[2]])
    expect_true(all(nchar(pilot$screen_text()) == size[[1]]))
  }
  expect_true(all(unname(outer$sizes) >= 0L))
})

test_that("a hidden pane gives the other one the whole area", {
  s <- sp_app()
  pilot <- test_app(s$app, 20, 3)
  s$right$visible <- FALSE
  pilot$step()
  expect_identical(s$left$region$width, 20L)
})
