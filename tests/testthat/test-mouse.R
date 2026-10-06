mouse_keys <- function(events) vapply(events, function(e) paste(e$type, e$button, e$screen_x, e$screen_y, e$direction), "")

test_that("SGR mouse reports are parsed", {
  p <- KeyParser$new()
  evs <- p$feed(paste0("\033[<0;5;3M", "\033[<0;5;3m", "\033[<32;6;3M", "\033[<64;2;2M", "\033[<65;2;2M", "\033[<18;1;1M", "x"))
  expect_identical(mouse_keys(evs[1:5]), c(
    "mouse.down left 5 3 NA", "mouse.up left 5 3 NA", "mouse.move left 6 3 NA",
    "mouse.scroll none 2 2 up", "mouse.scroll none 2 2 down"
  ))
  expect_true(evs[[6]]$ctrl)
  expect_identical(evs[[6]]$button, "right")
  expect_identical(evs[[7]]$key, "x")
})

test_that("Windows mouse records become press, release, move and wheel events", {
  down <- windows_mouse_events(5L, 3L, 1L, 0L, 0L, previous = 0L)
  expect_identical(mouse_keys(down), "mouse.down left 5 3 NA")
  up <- windows_mouse_events(5L, 3L, 0L, 0L, 2L, previous = 1L)
  expect_identical(mouse_keys(up), "mouse.up left 5 3 NA")
  expect_true(up[[1]]$shift)
  move <- windows_mouse_events(6L, 3L, 1L, 1L, 0L, previous = 1L)
  expect_identical(mouse_keys(move), "mouse.move left 6 3 NA")
  wheel_up <- windows_mouse_events(1L, 1L, 7864320, 4L, 0L, previous = 0L)
  expect_identical(wheel_up[[1]]$direction, "up")
  wheel_down <- windows_mouse_events(1L, 1L, 4287102976, 4L, 0L, previous = 0L)
  expect_identical(wheel_down[[1]]$direction, "down")
})

test_that("hit testing finds the deepest visible widget", {
  a <- label("aaa", id = "a")
  b <- button("B", id = "b")
  view <- scroll_view(lapply(1:10, function(i) label(paste("row", i), id = paste0("r", i))), style = style(height = 3))
  ui <- vertical(a, b, view)
  pilot <- test_app(app(ui), 20, 10)
  expect_identical(hit_test(pilot$app$screen, 1, 1)$id, "a")
  expect_identical(hit_test(pilot$app$screen, 3, 3)$id, "b")
  expect_identical(hit_test(pilot$app$screen, 1, 5)$id, "r1")
  # Rows scrolled out of the viewport cannot be hit.
  expect_null(hit_test(pilot$app$screen, 30, 1))
  hit <- hit_test(pilot$app$screen, 1, 8)
  expect_false(identical(hit$id, "r4"))
})

test_that("clicking a button presses and focuses it", {
  pressed <- character()
  a <- app(vertical(button("One", id = "one"), button("Two", id = "two")),
           on("button.pressed", function(event, app) pressed <<- c(pressed, event$sender$id)))
  pilot <- test_app(a, 20, 6)
  pilot$click("#two")
  expect_identical(pressed, "two")
  expect_identical(a$focused$id, "two")
  # Right clicks do not press.
  pilot$click("#one", button = "right")
  expect_identical(pressed, "two")
})

test_that("mouse events carry widget-relative coordinates and bubble", {
  got <- list()
  inner <- label("hello", id = "inner")
  outer <- vertical(label("top"), inner, id = "outer")
  outer$on("mouse.down", function(event, app) got[[length(got) + 1L]] <<- c(event$x, event$y, event$screen_x, event$screen_y))
  pilot <- test_app(app(outer), 20, 4)
  pilot$mouse("down", 3, 2, button = "left")
  expect_identical(got, list(c(3L, 1L, 3L, 2L)))
  expect_identical(pilot$app$focused, NULL)
})

test_that("click needs down and up on the same widget", {
  clicks <- 0L
  a <- app(vertical(label("a", id = "a"), label("b", id = "b")),
           on("click", function(event, app) clicks <<- clicks + 1L))
  pilot <- test_app(a, 10, 2)
  pilot$mouse("down", 1, 1, button = "left")
  pilot$mouse("up", 1, 2, button = "left")
  expect_identical(clicks, 0L)
  pilot$click("#b")
  expect_identical(clicks, 1L)
})

test_that("disabled widgets do not receive mouse events", {
  pressed <- 0L
  a <- app(vertical(button("No", id = "no", disabled = TRUE)),
           on("button.pressed", function(event, app) pressed <<- pressed + 1L))
  pilot <- test_app(a, 20, 4)
  pilot$click("#no")
  expect_identical(pressed, 0L)
  expect_null(a$focused)
})

test_that("the mouse wheel scrolls scroll views, innermost first", {
  view <- scroll_view(lapply(1:30, function(i) label(paste("row", i))), id = "view")
  pilot <- test_app(app(view), 20, 5)
  pilot$scroll("#view", "down", times = 2)
  expect_identical(view$offset_y, 6L)
  pilot$scroll("#view", "up")
  expect_identical(view$offset_y, 3L)
})

test_that("clicking an input moves its cursor", {
  inp <- input(value = "abcdef", id = "in")
  pilot <- test_app(app(inp), 20, 3)
  # Content starts at column 3 (border + padding).
  pilot$mouse("down", 5, 2, button = "left")
  expect_identical(inp$cursor_position, 2L)
  pilot$mouse("down", 18, 2, button = "left")
  expect_identical(inp$cursor_position, 6L)
})

test_that("hover state follows the pointer", {
  b <- button("Hover me", id = "b", style = style(hover = style(background = "red")))
  a <- app(vertical(b, label("x", id = "x")))
  pilot <- test_app(a, 20, 5)
  pilot$hover("#b")
  expect_true(b$hovered)
  expect_true("hover" %in% b$pseudo_states())
  expect_identical(pilot$driver$terminal$screen$get_cell(3, 2)$bg, "red")
  pilot$hover("#x")
  expect_false(b$hovered)
  expect_identical(pilot$driver$terminal$screen$get_cell(3, 2)$bg, "")
})

test_that("mouse reporting is enabled while running and disabled on exit", {
  pilot <- test_app(app(label("x")), 10, 2)
  expect_true(all(c(1000L, 1006L) %in% pilot$driver$terminal$modes))
  pilot$stop()
  expect_false(any(c(1000L, 1002L, 1003L, 1006L) %in% pilot$driver$terminal$modes))
  pilot2 <- test_app(app(label("x"), mouse = FALSE), 10, 2)
  expect_false(1000L %in% pilot2$driver$terminal$modes)
})

test_that("clicking an invisible widget is an error", {
  view <- scroll_view(lapply(1:20, function(i) button(paste("b", i), id = paste0("b", i))), style = style(height = 4))
  pilot <- test_app(app(view), 20, 6)
  expect_error(pilot$click("#b10"), "not visible")
})
