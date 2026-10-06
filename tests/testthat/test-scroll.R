items <- function(n, prefix = "Item") lapply(seq_len(n), function(i) label(paste(prefix, i)))

scroll_pilot <- function(..., width = 20, height = 5) {
  view <- scroll_view(..., id = "view")
  pilot <- test_app(app(view), width, height)
  list(pilot = pilot, view = view)
}

test_that("vertical scrolling shows a window of the content", {
  x <- scroll_pilot(items(20))
  expect_identical(x$view$virtual_size, c(width = 19L, height = 20L))
  expect_identical(substr(x$pilot$screen_text()[[1]], 1, 6), "Item 1")
  x$pilot$press("down", "down", "down")
  expect_identical(x$view$offset_y, 3L)
  expect_identical(substr(x$pilot$screen_text(), 1, 6), paste("Item", 4:8))
})

test_that("a scroll bar shows the position", {
  x <- scroll_pilot(items(20))
  bar <- function() substring(x$pilot$screen_text(), 20, 20)
  expect_identical(bar()[[1]], "\u2588")
  x$pilot$press("end")
  expect_identical(bar()[[5]], "\u2588")
  expect_identical(bar()[[1]], "\u2502")
})

test_that("offsets are clamped to the scrollable range", {
  x <- scroll_pilot(items(20))
  x$view$scroll_to(y = 1000)
  x$pilot$step()
  expect_identical(x$view$offset_y, 15L)
  expect_identical(x$view$max_scroll, c(x = 0L, y = 15L))
  x$view$scroll_by(dy = -100)
  expect_identical(x$view$offset_y, 0L)
  x$pilot$press("up")
  expect_identical(x$view$offset_y, 0L)
})

test_that("page and home/end keys", {
  x <- scroll_pilot(items(20))
  x$pilot$press("pagedown")
  expect_identical(x$view$offset_y, 4L)
  x$pilot$press("pagedown", "pagedown", "pagedown", "pagedown")
  expect_identical(x$view$offset_y, 15L)
  x$pilot$press("pageup")
  expect_identical(x$view$offset_y, 11L)
  x$pilot$press("home")
  expect_identical(x$view$offset_y, 0L)
  x$pilot$press("end")
  expect_identical(x$view$offset_y, 15L)
})

test_that("horizontal scrolling", {
  x <- scroll_pilot(label(paste(LETTERS, collapse = "")), label("short"), direction = "both", width = 10, height = 4)
  expect_identical(x$view$virtual_size[["width"]], 26L)
  expect_identical(x$pilot$screen_text()[[1]], "ABCDEFGHIJ")
  x$pilot$press("right", "right")
  expect_identical(x$pilot$screen_text()[[1]], "CDEFGHIJKL")
  # Horizontal scroll bar on the last row.
  expect_match(x$pilot$screen_text()[[4]], "\u2588")
})

test_that("content is clipped to the viewport", {
  view <- scroll_view(items(10), style = style(height = 3))
  pilot <- test_app(app(vertical(label("top"), view, label("bottom"))), 20, 6)
  expect_identical(substr(pilot$screen_text(), 1, 6), c("top", "Item 1", "Item 2", "Item 3", "bottom", "") |> formatC(width = -6))
  view$scroll_to(y = 5)
  pilot$step()
  expect_identical(substr(pilot$screen_text()[c(1, 5)], 1, 6), c("top   ", "bottom"))
  expect_identical(substr(pilot$screen_text()[2], 1, 6), "Item 6")
})

test_that("focus moving to a hidden child scrolls it into view", {
  buttons <- lapply(1:8, function(i) button(paste("Button", i), id = paste0("b", i)))
  view <- scroll_view(buttons, focusable = FALSE)
  pilot <- test_app(app(view), 30, 7)
  expect_identical(pilot$app$focused$id, "b1")
  for (i in 2:6) {
    pilot$press("tab")
    focused <- pilot$app$focused
    vp <- view$viewport
    expect_true(focused$region$y >= vp$y && rect_bottom(focused$region) <= rect_bottom(vp), info = i)
  }
  expect_gt(view$offset_y, 0L)
  pilot$press("shift+tab", "shift+tab", "shift+tab", "shift+tab", "shift+tab")
  expect_identical(view$offset_y, 0L)
})

test_that("nested containers inside a scroll view", {
  view <- scroll_view(vertical(items(5, "A"), style = style(height = "auto")), horizontal(label("x"), label("y")), items(5, "B"))
  pilot <- test_app(app(view), 20, 4)
  expect_identical(view$virtual_size[["height"]], 11L)
  view$scroll_to(y = 5)
  pilot$step()
  expect_identical(substr(pilot$screen_text()[1], 1, 2), "xy")
})

test_that("resizing and shrinking content keep offsets in range", {
  x <- scroll_pilot(items(20))
  x$pilot$press("end")
  x$pilot$resize(20, 10)
  expect_identical(x$view$offset_y, 10L)
  for (w in x$view$children[11:20]) w$remove()
  x$pilot$step()
  expect_identical(x$view$offset_y, 0L)
  expect_identical(x$view$virtual_size[["height"]], 10L)
})

test_that("scroll offsets always stay within bounds (randomised)", {
  set.seed(42)
  x <- scroll_pilot(items(30), label(strrep("w", 40)), direction = "both", width = 15, height = 6)
  keys <- c("up", "down", "left", "right", "pageup", "pagedown", "home", "end")
  for (i in 1:60) {
    if (runif(1) < 0.2) {
      x$view$scroll_by(sample(-50:50, 1), sample(-50:50, 1))
      x$pilot$step()
    } else {
      x$pilot$press(sample(keys, 1))
    }
    max <- x$view$max_scroll
    expect_true(x$view$offset_x >= 0L && x$view$offset_x <= max[["x"]])
    expect_true(x$view$offset_y >= 0L && x$view$offset_y <= max[["y"]])
  }
})

test_that("scroll changes send a message", {
  got <- NULL
  view <- scroll_view(items(10))
  a <- app(view, on("scroll.changed", function(event, app) got <<- event$data))
  pilot <- test_app(a, 10, 3)
  pilot$press("down")
  expect_identical(got, list(x = 0L, y = 1L))
})
