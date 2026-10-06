test_that("visible ranges have half-open boundaries and skip empty extents", {
  expect_identical(visible_range(c(0, 2, 2, 5), c(2, 2, 5, 9), 2, 5), 3L)
  expect_identical(visible_range(c(0, 2), c(2, 5), 5, 8), integer())
  expect_identical(visible_range(numeric(), numeric(), 0, 10), integer())
})

test_that("indexed direct and nested scroll layouts match the original algorithm", {
  for (nested in c(FALSE, TRUE)) {
    set.seed(104)
    kids <- lapply(1:160, function(i) label(paste(rep(paste("row", i), 1 + i %% 5), collapse = " "),
                  style = style(wrap = "word", margin = c(i %% 2, 0, 0, 0))))
    content <- if (nested) vertical(kids) else kids
    view <- scroll_view(content)
    pilot <- test_app(app(view), 22, 8)
    for (i in 1:16) {
      view$scroll_to(y = sample.int(max(1, view$max_scroll[["y"]]), 1) - 1)
      if (i %% 4 == 0) pilot$resize(sample(14:30, 1), sample(4:12, 1))
      if (i == 5) kids[[20]]$update("changed\nheight\nthree")
      if (i == 7) kids[[8]]$visible <- FALSE
      if (i == 9) kids[[8]]$visible <- TRUE
      if (i == 11) kids[[9]]$remove()
      pilot$step()
      actual <- pilot$app$frame$copy()
      expected <- withr::with_options(list(termr.indexed_layout = FALSE), full_frame(pilot$app))
      expect_true(actual$equals(expected), info = paste(nested, i))
    }
    pilot$stop()
  }
})

test_that("scrolling reuses measurements and visits only the viewport", {
  calls <- 0L
  Probe <- R6::R6Class("IndexProbe", inherit = Label, public = list(
    content_height = function(width) { calls <<- calls + 1L; super$content_height(width) }
  ))
  children <- lapply(1:1000, function(i) Probe$new(paste("row", i)))
  content <- vertical(children)
  view <- scroll_view(content)
  pilot <- test_app(app(view), 20, 6)
  before <- calls
  for (i in 1:5) {
    view$scroll_by(dy = 1)
    pilot$step()
    expect_lte(length(render_children(content)), 10L)
    expect_lte(length(layout_snapshot(list(pilot$app$screen))$widgets), 13L)
  }
  expect_identical(calls, before)
  children[[900]]$update("new\nheight")
  pilot$step()
  expect_gt(calls, before)
  expect_identical(view$virtual_size[["height"]], 1001L)
  view$scroll_end()
  pilot$step()
  expect_match(pilot$screen_text()[[6]], "row 1000")
  expect_identical(hit_test(pilot$app$screen, 1, 6), children[[1000]])
  pilot$stop()
})

test_that("offscreen focus, zero-height children and layout state styles stay correct", {
  kids <- lapply(1:100, function(i) button(paste("b", i), style = style(height = if (i %% 5) 3 else 0)))
  view <- scroll_view(kids, focusable = FALSE)
  pilot <- test_app(app(view), 25, 8)
  kids[[98]]$focus()
  pilot$step()
  expect_true(rect_contains(view$viewport, kids[[98]]$region$x, kids[[98]]$region$y))
  expect_true(pilot$app$frame$equals(withr::with_options(list(termr.indexed_layout = FALSE), full_frame(pilot$app))))
  pilot$stop()
})
