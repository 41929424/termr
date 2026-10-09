random_ui <- function() {
  vertical(
    label("Title", id = "title", style = style(bold = TRUE)),
    horizontal(
      vertical(input(id = "in1"), checkbox("Flag", id = "cb"), style = style(height = "auto")),
      vertical(button("Go", id = "go"), label("counter: 0", id = "counter"), style = style(height = "auto"))
    ),
    data_table(data.frame(a = 1:30, b = letters[1:30]), id = "tbl", style = style(height = 6)),
    label("footer \u4e2d\u6587", id = "footer")
  )
}

test_that("incremental repaints match full repaints (randomised)", {
  set.seed(2026)
  a <- app(random_ui())
  pilot <- test_app(a, 40, 16)
  counter <- 0L
  ops <- list(
    function() { counter <<- counter + 1L; a$query_one("#counter")$update(paste("counter:", counter)) },
    function() a$query_one("#title")$update(strrep("T", sample(1:30, 1))),
    function() pilot$type(sample(c("a", "b", "\u4e2d"), 1)),
    function() pilot$press(sample(c("tab", "shift+tab", "down", "up", "space", "left", "right"), 1)),
    function() pilot$hover(sample(1:40, 1), sample(1:16, 1)),
    function() a$notify(paste("note", counter), timeout = 1),
    function() pilot$advance(0.6),
    function() { dlg <- modal(label("dialog")); a$push_screen(dlg) },
    function() if (length(a$screens) > 1) a$pop_screen(),
    function() a$query_one("#footer")$set(style = style(foreground = sample(c("red", "green"), 1)))
  )
  for (i in seq_len(stress_workload(120L, 50L))) {
    sample(ops, 1)[[1]]()
    pilot$step()
    expect_true(a$frame$equals(full_frame(a)), info = paste("step", i))
  }
  stats <- a$frame_stats
  expect_gt(stats[["incremental"]], 10L)
  expect_gt(stats[["full"]], 1L)
})

test_that("a small change repaints only its rows", {
  a <- app(vertical(label("a", id = "a"), label("b", id = "b"), label("c", id = "c")))
  pilot <- test_app(a, 10, 3)
  before <- a$frame_stats[["incremental"]]
  a$query_one("#b")$update("B")
  pilot$step()
  expect_identical(a$frame_stats[["incremental"]], before + 1L)
  expect_identical(pilot$screen_text(), c("a         ", "B         ", "c         "))
  withr::local_options(termr.incremental = FALSE)
  a$query_one("#c")$update("C")
  pilot$step()
  expect_identical(a$frame_stats[["incremental"]], before + 1L)
  expect_identical(pilot$screen_text()[[3]], "C         ")
})

test_that("dirty rectangles are clipped, widened, merged and bounded", {
  w <- function(x, y, wd, h) {
    l <- label("x")
    l$region <- rect(x, y, wd, h)
    pp <- widget_private(l)
    pp$.app <- "attached"
    l
  }
  bounds <- rect(1, 1, 20, 10)
  one <- dirty_rects(list(w(5, 2, 4, 2)), bounds)
  expect_identical(one, list(rect(4, 2, 6, 2)))
  # overlapping rectangles merge, far apart ones do not
  two <- dirty_rects(list(w(5, 2, 4, 2), w(7, 3, 4, 2), w(15, 9, 3, 1)), bounds)
  expect_length(two, 2)
  # clipping to the screen; fully outside is dropped
  clipped <- dirty_rects(list(w(18, 9, 10, 10), w(40, 40, 3, 3)), bounds)
  expect_identical(clipped, list(rect(17, 9, 4, 2)))
  # adjacent rectangles of equal extent merge
  expect_length(merge_rects(list(rect(1, 1, 5, 2), rect(6, 1, 5, 2))), 1)
  expect_length(merge_rects(list(rect(1, 1, 2, 2), rect(15, 8, 2, 2))), 2)
  # too many rectangles collapse into one
  many <- lapply(1:30, function(i) w(1 + (i %% 2) * 15, i %% 9 + 1, 1, 1))
  expect_lte(length(dirty_rects(many, bounds, max_rects = 3)), 3)
})

test_that("repaints report what they painted", {
  a <- app(vertical(label("one", id = "a"), label("two", id = "b")))
  pilot <- test_app(a, 20, 6)
  expect_true(a$last_paint$full)
  expect_identical(a$last_paint$screen_cells, 120)
  a$query_one("#b")$update("TWO")
  pilot$step()
  p <- a$last_paint
  expect_false(p$full)
  expect_identical(p$rects, 1L)
  expect_lt(p$repainted_cells, 40)
  expect_gt(p$ansi_bytes, 0)
})
