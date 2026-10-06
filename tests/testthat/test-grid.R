reg <- function(w) unname(unlist(unclass(w$region)))

test_that("grid places children row by row in fr columns", {
  kids <- lapply(1:5, function(i) label(paste0("c", i)))
  g <- grid_layout(kids, columns = c("1fr", "2fr", "1fr"))
  layout_tree(g, rect(1, 1, 40, 10))
  expect_identical(reg(kids[[1]]), c(1L, 1L, 10L, 1L))
  expect_identical(reg(kids[[2]]), c(11L, 1L, 20L, 1L))
  expect_identical(reg(kids[[3]]), c(31L, 1L, 10L, 1L))
  expect_identical(reg(kids[[4]]), c(1L, 2L, 10L, 1L))
})

test_that("fixed, auto and percent tracks, rows and gaps", {
  a <- label("aaaaaa")
  b <- label("b")
  c <- label("c\nc\nc")
  d <- label("d")
  g <- grid_layout(a, b, c, d, columns = c("auto", 5, "50%"), rows = c(2, "1fr"), gap = c(1, 2))
  layout_tree(g, rect(1, 1, 30, 10))
  expect_identical(reg(a), c(1L, 1L, 6L, 2L))
  expect_identical(reg(b), c(9L, 1L, 5L, 2L))
  expect_identical(reg(c), c(16L, 1L, 13L, 2L)) # 50% of (30 - 2 gaps * 2) = 13
  expect_identical(reg(d), c(1L, 4L, 6L, 7L)) # fr row takes the rest after the gap
})

test_that("column and row spans", {
  a <- label("a", style = style(column_span = 2))
  b <- label("b", style = style(row_span = 2))
  c <- label("c")
  d <- label("d")
  g <- grid_layout(a, b, c, d, columns = 3, rows = c(1, 1))
  layout_tree(g, rect(1, 1, 30, 2))
  expect_identical(reg(a), c(1L, 1L, 20L, 1L))
  expect_identical(reg(b), c(21L, 1L, 10L, 2L))
  expect_identical(reg(c), c(1L, 2L, 10L, 1L))
  expect_identical(reg(d), c(11L, 2L, 10L, 1L))
  wide <- label("w", style = style(column_span = 9))
  g2 <- grid_layout(wide, columns = 2)
  layout_tree(g2, rect(1, 1, 10, 1))
  expect_identical(wide$region$width, 10L)
})

test_that("auto-height grids measure their rows", {
  g <- grid_layout(label("a\nb"), label("c"), label("d"), columns = 2, style = style(height = "auto"))
  ui <- vertical(g, label("below", id = "below"))
  layout_tree(ui, rect(1, 1, 20, 10))
  expect_identical(g$region$height, 3L)
  expect_identical(ui$query_one("#below")$region$y, 4L)
  expect_identical(natural_width(grid_layout(label("abc"), label("de"), columns = c("auto", "auto"), style = style(width = "auto"))), 5L)
})

test_that("grid invariants hold for random grids", {
  set.seed(7)
  for (trial in 1:40) {
    ncol <- sample(1:4, 1)
    n <- sample(1:10, 1)
    kids <- lapply(seq_len(n), function(i) {
      label(strrep("x", sample(1:6, 1)), style = style(column_span = sample(1:2, 1)))
    })
    tracks <- sample(c("1fr", "2fr", "auto", "3", "25%"), ncol, replace = TRUE)
    W <- sample(10:60, 1)
    H <- sample(5:30, 1)
    gap <- sample(0:2, 1)
    g <- grid_layout(kids, columns = tracks, gap = gap)
    layout_tree(g, rect(1, 1, W, H))
    plan <- grid_plan(kids, W, H, g$computed_style())
    if (any(grepl("fr", tracks))) {
      # fr tracks absorb exactly the remaining width (when it is positive).
      fixed <- sum(plan$cols[!grepl("fr", tracks)])
      if (fixed + gap * (ncol - 1L) <= W) expect_identical(sum(plan$cols) + gap * (ncol - 1L), as.integer(W))
    }
    for (k in kids) {
      expect_true(k$region$width >= 0L && k$region$height >= 0L)
      expect_true(k$region$x >= 1L)
      expect_true(k$region$y >= 1L)
    }
  }
})

test_that("grid styles are validated", {
  expect_error(style(grid_columns = list()), "grid_columns")
  expect_error(style(grid_gap = c(1, 2, 3)), "grid_gap")
  expect_error(style(column_span = 0), "column_span")
  expect_error(grid_layout(columns = "wide"), "Invalid")
  expect_match(format(style(grid_columns = c("1fr", "auto"))), "1fr auto")
})

test_that("grid renders", {
  ui <- grid_layout(
    label("a", style = style(border = "round")), label("b", style = style(border = "round")),
    label("wide", style = style(column_span = 2, align = "center")),
    columns = 2, gap = c(0, 1), style = style(height = "auto")
  )
  expect_snapshot(print(render_widget(ui, 21, 4)))
})
