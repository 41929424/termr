table_pilot <- function(data, ..., width = 60, height = 8) {
  tbl <- data_table(data, id = "t", ...)
  pilot <- test_app(app(tbl), width, height)
  list(pilot = pilot, tbl = tbl)
}

small <- data.frame(
  name = c("alpha", "beta", "gamma", "delta"),
  score = c(1.5, 22.25, NA, 4),
  n = c(10L, 2L, 300L, NA),
  ok = c(TRUE, FALSE, TRUE, NA),
  stringsAsFactors = FALSE
)

test_that("data_table renders a header and formatted rows", {
  x <- table_pilot(small, width = 30, height = 6)
  expect_snapshot(x$pilot$snapshot())
  text <- x$pilot$screen_text()
  expect_match(text[[1]], "^name +score +n +ok")
  # Numbers are right aligned with a common number of decimals.
  expect_match(text[[2]], "alpha +1\\.50 +10 TRUE")
  expect_match(text[[4]], "gamma +NA +300 TRUE")
})

test_that("row names are shown when they are meaningful", {
  buf <- render_widget(data_table(mtcars[1:3, 1:3]), 40, 4)
  expect_match(buf$to_text()[[2]], "^Mazda RX4 +21\\.0")
  buf2 <- render_widget(data_table(mtcars[1:3, 1:3], row_names = FALSE), 40, 4)
  expect_match(buf2$to_text()[[2]], "^ *21\\.0")
  buf3 <- render_widget(data_table(small[1:2, ], row_names = TRUE), 40, 3)
  expect_match(buf3$to_text()[[2]], "^1 alpha")
})

test_that("keyboard navigation moves the cursor and sends messages", {
  got <- list()
  tbl <- data_table(small, id = "t")
  a <- app(tbl, on("datatable.row_selected", function(event, app) got[[length(got) + 1L]] <<- event$data))
  pilot <- test_app(a, 40, 6)
  expect_identical(tbl$cursor_row, 1L)
  pilot$press("down", "down")
  expect_identical(tbl$cursor_row, 3L)
  expect_identical(got[[2]]$row, 3L)
  expect_identical(got[[2]]$value$name, "gamma")
  pilot$press("end")
  expect_identical(tbl$cursor_row, 4L)
  pilot$press("up", "home")
  expect_identical(tbl$cursor_row, 1L)
  pilot$press("up")
  expect_identical(tbl$cursor_row, 1L)
  # The cursor row is highlighted.
  expect_identical(pilot$driver$terminal$screen$get_cell(1, 2)$bg, "blue")
})

test_that("enter activates the row", {
  got <- NULL
  tbl <- data_table(small)
  a <- app(tbl, on("datatable.row_activated", function(event, app) got <<- event$data))
  pilot <- test_app(a, 40, 6)
  pilot$press("down", "enter")
  expect_identical(got$row, 2L)
  expect_identical(got$value$score, 22.25)
})

test_that("cell cursor moves between columns", {
  got <- NULL
  tbl <- data_table(small, cursor = "cell")
  a <- app(tbl, on("datatable.cell_selected", function(event, app) got <<- event$data))
  pilot <- test_app(a, 40, 6)
  pilot$press("right", "down")
  expect_identical(c(tbl$cursor_row, tbl$cursor_column), c(2L, 2L))
  expect_identical(got$column, "score")
  expect_identical(got$value, 22.25)
  pilot$press("left", "left")
  expect_identical(tbl$cursor_column, 1L)
})

test_that("long tables scroll and keep the cursor visible", {
  df <- data.frame(i = 1:100, sq = (1:100)^2)
  x <- table_pilot(df, width = 20, height = 6) # header + 5 rows
  x$pilot$press("pagedown")
  expect_identical(x$tbl$cursor_row, 5L)
  x$pilot$press("pagedown")
  expect_identical(x$tbl$cursor_row, 9L)
  expect_identical(x$tbl$offset_row, 4L)
  expect_match(x$pilot$screen_text()[[2]], "^ +5 ")
  x$pilot$press("end")
  expect_identical(x$tbl$offset_row, 95L)
  expect_match(x$pilot$screen_text()[[6]], "^100 ")
})

test_that("wide tables scroll horizontally", {
  df <- as.data.frame(matrix(1:60, nrow = 3, dimnames = list(NULL, paste0("column_", 1:20))))
  x <- table_pilot(df, width = 30, height = 6)
  expect_match(x$pilot$screen_text()[[1]], "^column_1 column_2")
  x$pilot$press("right", "right")
  expect_identical(x$tbl$offset_column, 2L)
  expect_match(x$pilot$screen_text()[[1]], "^column_3")
  for (i in 1:30) x$pilot$press("right")
  expect_match(x$pilot$screen_text()[[1]], "column_20 *$")
})

test_that("the cell cursor scrolls columns into view", {
  df <- as.data.frame(matrix(1:60, nrow = 3, dimnames = list(NULL, paste0("column_", 1:20))))
  x <- table_pilot(df, cursor = "cell", width = 30, height = 6)
  for (i in 1:9) x$pilot$press("right")
  expect_identical(x$tbl$cursor_column, 10L)
  expect_match(x$pilot$screen_text()[[1]], "column_10")
})

test_that("only visible rows are formatted (virtualisation)", {
  n <- 1e6
  big <- data.frame(id = seq_len(n), value = seq_len(n) / 7)
  sizes <- integer()
  fmt <- function(x) {
    sizes <<- c(sizes, length(x))
    sprintf("%.3f", x)
  }
  tbl <- data_table(big, formatters = list(value = fmt))
  expect_identical(max(sizes), 1000L) # width estimate from a sample
  sizes <- integer()
  pilot <- test_app(app(tbl), 40, 12)
  pilot$press("pagedown", "end", "up")
  expect_true(all(sizes <= 11L))
  expect_identical(tbl$selected_row(), as.integer(n - 1))
  expect_match(pilot$screen_text()[[11]], "999999")
})

test_that("sort and filter change the view and keep the selection", {
  tbl <- data_table(small, id = "t")
  pilot <- test_app(app(tbl), 40, 6)
  pilot$press("down") # beta
  tbl$sort("score", decreasing = TRUE)
  pilot$step()
  expect_identical(tbl$view, c(2L, 4L, 1L, 3L))
  expect_identical(tbl$selected_row(), 2L)
  expect_identical(tbl$cursor_row, 1L)
  expect_match(pilot$screen_text()[[2]], "^beta")
  tbl$filter(small$n > 5)
  pilot$step()
  expect_identical(tbl$view, c(1L, 3L))
  expect_identical(tbl$row_count, 2L)
  tbl$filter(NULL)
  expect_identical(tbl$row_count, 4L)
  expect_error(tbl$sort("nope"), "Unknown table column")
  expect_error(tbl$filter(c(TRUE, FALSE)), "one value per row")
  expect_error(tbl$filter(99), "out of range")
})

test_that("mouse selects rows, reports header clicks and scrolls", {
  got <- NULL
  df <- data.frame(i = 1:50, x = letters[(0:49 %% 26) + 1])
  tbl <- data_table(df, id = "t")
  a <- app(tbl, on("datatable.header_selected", function(event, app) got <<- event$data))
  pilot <- test_app(a, 20, 6)
  pilot$mouse("down", 2, 4, button = "left")
  expect_identical(tbl$cursor_row, 3L)
  pilot$mouse("down", 4, 1, button = "left")
  expect_identical(got$column, "x")
  pilot$mouse("scroll", 2, 3, direction = "down")
  expect_identical(tbl$offset_row, 3L)
})

test_that("column options, formatters and style hooks", {
  tbl <- data_table(
    small,
    columns = list(name = column(label = "Name", width = 4), score = column(align = "left")),
    formatters = list(ok = function(x) ifelse(is.na(x), "?", ifelse(x, "yes", "no"))),
    zebra = TRUE,
    cell_style = function(value, row, column) if (identical(column, "n") && isTRUE(value > 100)) style(foreground = "red")
  )
  pilot <- test_app(app(tbl), 40, 6)
  text <- pilot$screen_text()
  expect_match(text[[1]], "^Name score")
  expect_match(text[[2]], "^alp\u2026 1\\.50")
  expect_match(text[[4]], "yes")
  screen <- pilot$driver$terminal$screen
  col_n <- regexpr("300", text[[4]])
  expect_identical(screen$get_cell(col_n, 4)$fg, "red")
  expect_identical(screen$get_cell(1, 3)$bg, "#262626") # zebra on the second row
  expect_error(data_table(small, columns = list(nope = column())), "Unknown column")
  expect_error(data_table(small, formatters = list(score = function(x) 1)) |> render_widget(30, 4), "must return one string")
})

test_that("matrices, empty tables and replacing data", {
  m <- matrix(1:6, nrow = 2)
  buf <- render_widget(data_table(m), 20, 3)
  expect_match(buf$to_text()[[1]], "V1 V2 V3")
  empty <- data_table(small[0, ])
  pilot <- test_app(app(empty), 30, 4)
  pilot$press("down", "enter", "end")
  expect_identical(empty$cursor_row, 0L)
  empty$data <- small
  pilot$step()
  expect_identical(empty$row_count, 4L)
  expect_identical(empty$cursor_row, 1L)
  expect_error(data_table(list(a = 1)), "data frame or a matrix")
})
