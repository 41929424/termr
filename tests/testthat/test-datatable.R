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

test_that("column order is independent of data order and supports visibility", {
  tbl <- data_table(small, row_names = FALSE)
  tbl$reorder_column("ok", 1L)
  expect_identical(tbl$column_info()$name, names(small))
  expect_match(render_widget(tbl, 40, 4)$to_text()[[1]], "^ok +name")
  tbl$set_column_visible("score", FALSE)
  expect_identical(names(tbl$visible_data()), c("ok", "name", "n"))
  expect_error(tbl$reorder_column("name", 99), "valid 1-based")
  tbl$set_column_visible("ok", FALSE)
  tbl$set_column_visible("name", FALSE)
  expect_error(tbl$set_column_visible("n", FALSE), "at least one visible")
})

test_that("range selection, selected and visible data, and TSV copy", {
  tbl <- data_table(small, row_names = FALSE)
  pilot <- test_app(app(tbl), 40, 4)
  pilot$press("shift+down")
  expect_identical(tbl$selected_data()$name, c("alpha", "beta"))
  expect_identical(tbl$visible_data()$name, c("alpha", "beta", "gamma"))
  tbl$filter(c(TRUE, FALSE, TRUE, TRUE))
  tbl$select_range(2, 3)
  expect_identical(tbl$selected_data()$name, c("gamma", "delta"))
  tbl$copy_selection(system = FALSE)
  expect_identical(pilot$app$clipboard, paste(
    "name\tscore\tn\tok", "gamma\tNA\t300\tTRUE", "delta\t4\tNA\tNA", sep = "\n"
  ))
  tbl$copy_selection(headers = FALSE, system = FALSE)
  expect_identical(pilot$app$clipboard, "gamma\tNA\t300\tTRUE\ndelta\t4\tNA\tNA")
  expect_error(tbl$select_range(0), "between 1 and")
})

test_that("columns can be reordered from the keyboard", {
  tbl <- data_table(small, row_names = FALSE)
  pilot <- test_app(app(tbl), 40, 4)
  pilot$press("ctrl+alt+right")
  expect_match(pilot$screen_text()[[1]], "^score +name")
})

test_that("column filters combine with AND and process bounded row chunks", {
  n <- 10001L
  df <- data.frame(id = seq_len(n), label = paste0("row-", seq_len(n)),
                   score = seq_len(n) %% 101, stringsAsFactors = FALSE)
  sizes <- integer()
  tbl <- data_table(df)
  tbl$filter_columns(list(
    label = table_filter("contains", "ROW-1"),
    score = table_filter("range", min = 20, max = 30),
    id = function(x) { sizes <<- c(sizes, length(x)); x %% 2L == 0L }
  ))
  expected <- which(grepl("row-1", df$label, fixed = TRUE) & df$score >= 20 & df$score <= 30 & df$id %% 2L == 0L)
  expect_identical(tbl$view, expected)
  expect_lte(max(sizes), 5000L)
  tbl$filter_columns(list(score = table_filter("regex", "^2[0-9]$")))
  expect_identical(tbl$view, which(grepl("^2[0-9]$", as.character(df$score))))
  tbl$filter_columns(NULL)
  expect_identical(tbl$row_count, n)
  expect_error(tbl$filter_columns(list(score = function(x) TRUE)), "one logical per value")
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
