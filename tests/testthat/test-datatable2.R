shot <- function(d) {
  d$pilot$step()
  d$pilot$screen_text()
}

dt_app <- function(df, width = 40, height = 8, ...) {
  tbl <- data_table(df, id = "t", ...)
  a <- app(tbl)
  list(app = a, tbl = tbl, pilot = test_app(a, width, height))
}

people <- data.frame(
  name = c("Ada", "Bob", "Cy", "Di", "Ed"),
  age = c(36L, 25L, 41L, 25L, 30L),
  city = c("Paris", "Oslo", "Rome", "Oslo", "Bern"),
  stringsAsFactors = FALSE
)

test_that("multi-column sort is stable and shows markers", {
  d <- dt_app(people, width = 40, height = 8)
  d$tbl$sort(c("age", "name"), decreasing = c(FALSE, TRUE))
  expect_identical(d$tbl$view, c(4L, 2L, 5L, 1L, 3L))
  expect_identical(d$tbl$sort_state$column, c("age", "name"))
  header <- shot(d)[[1]]
  expect_match(header, "age \u25b21")
  expect_match(header, "name \u25bc2")
  d$tbl$clear_sort()
  expect_identical(d$tbl$view, 1:5)
  expect_null(d$tbl$sort_state)
})

test_that("sorting survives filtering", {
  d <- dt_app(people)
  d$tbl$sort("age", decreasing = TRUE)
  d$tbl$filter(c(1, 2, 4))
  expect_identical(d$tbl$view, c(1L, 2L, 4L))
  d$tbl$filter(NULL)
  expect_identical(d$tbl$view, c(3L, 1L, 5L, 2L, 4L))
})

test_that("header clicks sort when header_sort is on: asc, desc, off", {
  d <- dt_app(people, header_sort = TRUE)
  header_x <- function(text) regexpr(text, shot(d)[[1]])[[1]]
  x <- regexpr("age", shot(d)[[1]])[[1]] + 1L
  d$pilot$mouse("down", x, 1, button = "left")
  d$pilot$mouse("up", x, 1, button = "left")
  expect_identical(d$tbl$view, c(2L, 4L, 5L, 1L, 3L))
  d$pilot$click(x, 1)
  expect_identical(d$tbl$view, c(3L, 1L, 5L, 2L, 4L))
  d$pilot$click(x, 1)
  expect_null(d$tbl$sort_state)
  expect_identical(d$tbl$view, 1:5)
})

test_that("keyboard sort uses the active column", {
  d <- dt_app(people, cursor = "cell", header_sort = TRUE)
  d$pilot$press("right", "s")
  expect_identical(d$tbl$sort_state$column, "age")
  d$pilot$press("s")
  expect_true(d$tbl$sort_state$decreasing)
  d$pilot$press("s")
  expect_null(d$tbl$sort_state)
})

test_that("column widths: set, auto size from a sample, keyboard and drag resize", {
  d <- dt_app(people, width = 50, height = 6)
  expect_identical(d$tbl$column_width("name"), 4L)
  d$tbl$set_column_width("name", 12)
  expect_identical(d$tbl$column_width("name"), 12L)
  expect_match(shot(d)[[1]], "^name {8}")
  d$tbl$auto_size_column("name")
  expect_identical(d$tbl$column_width("name"), 4L)
  d$tbl$set_column_width("city", 20)
  d$tbl$auto_size_all()
  expect_identical(d$tbl$column_width("city"), 5L)
  # Keyboard: Alt+Right widens the active column.
  d$pilot$press("alt+right", "alt+right")
  expect_identical(d$tbl$column_width("name"), 6L)
  d$pilot$press("alt+left")
  expect_identical(d$tbl$column_width("name"), 5L)
  d$tbl$set_column_width("name", 1)
  d$pilot$press("alt+left")
  expect_identical(d$tbl$column_width("name"), 1L)
  # Mouse: drag the separator after "name".
  d$tbl$set_column_width("name", 6)
  resized <- NULL
  d$app$on("datatable.column_resized", function(event, app) resized <<- event$data)
  sep_x <- 1 + 6
  d$pilot$drag(c(sep_x, 1), c(sep_x + 4, 1))
  expect_identical(d$tbl$column_width("name"), 10L)
  expect_identical(resized$column, "name")
  expect_identical(resized$width, 10L)
  d$pilot$drag(c(1 + 10, 1), c(1, 1))
  expect_identical(d$tbl$column_width("name"), 1L)
  expect_error(d$tbl$set_column_width("name", "x"), "number of cells")
})

test_that("auto sizing never scans a huge table", {
  n <- 1e6
  big <- data.frame(id = seq_len(n), s = rep("ab", n))
  big$s[[500000]] <- strrep("w", 30)
  tbl <- data_table(big)
  elapsed <- system.time(tbl$auto_size_column("s"))[["elapsed"]]
  expect_lt(elapsed, 2)
  expect_identical(tbl$column_width("s"), 2L)
  tbl$auto_size_column("s", rows = "all")
  expect_identical(tbl$column_width("s"), 30L)
})

test_that("columns can be hidden and shown", {
  d <- dt_app(people, width = 40, height = 6, cursor = "cell")
  d$tbl$set_column_visible("age", FALSE)
  expect_false(grepl("age", shot(d)[[1]]))
  expect_identical(d$tbl$column_info()$visible, c(TRUE, FALSE, TRUE))
  d$pilot$press("right")
  expect_identical(d$tbl$cursor_column, 3L)
  d$tbl$set_column_visible("age", TRUE)
  expect_match(shot(d)[[1]], "age")
  d$tbl$set_column_visible("name", FALSE)
  d$tbl$set_column_visible("age", FALSE)
  expect_error(d$tbl$set_column_visible("city", FALSE), "at least one")
  d2 <- dt_app(people, columns = list(age = column(visible = FALSE)))
  expect_false(grepl("age", shot(d2)[[1]]))
})

test_that("frozen columns stay while the rest scrolls", {
  df <- data.frame(id = 1:5, aaaa = "a1", bbbb = "b2", cccc = "c3", dddd = "d4", eeee = "e5")
  d <- dt_app(df, width = 20, height = 6, frozen_columns = 1)
  expect_match(shot(d)[[1]], "^id\u2502aaaa")
  d$pilot$press("right", "right")
  header <- shot(d)[[1]]
  expect_match(header, "^id\u2502cccc")
  expect_false(grepl("aaaa", header))
  # Frozen cells remain in the data rows too.
  expect_match(shot(d)[[2]], "^ 1\u2502c3")
  # A cell cursor scrolls to a scrolling column and stays on a frozen one.
  d2 <- dt_app(df, width = 20, height = 6, frozen_columns = 1, cursor = "cell")
  d2$pilot$press("right", "right", "right", "right", "right")
  expect_identical(d2$tbl$cursor_column, 6L)
  expect_true(grepl("eeee", shot(d2)[[1]]))
  expect_match(shot(d2)[[1]], "^id")
  d2$pilot$press("left", "left", "left", "left", "left")
  expect_identical(d2$tbl$cursor_column, 1L)
  # Mouse hit testing respects the frozen block.
  d2$pilot$press("right", "right", "right", "right")
  d2$pilot$click(2, 4)
  expect_identical(d2$tbl$cursor_column, 1L)
  expect_identical(d2$tbl$cursor_row, 3L)
})

test_that("find moves the cursor through matching cells in reading order", {
  d <- dt_app(people, width = 40, height = 8, cursor = "cell")
  expect_true(d$tbl$find("oslo"))
  expect_identical(d$tbl$match_hit$position, 2L)
  expect_identical(d$tbl$match_hit$column, "city")
  expect_identical(d$tbl$cursor_row, 2L)
  expect_identical(d$tbl$cursor_column, 3L)
  d$tbl$find_next()
  expect_identical(d$tbl$match_hit$position, 4L)
  d$tbl$find_next()
  expect_identical(d$tbl$match_hit$position, 2L)
  d$tbl$find_previous()
  expect_identical(d$tbl$match_hit$position, 4L)
  expect_false(d$tbl$find("zzz"))
  expect_null(d$tbl$match_hit)
  # One column only, case sensitivity and regular expressions.
  expect_true(d$tbl$find("^[A-Z]", column = "name", regex = TRUE))
  expect_identical(d$tbl$match_hit$column, "name")
  expect_false(d$tbl$find("oslo", case_sensitive = TRUE))
  expect_error(d$tbl$find("(", regex = TRUE), "Invalid regular expression")
  # Numbers are searched as displayed.
  expect_true(d$tbl$find("41"))
  expect_identical(d$tbl$match_hit$row, 3L)
})

test_that("find respects the current view and scrolls to the match", {
  df <- data.frame(n = 1:300, w = paste0("w", 1:300))
  d <- dt_app(df, width = 20, height = 6)
  d$tbl$find("w250")
  expect_identical(d$tbl$cursor_row, 250L)
  expect_lte(d$tbl$offset_row, 249L)
  expect_gte(d$tbl$offset_row, 245L)
  d$tbl$sort("n", decreasing = TRUE)
  expect_null(d$tbl$match_hit)
  d$tbl$find("w250")
  expect_identical(d$tbl$cursor_row, 51L)
})

test_that("the find bar searches incrementally and closes with Escape", {
  d <- dt_app(people, width = 44, height = 8)
  d$pilot$press("ctrl+f")
  d$pilot$type("rom")
  expect_true(any(grepl("Find: rom_", shot(d))))
  expect_identical(d$tbl$cursor_row, 3L)
  d$pilot$press("backspace", "backspace", "backspace")
  d$pilot$type("zz")
  expect_true(any(grepl("no matches", shot(d))))
  d$pilot$press("backspace", "backspace")
  d$pilot$type("o")
  expect_identical(d$tbl$cursor_row, 3L)
  d$pilot$press("enter")
  expect_identical(d$tbl$cursor_row, 4L)
  d$pilot$press("tab")
  expect_true(any(grepl("column city", shot(d))))
  d$pilot$press("escape")
  expect_false(any(grepl("Find:", shot(d))))
  d$pilot$press("up")
  expect_identical(d$tbl$cursor_row, 3L)
})

test_that("go to row bar jumps to a row", {
  df <- data.frame(n = 1:500)
  d <- dt_app(df, width = 20, height = 6)
  d$pilot$press("ctrl+g")
  d$pilot$type("321")
  expect_true(any(grepl("Go to row", shot(d))))
  d$pilot$press("enter")
  expect_identical(d$tbl$cursor_row, 321L)
  d$tbl$goto_row(9999)
  expect_identical(d$tbl$cursor_row, 500L)
  d$pilot$press("ctrl+g", "escape")
  expect_false(any(grepl("Go to row", shot(d))))
})

test_that("editing a cell changes the table's copy, not the original data", {
  changed <- NULL
  df <- data.frame(name = c("a", "b"), n = c(1.5, 2), ok = c(TRUE, FALSE), stringsAsFactors = FALSE)
  d <- dt_app(df, width = 40, height = 8, cursor = "cell", editable = TRUE)
  d$app$on("datatable.cell_changed", function(event, app) changed <<- event$data)
  d$pilot$press("right", "enter")
  expect_s3_class(d$app$screen, "ModalScreen")
  d$pilot$press("ctrl+a")
  d$pilot$type("9.25")
  d$pilot$press("enter")
  expect_false(inherits(d$app$screen, "ModalScreen"))
  expect_identical(d$tbl$data$n, c(9.25, 2))
  expect_identical(df$n, c(1.5, 2))
  expect_identical(changed, list(row = 1L, column = "n", old = 1.5, value = 9.25))
  expect_match(paste(shot(d), collapse = "\n"), "9.25")
  # Invalid input keeps the dialog open; Escape cancels.
  d$pilot$press("enter")
  d$pilot$press("ctrl+a")
  d$pilot$type("abc")
  d$pilot$press("enter")
  expect_s3_class(d$app$screen, "ModalScreen")
  d$pilot$press("escape")
  expect_identical(d$tbl$data$n, c(9.25, 2))
  # Logical column.
  d$pilot$press("right", "enter")
  d$pilot$press("ctrl+a")
  d$pilot$type("no")
  d$pilot$press("enter")
  expect_identical(d$tbl$data$ok, c(FALSE, FALSE))
})

test_that("on_edit can reject edits and set_cell validates types", {
  rejected <- character()
  df <- data.frame(age = c(10L, 20L), f = factor(c("x", "y")))
  tbl <- data_table(df, editable = TRUE, on_edit = function(row, column, value) {
    if (identical(column, "age") && value < 0L) "age must be positive"
  })
  a <- app(tbl)
  pilot <- test_app(a, 30, 6)
  expect_error(tbl$set_cell(1, "age", -5L), "age must be positive")
  expect_identical(tbl$data$age, c(10L, 20L))
  expect_error(tbl$set_cell(1, "age", "text"), "Enter a number|fit")
  tbl$set_cell(2, "age", 55)
  expect_identical(tbl$data$age, c(10L, 55L))
  expect_error(tbl$set_cell(1, "f", "z"), "Must be one of")
  tbl$set_cell(1, "f", "y")
  expect_identical(as.character(tbl$data$f), c("y", "y"))
  expect_error(tbl$set_cell(9, "age", 1L), "out of range")
  expect_error(data_table(df)$edit_cell(), "not editable")
})

test_that("double clicking a cell opens the editor", {
  df <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  d <- dt_app(df, width = 30, height = 6, editable = TRUE)
  d$pilot$click(3, 2)
  expect_false(inherits(d$app$screen, "ModalScreen"))
  d$pilot$click(3, 2)
  expect_s3_class(d$app$screen, "ModalScreen")
})

test_that("parse_cell converts text to the column type", {
  expect_identical(parse_cell("12", 1L)$value, 12L)
  expect_match(parse_cell("1.5", 1L)$error, "whole")
  expect_identical(parse_cell("", 1.5)$value, NA_real_)
  expect_identical(parse_cell("yes", TRUE)$value, TRUE)
  expect_match(parse_cell("maybe", TRUE)$error, "TRUE or FALSE")
  expect_identical(parse_cell("2020-01-02", Sys.Date())$value, as.Date("2020-01-02"))
  expect_match(parse_cell("x", Sys.Date())$error, "YYYY-MM-DD")
  expect_identical(parse_cell("hi\u001b[0m", "a")$value, "hi[0m")
})

test_that("random operations keep the table consistent", {
  set.seed(11)
  df <- data.frame(a = 1:40, b = rep(c("x", "y"), 20), c = runif(40), d = letters[(0:39 %% 26) + 1])
  d <- dt_app(df, width = 30, height = 7, cursor = "cell", frozen_columns = 1)
  ops <- list(
    function() d$pilot$press(sample(c("up", "down", "left", "right", "pagedown", "pageup", "home", "end"), 1)),
    function() d$tbl$sort(sample(names(df), sample(1:2, 1))),
    function() d$tbl$clear_sort(),
    function() d$tbl$filter(sample.int(40, sample(1:40, 1))),
    function() d$tbl$filter(NULL),
    function() d$tbl$find(sample(c("a", "x", "1", "q"), 1)),
    function() d$tbl$set_column_width(sample(names(df), 1), sample(1:15, 1)),
    function() d$tbl$auto_size_all(),
    function() d$pilot$resize(sample(8:40, 1), sample(3:10, 1)),
    function() d$pilot$press("alt+right")
  )
  for (i in seq_len(stress_workload(150L, 40L))) {
    ops[[sample(length(ops), 1)]]()
    n <- d$tbl$row_count
    if (n > 0L) {
      expect_true(d$tbl$cursor_row >= 1L && d$tbl$cursor_row <= n)
      expect_true(d$tbl$offset_row >= 0L && d$tbl$offset_row < n)
    }
    expect_true(d$tbl$cursor_column %in% 1:4)
  }
})
