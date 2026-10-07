test_that("lazy sources render only cached chunks and jump directly", {
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  src <- table_source(
    row_count = 10000000,
    column_names = c("row", "square"),
    get_rows = function(start, count, columns = NULL) {
      calls$requests[[length(calls$requests) + 1L]] <- list(start = start, count = count, columns = columns)
      i <- seq.int(start, length.out = count)
      out <- data.frame(row = i, square = as.numeric(i) * i)
      if (!is.null(columns)) out <- out[columns]
      out
    }
  )

  tbl <- data_table(src, cursor = "none")
  expect_equal(tbl$row_count, 10000000L)
  expect_true(any(grepl("49", render_widget(tbl, 40, 8)$to_text())))
  before <- tbl$source_stats()
  expect_equal(before$rows_rendered, 7L)
  expect_equal(before$cells_rendered, 14L)
  tbl$scroll_to_row(2)
  render_widget(tbl, 40, 8)
  expect_gt(tbl$source_stats()$cache_hits, before$cache_hits)
  tbl$scroll_to_row(5000000)
  expect_equal(tbl$offset_row, 4999999L)
  render_widget(tbl, 40, 8)
  after <- tbl$source_stats()
  expect_lt(after$rows_requested, 1000L)
  expect_true(all(vapply(calls$requests, function(x) x$count <= 100L, TRUE)))
  expect_error(tbl$sort("row"), "not supported")
  expect_error(tbl$filter_columns(list(row = function(x) x > 10)), "not supported")
})

test_that("lazy source sort and filters are delegated", {
  values <- data.frame(id = c(4L, 1L, 3L, 2L), group = c("b", "a", "b", "a"))
  state <- new.env(parent = emptyenv())
  state$view <- seq_len(nrow(values))
  src <- table_source(
    row_count = function() length(state$view),
    column_names = names(values),
    get_rows = function(start, count, columns = NULL) {
      idx <- state$view[seq.int(start, length.out = min(count, length(state$view) - start + 1L))]
      out <- values[idx, , drop = FALSE]
      if (!is.null(columns)) out <- out[columns]
      out
    },
    sort = function(spec) {
      if (is.null(spec)) state$view <- seq_len(nrow(values)) else {
        keys <- lapply(spec$columns, function(nm) values[[nm]][state$view])
        state$view <- state$view[do.call(order, c(keys, list(decreasing = spec$decreasing, method = "radix")))]
      }
      TRUE
    },
    filter = function(filters) {
      state$view <- if (is.null(filters)) seq_len(nrow(values)) else {
        keep <- Reduce(`&`, lapply(names(filters), function(nm) filters[[nm]](values[[nm]])))
        which(keep)
      }
      TRUE
    }
  )
  tbl <- data_table(src)
  tbl$sort("id")
  expect_equal(state$view, c(2L, 4L, 3L, 1L))
  tbl$move_cursor(row = 4L)
  tbl$filter_columns(list(group = function(x) x == "b"))
  expect_equal(state$view, c(3L, 1L))
  expect_equal(tbl$row_count, 2L)
  expect_equal(tbl$cursor_row, 2L)
  expect_true(src$capabilities()$sortable)
  expect_true(src$capabilities()$filterable)
  expect_false(src$capabilities()$searchable)
})

test_that("lazy source protocol errors name the invalid response", {
  src <- table_source(3, c("x", "y"), function(start, count, columns = NULL) {
    data.frame(wrong = seq_len(count))
  })
  expect_error(data_table(src), "inconsistent column names")

  short <- table_source(3, "x", function(start, count, columns = NULL) data.frame(x = 1L))
  expect_error(data_table(short), "returned 1 rows; expected 3")

  extra <- table_source(2, "x", function(start, count, columns = NULL) data.frame(x = 1:3))
  expect_error(data_table(extra), "returned 3 rows; expected 2")

  not_frame <- table_source(2, "x", function(start, count, columns = NULL) list(x = 1:2))
  expect_error(data_table(not_frame), "must return a data.frame")

  failed <- table_source(2, "x", function(start, count, columns = NULL) stop("backend offline"))
  expect_error(data_table(failed), "get_rows.*failed: backend offline")

  expect_error(table_source(-1, "x", function(...) data.frame()), "row_count")
  bad_names <- table_source(2, c("x", "x"), function(...) data.frame())
  expect_error(data_table(bad_names), "column_names")
})

test_that("randomized delegated views match a small in-memory reference", {
  values <- data.frame(id = sample.int(100L, 20L), group = rep(c("a", "b"), 10L))
  state <- new.env(parent = emptyenv())
  state$view <- seq_len(nrow(values))
  src <- table_source(
    function() length(state$view), names(values),
    function(start, count, columns = NULL) {
      take <- min(count, length(state$view) - start + 1L)
      idx <- if (take > 0L) state$view[seq.int(start, length.out = take)] else integer()
      out <- values[idx, , drop = FALSE]
      if (!is.null(columns)) out <- out[columns]
      out
    },
    sort = function(spec) {
      if (is.null(spec)) state$view <- seq_len(nrow(values)) else {
        nm <- spec$columns[[1L]]
        state$view <- state$view[order(values[[nm]][state$view], decreasing = spec$decreasing[[1L]])]
      }
      TRUE
    },
    filter = function(filters) {
      state$view <- if (is.null(filters)) seq_len(nrow(values)) else which(filters$group(values$group))
      TRUE
    }
  )
  tbl <- data_table(src, cursor = "none")
  set.seed(18)
  actions <- sample(c("ascending", "descending", "filter", "clear_filter"), 40L, replace = TRUE)
  keep <- rep(TRUE, nrow(values))
  decreasing <- NULL
  for (action in actions) {
    if (action == "ascending") {
      decreasing <- FALSE
      tbl$sort("id", decreasing)
    } else if (action == "descending") {
      decreasing <- TRUE
      tbl$sort("id", decreasing)
    } else if (action == "filter") {
      keep <- values$group == "b"
      tbl$filter_columns(list(group = function(x) x == "b"))
    } else {
      keep <- rep(TRUE, nrow(values))
      tbl$filter_columns(NULL)
    }
    expected <- which(keep)
    if (!is.null(decreasing)) expected <- expected[order(values$id[expected], decreasing = decreasing)]
    expect_equal(state$view, expected)
    actual <- lapply(seq_len(tbl$row_count), tbl$row_data)
    reference <- lapply(expected, function(i) as.list(values[i, , drop = FALSE]))
    expect_equal(actual, reference)
  }
})

test_that("source refresh clears cached rows and lifecycle is explicit", {
  state <- new.env(parent = emptyenv())
  state$value <- 1L
  state$closed <- FALSE
  src <- table_source(
    1, "value",
    function(start, count, columns = NULL) data.frame(value = rep(state$value, count)),
    refresh = function() state$value <- state$value + 1L,
    close = function() state$closed <- TRUE
  )
  tbl <- data_table(src, cursor = "none")
  expect_true(any(grepl("1", render_widget(tbl, 20, 4)$to_text())))
  tbl$refresh()
  expect_true(any(grepl("2", render_widget(tbl, 20, 4)$to_text())))
  expect_false(state$closed)
  tbl$close()
  expect_true(state$closed)
  expect_error(render_widget(tbl, 20, 4), "source is closed")
})

test_that("SQLite table sources paginate and delegate safe filters and sorts", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "items", data.frame(id = 1:4, name = c("beta", "alpha", "delta", "gamma"),
                                               active = c(TRUE, FALSE, TRUE, TRUE)))
  src <- db_table_source(con, "items")
  tbl <- data_table(src, cursor = "none")
  expect_equal(tbl$row_count, 4L)
  expect_equal(db_table_source(db_connection(con), "items")$row_count(), 4L)
  tbl$sort("name")
  expect_equal(vapply(seq_len(tbl$row_count), function(i) tbl$row_data(i)$name, ""),
               c("alpha", "beta", "delta", "gamma"))
  tbl$filter_columns(list(active = table_filter("equals", TRUE)))
  expect_equal(tbl$row_count, 3L)
  expect_equal(vapply(seq_len(tbl$row_count), function(i) tbl$row_data(i)$id, 0L), c(1L, 3L, 4L))
  tbl$filter_columns(list(name = table_filter("contains", "ta")))
  expect_equal(tbl$row_count, 2L)
  DBI::dbWriteTable(con, "items", data.frame(id = 5L, name = "zeta", active = TRUE), append = TRUE)
  tbl$refresh()
  expect_equal(tbl$row_count, 5L)
  tbl$close()
  expect_true(DBI::dbIsValid(con))
})

