test_that("JSON view formats nested R objects and keeps NA distinct from NULL", {
  view <- json_view(list(user = list(id = 42, name = "Alice"), active = TRUE,
                         missing = NA, empty = NULL))
  root <- view$root
  expect_false(root$loaded)
  root$expand()
  labels <- vapply(root$children, `[[`, "", "label")
  expect_true(any(grepl("user object", labels, fixed = TRUE)))
  expect_true(any(grepl("active true", labels, fixed = TRUE)))
  expect_true(any(grepl("missing NA", labels, fixed = TRUE)))
  expect_true(any(grepl("empty null", labels, fixed = TRUE)))
  user <- root$children[[which(grepl("user object", labels, fixed = TRUE))]]
  expect_false(user$loaded)
  user$expand()
  user_labels <- vapply(user$children, `[[`, "", "label")
  expect_true(any(grepl('name "Alice"', user_labels, fixed = TRUE)))
  expect_no_error(render_widget(view, 1, 1))
})

test_that("JSON arrays page children lazily and support expand/collapse", {
  view <- json_view(as.list(seq_len(10000L)), page_size = 50L)
  root <- view$root
  expect_false(root$loaded)
  root$expand()
  expect_equal(length(root$children), 51L)
  more <- root$children[[51L]]
  expect_match(more$label, "Load next 50")
  expect_false(more$loaded)
  more$expand()
  expect_equal(length(more$children), 51L)
  more$collapse()
  expect_false(more$expanded)
  expect_no_error(render_widget(view, 1, 1))
})

test_that("JSON view handles scalars, Unicode and optional JSON parsing", {
  view <- json_view(list(key = "\u4e16\u754c\U0001f642", when = as.Date("2026-10-07")))
  view$root$expand()
  labels <- vapply(view$root$children, `[[`, "", "label")
  expect_true(any(grepl("\u4e16\u754c\U0001f642", labels, fixed = TRUE)))
  expect_true(any(grepl("2026-10-07", labels, fixed = TRUE)))
  skip_if_not_installed("jsonlite")
  parsed <- json_view('{"a":1,"b":null}', parse = TRUE)
  parsed$root$expand()
  expect_length(parsed$root$children, 2L)
  expect_error(json_view("{", parse = TRUE), "parse error|lexical error|unexpected", ignore.case = TRUE)
})
