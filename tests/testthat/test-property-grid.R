test_that("property grid formats values and accepts named vectors", {
  value <- list(type = "numeric", missing = 12L, max = NA_real_, none = NULL,
                date = as.Date("2026-10-07"), values = 1:4,
                object = list(nested = TRUE))
  grid <- property_grid(value)
  text <- paste(render_widget(grid, 72, 12)$to_text(), collapse = "\n")
  expect_match(text, "numeric")
  expect_match(text, "NA")
  expect_match(text, "NULL")
  expect_match(text, "2026-10-07")
  expect_true(grepl("[1, 2, 3", text, fixed = TRUE))
  expect_true(grepl("<list[1]>", text, fixed = TRUE))
  expect_error(property_grid(c(a = "x", "unnamed")), "named")
  expect_match(paste(render_widget(property_grid(c(type = "numeric", missing = "12")), 40, 4)$to_text(), collapse = "\n"),
               "numeric")
})

test_that("property grid wraps values, scrolls, and calls a custom formatter", {
  calls <- character()
  grid <- property_grid(
    list(`a key much longer than the cap` = paste(rep("wide value", 8), collapse = " "),
         unicode = "\u4e16\u754c\U0001f642"),
    format = function(name, value) {
      calls <<- c(calls, name)
      paste0("value: ", value)
    },
    max_key_width = 8L
  )
  pilot <- test_app(app(grid), 16, 4)
  visible <- paste(pilot$screen_text(), collapse = "\n")
  expect_true(grepl("wide", visible, fixed = TRUE))
  expect_true(grepl("value", visible, fixed = TRUE))
  expect_true("a key much longer than the cap" %in% calls)
  expect_true(grid$focusable)
  expect_no_error(pilot$press("pagedown"))
  for (width in c(1L, 2L, 3L, 10L, 20L, 80L)) {
    expect_no_error(render_widget(property_grid(list(key = "\u4e16\u754c\U0001f642")), width, 5L))
  }
  pilot$stop()
})

test_that("property grid data is reactive and replacement disposes old children", {
  state <- signal(list(count = 1L))
  grid <- property_grid(function() state())
  pilot <- test_app(app(grid), 24, 4)
  expect_match(paste(pilot$screen_text(), collapse = "\n"), "1")
  old <- grid$children[[1L]]
  state(list(count = 9L))
  pilot$step()
  expect_match(paste(pilot$screen_text(), collapse = "\n"), "9")
  expect_null(old$parent)
  grid$set_data(list(new = TRUE))
  pilot$step()
  expect_match(paste(pilot$screen_text(), collapse = "\n"), "TRUE")
  pilot$stop()
})
