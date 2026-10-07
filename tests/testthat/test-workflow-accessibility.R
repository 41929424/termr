test_that("data workflow widgets render in monochrome high contrast with reduced motion", {
  withr::local_envvar(c(NO_COLOR = "1", COLORTERM = "", TERM = "xterm-256color"))
  json <- json_view(list(user = list(id = 42)))
  json$root$expand()
  widgets <- vertical(
    status_bar("ready", right = "1 row"),
    property_grid(list(type = "numeric", missing = NA, optional = NULL)),
    record_view(list(id = 42, name = "Alice")),
    data_profile(c(1, 2, NA_real_), name = "score"),
    json
  )
  pilot <- test_app(
    app(widgets, theme = "high-contrast", reduce_motion = TRUE),
    80, 24, color_mode = "none"
  )
  screen <- pilot$screen_text()
  expect_true(any(grepl("ready", screen, fixed = TRUE)))
  expect_true(any(grepl("numeric", screen, fixed = TRUE)))
  expect_true(any(grepl("Alice", screen, fixed = TRUE)))
  expect_true(any(grepl("score", screen, fixed = TRUE)))
  expect_true(any(grepl("user", screen, fixed = TRUE)))
  expect_no_error(pilot$resize(1, 1))
  expect_no_error(pilot$step())
  pilot$stop()
})
