test_that("status bar lays out three regions and truncates to terminal cells", {
  bar <- status_bar("SQLite :memory:", "127 rows", "8 ms", id = "status",
                    classes = "footer", style = style(background = "$surface"))
  text <- render_widget(bar, 40, 1)$to_text()
  expect_length(text, 1L)
  expect_match(text[[1L]], "^SQLite :memory:")
  expect_match(text[[1L]], "127 rows")
  expect_match(text[[1L]], "8 ms$")
  expect_equal(str_width(text[[1L]]), 40L)
  expect_true("footer" %in% bar$classes)
  narrow <- render_widget(status_bar("left", "center", "R"), 10, 1)$to_text()[[1L]]
  expect_true(startsWith(narrow, "left"))
  expect_true(endsWith(narrow, "R"))
  expect_false(grepl("center", narrow, fixed = TRUE))

  for (width in c(1L, 2L, 3L, 10L, 20L, 80L)) {
    out <- render_widget(status_bar("left side", "middle", "right side"), width, 1L)$to_text()
    expect_equal(str_width(out[[1L]]), width)
  }
  expect_no_error(render_widget(status_bar("界🙂 e\u0301", "中", "右"), 3, 1))
})

test_that("status bar reactive regions update in headless high contrast mode", {
  count <- signal(1L)
  bar <- status_bar("app", right = function() paste(count(), "rows"))
  pilot <- test_app(app(bar, theme = "high-contrast"), 24, 1, color_mode = "none")
  expect_match(pilot$screen_text()[[1L]], "1 rows")
  count(4L)
  pilot$step()
  expect_match(pilot$screen_text()[[1L]], "4 rows")
  expect_no_error(pilot$resize(1, 1))
  pilot$stop()
})
