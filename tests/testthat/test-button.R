test_that("enter and space press the focused button", {
  pressed <- character()
  a <- app(
    button("Go", id = "go"),
    on("button.pressed", "#go", function(event, app) pressed <<- c(pressed, event$data$label))
  )
  pilot <- test_app(a, 20, 3)
  pilot$press("enter", "space")
  expect_identical(pressed, c("Go", "Go"))
})

test_that("the pressed state is shown briefly", {
  b <- button("Go", id = "go")
  pilot <- test_app(app(b), 20, 3)
  pilot$press("enter")
  expect_true(b$pressed)
  expect_true("pressed" %in% b$pseudo_states())
  cell <- pilot$driver$terminal$screen$get_cell(5, 2)
  expect_true(attrs_decode(cell$attrs)[["reverse"]])
  pilot$advance(0.2)
  expect_false(b$pressed)
})

test_that("disabled buttons cannot be pressed or focused", {
  pressed <- 0L
  b <- button("No", disabled = TRUE)
  a <- app(b, on("button.pressed", function(event, app) pressed <<- pressed + 1L))
  pilot <- test_app(a, 20, 3)
  expect_null(a$focused)
  expect_false(b$press())
  pilot$step()
  expect_identical(pressed, 0L)
})

test_that("buttons render with a border and focus style", {
  a <- app(horizontal(button("OK", id = "ok"), button("Cancel", id = "cancel")))
  pilot <- test_app(a, 30, 3)
  expect_snapshot(pilot$snapshot())
  term <- pilot$driver$terminal$screen
  expect_identical(term$get_cell(1, 1)$fg, "bright_cyan") # focused border
  expect_true(attrs_decode(term$get_cell(5, 2)$attrs)[["bold"]])
  expect_identical(term$get_cell(11, 1)$fg, "") # unfocused border
})

test_that("button variants and label updates", {
  b <- button("Save", variant = "primary")
  expect_identical(b$computed_style()$background, "blue")
  expect_error(button("x", variant = "loud"), "variant")
  b$label <- "Saved"
  # min_width = 10: border + padding + "Saved" + one cell of slack
  row <- render_widget(b, 12, 3)$to_text()[[2]]
  expect_identical(substring(row, 2L, 9L), " Saved  ")
  expect_error(b$pressed <- TRUE, "read-only")
})
