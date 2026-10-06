make_tree <- function() {
  vertical(
    id = "root",
    label("Title", id = "title", classes = "heading"),
    horizontal(
      id = "row",
      label("a", classes = "item danger"),
      vertical(label("b", id = "deep", classes = "item"))
    ),
    label("c", classes = "item")
  )
}

ids_of <- function(ws) vapply(ws, function(w) w$id %||% w$format(), character(1))

test_that("type, id and class selectors match", {
  ui <- make_tree()
  expect_length(ui$query("Label"), 4)
  expect_length(ui$query("Vertical"), 1) # query() excludes the root itself
  expect_identical(ui$query_one("#deep")$id, "deep")
  expect_length(ui$query(".item"), 3)
  expect_length(ui$query("Label.item.danger"), 1)
  expect_length(ui$query("Widget"), 6)
  expect_length(ui$query("*"), 6)
})

test_that("descendant and child combinators", {
  ui <- make_tree()
  expect_identical(ids_of(ui$query("#row Label")), c("<Label .item .danger> \"a\"", "deep"))
  expect_identical(ids_of(ui$query("#row > Label")), "<Label .item .danger> \"a\"")
  expect_identical(ids_of(ui$query("Horizontal Vertical > .item")), "deep")
  expect_length(ui$query("#title, #deep"), 2)
})

test_that("matches() and query_one errors", {
  ui <- make_tree()
  expect_true(ui$query_one("#title")$matches("Label.heading"))
  expect_error(ui$query_one("#missing"), "No widget matches")
  expect_error(ui$query("Label >"), "Invalid selector")
  expect_error(ui$query("#a#b"), "Invalid selector")
  expect_error(ui$query("La bel!"), "Invalid selector")
})

test_that("type selectors match subclasses", {
  Special <- R6::R6Class("Special", inherit = Label)
  ui <- vertical(Special$new("s"))
  expect_length(ui$query("Label"), 1)
  expect_length(ui$query("Special"), 1)
})
