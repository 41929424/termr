project <- function() {
  tree_node(
    "Project",
    tree_node("R", "app.R", "widget.R", id = "R"),
    tree_node("tests", tree_node("testthat", "test-a.R", "test-b.R"), id = "tests"),
    "README.md",
    expanded = TRUE, id = "root"
  )
}

test_that("tree nodes form a model", {
  root <- project()
  expect_length(root$children, 3)
  r <- root$find("R")
  expect_identical(r$path(), c("Project", "R"))
  expect_identical(r$children[[1]]$depth(), 2L)
  expect_true(r$expandable())
  expect_false(r$expanded)
  leaf <- r$add("new.R")
  expect_identical(leaf$parent, r)
  leaf$remove()
  expect_length(r$children, 2)
  expect_error(r$parent <- root, "read-only")
})

test_that("the tree view shows expanded nodes with guides", {
  pilot <- test_app(app(tree_view(project(), id = "tree")), 30, 6)
  expect_snapshot(pilot$snapshot())
})

test_that("keyboard navigation expands, collapses and moves", {
  got <- character()
  tv <- tree_view(project(), id = "tree")
  a <- app(tv, on("*", "#tree", function(event, app) if (startsWith(event$type, "tree.")) got <<- c(got, paste(event$type, event$data$label))))
  pilot <- test_app(a, 30, 10)
  pilot$press("down")
  expect_identical(tv$cursor_node$label, "R")
  pilot$press("right")
  expect_true(tv$cursor_node$expanded)
  pilot$press("right")
  expect_identical(tv$cursor_node$label, "app.R")
  pilot$press("left")
  expect_identical(tv$cursor_node$label, "R")
  pilot$press("left")
  expect_false(tv$cursor_node$expanded)
  pilot$press("end", "enter")
  expect_identical(got, c(
    "tree.node_selected R", "tree.node_expanded R", "tree.node_selected app.R",
    "tree.node_selected R", "tree.node_collapsed R", "tree.node_selected README.md",
    "tree.node_activated README.md"
  ))
  pilot$press("home", "space")
  expect_identical(tv$visible_count, 1L)
})

test_that("lazy loaders run once on first expand", {
  calls <- 0L
  lazy <- tree_node("lazy", loader = function(node) {
    calls <<- calls + 1L
    list("child 1", tree_node("child 2", data = 2))
  })
  root <- tree_node("root", lazy, expanded = TRUE)
  tv <- tree_view(root)
  pilot <- test_app(app(tv), 30, 6)
  expect_true(lazy$expandable())
  pilot$press("down", "right")
  expect_identical(calls, 1L)
  expect_identical(tv$visible_count, 4L)
  pilot$press("left", "right")
  expect_identical(calls, 1L)
  expect_identical(lazy$children[[2]]$data, 2)
})

test_that("large trees scroll and only visible lines are painted", {
  root <- tree_node("big", expanded = TRUE)
  for (i in 1:2000) root$add(paste("item", i))
  tv <- tree_view(root)
  pilot <- test_app(app(tv), 30, 5)
  pilot$press("end")
  expect_identical(tv$cursor_node$label, "item 2000")
  expect_identical(tv$offset, 1996L)
  expect_match(pilot$screen_text()[[5]], "item 2000")
  pilot$press("pageup")
  expect_identical(tv$cursor_node$label, "item 1996")
})

test_that("mouse selects and toggles; show_root = FALSE hides the root", {
  tv <- tree_view(project(), show_root = FALSE)
  pilot <- test_app(app(tv), 30, 8)
  # Top-level nodes have no guides when the root is hidden.
  expect_match(pilot$screen_text()[[1]], "^\u25b6 R")
  pilot$mouse("down", 1, 1, button = "left") # the arrow of "R"
  expect_true(tv$cursor_node$expanded)
  pilot$mouse("down", 10, 2, button = "left")
  expect_identical(tv$cursor_node$label, "app.R")
  tv$select(tv$root$find("tests"))
  expect_identical(tv$cursor_node$label, "tests")
  tv$root$find("tests")$children[[1]]$add("x")
  pilot$step()
  expect_false(tv$root$find("tests")$children[[1]]$expanded)
})
