form_app <- function() {
  app(vertical(
    label("Form"),
    input(id = "a"),
    button("Off", id = "off", disabled = TRUE),
    vertical(input(id = "b"), id = "box"),
    button("OK", id = "ok")
  ))
}

focused_id <- function(a) if (is.null(a$focused)) NA_character_ else a$focused$id

test_that("the first focusable widget gets focus on start", {
  a <- form_app()
  pilot <- test_app(a, 30, 12)
  expect_identical(focused_id(a), "a")
  expect_identical(vapply(a$focus_chain(), function(w) w$id, ""), c("a", "b", "ok"))
})

test_that("tab and shift+tab traverse the focus chain and wrap around", {
  a <- form_app()
  pilot <- test_app(a, 30, 12)
  pilot$press("tab")
  expect_identical(focused_id(a), "b")
  pilot$press("tab")
  expect_identical(focused_id(a), "ok")
  pilot$press("tab")
  expect_identical(focused_id(a), "a")
  pilot$press("shift+tab")
  expect_identical(focused_id(a), "ok")
})

test_that("focus() and blur() change focus and send events", {
  log <- character()
  a <- form_app()
  a$on("focus", function(event, app) log <<- c(log, paste("focus", event$sender$id)))
  a$on("blur", "Input", function(event, app) log <<- c(log, paste("blur", event$sender$id)))
  pilot <- test_app(a, 30, 12)
  a$query_one("#ok")$focus()
  pilot$step()
  expect_identical(focused_id(a), "ok")
  a$query_one("#ok")$blur()
  pilot$step()
  expect_true(is.na(focused_id(a)))
  expect_identical(log, c("focus a", "blur a", "focus ok"))
  # Disabled widgets cannot be focused.
  expect_false(a$query_one("#off")$focus())
})

test_that("focus moves on when the focused widget is disabled, hidden or removed", {
  a <- form_app()
  pilot <- test_app(a, 30, 12)
  first <- a$query_one("#a")
  first$disabled <- TRUE
  pilot$step()
  expect_identical(focused_id(a), "b")
  box <- a$query_one("#box")
  box$visible <- FALSE
  pilot$step()
  expect_identical(focused_id(a), "ok")
  a$query_one("#ok")$remove()
  pilot$step()
  expect_true(is.na(focused_id(a)))
  first$disabled <- FALSE
  pilot$press("tab")
  expect_identical(focused_id(a), "a")
})

test_that("widgets mounted while running get mount events and can be focused", {
  mounted <- character()
  a <- app(vertical(id = "root"), on("mount", "Input", function(event, app) mounted <<- c(mounted, event$sender$id)))
  pilot <- test_app(a, 20, 5)
  a$query_one("#root")$mount(input(id = "late"))
  pilot$step()
  expect_identical(mounted, "late")
  pilot$press("tab")
  expect_identical(focused_id(a), "late")
})
