make_tabs <- function() {
  tabs(
    tab("Overview", label("overview text"), button("Go", id = "go"), id = "overview"),
    tab("Logs", label("log text"), input(id = "filter"), id = "logs"),
    tab("Settings", label("settings text"), id = "settings"),
    id = "tabs"
  )
}

test_that("tabs show only the active pane", {
  t <- make_tabs()
  pilot <- test_app(app(t), 40, 6)
  text <- pilot$screen_text()
  expect_match(text[[1]], "^ Overview   Logs   Settings ")
  expect_match(text[[3]], "^overview text")
  expect_false(any(grepl("log text", text)))
  expect_identical(t$active, 1L)
  expect_identical(t$active_tab$id, "overview")
  expect_identical(t$tab_count, 3L)
})

test_that("keyboard and mouse switch tabs and send messages", {
  got <- list()
  t <- make_tabs()
  a <- app(t, on("tabs.changed", function(event, app) got[[length(got) + 1L]] <<- event$data))
  pilot <- test_app(a, 40, 6)
  expect_identical(a$focused$id, "tabs")
  pilot$press("right")
  expect_identical(t$active, 2L)
  expect_match(pilot$screen_text()[[3]], "^log text")
  pilot$press("left", "left")
  expect_identical(t$active, 3L)
  pilot$click(13, 1) # "Logs" label
  expect_identical(t$active, 2L)
  expect_identical(got[[1]][c("index", "previous", "id", "label")], list(index = 2L, previous = 1L, id = "logs", label = "Logs"))
})

test_that("inactive panes are not focusable and focus follows the switch", {
  t <- make_tabs()
  a <- app(t)
  pilot <- test_app(a, 40, 8)
  expect_identical(vapply(a$focus_chain(), function(w) w$id, ""), c("tabs", "go"))
  pilot$press("tab")
  expect_identical(a$focused$id, "go")
  pilot$press("ctrl+pagedown")
  expect_identical(t$active, 2L)
  expect_identical(a$focused$id, "tabs")
  expect_identical(vapply(a$focus_chain(), function(w) w$id, ""), c("tabs", "filter"))
})

test_that("tabs can be added, removed and activated by id", {
  t <- make_tabs()
  pilot <- test_app(app(t), 50, 6)
  t$add_tab(tab("Extra", label("extra text"), id = "extra"), activate = TRUE)
  pilot$step()
  expect_identical(t$active, 4L)
  expect_match(pilot$screen_text()[[3]], "^extra text")
  t$activate("logs")
  t$remove_tab("logs")
  pilot$step()
  expect_identical(t$active_tab$id, "settings")
  t$remove_tab(1)
  expect_identical(t$active_tab$id, "settings")
  expect_identical(t$active, 1L)
  expect_error(t$activate("nope"), "No tab")
  pane <- t$active_tab
  pane$label <- "Prefs"
  pilot$step()
  expect_match(pilot$screen_text()[[1]], "Prefs")
})
