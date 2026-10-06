# The bundled examples call run() at the end. Source them with run()
# replaced by a function that captures the app, then drive it headlessly.
test_that("all examples are listed", {
  expect_setequal(run_example(), c("buffer-demo", "counter", "dataframe-browser", "file-tree", "form", "hello",
                                   "keys", "kitchen-sink", "model-monitor", "system-monitor", "data-explorer",
                                   "task-runner", "terminal-dashboard", "editor", "accessibility-demo", "custom-widget"))
  expect_error(run_example("nope"), "Unknown example")
})

test_that("the hello example greets the user", {
  pilot <- test_app(load_example("hello"), 40, 10)
  pilot$type("Ada")
  pilot$press("tab", "enter")
  expect_identical(pilot$query_one("#result")$text, "Hello, Ada")
  expect_snapshot(pilot$snapshot())
})

test_that("the form example greets, clears and quits", {
  pilot <- test_app(load_example("form"), 60, 16)
  pilot$type("Ann")
  pilot$press("enter")
  expect_identical(pilot$query_one("#result")$text, "Hello, Ann!")
  pilot$press("tab", "tab", "space")
  expect_identical(pilot$query_one("#name")$value, "")
  expect_true(pilot$query_one("#name")$focused)
  pilot$press("escape")
  expect_true(pilot$exited)
})

test_that("the counter example counts, totals and ticks", {
  pilot <- test_app(load_example("counter"), 70, 14)
  pilot$press("up", "up", "b", "tab", "down")
  expect_identical(pilot$query_one("#left")$count, 102L)
  expect_identical(pilot$query_one("#right")$count, 9L)
  expect_identical(pilot$query_one("#total")$text, "Total: 111")
  pilot$advance(1)
  expect_match(pilot$query_one("#clock")$text, "^Time: ")
  pilot$press("q")
  expect_true(pilot$exited)
})

test_that("the keys example shows key names", {
  pilot <- test_app(load_example("keys"), 60, 10)
  pilot$press("ctrl+up")
  expect_match(pilot$query_one("#key")$text, "ctrl+up", fixed = TRUE)
  pilot$press("q")
  expect_true(pilot$exited)
})

test_that("the system monitor updates metrics on a timer", {
  pilot <- test_app(load_example("system-monitor"), 90, 30)
  pilot$advance(1.1)
  expect_match(pilot$query_one("#cpu")$computed_style()$border, "round")
  expect_true(is.numeric(pilot$query_one("#cpu")$value))
  expect_gt(length(pilot$query_one("#cpu-spark")$data), 1)
  expect_true(any(grepl("Monitor started", pilot$screen_text())))
  pilot$press("q")
  expect_true(pilot$exited)
})

test_that("the kitchen sink renders every tab", {
  pilot <- test_app(load_example("kitchen-sink"), 100, 30)
  for (i in 1:5) {
    expect_identical(pilot$query_one("#tabs")$active, i)
    pilot$press("ctrl+pagedown")
  }
  pilot$press("ctrl+d")
  expect_true(inherits(pilot$app$screen, "ModalScreen"))
  pilot$press("escape", "ctrl+n") # Escape cancels the dialog, which notifies too
  expect_length(pilot$app$notifications(), 2)
})

test_that("the file tree previews files lazily", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "sub"))
  writeLines(c("first line", "second line"), file.path(dir, "a.txt"))
  writeLines("inner", file.path(dir, "sub", "b.txt"))
  withr::local_envvar(TERMR_ROOT = dir)
  pilot <- test_app(load_example("file-tree"), 80, 20)
  pilot$press("down") # "sub" (directories first)
  expect_true(any(grepl("1 entries", pilot$screen_text())))
  pilot$press("down") # a.txt
  expect_true(any(grepl("second line", pilot$screen_text())))
})

test_that("the model monitor and dataframe browser examples build", {
  mm <- load_example("model-monitor")
  pilot <- test_app(mm, 80, 24)
  expect_identical(pilot$query_one("#epoch")$value, "0/30")
  pilot$stop()
  db <- load_example("dataframe-browser")
  pilot2 <- test_app(db, 120, 30)
  expect_match(pilot2$screen_text()[[1]], "Fruit sales")
  pilot2$stop()
})
