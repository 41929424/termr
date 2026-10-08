worker_ci_snapshot()

test_that("inline workers deliver results through callbacks and events", {
  got <- list()
  status <- label("", id = "status")
  a <- app(status, on("*", function(event, app) if (startsWith(event$type, "worker.")) got[[length(got) + 1L]] <<- event$type))
  pilot <- test_app(a, 20, 2)
  w <- a$run_worker(function(x) x * 2, args = list(x = 21), inline = TRUE,
                    on_complete = function(result, app) app$query_one("#status")$update(result))
  pilot$step()
  expect_identical(w$state, "completed")
  expect_identical(w$result, 42)
  expect_identical(unlist(got), c("worker.started", "worker.completed"))
  expect_identical(pilot$screen_text()[[1]], "42                  ")
})

test_that("inline worker errors call on_error", {
  err <- NULL
  a <- app(label("x"))
  pilot <- test_app(a, 10, 1)
  w <- a$run_worker(function() stop("no data"), inline = TRUE, on_error = function(message, app) err <<- message)
  expect_identical(w$state, "failed")
  expect_identical(err, "no data")
  expect_error(a$run_worker("not a function"), "must be a function")
  expect_error(a$run_worker(function() 1, args = 1), "must be a list")
})

test_that("process workers run in the background with progress, and can be cancelled", {
  skip_on_cran()
  progress <- numeric()
  done <- NULL
  owner <- label("owner", id = "owner")
  a <- app(vertical(owner, label("other")))
  pilot <- test_app(a, 20, 3)
  w <- owner$run_worker(
    function(n) {
      for (i in seq_len(n)) termr_progress(i / n, paste("step", i))
      sum(seq_len(n))
    },
    args = list(n = 3),
    on_progress = function(value, message, app) progress <<- c(progress, value),
    on_complete = function(result, app) done <<- result
  )
  expect_true(w$is_running())
  expect_length(a$workers(), 1)
  pilot$wait_for_workers(60)
  expect_identical(done, 6L)
  expect_equal(progress, c(1 / 3, 2 / 3, 1), tolerance = 1e-6)
  expect_length(a$workers(), 0)

  slow <- owner$run_worker(function() {
    Sys.sleep(30)
    "late"
  })
  expect_true(slow$is_running())
  owner$remove()
  pilot$step()
  expect_identical(slow$state, "cancelled")
})

test_that("process worker errors are reported and workers stop with the app", {
  skip_on_cran()
  err <- NULL
  a <- app(label("x"))
  pilot <- test_app(a, 10, 1)
  a$run_worker(function() stop("boom in worker"), on_error = function(message, app) err <<- message)
  pilot$wait_for_workers(60)
  expect_identical(err, "boom in worker")
  w <- a$run_worker(function() Sys.sleep(30))
  pilot$stop()
  expect_identical(w$state, "cancelled")
})
