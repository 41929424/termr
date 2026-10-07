# Small, fast worker bootstrap probes. They run in every context (plain
# test_local(), R CMD check, the dedicated CI probe) and fail with the
# worker's bootstrap phase in the message, so "the worker never started" is
# told apart from "the worker started and hung".

probe_rscript <- function() file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

# Run one worker to its end; returns the worker and its final diagnostics.
probe_worker <- function(fn, timeout = 25, ...) {
  pilot <- test_app(app(label("x")), 20, 2)
  withr::defer(pilot$stop())
  w <- pilot$app$run_worker(fn, timeout = timeout, ...)
  p <- w$.__enclos_env__$private$process
  pilot$wait_for_workers(timeout + 5)
  list(worker = w, process = p, diagnostics = w$.__enclos_env__$private$last_diagnostics, pilot = pilot)
}

expect_worker_gone <- function(r) {
  expect_false(r$process$is_alive())
  expect_length(r$process$kill_tree(), 0L)
  expect_length(r$pilot$app$workers(), 0L)
}

test_that("probe A: a trivial worker completes and reaches every bootstrap phase", {
  r <- probe_worker(function() 42)
  expect_identical(r$worker$state, "completed", info = r$worker$error)
  expect_identical(r$worker$result, 42)
  expect_match(r$diagnostics, "phase=result_written")
  for (phase in c("helper_entered", "helper_started", "payload_loaded", "payload_started",
                  "payload_finished", "result_written")) {
    expect_match(r$diagnostics, phase, fixed = TRUE)
  }
  expect_worker_gone(r)
})

test_that("probe B: an erroring worker reports its error", {
  r <- probe_worker(function() stop("boom"))
  expect_identical(r$worker$state, "failed")
  expect_match(r$worker$error, "boom")
  expect_match(r$diagnostics, "phase=result_written")
  expect_worker_gone(r)
})

test_that("probe C: quit(status = 0) without a result is reported with its phase", {
  r <- probe_worker(function() quit(save = "no", status = 0L, runLast = FALSE))
  expect_identical(r$worker$state, "failed")
  expect_match(r$worker$error, "status 0 without a valid result")
  expect_match(r$diagnostics, "phase=payload_started")
  expect_worker_gone(r)
})

test_that("probe D: an inherited R_TESTS startup hook cannot reach the worker", {
  dir <- tempfile("check-tests-")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE))
  marker <- file.path(dir, "marker")
  # R CMD check exports a relative R_TESTS ("startup.Rs", written in the tests
  # directory) which Rscript sources at start-up even with --vanilla; the real
  # hook of this R installation is sourced first, as check's would be.
  real <- file.path(R.home("share"), "R", "tests-startup.R")
  writeLines(c(if (file.exists(real)) sprintf("source(%s)", deparse(real)),
               sprintf("writeLines('hook ran', %s)", deparse(marker))),
             file.path(dir, "startup.Rs"))
  withr::local_dir(dir)
  withr::local_envvar(R_TESTS = "startup.Rs")

  # Control: an ordinary Rscript does source the hook.
  control <- processx::run(probe_rscript(), c("--vanilla", "-e", "invisible(NULL)"), error_on_status = FALSE)
  expect_identical(control$status, 0L)
  expect_true(file.exists(marker))
  unlink(marker)

  r <- probe_worker(function() list(r_tests = Sys.getenv("R_TESTS"), wd = getwd()))
  expect_identical(r$worker$state, "completed", info = r$worker$error)
  expect_identical(r$worker$result$r_tests, "")
  expect_false(file.exists(marker))
  expect_match(r$diagnostics, "helper_entered\\s+pid=[0-9]+ wd=.* R_TESTS=\\[\\]")
  expect_worker_gone(r)
})

test_that("probe E: the helper does not depend on the working directory", {
  elsewhere <- tempfile("unrelated-cwd-")
  dir.create(elsewhere)
  withr::defer(unlink(elsewhere, recursive = TRUE))
  withr::local_dir(elsewhere)
  r <- probe_worker(function() getwd())
  expect_identical(r$worker$state, "completed", info = r$worker$error)
  expect_identical(normalizePath(r$worker$result), normalizePath(elsewhere))
  expect_worker_gone(r)
})

test_that("a timed-out worker's diagnostics name the phase and the process tree", {
  r <- probe_worker(function() Sys.sleep(60), timeout = 2)
  expect_true(r$worker$timed_out)
  expect_match(r$worker$error, "Timed out")
  expect_match(r$worker$error, "state=alive")
  expect_match(r$worker$error, "phase=payload_started")
  expect_match(r$worker$error, sprintf("tree=%d:", r$process$get_pid()))
  expect_match(r$worker$error, "spawn: wd=")
  expect_worker_gone(r)
})
