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
  for (phase in c("helper_entered", "helper_started", "args_received",
                  "job_path_resolved", "job_file_exists", "before_read_rds",
                  "after_read_rds", "payload_validated", "payload_loaded",
                  "payload_started", "payload_finished", "result_written")) {
    expect_match(r$diagnostics, phase, fixed = TRUE)
  }
  expect_match(r$diagnostics, "job_file_exists.*exists=TRUE size=[1-9][0-9]*")
  expect_worker_gone(r)
})

test_that("probe A2: base function, scalar closure and parent readRDS complete", {
  withr::local_envvar(TERMR_WORKER_DIAG_READRDS = "1")
  parent_log <- capture.output({
    base_result <- probe_worker(base::identity, args = list(17L))
    scalar <- 11L
    closure_result <- probe_worker(function() scalar + 1L)
  }, type = "message")
  expect_identical(base_result$worker$result, 17L)
  expect_identical(closure_result$worker$result, 12L)
  expect_match(paste(parent_log, collapse = "\n"), "parent_before_read_rds")
  expect_match(paste(parent_log, collapse = "\n"), "parent_after_read_rds")
  expect_match(paste(parent_log, collapse = "\n"), "fn_env_chain=")
  expect_worker_gone(base_result)
  expect_worker_gone(closure_result)
})

test_that("worker payloads omit unused caller bindings but retain lexical captures", {
  capture_env <- new.env(parent = baseenv())
  capture_env$junk <- raw(50 * 1024^2)
  capture_env$scalar <- 11L
  for (i in seq_len(256L)) {
    assign(sprintf("unrelated_%03d", i), seq_len(32L), envir = capture_env)
  }
  fn <- eval(quote(function() scalar + 1L), envir = capture_env)

  old_payload <- list(fn = fn, args = list(), packages = character())
  old_fn_bytes <- length(serialize(fn, NULL, version = 3L))
  old_bytes <- length(serialize(old_payload, NULL, version = 3L))
  payload <- worker_prepare_payload(fn, list(), character())
  new_fn_bytes <- length(serialize(payload$fn, NULL, version = 3L))
  new_bytes <- length(serialize(payload, NULL, version = 3L))
  expect_gt(old_bytes, 50 * 1024^2)
  expect_lt(new_bytes, 1024^2)

  old_rds <- tempfile("termr-old-payload-")
  new_rds <- tempfile("termr-new-payload-")
  withr::defer(unlink(c(old_rds, new_rds)))
  saveRDS(old_payload, old_rds, compress = FALSE)
  saveRDS(payload, new_rds, compress = FALSE)
  old_rds_bytes <- file.info(old_rds)$size
  new_rds_bytes <- file.info(new_rds)$size
  expect_gt(old_rds_bytes, 50 * 1024^2)
  expect_lt(new_rds_bytes, 1024^2)

  if (identical(Sys.getenv("TERMR_WORKER_DIAG_ENV"), "1")) {
    message("WORKER_CAPTURE_DIAG old_fn_bytes=", old_fn_bytes,
            " new_fn_bytes=", new_fn_bytes,
            " old_payload_bytes=", old_rds_bytes,
            " new_payload_bytes=", new_rds_bytes,
            " unused_binding_bytes=", as.numeric(object.size(capture_env$junk)),
            " source_env={", worker_environment_summary(fn, max_inspected_bindings = 1024L), "}")
  }

  used <- eval(quote(function() length(junk)), envir = capture_env)
  used_payload <- worker_prepare_payload(used, list(), character())
  used_bytes <- length(serialize(used_payload, NULL, version = 3L))
  expect_gt(used_bytes, 50 * 1024^2)

  withr::local_envvar(TERMR_WORKER_DIAG_READRDS = "1")
  result <- probe_worker(fn)
  expect_identical(result$worker$result, 12L)
  expect_match(result$diagnostics, "phase=result_written")
  expect_match(result$worker$.__enclos_env__$private$spawn_info,
               "job_bytes=[0-9]{1,6}($|[^0-9])")
  expect_worker_gone(result)
})

test_that("worker captures preserve nested helpers and recursive closures", {
  multiplier <- 3L
  helper <- function(x) x * multiplier
  nested <- function(x) helper(x) + stats::median(1:3)
  nested_result <- probe_worker(nested, args = list(x = 4L))
  expect_identical(nested_result$worker$result, 14L)
  expect_match(nested_result$diagnostics, "phase=result_written")
  expect_worker_gone(nested_result)

  factorial <- function(n) if (n <= 1L) 1L else n * factorial(n - 1L)
  recursive <- probe_worker(function(n) factorial(n), args = list(n = 6L))
  expect_identical(recursive$worker$result, 720L)
  expect_match(recursive$diagnostics, "phase=result_written")
  expect_worker_gone(recursive)
})

test_that("unsupported dynamic lookup and active bindings fail before spawn", {
  expect_error(worker_prepare_payload(function() get("value"), list(), character()),
               "dynamic lexical lookup.*args")
  evaluated <- FALSE
  env <- new.env(parent = baseenv())
  makeActiveBinding("active_value", function() {
    evaluated <<- TRUE
    1L
  }, env)
  active_fn <- eval(quote(function() active_value), envir = env)
  expect_error(worker_prepare_payload(active_fn, list(), character()), "active binding.*args")
  expect_false(evaluated)
})

test_that("the exact testthat worker job is readable by a clean Rscript", {
  pilot <- test_app(app(label("x")), 20, 2)
  withr::defer(pilot$stop())
  worker <- pilot$app$run_worker(function() 42, timeout = 25)
  job <- worker$.__enclos_env__$private$files[[1L]]
  expect_true(file.exists(job))
  expect_gt(file.info(job)$size, 0)
  # The parent has not polled the worker yet, so its normal cleanup cannot
  # remove this exact job before the independent Rscript reads it.
  script <- "cat('standalone_before_read_rds\\n'); readRDS(commandArgs(trailingOnly = TRUE)[[1L]]); cat('standalone_after_read_rds\\n')"
  standalone <- processx::run(probe_rscript(), c("--vanilla", "-e", script, job),
                              env = c("current", R_TESTS = ""), timeout = 25000,
                              error_on_status = FALSE)
  expect_identical(standalone$status, 0L, info = standalone$stderr)
  expect_match(standalone$stdout, "standalone_before_read_rds", fixed = TRUE)
  expect_match(standalone$stdout, "standalone_after_read_rds", fixed = TRUE)
  pilot$wait_for_workers(30)
  expect_identical(worker$state, "completed", info = worker$error)
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
