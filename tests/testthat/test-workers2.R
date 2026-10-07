rscript <- function() file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

termr_temp_files <- function() list.files(tempdir(), pattern = "^termr-(job|result|progress|worker-tmp|trace)-")

test_that("worker stdout and stderr are streamed, separate from progress", {
  skip_on_cran()
  out <- character()
  err <- character()
  prog <- numeric()
  events <- character()
  a <- app(label("x"))
  a$on("*", function(event, app) if (startsWith(event$type, "worker.")) events <<- c(events, event$type))
  pilot <- test_app(a, 20, 2)
  w <- a$run_worker(
    function() {
      cat("hello out\n")
      message("hello err")
      termr_progress(0.5, "half")
      cat("PROGRESS\t0.99\tfake\n")
      cat("\036PROGRESS\t0.77\tfake2\n")
      "ok"
    },
    on_stdout = function(line, app) out <<- c(out, line),
    on_stderr = function(line, app) err <<- c(err, line),
    on_progress = function(value, message, app) prog <<- c(prog, value)
  )
  pilot$wait_for_workers(60)
  pilot$step()
  expect_identical(w$state, "completed")
  expect_identical(w$result, "ok")
  expect_identical(out[1], "hello out")
  expect_true("PROGRESS 0.99 fake" %in% out)
  expect_identical(err, "hello err")
  expect_identical(prog, 0.5)
  expect_true(all(c("worker.stdout", "worker.stderr", "worker.progress", "worker.completed") %in% events))
  expect_identical(w$stdout[1], "hello out")
})

test_that("control characters in worker output never reach the screen", {
  skip_on_cran()
  got <- character()
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  w <- a$run_worker(
    function() {
      cat("a\033[31mred\033]52;c;AAAA\007b\n")
      1
    },
    on_stdout = function(line, app) got <<- c(got, line)
  )
  pilot$wait_for_workers(60)
  expect_identical(got, "a[31mred]52;c;AAAAb")
})

test_that("workers time out and report it", {
  skip_on_cran()
  msg <- NULL
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  w <- a$run_worker(function() {
    Sys.sleep(60)
    1
  }, timeout = 1.5, on_error = function(message, app) msg <<- message)
  t0 <- now_seconds()
  pilot$wait_for_workers(30)
  expect_identical(w$state, "failed")
  expect_true(w$timed_out)
  expect_match(msg, "Timed out")
  expect_lt(now_seconds() - t0, 20)
  expect_error(a$run_worker(function() 1, timeout = -1), "positive")
})

test_that("temporary files are unique and removed however the worker ends", {
  skip_on_cran()
  before <- termr_temp_files()
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  ok <- a$run_worker(function() 1)
  bad <- a$run_worker(function() stop("x"))
  slow <- a$run_worker(function() Sys.sleep(60))
  during <- setdiff(termr_temp_files(), before)
  expect_gte(length(during), 3L)
  expect_identical(anyDuplicated(during), 0L)
  slow$cancel()
  pilot$wait_for_workers(60)
  expect_identical(c(ok$state, bad$state, slow$state), c("completed", "failed", "cancelled"))
  expect_identical(setdiff(termr_temp_files(), before), character())
})

test_that("cancel stops the process promptly and workers do not keep the app alive", {
  skip_on_cran()
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  w <- a$run_worker(function() Sys.sleep(60))
  pid <- w$pid()
  expect_false(is.na(pid))
  t0 <- now_seconds()
  w$cancel(grace = 0.2)
  expect_lt(now_seconds() - t0, 10)
  expect_identical(w$state, "cancelled")
  # A cancelled worker no longer references the app.
  expect_null(w$.__enclos_env__$private$app)
})

test_that("run_process streams output and reports the exit status", {
  skip_on_cran()
  out <- character()
  err <- character()
  status <- NULL
  done <- NULL
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  w <- a$run_process(
    rscript(), c("-e", "cat('one\\n'); cat('two\\n'); message('oops'); quit(status = 0)"),
    on_stdout = function(line, app) out <<- c(out, line),
    on_stderr = function(line, app) err <<- c(err, line),
    on_exit = function(s, app) status <<- s,
    on_complete = function(result, app) done <<- result
  )
  expect_identical(w$kind, "process")
  pilot$wait_for_workers(60)
  expect_identical(out, c("one", "two"))
  expect_identical(err, "oops")
  expect_identical(status, 0L)
  expect_identical(done, 0L)
  expect_identical(w$state, "completed")
})

test_that("a failing process fails with its status; a missing program errors", {
  skip_on_cran()
  msg <- NULL
  status <- NULL
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  w <- a$run_process(rscript(), c("-e", "quit(status = 3)"), on_error = function(message, app) msg <<- message,
                     on_exit = function(s, app) status <<- s)
  pilot$wait_for_workers(60)
  expect_identical(w$state, "failed")
  expect_match(msg, "status 3")
  expect_match(msg, "PID=[0-9]+; executable=.*state=exited.*exit_status=3")
  expect_identical(status, 3L)
  expect_error(a$run_process("definitely-not-a-program-xyz"), "Could not start")
  expect_error(a$run_process(c("a", "b")), "command")
  expect_error(a$run_process("x", args = 1), "character vector")
})

test_that("process arguments are never interpreted by a shell", {
  skip_on_cran()
  marker <- tempfile("termr-inject-")
  out <- character()
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  nasty <- paste0("a; touch ", marker, " && echo $HOME `id` | cat > ", marker)
  w <- a$run_process(rscript(), c("-e", "cat(commandArgs(TRUE), sep = '\\n')", "--args", nasty),
                     on_stdout = function(line, app) out <<- c(out, line))
  pilot$wait_for_workers(60)
  expect_false(file.exists(marker))
  expect_identical(out, c("--args", nasty))
})

test_that("owner widgets cancel their processes when removed", {
  skip_on_cran()
  owner <- label("owner")
  a <- app(vertical(owner, label("x")))
  pilot <- test_app(a, 20, 3)
  w <- owner$run_process(rscript(), c("-e", "Sys.sleep(60)"), env = child_tmp())
  expect_true(w$is_running())
  owner$remove()
  pilot$step()
  expect_identical(w$state, "cancelled")
})

test_that("Pilot$run_worker waits for the result", {
  skip_on_cran()
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  w <- pilot$run_worker(function(x) x + 1, args = list(x = 1))
  expect_identical(w$result, 2)
  seen <- character()
  completed <- NULL
  pilot$app$on("worker.completed", function(event, app) completed <<- event$data$worker)
  streamed <- pilot$app$run_worker(function() cat("tick\n"), on_stdout = function(line, app) seen <<- c(seen, line))
  # If the child dies before stdout reaches the callback, stop at that point
  # and report its diagnostics instead of timing out on a generic predicate.
  pilot$wait_for(function(app) !streamed$is_running(), timeout = 30)
  expect_identical(seen, "tick")
  expect_identical(streamed$state, "completed")
  expect_identical(completed, streamed)
})

test_that("installed worker helper and R CMD check startup hooks are safe", {
  helper <- system.file("helpers", "termr-worker.R", package = "termr")
  expect_true(nzchar(helper) && file.exists(helper))

  old <- Sys.getenv("R_TESTS", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("R_TESTS") else Sys.setenv(R_TESTS = old), add = TRUE)
  hook <- tempfile(fileext = ".R")
  marker <- tempfile()
  on.exit(unlink(c(hook, marker)), add = TRUE)
  writeLines(sprintf("writeLines('startup hook ran', %s)", deparse(marker)), hook)
  Sys.setenv(R_TESTS = hook)

  # Confirm that this hook actually executes in a contaminated Rscript.
  contaminated <- processx::run(rscript(), c("--vanilla", "-e", "invisible(NULL)"),
                               error_on_status = FALSE)
  expect_identical(contaminated$status, 0L)
  expect_true(file.exists(marker))
  unlink(marker)

  pilot <- test_app(app(label("x")), 20, 2)
  w <- pilot$run_worker(function() Sys.getenv("R_TESTS"))
  expect_identical(w$state, "completed")
  expect_identical(w$result, "")
  expect_false(file.exists(marker))
  pilot$stop()
})

test_that("a crashed worker reports stderr and exit status promptly", {
  skip_on_cran()
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  w <- a$run_worker(function() {
    cat("worker crash diagnostic\n", file = stderr())
    flush(stderr())
    quit(save = "no", status = 23L, runLast = FALSE)
  })

  pilot$wait_for_workers(10)
  expect_identical(w$state, "failed")
  expect_match(w$error, "status 23")
  expect_match(w$error, "worker crash diagnostic")
  expect_match(w$error, "PID=[0-9]+; executable=.*Rscript")
  expect_match(w$error, "state=exited.*exit_status=23; result_exists=FALSE")
  expect_identical(w$stderr, "worker crash diagnostic")
  expect_length(a$workers(), 0L)
  expect_false(w$.__enclos_env__$private$process$is_alive())
})

test_that("installed workers use parent library paths from an unrelated cwd", {
  library_path <- tempfile("termr-worker-library-")
  dir.create(library_path)
  r6 <- find.package("R6")
  expect_true(file.copy(r6, library_path, recursive = TRUE))
  old_libs <- .libPaths()
  on.exit({ .libPaths(old_libs); unlink(library_path, recursive = TRUE) }, add = TRUE)
  .libPaths(c(library_path, old_libs))
  withr::local_dir(tempdir())
  pilot <- test_app(app(label("x")), 20, 2)
  on.exit(pilot$stop(), add = TRUE)
  w <- pilot$run_worker(function() list(path = find.package("R6"), tty = isatty(stdin())),
                        packages = "R6")
  expect_identical(w$state, "completed")
  expect_identical(normalizePath(w$result$path), normalizePath(file.path(library_path, "R6")))
  # Windows CRT reports the NUL character device as a TTY. The processx
  # stdin configuration is portable; the real Unix PTY asserts isatty too.
  expect_null(w$.__enclos_env__$private$process$get_input_file())
  if (.Platform$OS.type != "windows") expect_false(w$result$tty)
})

test_that("an exited worker without a result is removed even at status zero", {
  pilot <- test_app(app(label("x")), 20, 2)
  on.exit(pilot$stop(), add = TRUE)
  w <- pilot$app$run_worker(function() quit(save = "no", status = 0L, runLast = FALSE))
  pilot$wait_for_workers(10)
  expect_identical(w$state, "failed")
  expect_match(w$error, "status 0 without a valid result")
  expect_match(w$error, "result_exists=FALSE")
  expect_length(pilot$app$workers(), 0L)
})

test_that("Pilot timeouts include bounded process diagnostics", {
  pilot <- test_app(app(label("x")), 20, 2)
  on.exit(pilot$stop(), add = TRUE)
  w <- pilot$app$run_worker(function() Sys.sleep(60))
  expect_error(pilot$wait_for_workers(0), "PID=[0-9]+; executable=.*state=alive.*result_exists=FALSE")
  expect_error(pilot$wait_for(function(app) FALSE, timeout = 0), "PID=[0-9]+; executable=")
  w$cancel(grace = 0)
  pilot$step()
  expect_length(pilot$app$workers(), 0L)
})

test_that("an exited worker cannot hang polling on a descendant-held pipe", {
  pilot <- test_app(app(label("x")), 20, 2)
  on.exit(pilot$stop(), add = TRUE)
  marker <- tempfile()
  on.exit(unlink(marker), add = TRUE)
  w <- pilot$app$run_worker(function(marker) {
    child <- processx::process$new(
      file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
      c("--vanilla", "-e", "Sys.sleep(5)"), stdin = NULL, stdout = "", stderr = "",
      env = c("current", R_TESTS = ""), cleanup = FALSE, cleanup_tree = FALSE)
    writeLines(as.character(child$get_pid()), marker)
    quit(save = "no", status = 0L, runLast = FALSE)
  }, args = list(marker = marker))
  p <- w$.__enclos_env__$private$process
  on.exit(p$kill_tree(), add = TRUE)
  p$wait(5000)
  expect_false(p$is_alive())
  expect_true(file.exists(marker))
  expect_true(p$is_incomplete_output())
  started <- now_seconds()
  pilot$wait_for_workers(5)
  expect_lt(now_seconds() - started, 2)
  expect_identical(w$state, "failed")
  expect_length(pilot$app$workers(), 0L)
  expect_length(p$kill_tree(), 0L) # No orphan descendant remains.
})

test_that("inline workers still fire callbacks and keep the app unreferenced", {
  a <- app(label("x"))
  pilot <- test_app(a, 20, 2)
  w <- a$run_worker(function() 5, inline = TRUE)
  expect_identical(w$result, 5)
  expect_null(w$.__enclos_env__$private$app)
})

test_that("a finite output backlog is fully delivered after process exit", {
  pilot <- test_app(app(label("x")), 20, 2)
  on.exit(pilot$stop(), add = TRUE)
  count <- 0L
  w <- pilot$app$run_worker(function() cat(paste0(seq_len(5000), "\n"), sep = ""),
                            on_stdout = function(line, app) count <<- count + 1L)
  # Let output accumulate before polling; Unix pipe backpressure may keep
  # the producer alive, while Windows can buffer the entire payload.
  w$.__enclos_env__$private$process$wait(1000)
  pilot$wait_for_workers(10)
  expect_identical(w$state, "completed")
  expect_identical(count, 5000L)
  expect_identical(tail(w$stdout, 1L), "5000")
  expect_length(w$stdout, 1000L)
})
