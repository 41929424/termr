# Diagnostic only: print structural state immediately before test-workers.R.
# Keep values narrow so CI logs never contain arbitrary environment contents.
worker_ci_snapshot <- function() {
  if (!identical(Sys.getenv("TERMR_CI_RESOURCE_SNAPSHOT"), "1")) return(invisible())
  selected_options <- c("termr.incremental", "termr.skip_layout", "termr.indexed_layout",
                        "termr.profile", "termr.color_mode", "termr.ascii", "termr.reduce_motion")
  option_value <- function(name) {
    value <- getOption(name)
    if (is.null(value)) "<unset>" else paste(as.character(value), collapse = ",")
  }
  selected_env <- c("TERMR_WORKER_DIAG_READRDS", "TERMR_ESC_TIMEOUT_MS", "TERMR_TEST_FILTER", "R_TESTS")
  env_value <- function(name) {
    value <- Sys.getenv(name, unset = "<unset>")
    if (nzchar(value)) value else "<empty>"
  }
  state <- get("termr_env", envir = asNamespace("termr"))
  apps <- state$apps
  active_timers <- sum(vapply(apps, function(app) {
    timers <- app$.__enclos_env__$private$.timers$timers
    sum(vapply(timers, function(timer) isTRUE(timer$active), logical(1)))
  }, integer(1)))
  active_workers <- sum(vapply(apps, function(app) length(app$workers()), integer(1)))
  child_processes <- if (requireNamespace("ps", quietly = TRUE)) {
    tryCatch(length(ps::ps_children(ps::ps_handle(), recursive = FALSE)),
             error = function(e) NA_integer_)
  } else NA_integer_
  descriptor_path <- if (dir.exists("/proc/self/fd")) "/proc/self/fd" else if (
    identical(Sys.info()[["sysname"]], "Darwin") && dir.exists("/dev/fd")) "/dev/fd" else NULL
  descriptor_count <- if (is.null(descriptor_path)) NA_integer_ else
    length(setdiff(list.files(descriptor_path, all.files = TRUE), c(".", "..")))
  temp_count <- length(list.files(tempdir(), all.files = TRUE, recursive = TRUE,
                                 include.dirs = FALSE))
  emit <- function(name, value) cat("WORKER_RESOURCE ", name, "=", value, "\n", sep = "")
  emit("pid", Sys.getpid())
  emit("wd", getwd())
  emit("libPaths", paste(.libPaths(), collapse = .Platform$path.sep))
  emit("options", paste(paste0(selected_options, ":", vapply(selected_options, option_value, "")), collapse = ";"))
  emit("env", paste(paste0(selected_env, ":", vapply(selected_env, env_value, "")), collapse = ";"))
  emit("child_processes", child_processes)
  emit("active_apps", length(apps))
  emit("active_workers", active_workers)
  emit("active_timers", active_timers)
  emit("temp_files", temp_count)
  emit("connections", nrow(showConnections(all = TRUE)))
  emit("file_descriptors", descriptor_count)
  invisible()
}
