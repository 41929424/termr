# termr background worker.
#
# Runs one job in a fresh R process:
#   Rscript --vanilla termr-worker.R <job.rds> <result.rds> <progress.log> [<trace.log>]
# The job is list(fn, args, packages). The result is list(ok = TRUE, value)
# or list(ok = FALSE, message, call). termr_progress() appends one record
# "P<TAB>value<TAB>message" per call to <progress.log>, a channel separate
# from stdout and stderr, which stay free for the job's own output.
#
# <trace.log> is a diagnostic channel for the parent: one line per bootstrap
# phase (including the boundaries around readRDS), so a worker that never
# starts can be told apart from one that stalls while loading its payload.

# First executable statements: nothing before this line may fail or load a
# package, and the job payload has not been read yet.
local({
  args <- commandArgs(trailingOnly = TRUE)
  trace_path <- if (length(args) >= 4L) args[[4]] else NULL
  if (is.null(trace_path) || !nzchar(trace_path)) return(invisible())
  try({
    cat(sprintf("%s\thelper_entered\tpid=%d wd=%s R_TESTS=[%s] libPaths=[%s]\n",
                format(Sys.time(), "%H:%M:%OS3"), Sys.getpid(), getwd(), Sys.getenv("R_TESTS"),
                paste(.libPaths(), collapse = .Platform$path.sep)),
        file = trace_path, append = TRUE)
  }, silent = TRUE)
})

local({
  args <- commandArgs(trailingOnly = TRUE)
  trace_path <- if (length(args) >= 4L) args[[4]] else NULL
  trace_phase <- function(phase, detail = "") {
    if (is.null(trace_path) || !nzchar(trace_path)) return(invisible())
    try(cat(sprintf("%s\t%s\t%s\n", format(Sys.time(), "%H:%M:%OS3"), phase, detail),
            file = trace_path, append = TRUE), silent = TRUE)
    invisible()
  }
  trace_phase("helper_started")
  trace_phase("args_received", paste0("count=", length(args)))
  if (length(args) < 3L || !nzchar(args[[1]]) || !nzchar(args[[2]])) {
    stop("The termr worker helper needs job, result and progress paths.", call. = FALSE)
  }
  job_path <- normalizePath(args[[1]], mustWork = FALSE)
  trace_phase("job_path_resolved", paste0("basename=", basename(job_path), " path=", job_path))
  job_exists <- file.exists(job_path)
  job_size <- if (job_exists) file.info(job_path)$size else NA_real_
  trace_phase("job_file_exists", paste0("exists=", job_exists, " size=", job_size))
  if (!job_exists || is.na(job_size) || job_size <= 0) {
    stop("The termr worker job file is missing or empty.", call. = FALSE)
  }
  trace_phase("before_read_rds")
  job <- readRDS(job_path)
  trace_phase("after_read_rds", paste0("object_bytes=", as.numeric(object.size(job))))
  if (!is.list(job) || !is.function(job$fn) || !is.list(job$args) ||
      !is.character(job$packages)) {
    stop("The termr worker job payload is invalid.", call. = FALSE)
  }
  trace_phase("payload_validated")
  trace_phase("payload_loaded")
  out <- args[[2]]
  progress_path <- if (length(args) >= 3L) args[[3]] else NULL
  for (pkg in job$packages) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  assign("termr_progress", function(value = NA, message = "") {
    if (!is.null(progress_path)) {
      message <- gsub("[\t\r\n]", " ", paste(message, collapse = " "))
      cat("P\t", format(value), "\t", message, "\n", sep = "", file = progress_path, append = TRUE)
    }
    invisible(value)
  }, envir = globalenv())
  trace_phase("payload_started")
  result <- tryCatch(
    list(ok = TRUE, value = do.call(job$fn, job$args)),
    error = function(e) {
      list(ok = FALSE, message = conditionMessage(e), call = paste(deparse(conditionCall(e)), collapse = " "))
    }
  )
  trace_phase("payload_finished", if (isTRUE(result$ok)) "ok" else "error")
  saveRDS(result, out)
  trace_phase("result_written")
})
