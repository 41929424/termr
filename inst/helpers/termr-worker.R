# termr background worker.
#
# Runs one job in a fresh R process:
#   Rscript --vanilla termr-worker.R <job.rds> <result.rds> <progress.log>
# The job is list(fn, args, packages). The result is list(ok = TRUE, value)
# or list(ok = FALSE, message, call). termr_progress() appends one record
# "P<TAB>value<TAB>message" per call to <progress.log>, a channel separate
# from stdout and stderr, which stay free for the job's own output.

local({
  args <- commandArgs(trailingOnly = TRUE)
  job <- readRDS(args[[1]])
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
  result <- tryCatch(
    list(ok = TRUE, value = do.call(job$fn, job$args)),
    error = function(e) {
      list(ok = FALSE, message = conditionMessage(e), call = paste(deparse(conditionCall(e)), collapse = " "))
    }
  )
  saveRDS(result, out)
})
