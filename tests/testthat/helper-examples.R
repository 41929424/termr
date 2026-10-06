load_example <- function(name) {
  captured <- NULL
  env <- new.env(parent = globalenv())
  env$run <- function(x, ...) captured <<- x
  env$library <- function(...) invisible()
  path <- system.file("examples", paste0(name, ".R"), package = "termr")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "examples", paste0(name, ".R"))
  sys.source(path, envir = env)
  captured
}

# Environment variables that send a child process's temporary files to a
# directory removed at the end of the calling test (killed R processes leave
# their start-up files behind otherwise).
child_tmp <- function() {
  d <- tempfile("child-tmp-")
  dir.create(d)
  withr::defer(unlink(d, recursive = TRUE), envir = parent.frame())
  c(TMPDIR = d, TMP = d, TEMP = d)
}
