#' Run a bundled example app
#'
#' @param name Example name. Call `run_example()` without arguments to list
#'   the available examples.
#' @return The example names (invisibly when an example was run).
#' @export
#' @examples
#' run_example()
run_example <- function(name = NULL) {
  dir <- system.file("examples", package = "termr")
  available <- sub("\\.R$", "", list.files(dir, pattern = "\\.R$"))
  if (is.null(name)) return(available)
  if (!(name %in% available)) {
    stop(sprintf("Unknown example \"%s\". Available: %s.", name, paste(available, collapse = ", ")),
         call. = FALSE)
  }
  source(file.path(dir, paste0(name, ".R")), local = new.env(parent = globalenv()))
  invisible(available)
}
