# File tree with a preview: directories load lazily when expanded, and
# only the first lines of a file are read for the preview.
#
#   Rscript -e 'termr::run_example("file-tree")'      # current directory
#   TERMR_ROOT=/some/dir Rscript ...                    # another directory

library(termr)

root_dir <- normalizePath(Sys.getenv("TERMR_ROOT", getwd()), mustWork = TRUE)

dir_node <- function(path, expanded = FALSE) {
  tree_node(
    basename(path), data = path, expanded = expanded,
    loader = function(node) {
      entries <- list.files(path, full.names = TRUE, all.files = FALSE, no.. = TRUE)
      is_dir <- dir.exists(entries)
      entries <- c(sort(entries[is_dir]), sort(entries[!is_dir]))
      lapply(entries, function(e) if (dir.exists(e)) dir_node(e) else tree_node(basename(e), data = e))
    }
  )
}

preview_text <- function(path) {
  if (dir.exists(path)) {
    return(sprintf("%s\n\n%d entries", path, length(list.files(path))))
  }
  info <- file.info(path)
  head <- sprintf("%s\n%s bytes, modified %s\n", path, format(info$size, big.mark = ","),
                  format(info$mtime, "%Y-%m-%d %H:%M"))
  lines <- tryCatch(readLines(path, n = 60, warn = FALSE), error = function(e) character())
  if (any(grepl("[\001-\010\016-\037]", lines, useBytes = TRUE))) lines <- "(binary file)"
  paste(c(head, lines), collapse = "\n")
}

tree <- tree_view(dir_node(root_dir, expanded = TRUE), id = "tree")
preview <- label("", id = "preview", wrap = "char", style = style(width = "1fr"))

ui <- vertical(
  horizontal(
    panel(tree, title = "Files", style = style(width = "40%", height = "1fr")),
    panel(scroll_view(preview, focusable = FALSE), title = "Preview", style = style(width = "1fr", height = "1fr")),
    style = style(height = "1fr")
  ),
  label("arrows navigate   Right/Left expand/collapse   q quit", style = style(foreground = "$muted"))
)

browser <- app(
  ui,
  bind("q", "quit"),
  on("tree.node_selected", "#tree", function(event, app) {
    app$query_one("#preview")$update(preview_text(event$data$data))
  })
)

run(browser)
