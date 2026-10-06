# Measure line-vector editor behavior at several document sizes.
# Rscript tools/bench/textarea-storage.R output.csv [100000,1000000,5000000,20000000]
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else tempfile(fileext = ".csv")
sizes <- if (length(args) > 1L) as.numeric(strsplit(args[[2L]], ",", fixed = TRUE)[[1L]]) else
  c(100000, 1000000, 5000000, 20000000)
rows <- lapply(sizes, function(bytes) {
  cat("TextArea", bytes, "bytes\n")
  line <- strrep("x", 100L)
  n <- max(1L, floor(bytes / 101L))
  text <- paste(rep(line, n), collapse = "\n")
  start <- proc.time()[["elapsed"]]
  ed <- text_area(text)
  construct <- (proc.time()[["elapsed"]] - start) * 1000
  start <- proc.time()[["elapsed"]]
  ed$goto_line(1L, 1L); ed$insert("z")
  edit_start <- (proc.time()[["elapsed"]] - start) * 1000
  start <- proc.time()[["elapsed"]]
  ed$undo(); ed$redo()
  undo_redo <- (proc.time()[["elapsed"]] - start) * 1000
  start <- proc.time()[["elapsed"]]
  ed$find("needle")
  find_ms <- (proc.time()[["elapsed"]] - start) * 1000
  start <- proc.time()[["elapsed"]]
  render_widget(ed, 80L, 12L)
  render_ms <- (proc.time()[["elapsed"]] - start) * 1000
  data.frame(bytes = nchar(text, type = "bytes"), lines = ed$n_lines,
             construct_ms = construct, insert_start_ms = edit_start,
             undo_redo_ms = undo_redo, find_miss_ms = find_ms, render_ms = render_ms,
             undo_chars = ed$history_size)
})
result <- do.call(rbind, rows)
utils::write.csv(result, output, row.names = FALSE)
print(result, row.names = FALSE)
