# Static export baseline. Run from the package root with Rscript.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

ui <- do.call(termr::vertical, c(
  lapply(seq_len(18), function(i) termr::label(sprintf("Row %02d  界  ready", i))),
  list(style = termr::style(width = "1fr", height = "1fr"))
))

measure <- function(call, n = 5L) {
  env <- parent.frame()
  elapsed <- replicate(n, unname(system.time(eval(call, envir = env))["elapsed"]))
  c(median_seconds = median(elapsed), min_seconds = min(elapsed))
}

for (size in list(c(80L, 24L), c(120L, 40L), c(200L, 60L), c(300L, 100L))) {
  width <- size[[1]]; height <- size[[2]]
  outputs <- list(
    text = termr::render_text(ui, width, height),
    markdown = termr::render_markdown(ui, width, height),
    html = termr::render_html(ui, width, height),
    svg = termr::render_svg(ui, width, height)
  )
  if (requireNamespace("jsonlite", quietly = TRUE)) outputs$json <- termr::snapshot_json(ui, width, height)
  cat(sprintf("\n%d x %d (%d style runs)\n", width, height,
    length(termr::screen_snapshot(ui, width, height)$runs)))
  for (name in names(outputs)) {
    call <- switch(name,
      text = quote(termr::render_text(ui, width, height)),
      markdown = quote(termr::render_markdown(ui, width, height)),
      html = quote(termr::render_html(ui, width, height)),
      svg = quote(termr::render_svg(ui, width, height)),
      json = quote(termr::snapshot_json(ui, width, height)))
    timing <- measure(call, n = 5L)
    cat(sprintf("%-9s median %.4fs, min %.4fs, %d bytes\n", name,
      timing[[1]], timing[[2]], nchar(outputs[[name]], type = "bytes")))
  }
}
