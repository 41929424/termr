# Render one widget tree into portable artifacts without a terminal.
ui <- termr::panel(
  termr::vertical(
    termr::label("Model finished", style = termr::style(foreground = "green", bold = TRUE)),
    termr::label("Accuracy: 0.94")
  ),
  title = "Training report"
)

out <- tempdir()
termr::write_rendered(ui, file.path(out, "screen.txt"), "text", width = 48, height = 8)
termr::write_rendered(ui, file.path(out, "screen.html"), "html", width = 48, height = 8)
termr::write_rendered(ui, file.path(out, "screen.svg"), "svg", width = 48, height = 8)
if (requireNamespace("jsonlite", quietly = TRUE)) {
  termr::write_rendered(ui, file.path(out, "snapshot.json"), "json", width = 48, height = 8)
}
message("Wrote export examples to: ", out)
