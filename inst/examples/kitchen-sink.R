# Kitchen sink: every built-in widget, in tabs.
#
#   Rscript -e 'termr::run_example("kitchen-sink")'

library(termr)

controls <- vertical(
  input(placeholder = "Type here (validated: letters only)", id = "text",
        validate = function(x) if (grepl("[^A-Za-z ]", x)) "Letters only"),
  input(placeholder = "Password", password = TRUE),
  horizontal(checkbox("Enable logs", value = TRUE, id = "logs"), checkbox("Verbose"), style = style(height = "auto")),
  radio_set(radio_button("CSV", "csv"), radio_button("JSON", "json"), radio_button("Parquet", "parquet"),
            selected = "csv", id = "format"),
  dropdown(c(Small = "s", Medium = "m", Large = "l"), value = "m", id = "size"),
  horizontal(
    button("Default"), button("Primary", variant = "primary"), button("Success", variant = "success"),
    button("Warning", variant = "warning"), button("Error", variant = "error"), button("Disabled", disabled = TRUE),
    style = style(height = "auto")
  ),
  style = style(padding = c(1, 1))
)

display <- vertical(
  horizontal(metric("Accuracy", 0.943, delta = 0.012), metric("Loss", 0.231, delta = -0.04),
             metric("Rows", 1234567), style = style(height = "auto")),
  rule(title = "progress"),
  progress_bar(0.42),
  progress_bar(NA),
  spinner("Working..."),
  rule(title = "sparkline"),
  sparkline(sin(seq(0, 6 * pi, length.out = 80)) + 1),
  key_value(list(Package = "termr", Version = as.character(utils::packageVersion("termr")), Widgets = "lots")),
  style = style(padding = c(1, 1))
)

data <- vertical(data_table(mtcars, zebra = TRUE, style = style(height = "1fr")))

tree <- tree_view(tree_node("termr",
  tree_node("widgets", "label", "button", "input", "data_table", "tree_view", expanded = TRUE),
  tree_node("layouts", "vertical", "horizontal", "grid_layout", "scroll_view"),
  expanded = TRUE))

text <- scroll_view(label(paste(rep(
  "termr renders every frame into a virtual screen and sends only the cells that changed to the terminal.",
  20), collapse = "\n\n"), wrap = "word"))

ui <- vertical(
  tabs(
    tab("Controls", controls),
    tab("Display", display),
    tab("Data", data),
    tab("Tree", tree),
    tab("Text", text),
    id = "tabs"
  ),
  label("Ctrl+PgUp/PgDn switch tabs   Ctrl+P commands   F12 debug   Ctrl+D dialog   Ctrl+N notify   q quit",
        style = style(foreground = "$muted"))
)

sink_app <- app(
  ui,
  debug = TRUE,
  bind("q", "quit", "Quit"),
  bind("ctrl+n", function(app) app$notify("Hello from termr!", title = "Notification", severity = "success"), "Notify"),
  bind("ctrl+t", function(app) app$theme <- if (app$theme$name == "dark") "light" else "dark", "Switch theme"),
  bind("ctrl+d", function(app) app$push_screen(confirm_dialog(
    "Do you like termr?",
    on_confirm = function(app) app$notify("Thank you!"),
    on_cancel = function(app) app$notify("Fair enough.", severity = "warning")
  )), "Show dialog")
)

run(sink_app)
