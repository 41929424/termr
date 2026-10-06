# Phase 4 demo: focus, buttons, inputs and bindings.
#
# Tab / Shift+Tab move between the fields, Enter in the name field or on
# the button greets you, Escape quits.
#
#   Rscript -e 'termr::run_example("form")'

library(termr)

ui <- vertical(
  label("termr demo", style = style(bold = TRUE, foreground = "bright_cyan")),
  label("Type your name, then press Enter or the button.", style = style(foreground = "bright_black")),
  input(id = "name", placeholder = "Your name"),
  horizontal(
    button("Say hello", id = "submit", variant = "primary"),
    button("Clear", id = "clear"),
    button("Quit", id = "quit", variant = "error"),
    style = style(height = "auto")
  ),
  label("", id = "result", style = style(bold = TRUE, margin = c(1, 0, 0, 1))),
  label("Tab: next field   Shift+Tab: previous   Esc: quit",
        style = style(foreground = "bright_black", margin = c(1, 0, 0, 0))),
  style = style(padding = c(1, 2))
)

greet <- function(app) {
  name <- trimws(app$query_one("#name")$value)
  app$query_one("#result")$update(
    if (nzchar(name)) paste0("Hello, ", name, "!") else "Please type a name first."
  )
}

application <- app(ui, bind("escape", "quit"))

application$on("button.pressed", "#submit", function(event, app) greet(app))
application$on("input.submitted", "#name", function(event, app) greet(app))
application$on("button.pressed", "#clear", function(event, app) {
  app$query_one("#name")$clear()
  app$query_one("#result")$update("")
  app$query_one("#name")$focus()
})
application$on("button.pressed", "#quit", function(event, app) app$exit())

run(application)
