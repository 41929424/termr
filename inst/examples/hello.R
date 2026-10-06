# The first termr demo: a label, an input, a button and a result label.
#
# Type a name, press Tab to reach the button and Enter to press it.
# Ctrl+C quits.
#
#   Rscript -e 'termr::run_example("hello")'

library(termr)

ui <- vertical(
  label(
    "termr demo",
    style = style(bold = TRUE)
  ),

  input(
    id = "name",
    placeholder = "Your name"
  ),

  button(
    "Say hello",
    id = "submit"
  ),

  label(
    "",
    id = "result"
  )
)

application <- app(ui)

application$on(
  "button.pressed",
  "#submit",
  function(event, app) {
    name <- app$query_one("#name")$value

    app$query_one("#result")$update(
      paste("Hello,", name)
    )
  }
)

run(application)
