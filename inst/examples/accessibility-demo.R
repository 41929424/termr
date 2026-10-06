# Accessibility and theme preview: the same screen in every built-in theme,
# with controls whose states (focus, selection, error, disabled) stay
# distinguishable without colour.
#
#   Rscript -e 'termr::run_example("accessibility-demo")'
#   NO_COLOR=1 Rscript -e 'termr::run_example("accessibility-demo")'   # monochrome
#
# Keys: t next theme   m toggle reduced motion   F1 help   q quit

library(termr)

themes <- c("default", "dark", "light", "high-contrast")
theme_name <- signal("default")

ui <- vertical(
  label("Accessibility preview", style = style(bold = TRUE, foreground = "$accent")),
  label(function() paste("Theme:", theme_name()), id = "theme"),
  horizontal(
    vertical(
      button("Primary", variant = "primary"),
      button("Default"),
      button("Disabled", disabled = TRUE),
      checkbox("A checkbox", value = TRUE),
      radio_set(radio_button("Option one"), radio_button("Option two"), selected = "Option one"),
      style = style(width = 28, height = "auto")
    ),
    vertical(
      input("valid text", id = "ok"),
      input("", id = "bad", validate = function(v) "This field is required"),
      dropdown(c("small", "medium", "large"), value = "medium"),
      progress_bar(0.6),
      spinner("working (still when motion is reduced)"),
      style = style(width = "1fr", height = "auto")
    ),
    style = style(height = "auto")
  ),
  option_list(c("selected row is reversed or highlighted", "second option", "third option"), style = style(height = 4)),
  label("Focus: heavier border / bold / reverse.  Error: bold underline.  Disabled: dim.", style = style(foreground = "$muted")),
  label("t theme   m reduced motion   q quit", style = style(foreground = "$muted")),
  style = style(padding = c(0, 1))
)

preview <- app(
  ui, title = "Accessibility",
  bind("q", "quit", "Quit"),
  bind("t", function(app) {
    next_theme <- themes[[match(app$theme$name, themes, nomatch = 0L) %% length(themes) + 1L]]
    app$theme <- next_theme
    theme_name(next_theme)
  }, "Next theme"),
  bind("m", function(app) {
    app$reduce_motion <- !isTRUE(motion_reduced(app))
    app$notify(if (app$reduce_motion) "Reduced motion on" else "Reduced motion off", timeout = 2)
  }, "Toggle reduced motion")
)

run(preview)
