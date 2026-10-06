# A small multi-line editor built on text_area(): selection, undo/redo,
# find, line/column status. It edits a demo text in memory (saving would be
# one line: writeLines(editor$value, path)).
#
#   Rscript -e 'termr::run_example("editor")'
#
# Keys: Ctrl+Z / Ctrl+Y undo / redo   Ctrl+F find (Enter next, Esc close)
#       Ctrl+C/X/V copy, cut, paste (copy also reaches the system clipboard
#       on terminals with OSC 52)   Ctrl+G go to line   F1 help   Ctrl+Q quit

library(termr)

demo_text <- paste(c(
  "# termr editor demo",
  "",
  "This is a text_area(): edit freely.",
  "Select with Shift+arrows, undo with Ctrl+Z.",
  "",
  "Wrapped lines follow the width of the window, so a long sentence like this one",
  "keeps reading naturally when you resize the terminal.",
  "",
  "Unicode is handled by grapheme: caf\u00e9, \u4e2d\u6587, emoji \U0001F600 and family \U0001F468\u200d\U0001F469\u200d\U0001F467.",
  "",
  sprintf("line %d of filler text to scroll through", 1:40)
), collapse = "\n")

status <- function(app) {
  ed <- app$query_one("#editor")
  sel <- nchar(ed$selection)
  position <- ed$cursor_position()
  app$query_one("#position")$update(sprintf(
    "Ln %d, Col %d%s   %d lines%s", position$line, position$display_column,
    if (sel) sprintf("   (%d selected)", sel) else "", ed$n_lines,
    if (ed$can_undo) "   modified" else ""
  ))
}

editor <- app(
  vertical(
    label("termr editor", style = style(bold = TRUE, foreground = "$accent")),
    text_area(demo_text, id = "editor", line_numbers = TRUE, wrap = TRUE, auto_indent = TRUE,
              style = style(height = "1fr")),
    label("", id = "position", style = style(background = "$surface")),
    style = style(padding = c(0, 1))
  ),
  title = "Editor",
  bind("ctrl+q", "quit", "Quit"),
  on("textarea.changed", function(event, app) status(app)),
  on("textarea.selection_changed", function(event, app) status(app))
)
editor$add_command(command("Toggle line wrapping", function(app) {
  ed <- app$query_one("#editor")
  ed$set(wrap = !ed$wrap)
}, category = "View"))
editor$call_later(function(app) {
  app$query_one("#editor")$focus()
  status(app)
})

run(editor)
