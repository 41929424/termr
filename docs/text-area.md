# TextArea (experimental)

`text_area()` is a multi-line editor widget.

```r
ed <- text_area("Hello\nworld", id = "editor", line_numbers = TRUE, wrap = TRUE)
ed$insert("!"); ed$value; ed$find("wor"); ed$selection
```

* **Model.** Text is a vector of lines; positions are `(row, grapheme column)`,
  never byte offsets, so emoji, accents and CJK text are not split. Tabs in
  loaded text become spaces. Only visible lines are formatted when drawing;
  wrapping is cached per line.
* **Keys.** Arrows, Home/End, Ctrl+Home/End, Ctrl+Left/Right (words),
  PageUp/PageDown, Shift+ to select, Ctrl+A, Enter (optional `auto_indent`),
  Backspace/Delete, Ctrl+Backspace/Delete, Ctrl+U/K, Ctrl+C/X/V, Ctrl+Z /
  Ctrl+Y, Ctrl+F find (Enter/Down next, Up previous, Ctrl+R regex, Alt+C case,
  Esc closes), F3 / Shift+F3. Tab moves focus unless `tab_behavior = "indent"`.
* **Mouse.** Click places the cursor, drag selects, the wheel scrolls.
* **Paste.** A bracketed paste is inserted at once as one undo step.
* **Undo.** Typing is grouped per word, paste / delete-selection / line
  operations are single steps; the history is bounded (`max_history` steps,
  about two million characters).
* **Find.** `$find(query, case_sensitive, regex)`, `$find_next()`,
  `$find_previous()`, `$match_count`. Invalid regular expressions raise a
  readable error (the find bar shows it instead).
* **Events.** `textarea.changed` (read the text with `$value`),
  `textarea.selection_changed`, `textarea.valid` / `textarea.invalid`.
  `validate = function(value)` returns `NULL` or a message, like `input()`.
* **Read-only** mode keeps navigation, selection, copy and search.
* **Performance.** A 1 MB document opens in ~100 ms; typing and scrolling stay
  in the tens of milliseconds (`tools/bench/bench.R textarea`).
* **Not included**: syntax highlighting (`language` is reserved), multiple
  cursors, rectangular selection.
