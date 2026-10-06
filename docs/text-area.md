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
  Esc closes), F3 / Shift+F3. Alt+R switches Ctrl+F to replacement input,
  Tab switches between find and replace text, Enter replaces the selected
  match and Ctrl+Enter replaces every match. Ctrl+G opens the line prompt.
  Tab moves focus unless `tab_behavior = "indent"`.
* **Mouse.** Click places the cursor, drag selects, the wheel scrolls.
* **Paste.** A bracketed paste is inserted at once as one undo step.
* **Undo.** Typing is grouped per word, paste / delete-selection / line
  operations are single steps; the history is bounded (`max_history` steps,
  about two million characters). An individual edit larger than the memory
  budget clears history and is applied without an undo record.
* **Find.** `$find(query, case_sensitive, regex)`, `$find_next()`,
  `$find_previous()`, `$match_count`. Invalid regular expressions raise a
  readable error (the find bar shows it instead).
* **Replace.** `$replace(replacement, query = NULL)` replaces the selected
  match or finds and replaces the next match. `$replace_all(query,
  replacement)` makes one edit and records it when it fits the undo budget.
  Both support case and regex options.
* **Position.** `$cursor_position()` reports 1-based line, grapheme column,
  and display column in terminal cells.
* **Highlighting (experimental).** Pass `highlighter = r_highlighter()` for
  lightweight R tokens, or a function `(lines, state = NULL)` returning a
  list of per-line data frames with 1-based inclusive `start`, `end`, and one
  of `keyword`, `string`, `comment`, `number`, `constant`, `operator`, or
  `function`. Only visible lines are highlighted; the tokenizer tolerates
  incomplete code.
* **Events.** `textarea.changed` (read the text with `$value`),
  `textarea.selection_changed`, `textarea.valid` / `textarea.invalid`.
  `validate = function(value)` returns `NULL` or a message, like `input()`.
* **Read-only** mode keeps navigation, selection, copy and search.
* **Performance.** A 1 MB document opens in ~100 ms; typing and scrolling stay
  in the tens of milliseconds (`tools/bench/bench.R textarea`).
* **Not included**: parsing or semantic analysis, multiple cursors,
  rectangular selection.
