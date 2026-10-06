# Layout

Every widget gets a rectangular *region* from its parent. Containers place
their children according to their `layout` style: `"vertical"`,
`"horizontal"` or `"grid"`.

## Sizes

`width` and `height` accept:

* a number of cells: `style(width = 20)`
* `"auto"`: the natural size of the content
* `"1fr"`, `"2fr"`: shares of the space that is left
* `"50%"`: a percentage of the parent's content size

plus `min_width`, `max_width`, `min_height`, `max_height`.

Defaults: containers fill their parent (`vertical()`: `1fr` x `1fr`;
`horizontal()`: as tall as its tallest child); labels and buttons are
`auto`; inputs fill the width.

## Box model

```text
margin | border | padding | content
```

`margin` and `padding` take 1, 2 or 4 values like CSS
(`c(top/bottom, left/right)` or `c(top, right, bottom, left)`). Borders:
`"ascii"`, `"single"`, `"round"`, `"double"`, `"heavy"`, `"blank"`.
`align` / `valign` on a container position children that do not fill it.

## Grid

```r
grid_layout(
  label("a"), label("b"), label("c"),
  label("wide", style = style(column_span = 2)),
  columns = c("auto", "1fr", "2fr"),
  rows = c(3, "1fr"),
  gap = c(0, 1)          # rows, columns
)
```

Children are placed row by row; rows not listed are `"auto"`. Named
`grid_layout()` because `grid()` would mask `graphics::grid()`.

## Scrolling

`scroll_view()` lays its content out at its natural size and shows a
viewport into it, with scroll bars. Arrow keys, Page Up/Down, Home/End and
the mouse wheel scroll; when the focus moves to a widget inside, the view
scrolls to show it.

## Text wrapping

`style(wrap = "word")` (or `label(..., wrap = "word")`) wraps text at the
widget's width, by display width and grapheme; `"auto"` heights grow with
the wrapped text and re-wrap when the terminal is resized.

## Unicode

Text is split into grapheme clusters: `"é"`, CJK characters, emoji
ZWJ sequences (`"\U0001F468‍\U0001F469‍\U0001F467"`), flags and
skin tones each take one or two cells as in a terminal. Use
`options(termr.ascii = TRUE)` for terminals without Unicode fonts.

## Split panes

`split_pane(left, right, ratio = 0.4)` (or `direction = "vertical"`) divides
the space between two panes with a one-cell divider. The user drags the divider
with the mouse, or focuses it (Tab) and uses the arrow keys (Shift for ten
cells, Home/End for the extremes). `min_size` keeps panes from collapsing;
`pane$ratio` and `pane$sizes` read and set the split; `splitpane.resized` is
sent when a move ends.

## What triggers a new layout

Layout is recomputed when sizes can change (text, size styles, mounting,
resizing, theme) and skipped for paint-only changes (cursor moves, colours,
scrolling in tables and lists). See [performance.md](performance.md).
