# Performance

termr is written in R, so it avoids doing work rather than doing it fast.
This page explains what it avoids and how to measure. It makes no promise
about frame rates: R, the terminal and the OS decide those.

## The pipeline

```text
widget changes -> (layout) -> paint into a virtual screen -> diff -> ANSI
```

* **Virtual screen.** Widgets paint into a `ScreenBuffer` (four matrices:
  character, foreground, background, attributes). Nothing is written to the
  terminal while painting.
* **Diff.** The new buffer is compared with what the terminal already
  shows; only changed runs of cells become cursor moves and colour changes.
  A one-letter change sends a few dozen bytes.
* **Synchronized output.** Terminals that support it hold the frame until it
  is complete (no tearing).

## Dirty rectangles

After a small change termr does not repaint the screen. It collects the
region of each changed widget (plus the old and new region of widgets that
moved or were resized), widens each by one column so a wide character is
never cut, merges overlapping or touching rectangles, copies the previous
frame and repaints only inside those rectangles - all layers inside the
rectangle, so overlays, transparency and dimming under modals composite
correctly. The result is compared with the front buffer, so the bytes sent
depend on what really changed.

A **full repaint** is used when it is the safe choice: first frame, terminal
resize, a screen pushed or popped, theme or stylesheet changes, widgets
mounted or removed, or more than 40 widgets changing region at once.
A randomised test checks that incremental frames are identical to full
renders.

Typical numbers (see the benchmark below; a 120x40 screen has 4,800 cells):

| Change                         | Repainted cells | ANSI bytes |
|--------------------------------|-----------------|------------|
| one label's text               | 8-20            | ~30        |
| a label's colour               | 5               | ~40        |
| moving focus in a form         | 480             | ~1,100     |
| table cursor / scroll          | the table       | 300-1,100  |
| opening a modal, resizing      | full screen     | -          |

## Layout versus paint

Every change is either paint-only or layout-affecting:

| Paint only (no layout pass)                        | Layout (sizes may change)            |
|----------------------------------------------------|--------------------------------------|
| colours and attributes (`style` with the same size properties) | text, labels, values that change size |
| cursor and selection in `input()`, `text_area()`   | `width`, `height`, margin, padding, border, wrap |
| `data_table()` cursor, scrolling, search highlight | mounting / removing / hiding widgets |
| lists and trees: cursor and scrolling              | resizing the terminal                |
| progress bar value, spinner frames                 | scrolling a `scroll_view()` (children move) |
| focus / hover when the widget's style has the same size properties in both states | stylesheet / theme changes |

Widgets declare paint-only state names in `paint_states` and call
`invalidate_paint()`; everything else calls `invalidate()`. When a paint-only
change is the only thing pending, the layout pass is skipped entirely
(`app$frame_stats[["layouts"]]` counts layout passes).

## Caches

* **Computed styles** are cached per widget and dropped when the global
  *style epoch* changes (class/style/state changes that can restyle).
  Colour-only changes bump the epoch without forcing a layout.
* **Natural sizes** (what a widget needs) are cached per widget and cleared
  by `invalidate()` for the widget and its ancestors only.
* **Text wrapping** in `text_area()` is cached per line and width.
* **Scroll views** index ordinary vertical child rectangles. With 64 or more
  children, binary search selects the viewport plus two rows of overscan;
  layout, painting, mouse hit testing and snapshots visit that selection.
  Initial measurement and changes to sizes/content/structure rebuild the
  index. Scrolling translates cached positions without remeasuring children.
  Width changes rebuild it, including variable-height wrapped text.
  Custom and nonvertical layouts keep their existing algorithm. Focus reveal
  can request a complete layout. `options(termr.indexed_layout = FALSE)` is
  an internal debugging oracle, not an application tuning API.

### Frame profiling

Set `options(termr.profile = TRUE)` to collect `app$profile_last_frame`:
layout, paint, diff and ANSI milliseconds; widgets measured, laid out and
painted; style resolutions; selector matches; dirty rectangles/cells; ANSI
bytes. Profiling is off by default. Counters describe work, while timings
depend on the host and can be zero at the Windows timer's resolution.

## Virtualisation

* `data_table()`: formats and paints only the visible rows and columns.
  Column widths come from a sample (first and last 500 rows plus the visible
  rows), search scans in chunks, sorting uses `order()` once per sort.
  Construction of a 1,000,000-row table takes tens of milliseconds.
  An experimental `table_source()` can fetch only visible source chunks and
  jump directly to a row; sorting, filtering and search require backend
  callbacks. See [Lazy table sources](data-sources.md).
* `tree_view()` flattens only expanded nodes; lazy nodes load on expand.
* `log_view()` keeps a bounded number of lines.
* `text_area()` stores lines (not one big string), edits touch the affected
  lines, and only visible lines are formatted.

## Measuring

```sh
Rscript tools/bench/bench.R                  # all scenarios
Rscript tools/bench/bench.R datatable        # a subset
Rscript tools/bench/datatable-source.R       # source fetch and cache counters
Rscript tools/bench/compare.R 87b846b HEAD   # compare git revisions
```

The suite times batches of ticks of the real event loop on a headless driver
and reports the median and 90th percentile per tick (milliseconds) together
with what was repainted (`repaint_cells`, `rects`, `ansi_bytes`, how many
repaints were full). Wall time is noisy (the Windows clock ticks at 10-15
ms); the repaint columns are exact. `app$last_paint` exposes the same
instrumentation.

One recorded run on Windows 11 (R 4.5.2), 120x40, median ms per tick:

| Scenario                         | ms  | repainted cells | ANSI bytes | full |
|----------------------------------|-----|-----------------|------------|------|
| label update, 341-widget tree    | 29  | 8               | 31         | 0/70 |
| colour change, 341-widget tree   | 4   | 5               | 37         | 0/70 |
| label update, ~1,000 widgets     | 124 | 8               | 31         | 0/35 |
| table cursor move, 1M rows       | 22  | 4,800 (the table)| 286       | 0/70 |
| table page scroll, 1M rows       | 27  | 4,800           | 1,108      | 0/70 |
| table horizontal scroll (30x40)  | 17  | 1,200           | 59         | 0/51 |
| modal open/close                 | 36  | 4,800           | -          | 70/70 |
| 2,000-label scroll view scroll   | 240 | 4,800           | 340        | 35/35 |

Against the 0.2.0 baseline (same machine): colour change in the large tree
139 -> 5 ms, table cursor/scroll about 2x faster, label updates unchanged.
The full run is recorded in `tools/bench/last-run.txt`.

The 0.4 development ScrollView profile is recorded in
`tools/bench/baselines/windows-0.3-scroll.csv` and
`tools/bench/baselines/windows-0.4-scroll.csv`. On the same host and 100x30
viewport, scrolling five 10,000-child rows fell from about 1,472 ms to 20 ms.
The new `tools/bench/tree-profile.R` records wide-tree mount and traversal
costs. These local timings are advisory; structural profile counters are more
stable than wall-clock values.

`tools/bench/textarea-storage.R` measures construction, insertion, undo/redo,
find misses and viewport rendering at 100 KB, 1 MB, 5 MB and 20 MB. The 20 MB
case constructed in 180 ms, searched in 820 ms and rendered in under 10 ms on
this host, so the line-vector model remains adequate for these operations.
The tree profile also includes batched mount and `walk()` timings at 100,
1,000, 5,000 and 10,000 children.

## Tips

* Update widgets from one timer callback rather than many timers; several
  changes in one tick cost one repaint.
* Keep `height = "auto"` containers small; very deep trees of `auto` sizes
  are the most expensive layouts.
* For very large repeated data use `data_table()` or `log_view()`. Vertical
  `scroll_view()` now indexes large ordinary child lists and lays out the
  visible rows plus two rows of overscan. First layout still measures all
  children, and custom container layout methods use the regular path.
* Use signals / `batch()` to group state changes.
