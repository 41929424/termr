# Static rendering and exports

termr can render one stable frame from a widget tree, screen, app, or existing
`ScreenBuffer`. The static path runs normal layout and paint into the
framebuffer. It does not open a terminal, emit ANSI, start an app event loop,
or wait for workers and timers. A screen snapshot captures what has been
painted at that moment.

## Plain text

`render_text(x, width, height, trim = TRUE)` returns one character string with
newlines. `render_lines()` returns one string per row. Trailing spaces are
trimmed by default while internal spaces and row order are preserved; pass
`trim = FALSE` to retain the full cell width. Both use termr's grapheme and
display-width layout, including wide CJK characters and emoji.

## Markdown

`render_markdown()` wraps the plain screen in a fenced `text` block. It picks
a fence longer than any backtick run in the content, so screen text cannot
close the generated block.

## HTML

`render_html()` returns an embeddable `<pre class="termr-screen">` fragment
with compact style runs. It preserves available foreground/background
colours, bold, dim, italic, underline, strike, and reverse video. The
`color = TRUE` default means terminal `NO_COLOR` does not suppress explicitly
requested HTML styling. Browser fonts and CSS affect the appearance, so HTML
is not pixel-perfect terminal output. Hyperlinks are not yet represented in
the ScreenBuffer and therefore are not exported.

Text is HTML-escaped. Colors and attributes come from the painted buffer;
terminal control sequences are not emitted.

## SVG

`render_svg()` emits a self-contained vector image using grouped text runs and
background rectangles. `cell_width`, `line_height`, and `font_family` control
its geometry. SVG uses a generic monospace font by default and bundles no
fonts. `render_svg()` is experimental; browser/font metrics may differ from
terminal cell metrics.

## JSON screen snapshots

`screen_snapshot()` returns a base R list with `schema_version`, dimensions,
full lines, and row-major style runs. It needs no optional package. Schema
version 1 is intended for reproducible snapshots; consumers should check the
version before relying on its structure. `screen_snapshot_json()` serializes the
same structure through optional `jsonlite`.

## Widget inspection and redaction

`widget_snapshot()` and `widget_snapshot_json()` serialize only structural fields:
`schema_version` (root only), type, id, classes, region,
visible/enabled/focused flags, and children.
Arbitrary widget state and values are omitted, including password input values,
database credentials, and worker data. The actual screen snapshot can of
course show text that was painted on screen, including a widget's visible
masked or unmasked representation.

## knitr and Quarto

`knit_termr(x, format = "markdown")` returns knitr's as-is output. Select
`format = "html"` explicitly for styled HTML output. `knitr` is a Suggests
dependency only. For example:

```{r, results='asis'}
ui <- termr::panel(termr::label("Model finished"), title = "Training")
termr::knit_termr(ui)
```

The same helper works in R Markdown and Quarto. Neither Quarto nor Pandoc is a
termr dependency.

## PDF workflow

termr does not implement a PDF engine. Render to Markdown or HTML, then use
Quarto/Pandoc and its configured PDF toolchain to create a PDF.

## Files

`write_rendered(x, file, format)` supports `text`, `markdown`, `html`, `svg`,
and `json`. JSON output requires `jsonlite`.

## Limitations

These are static snapshots, not interactive HTML applications. Rendering does
not advance timers, run workers, or capture animation. termr does not export
PNG, PDF, recordings, or video. Optional JSON and knitr integrations require
`jsonlite` and `knitr`, respectively; neither is a runtime dependency.
