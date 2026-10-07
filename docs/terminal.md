# Terminal features and accessibility

## Capabilities

The driver detects what the terminal can do once, at start-up:

```r
terminal_capabilities()      # what the current environment would give
test_app(my_app())$driver$capabilities    # the headless terminal (everything on)
```

Fields: `colors` (`"truecolor"`, `"256"`, `"16"`, `"none"`), `unicode`, `mouse`,
`sgr_mouse`, `bracketed_paste`, `alternate_screen`, `synchronized_output`,
`cursor_shape`, `hyperlinks` (OSC 8) and `osc52`. Detection uses `TERM`,
`COLORTERM`, `NO_COLOR`, `CI` and a few terminal-specific variables, and is
deliberately conservative: unknown features are off and termr falls back.
Overrides: `options(termr.color_mode =)`, `options(termr.ascii = TRUE)`,
`options(termr.osc52 =)`, `options(termr.hyperlinks =)`, or the environment
variables `TERMR_OSC52=1` / `TERMR_HYPERLINKS=1`. An SSH or multiplexer
session does not inherit OSC capability from a local terminal marker by
default; explicitly enable OSC 52 only when the whole path supports it.

Common remote `TERM` names are handled conservatively. `vt100` selects
monochrome and does not enable SGR mouse, bracketed paste, or alternate screen;
`linux` does not enable SGR mouse or bracketed paste. Generic `xterm` and
`screen`/`tmux` names do not establish synchronized-output support. See
[Running termr over SSH](ssh.md) for multiplexer caveats and the manual smoke
checklist.

## Bracketed paste

Terminals that support it wrap pasted text in markers. termr turns it into one
`PasteEvent` (`event$text`; line breaks are `"\n"`, other control characters are
removed, so a paste can never carry escape sequences). `input()` inserts it
atomically with line breaks turned into spaces; `text_area()` keeps the line
breaks and makes the paste one undo step. Widgets handle it with an
`on_paste(event)` method. Test with `pilot$paste("text")`. Windows consoles do
not provide bracketed paste: pasted text arrives as individual keys.

## Clipboard

`app$clipboard_write(text)` sets the app clipboard (`app$clipboard`) and, on
terminals with OSC 52, emits a write request to the system clipboard. Its
`TRUE` result means termr sent the sequence; a terminal or multiplexer may
ignore it, and termr cannot confirm that the OS clipboard changed. It is
**write-only**: termr never reads the system clipboard. The payload is base64
encoded and limited to 100 kB, so user text can never inject control
sequences. `input()` and
`text_area()` copy and cut through it. In tests, `pilot$system_clipboard()`
returns the last OSC 52 text.

## Colour degradation and `NO_COLOR`

Colours are mapped to the terminal's palette (truecolor -> 256 -> 16). With
`NO_COLOR` set (or `TERM=dumb`) no colour is sent at all, and termr keeps
every state distinguishable without it:

* selections, cursors and "primary" highlights become **reverse video**;
* errors are **bold and underlined**, warnings bold, secondary text **dim**;
* the focused widget gets a **heavier border** (and bold/reverse where it has
  none); the cursor of a table or list is reverse video.

Test it with `test_app(app, color_mode = "none")` (also `"16"`, `"256"`).

## Themes and high contrast

`app(ui, theme = "high-contrast")`: white on black, saturated yellow selection
with black text, cyan accents, and a heavier border around the focused widget.
Contrast ratios of the main colour pairs are at least 7:1 (checked by a
test). Other built-in themes: `"default"` (the terminal's palette), `"dark"`,
`"light"`. Customise with `termr_theme("dark", accent = "orange")`.

## Reduced motion

Decorative animation can be turned off with
`app(ui, reduce_motion = TRUE)`, `options(termr.reduce_motion = TRUE)` or the
environment variable `TERMR_REDUCE_MOTION=1` (`motion_reduced()` reports the
effective setting). Then `animate()` jumps to the final value at once (its
`on_complete` still runs), spinners show a still symbol (their label still
says what is going on) and the indeterminate progress shimmer stops. Timers,
events and real progress updates are not affected.

## Hyperlinks

Link metadata is kept on `span(link = )` and in `markdown_view()` links
(`$links`). Link targets are validated by `safe_url()` (http, https, file,
mailto only). Emitting OSC 8 links to the terminal is not implemented yet.
