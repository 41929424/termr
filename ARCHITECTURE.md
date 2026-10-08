# termr architecture

How termr is built: the layers, the data flowing between them, the main
decisions and where new features plug in. Written for contributors; it
describes the implementation in the 1.0 release-candidate source tree.

## Layers

```text
 R application         app(), widget(), on(), bind(), style(), stylesheet()
      |
 Widget tree           Widget + containers + widgets (widget-*.R)
      |
 Reactive state        set_state() -> invalidate() -> request_repaint(widget)
      |
 Events                Event classes, EventQueue, dispatch + bubbling     events.R, mouse.R, app.R
 Bindings / focus      bind(), actions, FocusManager, command palette     bindings.R, focus.R, command-palette.R
 Timers / workers /    TimerManager, Worker (processx), Animator          timers.R, workers.R, animation.R
 animations
 Screens               ScreenStack, ModalScreen, toast layer              screens.R, notifications.R
      |
 Style                 cascade: defaults < stylesheet < inline; themes    style.R, stylesheet.R, theme.R, selectors.R
      |
 Layout engine         layout_tree(): regions                             layout.R, layout-grid.R
      |
 Compositor            paint_tree() / paint_layers(), dirty regions        compositor.R, borders.R, span.R
      |
 Virtual screen        ScreenBuffer: chars / fg / bg / attrs matrices      screen-buffer.R, unicode.R
      |
 Screen diff           diff_screen(old, new) -> patch                      diff.R
      |
 ANSI renderer         patch_to_ansi(); Renderer keeps the front buffer    renderer.R, ansi.R, color.R
      |
 Terminal driver       Posix / Windows / Headless                          terminal*.R, keys.R
```

Rules: only drivers do terminal IO; layout never paints; widgets never
produce ANSI; the App coordinates small components (focus, timers, queue,
screens, workers, animator, renderer).

## Event loop

`App$run()` repeats `tick()` until `exit()`:

1. Wait for input until the next timer (at most 0.5 s; 0.05 s while
   workers run). Drivers return key, mouse and resize events.
2. Route input: keys to the focused widget (or the top screen); mouse
   events to the hit-tested widget (below).
3. Poll background workers (non-blocking).
4. Fire due timers (also drives animations).
5. Dispatch queued events (bounded), running `call_later()` callbacks.
6. Repaint once if anything was invalidated (below).

Dispatch: target, then ancestors (if the event bubbles), then the app.
Per widget: `on_<type>` method, `widget$on()` handlers, key bindings. At
the app: `on()` handlers, app bindings. Handler selectors match the
message sender, or the target of key/mouse events.

## Rendering

### Framebuffer and Unicode

`ScreenBuffer` keeps four `height x width` matrices; the diff is a
vectorised comparison. Text is split into **extended grapheme clusters**
(UAX #29) with PCRE2's `\X` in base R (`gsub` inserts separators, then
`strsplit`; ASCII takes a fast path). Cluster widths follow terminals:
wide East Asian characters and emoji are 2 cells, ZWJ sequences / flags /
skin tones / VS16 sequences are 2, VS15 forces 1, combining marks add
nothing. A wide cluster occupies its cell plus a continuation cell (`""`);
writes never leave half a cluster. All segmentation lives in `unicode.R`.

### Diff and ANSI

`diff_screen()` produces horizontal runs (wide clusters kept whole, nearby
runs merged); `patch_to_ansi()` emits cursor moves and SGR changes only.
Frames use synchronized output (mode 2026) only when terminal capabilities
enable it; generic xterm and multiplexer paths leave it off. `VirtualTerminal` interprets
the output back into a buffer; tests check `apply(old, ansi(diff)) == new`.

### Frames, layers and dirty regions

A frame is the visible screens (the top non-modal screen and any modal
screens above it; modal screens dim what is below) plus overlay layers
(toasts, debug overlay). `refresh_screen()`:

1. decides whether a layout pass is needed: it is skipped when only
   paint-only invalidations (`invalidate_paint()`) are pending and nothing
   else changed (same layers, size, layout epoch, no focus reveal);
2. otherwise lays out every layer (layout caches make unchanged parts cheap;
   scroll views lay out only children in view) and takes a **layout
   snapshot**: the widgets in tree order and their regions;
3. compares the snapshot with the previous one. Same widgets: the changed
   widgets' old and new regions become extra dirty rectangles. Different
   widgets (mount/remove), another size, other layers, a requested full
   repaint, or more than 40 changed regions: **full repaint**;
4. incremental path (`dirty.R`): rectangles of invalidated widgets and moved
   widgets are clipped to the screen, widened by one column (wide graphemes
   are never cut), merged when they overlap or touch with little waste
   (at most 12 are kept, else their bounding box), then painted into a copy
   of the previous frame - every layer inside each rectangle, so overlays,
   transparency and modal dimming composite correctly;
5. diffs against the front buffer and writes the ANSI patch.

`Widget$invalidate()` reports a layout-affecting change; `invalidate_paint()`
a change that cannot move or resize anything (cursor, scroll offset of a
table, colour). Changes without a known widget (theme, stylesheets, screens,
resize) force a full repaint. Instrumentation (`app$last_paint`: screen
cells, repainted cells, rectangles, full or not, ANSI bytes) feeds the
benchmark suite (`tools/bench/`). Randomised tests check that incremental
frames always equal full repaints.

### Layout caches

Computed styles are cached per **style epoch**, bumped by changes that can
restyle widgets (style/class changes, mount/remove, focus, hover, state
classes such as `disabled`, themes, stylesheets). A separate **layout epoch**
is bumped only when the change can affect sizes: colour-only style changes
and focus/hover changes whose widgets have the same size properties in both
states bump only the style epoch. Natural sizes are cached
per widget and cleared by `invalidate()` for the widget and its ancestors
only. Result (tools/bench/bench.R, Windows, 10 ms timer resolution): a
label update in a 341-widget tree costs ~30 ms per tick instead of ~200 ms.

## Layout

`layout_tree()` assigns border-box regions. `Widget$arrange_children()`
places children (default: the algorithm named by the `layout` style from
`layout_algorithms`: vertical, horizontal, grid); `child_clip()` says where
children are clipped. Algorithms provide `arrange()` and `measure()`.

* **Vertical/horizontal**: fixed, auto, percent and fr sizes on the main
  axis (largest-remainder distribution), fill/auto/fixed on the cross
  axis, margins, align/valign.
* **Grid** (`layout-grid.R`): row-major auto placement with spans, track
  sizes fixed/auto/percent/fr, gaps; spanning children do not size auto
  tracks.
* **ScrollView**: measures the content, reserves scroll bars, lays the
  children out in a virtual rectangle shifted by the offsets and clips them
  to the viewport. Offsets are clamped silently during layout.
* Wrapped text: `render_lines(width)` wraps per style; natural heights use
  the width the parent gives.

## Input, focus and mouse

* Keys: `KeyParser` (Unix escape sequences incl. SGR mouse) and Windows
  console records share canonical key names; graphemes can be keys.
* Focus: `FocusManager` walks the top screen (focusable, displayed,
  enabled widgets). Focus moves on when the focused widget is removed,
  hidden or disabled; each screen remembers its focus.
* Mouse **hit testing** (`hit_test()`): the deepest visible widget whose
  region contains the point, intersecting every ancestor's `child_clip()`
  (so scrolled-out content cannot be hit); later children win. Overlays
  (toasts) are tested before the top screen; lower screens never receive
  input. Disabled targets pass events to their parent. The app keeps hover
  state (`:hover`), focuses on click and synthesises `click` after a down
  and up on the same widget.

## Widgets with their own viewports

`DataTable`, `TreeView` and `ListBase` (option lists, logs) paint only the
visible rows themselves:

* **DataTable** keeps the data, a *view* (row indices, for sort/filter),
  column specs (widths from a 1,000-row sample, formatters chosen by
  type), cursor and offsets. `paint()` formats only the visible slice of
  each visible column and writes each row with one `put_text()` call,
  overlaying the cursor cell and `cell_style` results.
* **TreeView** flattens expanded nodes into cached lines (rebuilt when any
  node reports a change); lazy loaders run on first expand.

For a `table_source()`, DataTable keeps bounded 100-row chunks and asks for
all visible columns in one source call per page. Sorting, filtering and
search delegate to explicit callbacks; unsupported operations never silently
scan or materialize the whole source. Cache entries invalidate on source
refresh or view changes. Source fetches are synchronous.

## Reactive graph

`signals.R` implements signals, lazy cached computed values and watchers.
Reads track dependencies; writes invalidate subscribers. Watchers settle in
creation order, and `batch()` defers them until the outer batch exits.
`peek()` and `untracked()` bypass tracking; `dispose()` unlinks a computed or
watcher. Reactive widget value callbacks bind to this graph and detach when
the widget is removed. Handler/timer execution uses batching, while widget
field invalidation still determines layout versus paint work.

## Data and database layers

`table_source()` defines the public paging/capability/lifecycle protocol.
`db_table_source()` supplies SQLite table/view paging with quoted identifiers,
bound filters and `LIMIT`/`OFFSET`. `db_query_source()` supplies read-only
query paging with a SQLite implementation or driver-specific callbacks.
Counts are known before paging, and may themselves require a database scan.

The optional DBI wrapper records connection ownership: externally supplied
connections stay open; owned connections disconnect on their owner's cleanup.
Sources expose explicit close methods, and query sources never own the supplied
connection. `db_metadata()` supplies portable metadata with SQLite-specific
view support. `db_explorer()` composes that metadata, lazy previews, SQL
editing, result tables and in-memory history. SQL execution remains synchronous;
default editor results are materialized, with lazy result mode as an opt-in.

## Screens and overlays

`ScreenStack` holds the screens; `push_screen()` saves the focus of the
covered screen, mounts the new one and sends `screen.hide/show`;
`pop_screen()` unmounts (cancelling timers), restores focus and calls the
dismiss callbacks. `ModalScreen` (dialogs, the drop-down list, the command
palette) is drawn over the dimmed screens below and closes with Escape.
Toasts live in a separate root that never takes focus.

## Style

`computed_style()` = `resolve_style(type defaults (+states) < matching
stylesheet rules < own style (+states))`. Stylesheets are parsed by a
hand-written parser into rules with selectors (shared with `query()`),
specificity `c(ids, classes + pseudo, types)` and source order. Pseudo
classes are matched against the widget's `pseudo_states()`. Colours may
be theme tokens (`$primary`), resolved against the app's theme in
`resolve_style()`; built-in widgets only use tokens, and the `default`
theme maps them to ANSI colours.

## Workers and animation

Workers prepare a minimized function closure before serializing `fn` and
`args` to an RDS file. `codetools::findGlobals()` identifies referenced bindings;
local helper functions are copied recursively into minimal environments, with
unrelated enclosing state omitted and the caller's function unchanged.
Scalar captures, nested helpers and recursive helpers work. Referenced values
are captured by value; explicit large objects and `args` still serialize.
Active bindings and dynamic lexical lookup fail before spawn. Namespace/base
functions retain their normal namespace semantics.

Workers run
`inst/helpers/termr-worker.R` with `Rscript --vanilla` via processx;
progress records (`termr_progress()`) are appended to a separate file and the
result written to an RDS file, so stdout and stderr stay free for the job's
output (streamed as events). Minimal capture environments are reconnected in
the child after requested packages and `termr_progress()` are available. The
child receives the parent's library paths with `R_TESTS` cleared. Bootstrap
trace files record progress through `result_written`; diagnostics identify
unexpected exits and timeouts. Pipe draining is bounded per poll, and exit or
cancellation cleans up process descendants and temporary files. Widget removal
and app shutdown cancel owned workers.

`run_process()` runs programs without a shell. The app polls workers every
tick and turns them into `worker.*` events and callbacks. Animations share one 30 fps timer and use
the app clock, so headless tests are deterministic.

## Terminal drivers

* **Windows**: a PowerShell helper reads console input records (keys and
  mouse) with `ReadConsoleInputW` via P/Invoke, sets the input mode (no
  line editing, Ctrl+C as a key, mouse on, quick-edit off), enables VT
  output in legacy consoles and restores everything when R creates its
  stop file or disappears.
  Unsigned DWORD mouse button-state/flag fields are parsed as exact R doubles;
  word extraction is arithmetic, including signed wheel deltas, and malformed
  helper records are rejected before event dispatch.
* All drivers expose `capabilities` (detected once: colours, mouse, bracketed
  paste, OSC 52, ...); the bracketed-paste markers are parsed into a
  `PasteEvent` by `KeyParser` (any fragmentation of the input).
* **Unix**: saves `stty` state, selects `/dev/tty` or a TTY stdin fallback
  through `/dev/stdin`, then enables `stty raw -echo`. A `cat` reader is polled
  with processx; `KeyParser` handles byte fragmentation and `stty size` polls
  resizes. No controlling terminal and no TTY stdin produce a clear error.
  Checked by unit tests with a
  fake reader and by `tools/pty/pty-check.py` in CI.
* **Headless**: input queue, simulated clock, `VirtualTerminal` output.

The terminal is restored on exit, on error (caught, restored, re-raised)
and on interrupts; `stop()` is idempotent and each step protected. Mouse
reporting is enabled only while the app runs.

## Static output and headless testing

The headless driver uses an input queue, simulated clock and ANSI interpreter
with the normal App event loop. `test_app()` exposes a Pilot for input, focus,
resize and worker tests without a real terminal.

The static path (`export.R`, `testing.R`) lays out and paints one frame
into a ScreenBuffer without terminal IO or an app event loop. An unattached
root widget is temporarily wrapped in a Screen, then detached on exit;
layout assigns regions for the requested size. Text, Markdown, styled HTML and
SVG use this buffer; optional jsonlite serializes screen/structural snapshots,
and optional knitr supplies as-is Markdown/HTML for reports. Static rendering
does not advance timers or start workers. See [static export](docs/export.md)
and [testing](docs/testing.md).

## Extension points

| Feature | Where |
|---------|-------|
| New layout | Public `register_layout()` / `unregister_layout()`, or documented widget arrangement methods |
| New widget | Public `widget()`, or subclass exported `Widget`; `ListBase` is internal |
| Lazy table backend | Public `table_source()` callbacks; DBI query paging via `db_query_source(adapter = )` |
| New style property | `style()` + `style_property_parsers` + `css_kinds` |
| Grapheme rules | `unicode.R` |
| Dirty-region policy | `dirty_rects()`, `snapshot_changes()` in `dirty.R` |
| System clipboard (OSC 52) | `App$clipboard_write()` |
| Terminal features | `terminal_capabilities()` in `capabilities.R` |
| Reactive graph | `signals.R` (`signal`, `computed`, `watch`) |
| Accessibility | theme flags `mono` / `strong_focus` in `resolve_style()` and `computed_style()`; `motion_reduced()` |

This contributor table includes internal implementation hooks as well as
public protocols. External packages should follow [Extensions](docs/extensions.md)
and the [stability classification](docs/stability.md); direct access to
registries, parsers or dirty-region helpers is internal.
