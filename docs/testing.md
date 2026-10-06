# Testing

termr apps are tested without a terminal. `test_app()` starts the app
with a headless driver: input is simulated, time is simulated, and the
output goes through the real renderer into a virtual terminal.

```r
test_that("the form greets", {
  pilot <- test_app(my_app(), width = 40, height = 12)

  pilot$type("Ada")                # characters
  pilot$press("tab", "enter")      # keys
  pilot$click("#save")             # mouse (selector, widget or x, y)
  pilot$scroll("#list", "down", times = 3)
  pilot$hover("#help")
  pilot$advance(1.5)               # timers, animations, spinners
  pilot$resize(80, 24)
  pilot$wait_for_workers()         # real background workers

  expect_identical(pilot$query_one("#greeting")$text, "Hello, Ada")
  expect_snapshot(pilot$snapshot())
  pilot$stop()
})
```

More pilot methods: `paste(text)`, `drag(from, to)`, `focus(target)`,
`find(selector)`, `wait_for(function(app) ..., timeout)` (simulated time, real
time while workers run), `run_worker(fn)`, `system_clipboard()` (last OSC 52
text). `test_app(app, width, height, color_mode = "none")` renders without
colour (also `"16"`, `"256"`), e.g. to test accessibility.

* `pilot$screen_text()`: the screen as text;
  `pilot$driver$terminal$screen$get_cell(x, y)` for colours and
  attributes.
* `render_widget(widget, width, height)`: lay out and paint once.
* `app$run_worker(..., inline = TRUE)`: deterministic workers.
* `options(termr.incremental = FALSE)`: force full repaints;
  `options(termr.skip_layout = FALSE)`: always re-lay out. `app$last_paint`
  and `app$frame_stats` show what a repaint did.

## Inside termr

The test suite (`tests/testthat`) includes randomised property tests:
framebuffer diffs round-trip through the ANSI interpreter, incremental
rectangular repaints equal full repaints (rich UIs with wide graphemes,
overlays, scroll views), random resize and focus sequences keep geometry and
focus valid, the key parser gives the same events however input is split, scroll offsets stay in range, grid
allocations sum correctly, and parent/child pointers stay consistent.

Integration in real terminals:

* Windows: `tools/e2e-windows.py` drives an app in a ConPTY.
* Unix: `tools/pty/pty-check.py` checks raw mode, keys, resize, Ctrl+C
  and terminal restoration in a pseudo terminal (also run in CI).
* Benchmarks: `Rscript tools/bench/bench.R`.
