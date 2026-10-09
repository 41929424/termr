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

On CRAN (`NOT_CRAN` unset), `skip_on_cran()` skips only tests whose
outcome depends on subprocess timing (worker timeouts, cancellation,
streaming and long-running child programs) and two long example-app
scenarios (`data-explorer`, and `task-runner`, which waits for background
workers), to keep check time short. Worker spawn, result, error and
`R_TESTS` isolation probes still run there.

CRAN also runs representative bounded stress/property coverage. The test-only
`stress_workload()` helper selects shorter workloads when `NOT_CRAN` is unset
or false. CI with `NOT_CRAN=true` keeps the original seeds, iterations, random
operation sequences and assertions:

| Property/stress scenario | CRAN workload | Full CI workload |
| --- | --- | --- |
| Extreme resize | Seed 1, all 10 listed extreme sizes | Seeds 1–4, 30 random sizes each |
| Focus under tree changes | Seed 1, 40 operations | Seeds 1–5, 80 operations each |
| Rich UI incremental/full repaint | Seed 11, 40 operations | Seeds 11–13, 100 operations each |
| Lazy scroll layout/full repaint | Seed 21, 30 operations | Seeds 21–23, 80 operations each |
| Parent/child tree consistency | Seed 11, 40 edits | Seed 11, 200 edits |
| Layout skipping/full layout | Seed 1, 30 operations in each mode | Seeds 1–3, 80 operations in each mode |
| Incremental/full repaint | Seed 2026, 50 operations | Seed 2026, 120 operations |
| Table consistency | Seed 11, 40 operations | Seed 11, 150 operations |
| Grid invariants | Seed 7, 12 grids | Seed 7, 40 grids |

These loops still check their original invariants, including comparisons with
full rendering, focus validity, geometry, scroll offsets and parent/child
pointers. CRAN explicitly visits every listed extreme terminal size, including
1×1, 300×3 and 3×80. No functional area is newly skipped, and the deterministic
functional tests and worker probes keep their original workloads and timeouts.

For a source-tree CRAN-mode run, use an explicit false value because
`test_local()` assumes `NOT_CRAN=true` when the variable is unset:

```r
withr::with_envvar(c(NOT_CRAN = "false"), testthat::test_local())
withr::with_envvar(c(NOT_CRAN = "true"), testthat::test_local())
```

No test needs an interactive terminal; the POSIX driver test is skipped on
Windows.

Terminal integration and RC1 validation:

* Windows CI checks the PowerShell script, numeric input protocol and shared
  input/lifecycle behavior. `tools/e2e-windows.py` is a local ConPTY harness;
  its presence does not establish a successful interactive CI run.
* Unix: `tools/pty/pty-check-ci.py` drives the PTY harness for raw mode, keys,
  mouse, paste, resize, Ctrl+C and terminal restoration on Ubuntu/macOS CI.
* Benchmarks: `Rscript tools/bench/bench.R`.

Full CI [37758703750](https://github.com/41929424/termr/actions/runs/37758703750)
passed at `1846527d3968cdd12fa9900bbe80a9f2a89a1a3e`: Ubuntu release,
oldrel-1, R 4.1 and devel; macOS and Windows release; SQL integration;
both PTY jobs; and Windows input regressions. All six R CMD check jobs
reported 0 errors, 0 warnings and 0 notes, and their extended regression
suites passed too.

The worker regression asserts that an unused 50 MiB object does not travel
with a scalar closure, an explicitly referenced large object does, the result
is correct, and bootstrap reaches `result_written`. It also covers nested
and recursive helpers and pre-spawn rejection of unsupported bindings.

Windows real-console and Linux `ssh -t` smoke were manually reported PASS.
Automated PTY tests do not replace manual terminal checks; see
[Platform confidence](platform-testing.md) for the remaining NOT RUN hosts.
