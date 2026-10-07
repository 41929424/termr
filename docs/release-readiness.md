# 1.0 release readiness audit

Audit date: 2026-10-07. This is an audit of the current 0.9 development tree;
no feature work was added. Local checks ran on Windows 11 x64 with R 4.5.2,
testthat 3.3.2, and Python 3.14.4. The source package version is
`0.9.0.9000`.

## Gate results

| Gate | Result | Evidence / limitation |
|---|---|---|
| Full test suite | **FAIL** | `testthat::test_local()` reaches the process/worker tests, where `processx` cannot create a write pipe for child `Rscript.exe` (`system error 5, Access is denied`). This affects worker, process-view and task-runner tests. The same environment restriction was previously observed by R CMD check. |
| `NOT_CRAN=true` suite | **FAIL** | The extended suite reaches extra process-backed checks and stops after testthat's 10-failure reporting limit. The reported failures are child-process launch denials, including `task-runner`; they do not establish that those lifecycle paths pass. |
| `R CMD build` | **PASS** | Built `termr_0.9.0.9000.tar.gz` with `--no-manual --no-build-vignettes`. |
| `R CMD check --as-cran` | **FAIL** | Rd, examples, install/load, code checks and documentation checks pass. Tests fail on `test-workers2.R:199` for the same processx pipe denial. Final status: 1 ERROR, 3 NOTEs. CRAN incoming checks could not connect to CRAN; DBI/RSQLite were unavailable, Pandoc was absent, and the environment could not verify current time. |
| Clean tarball install | **PASS** | Installed the built archive into a new temporary library; staged install, load, and final-location checks passed. |
| Example smoke tests | **PARTIAL** | 18 of 19 bundled examples were sourced and their app trees rendered headlessly; `database-explorer` was skipped because DBI/RSQLite are unavailable. `sql-workspace` ran in its documented no-database mode. |
| Benchmarks / baseline | **PASS WITH WARNING** | All 29 scenarios completed with 3 samples and were compared with `tools/bench/last-run.csv` (7-sample Windows R 4.5.2 run). For the separate ScrollView profile, 100/1k/2k/10k child scroll medians were 16/18/16/18 ms versus the checked-in 0.4 baseline's 20 ms; layout counts stayed 38 laid out / 34 painted. See benchmark notes below. |
| Export and knitr outside a TTY | **PASS** | Using the clean-installed tarball under non-interactive `Rscript`, text, Markdown, HTML, SVG, JSON snapshot, Markdown/HTML `knit_termr()`, and `write_rendered()` checks passed. |
| DBI/RSQLite integration | **UNTESTED** | Neither package is installed. An install attempt failed because CRAN indexes were unreachable. SQL/SQLite integration tests therefore skipped. |
| Linux/macOS PTY | **UNTESTED** | This audit host is Windows. Its Python has no `fcntl` or `termios`; no Linux/macOS runner was available. The configured GitHub Actions PTY jobs were not run from this environment. |
| Windows terminal smoke | **UNTESTED** | The Windows input/parser tests ran in the local suite, and the PowerShell helper parsed successfully. There is no `wt.exe`/Windows Terminal, and Python `winpty`/`pyte` are absent, so a real Windows Terminal/ConPTY session could not be driven. |
| Package size | **MEASURED** | Final source tarball: 386,594 bytes. Installed package tree: 3,928,558 bytes. No release size budget is defined. |
| Installed helpers/resources | **PASS** | Clean install exposes 19 files under `system.file("examples", package="termr")` and both helpers under `system.file("helpers", package="termr")`. `run_example()` lists all 19 examples. |
| Orphan processes | **NOT CERTIFIED** | All commands started during this audit returned or exited. Several `R`/`Rterm` processes were already present before the audit (start times around 09:34 and 11:55); process ownership/arguments could not be queried because Windows CIM returned Access denied. They were left untouched because their ownership is unknown. |
| Warnings | **PARTIAL** | R CMD check reported zero test warnings. Startup `LC_* = C.UTF-8` warnings and CRAN connection warnings are environment-related. The 10,000-child ScrollView benchmark emitted one reproducible app warning when initial mount-event processing hit `max_events_per_tick = 10000`; this is explained below but remains visible. |

## Benchmark details

The current full benchmark run used 3 samples per scenario; the saved
`tools/bench/last-run.csv` uses 7 samples, so wall-clock ratios are directional
and should not be treated as a controlled release-to-release comparison.
Repaint-cell counts match in most scenarios. The viewport, event-loop and
large-tree timings show no broad regression; `textarea-1MB-scroll` measured
40 ms versus 38 ms in the saved run (+5%), within the timing noise expected on
this Windows host. The current `scrollview-2000-labels` and `scrollview-page`
medians were 28 ms versus 270 ms and 252 ms in the saved run; exact repaint
counts remained 4,800.

The separate profile was run against
`tools/bench/baselines/windows-0.4-scroll.csv`. Current initial layout times
for 100/1k/2k/10k children were 280/560/1,310/7,450 ms versus
280/580/1,250/8,080 ms in that baseline. Current scroll medians were
16/18/16/18 ms versus 20 ms. At 10,000 children, initial app startup emits
`termr: too many events in one tick; possible event loop.` The warning comes
from the event-queue guard in `App$process_queue()` during `private$start()`;
the queue has at least 10,000 initial lifecycle events. Smaller profiles do
not emit it. This audit records the warning as a release issue rather than
silently suppressing it.

## Release blockers

- The full and `NOT_CRAN=true` suites do not pass on the available host because
  child process pipes are denied; consequently worker/process lifecycle and
  task-runner behavior have no successful local release run.
- `R CMD check --as-cran` exits with a test ERROR for that same denial.
- DBI/RSQLite integration, Linux/macOS PTY coverage, and real Windows
  Terminal/ConPTY smoke coverage remain untested.
- The audit cannot certify that no orphan process exists while pre-existing
  R/Rterm processes have unknown ownership and Windows CIM access is denied.
- The 10,000-child startup benchmark emits the event-queue limit warning.

