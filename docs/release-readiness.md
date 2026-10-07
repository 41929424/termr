# 1.0 release readiness audit

Audit date: 2026-10-07, on the 0.9 development tree (`0.9.0.9000`). Host:
Windows 11 x64, R 4.5.2, testthat 3.3.2, DBI 1.3.0, RSQLite 3.53.3, jsonlite
2.0.0, knitr 1.51; no pandoc, no Linux/macOS host, no SSH host.

## Gate results

| Gate | Result | Evidence / limitation |
|---|---|---|
| Full suite, `NOT_CRAN=true` | **PASS** | 8110 expectations, 0 failures, 1 skip (POSIX driver test on Windows). Before the audit fixes the same run had 15 failures and 3 errors, all in database code. |
| Full suite, CRAN mode | **PASS** | 8012 expectations, 0 failures, 31 skips: the POSIX test, snapshot tests and subprocess/worker tests marked `skip_on_cran`. All of them run in the `NOT_CRAN` gate and in CI. |
| `R CMD build` | **PASS** | `termr_0.9.0.9000.tar.gz`, 409,249 bytes. |
| `R CMD check --as-cran` | **PASS (3 NOTEs)** | 0 errors, 0 warnings. NOTEs: the maintainer is not a named person (`termr authors`; must be fixed for CRAN) and the `.9000` development version; no time server; no pandoc to check README/NEWS. The last two are host-only. |
| Clean install | **PASS** | Tarball installed into an empty library; loaded from it. |
| Installed-package smoke | **PASS** | From a neutral directory, against the clean install: all 20 bundled examples source and render headlessly (three are static by design), a real worker subprocess (helper found through the installed package), worker crash diagnostics, shell-free `run_process()` arguments, text/Markdown/HTML/SVG/JSON export, `knit_termr()`, `write_rendered()`, `run()` refusing a non-terminal, and the SQLite explorer preview and query. |
| Windows ConPTY (real pseudo console) | **PASS, one flake** | `tools/e2e-windows.py` with pywinpty: 12 scenarios (typing, Tab/Enter, resize, Ctrl+C, mouse clicks, workers, worker errors, handler error restore, editor, form Escape, kitchen sink, data browser). 11 passed first time; `gate-data-explorer` exited before its last keystroke once, then passed 3 of 3 reruns. Unexplained; needs a manual check. |
| DBI/RSQLite integration | **PASS** | All DB tests run. They exposed that `db_table_source()` failed on current RSQLite (fixed). |
| Linux/macOS PTY | **NOT RUN HERE** | Windows host. Covered by the `pty` CI jobs; no result was available to this audit. |
| SSH, tmux, RStudio Terminal | **NOT TESTED** | No host available. |
| Benchmarks | **PASS** | Structural counters (repainted cells, rectangles) equal the checked-in run. A controlled A/B against the pre-audit commit shows no regression (`datatable-1M-cursor` 0.96, `signals-1000` 0.94). Lazy DB query source over 1M rows: 100 rows fetched for the first screen, 300 rows in 3 fetches after jumping to row 500,000. |

## Defects found and fixed in this audit

See NEWS for the user-facing list. In short: `db_table_source()` broken on
current RSQLite; per-column source fetches; explorer filter and disconnected
status; parser crash on non-UTF-8 bytes and unbounded escape buffering;
widget moves dropping reactive bindings, timers and workers; reactive graph
stuck after an interrupt; C1 control characters reaching the terminal;
password values in logs, inspection and clipboards; HTML export column shift;
a spurious event-loop warning for large trees; `widget$on()` argument order;
an empty README.

## Open items

Tracked in the audit report; the ones that matter for 1.0 are the CRAN
maintainer field, Linux/macOS/SSH/RStudio manual runs, and the decisions on
event-name and snapshot-function naming listed there.
