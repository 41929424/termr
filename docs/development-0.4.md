# Development towards 0.4

This branch is a development version, not a released 0.4.0. The scope is
large UI/data performance, DataTable and TextArea workflows, and a small
documented extension surface. Optional features do not take priority over
correctness or measured performance.

## Baseline (Windows 11, R 4.5.2)

The starting revision is `4834ca8`, including the unsigned Windows mouse
record fix. All DWORD boundaries and positive/negative wheel regressions
pass. The preceding source-package check finished with `Status: OK`.
The full NOT_CRAN run had no assertion failures; 18 subprocess tests were
blocked by sandbox pipe permissions. Those suites were rerun outside the
sandbox: 136 expectations, zero failures, errors, warnings or skips.

No release tags are present locally. GitHub CI/tags/issues return 404 with
the available access, so remote release status and Unix CI are unverified.
The Windows fix was committed separately on main; feature work is on
`develop-0.4`. A release tag has not been created.

Baseline CSVs live in `tools/bench/baselines/`. Times are advisory and use
three samples on this host. `scroll-profile.R` measures 100x30 viewports;
`bench.R` normally uses 120x40. Do not compare unlike viewport sizes.

## Completed slices

* Vertical scroll layout index with two-row overscan, reused by ScrollView
  and ordinary nested vertical containers. Full layout remains available
  for focus reveal and as a correctness oracle.
* Viewport-limited compositor, mouse hit testing and layout snapshots.
* Opt-in frame timing and structural counters (`options(termr.profile=TRUE)`,
  `app$profile_last_frame`). The debug overlay shows the timing breakdown.
* Batched widget-ID validation for multi-child mounts, bounded tree traversal,
  table display-column order/range selection/TSV extraction, TextArea replace,
  goto-line, cursor status and viewport-first token highlighting, named signal
  diagnostics, and experimental custom layout registration.

The scroll profile on this Windows host (R 4.5.2, 100x30) measured five
scroll ticks at about 1,472 ms with 10,000 children on the starting code and
20 ms after indexing. The profiled tick measured zero widgets and laid out 38;
the two-row overscan tree painted 34. Initial layout remains proportional to
the child count and took about 8.1 seconds at 10,000. A separate tree profile
measured one `walk()` over 10,001 nodes at about 60 ms; batched 10,000-child
mount took about 5.7 seconds including R6 widget construction. These are
single-run advisory values, not release guarantees.

The TextArea storage profile is recorded in
`tools/bench/baselines/windows-0.4-textarea.csv`. At 20 MB and about 198,000
lines, construction took 180 ms, a missing search took 820 ms, and viewport
rendering took less than 10 ms; insertion and undo/redo were below the
10 ms timer resolution. The line-vector model remains adequate for these
operations. An individual edit larger than the 2-million-character history
budget is applied without retaining an undo record.

## Release work still required

Lazy table sources; bracket matching; reactive graph scale benchmarks;
inspector and terminal polish; resource tests; upgraded examples and docs;
final clean install, full NOT_CRAN tests, as-CRAN check, ConPTY and available
Unix CI evidence. The 10,000-node mount path still merits profiling beyond
correct ID batching.

No release-ready claim is made until these gates have been assessed.
