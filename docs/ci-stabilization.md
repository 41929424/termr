# Targeted CI stabilization before 1.0-rc1

Baseline recorded before implementation changes on 2026-10-07:

- HEAD: `fd36a712ae2f0026bdc90319f738d9240dce5af1`.
- `git status --porcelain=v1`: empty (clean working tree).
- Reported CI failures: workers in R CMD check, invalid UTF-8 on R 4.1,
  Linux/macOS PTY integration, and an unclosed DBI connection warning.

The failing Actions run examined was
[37623299073](https://github.com/41929424/termr/actions/runs/37623299073),
at commit `708416fbaa5b208e7aaabfe9583b3506ccdf2719`. Its worker runtime,
Pilot implementation, worker tests and key parser match the baseline runtime
(the worker file has additional local documentation only).

## Worker root cause

The exact cause of the reported CI worker timeouts is **not established**.
The untouched baseline passed the installed targeted worker/parser tests
(141 assertions), an actual targeted R CMD check, and a full Windows
R CMD check (8125 assertions, no test failures or warnings). Public check
annotations confirm the reported timeouts, but the full Actions logs endpoint
returns HTTP 403. Do not attribute those failures to CI slowness or claim
that a local pass proves their cause is fixed.

Two independent defects were demonstrated:

* Runtime changes to the parent's `.libPaths()` were not passed to its
  Rscript worker. An installed R6 copy in a private library was therefore
  invisible to the child. The regression checks which installed copy the
  worker actually loads from an unrelated working directory.
* `Worker` called processx `read_all_output_lines()`/`read_all_error_lines()`
  after detecting parent exit. These methods wait for pipe EOF. A surviving
  descendant can retain those pipes. The original installed package blocked
  polling for **5.3 seconds** with a five-second descendant; its parent had
  already exited. A separate regression demonstrated that a descendant
  remained alive after worker completion, until an explicit `kill_tree()`.

Neither defect has been proven to explain the particular pure `cat()` and
`quit()` CI timeouts. The new diagnostics are intended to make that distinction
observable on the next runner execution.

## Worker fix

`Worker$private$start_process()` still uses the installed helper through
`system.file()` and the current R's absolute Rscript path, with `--vanilla`
and detached stdin. It now sets `R_LIBS` from the parent's current library
search paths. It preserves inherited PATH, locale and other legitimate
environment values, while retaining the existing empty `R_TESTS` and owned
temporary directory overrides. `run_worker()`, widget/App worker methods and
`Pilot$run_worker()` converge on this path; inline workers do not spawn.
`run_process()` continues to preserve its caller's environment semantics.

Polling reads available output without waiting indefinitely for EOF. On
confirmed process exit, it first stops surviving descendants while keeping
the pipes open for reading. The finite buffered output is drained over ticks;
after that, a bounded **100 ms** idle window handles the observed Windows
race between exit and pipe EOF, including the final unterminated output line.
No I/O wait blocks the UI during that window. A 5000-line regression verifies
that every stdout callback is delivered and the last 1000 lines are retained.
Existing worker and Pilot timeouts were not increased.

Completion, failure and cancellation clean up descendants immediately, even
when the root process has already exited. Missing or malformed results become
failures and leave the app's worker registry. The streamed-output test waits
for completion rather than assuming the first stdout line means completion.

Unexpected exits, worker timeouts and Pilot timeout errors include PID,
executable, process/worker state, exit status, result-file existence and a
bounded stderr tail. Diagnostics are retained before temporary-file cleanup;
arguments and full environment values are not dumped. No new public API or
runtime dependency was added.

## R_TESTS regression

The old fix is present and functioning; its recurrence was not reproduced.
The regression creates a valid startup hook that writes a marker. A control
Rscript with inherited `R_TESTS` actually creates that marker. An installed
worker runs only its payload, sees empty `R_TESTS`, and does not create it.
The test restores the parent's environment and removes the hook/marker.

## R 4.1 parser root cause

`KeyParser$feed()` previously called `enc2utf8()` before checking validity.
R 4.1's
[UTF-8 translation source](https://svn.r-project.org/R/branches/R-4-1-branch/src/main/sysutils.c)
substitutes an untranslatable native byte with printable hex text, such as
`<ff>`. Thus `a<ff>b` is already valid ASCII when validation runs; its third
key is `f`. This explains the reported Ubuntu R 4.1 result. It is a
locale-dependent conversion, not a different interpretation of the `b` byte.
The old conversion on the local Windows R 4.1/CP1252 runtime instead maps FF
to U+00FF, illustrating why a pass on one host does not settle the bug.

## Parser fix

The internal decoder validates terminal bytes before native encoding
conversion. It uses a validated fast path for complete UTF-8; otherwise it
decodes code points arithmetically, replaces malformed prefixes with U+FFFD,
and resumes at the next valid byte. Valid incomplete prefixes remain buffered
across reads. It rejects overlong encodings, surrogate code points and values
beyond U+10FFFF. `flush()` finishes a truncated prefix explicitly.

Both POSIX `cat` reader paths request UTF-8 from processx explicitly, avoiding
native-locale recoding of valid terminal input. A binary pipe fixture checks
ASCII, accented characters, Euro and emoji bytes through actual processx.

Regressions cover the original FF-between-ASCII case, truncated sequences,
invalid continuations, invalid sequences split at every byte, valid keys after
malformed bytes, valid UTF-8 split at every byte, and fragmented bracketed paste.
The old expectation of `b` was retained.

## PTY root causes and changes

The confirmed fixture/harness defects are separate from terminal runtime bugs:

* The harness inherited TERM while demanding capable-terminal modes. Capability
  policy disables those modes for missing/dumb TERM. The historical runner's
  exact TERM was not available from protected logs. Capable children now use
  `xterm-256color` explicitly, print TERM/COLORTERM/TERM_PROGRAM, and have
  terminal identity hints removed. A separate `TERM=dumb` case checks input,
  omission of unsupported modes and restoration.
* The old focused Input consumed printable/special keys and paste before the
  App logger. Timers still reached the log, explaining the observed tick-only
  records. A passive label and a logging handler now observe the input and
  prevent default commands from altering the probe. The exported-API headless
  fixture smoke verifies these events, including F1, paste, wheel and drag.
* `ready` was written before `run()`. It now comes from the first event-loop
  tick after driver startup. The harness checks raw mode at readiness and waits
  for expected event records after each write instead of fixed read/sleep
  guesses. Delays in deliberately fragmented transport cases are only 10 ms.
* The stdin-only check inspected an empty `Session.output` before any read.
  It now waits for the child marker and verifies stdin is a TTY with no usable
  `/dev/tty`. SIGHUP is ignored only in the child during `TIOCNOTTY` and restored
  afterwards, rather than globally in the harness.
* PTY EIO/EOF is treated as slave closure. Diagnostics retain observed child
  status/output; writes after early exit fail the case with that evidence.
  Cases are never converted to skips. Failed cases kill their process group,
  reap the child and close the master FD; temporary event logs are removed.

Mode teardown assertions run only for modes that were enabled. Cursor recovery
and identical stty state remain required for normal exit, Ctrl+C, handler error,
stdin fallback and degraded TERM. SIGINT is also exercised. Resize storms and
tiny/wide terminals use event completion rather than fixed sleeps.

The pre-existing mouse model in `R/mouse.R` defines click as down/up on the
same widget. Release over the origin therefore emits `mouse.up`, `drag.end`,
then `click`, including after movement; release outside does not click.
This behavior was retained, characterized in a test and documented in
[events.md](events.md). No drag runtime behavior was changed.

## DB resource warning

The original check's `termr-Ex.Rout` records the warning immediately at
`cleanEx()` after the `db_explorer()` Rd example. That example constructed an
App with an owned SQLite connection but never started/stopped it. Ownership
alone does not perform app teardown. The example now starts a headless App
with `test_app()` and calls `pilot$stop()`, closing the owned connection.
The matching roxygen source and Rd are updated. Database ownership tests also
guard valid handles on exit so a failed assertion cannot produce a secondary
connection-leak warning. External connection ownership semantics are unchanged.

Canceled lifecycle subprocess fixtures now use `child_tmp()`, preventing
killed Rscript startup files from becoming check-directory detritus.

## Validation

Local host: Windows 11 x64, R 4.5.2 UCRT, DBI 1.3.0, RSQLite 3.53.3.
The shell's unsupported Windows `C.UTF-8` locale overrides were unset for test
commands; workers themselves continue to inherit their parent's locale.
Official R 4.1.3 was extracted into an ignored workspace directory without
running its installer or changing the system R installation.

| Check | Result |
| --- | --- |
| Final targeted Windows/SQL/input/worker/parser/pipe set | PASS: 1095 assertions, 0 failures/warnings/skips |
| Latest R 4.1.3 source worker/parser/pipe/lifecycle set | PASS: 279 assertions, 0 failures/warnings/skips |
| R 4.1.3 installed worker/parser set | PASS: 233 assertions, 0 failures/warnings/skips |
| R 4.1.3 parser in C and CP1252 locales | PASS: 140 assertions per locale, 0 failures/warnings/skips |
| Headless smoke of actual PTY R fixture | PASS: exported API only |
| Python PTY scripts | Syntax compilation PASS |
| Final full default suite | PASS: 8095 assertions, 0 failures/warnings, 31 existing CRAN/platform skips |
| Final full NOT_CRAN=true suite | PASS: 8226 assertions, 0 failures/warnings, one existing POSIX-only skip on Windows |
| R CMD build | PASS |
| Final R CMD check --as-cran --no-manual | PASS: 0 errors, 0 warnings, 3 explained NOTEs; 8226 assertions passed |
| Actual Linux/macOS PTY | NOT RUN: Windows host; WSL is not installed and no Unix/macOS runtime is available |

The final suites include the pipe, finite-backlog and process-diagnostic
assertions. No new skip or generic timeout increase was added.
PDF manual building is not part of the local check, matching the workflow's
existing `--no-manual` flag.

The three NOTEs are CRAN incoming/new submission with development version
`0.9.0.9000`, inability to verify the host's current time, and missing local
pandoc for checking README/NEWS. Temp-directory detritus is now OK. The final
example log has no DBI connection warning; the test reporter has zero warnings.
The clean check install also passes the headless PTY fixture smoke. A process
scan after testing found no workspace `termr-worker.R` helper alive; the
descendant-held-pipe regression separately verifies no descendant remains.

Final tarball SHA-256:
`cfc8fa7fa0cd5947799fd2203851e8ffd4e72afd5b7805bcdfbe4980963328fd`.
Raw local validation logs are in the ignored `tools/ci-work/` directory.

## CI observation

All results below distinguish the inspected historical run from this working
tree, which has not been pushed or run on Actions.

| Job | Historical run | After these changes |
| --- | --- | --- |
| Ubuntu release | FAIL | NOT RUN |
| Ubuntu oldrel-1 | FAIL | NOT RUN |
| Ubuntu 4.1 | FAIL | NOT RUN |
| Ubuntu devel | FAIL | NOT RUN |
| macOS release | FAIL | NOT RUN |
| Windows release | FAIL | NOT RUN |
| SQL integration (DBI + SQLite) | PASS | NOT RUN |
| Linux PTY | FAIL | NOT RUN |
| macOS PTY | FAIL | NOT RUN |
| Windows input regressions | PASS | NOT RUN |

The existing matrix is retained. Extended NOT_CRAN=true tests now run on every
R CMD check platform rather than only Windows. The PTY jobs explicitly set
TERM and run the headless fixture preflight; Windows input also runs that
preflight. These workflow changes have only local validation so far.

## Remaining blockers

The original CI worker timeout cause remains unverified without runner logs
or a diagnostic runner execution. Actual Linux/macOS PTY execution and the
updated Actions matrix are not observed. Local test results must not be used
to label those remote jobs green.

## Verdict

NOT READY TO RE-RUN FULL 1.0-RC CI

The requested exact CI worker diagnosis and actual Unix PTY validation remain
open. The implementation and local evidence are ready for review, but those
missing observations prevent claiming that all requested CI regressions are
resolved.
