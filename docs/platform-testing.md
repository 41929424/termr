# Platform confidence and terminal testing

termr has deterministic input-parser and headless rendering tests, plus an
interactive PTY harness for Unix. These test kinds are deliberately distinct:
headless tests exercise event/render behavior, parser tests exercise driver
translation, and PTY tests exercise a real terminal interface and restoration.
A parser or headless pass does not certify a particular terminal host.

## RC1 validation

Validated source: `1846527d3968cdd12fa9900bbe80a9f2a89a1a3e`, tagged
`v1.0.0-rc1` with package version `0.9.0.9000`. Full workflow
[37758703750](https://github.com/41929424/termr/actions/runs/37758703750)
passed. Manual statuses below record the maintainer's RC smoke reports;
they do not certify every emulator, version or client/server combination.

| Environment | Automated | Manual | Confidence / notes |
|---|---|---|---|
| Ubuntu R release, oldrel-1, 4.1, devel | PASS | Not a terminal-host test | Four R CMD check jobs: 0 ERROR / WARNING / NOTE, plus passing extended suites. |
| macOS R release | PASS | NOT RUN for a real terminal | R CMD check: 0 ERROR / WARNING / NOTE; extended suite PASS. |
| Windows R release | PASS | See real-console row | R CMD check: 0 ERROR / WARNING / NOTE; extended suite PASS. |
| SQL integration (DBI + RSQLite) | PASS | No separate database-host certification | Conditional database tests ran with both packages installed. |
| Linux PTY, `/dev/tty` and stdin-TTY fallback | PASS | See Linux SSH row | Raw mode, keyboard/UTF-8, mouse click/wheel/drag, bracketed paste, resize storms, Ctrl+C, handler errors, restoration and no-TTY errors. |
| macOS PTY | PASS | NOT RUN for a real terminal | Same harness on GitHub-hosted macOS; emulator behavior still needs manual testing. |
| Windows input/parser | PASS | Not a real-console test | Deterministic wire parser, mouse/paste/Unicode/bounds/lifecycle tests and PowerShell script parsing. |
| Windows real interactive console | Parser coverage above | PASS | Real-console RC smoke was performed and reported PASS, including the native-crash check. Host/version details were not recorded here. |
| Linux over real `ssh -t` | POSIX mechanics above | PASS | RC smoke on a real SSH path; arbitrary clients, servers and clipboard forwarding are not certified. |
| tmux | Capability/input unit coverage only | NOT RUN | No manual multiplexer smoke report. |
| screen | Capability/input unit coverage only | NOT RUN | No manual multiplexer smoke report. |
| RStudio Terminal | No host-specific CI | NOT RUN | Requires an installed RStudio desktop/server terminal session. |

**GitHub Windows CI does not replace a real interactive Windows console
test.** The manual Windows real-console smoke is separate evidence. Neither
the parser pass nor that smoke certifies every ConPTY harness or console host.

The POSIX harness chooses `/dev/tty` when it is a controlling terminal, then
checks the stdin-TTY fallback after detaching the controlling terminal. It also
runs a no-TTY child and compares `stty -g` before and after app exit. The Windows
PowerShell helper uses a `finally` block to restore console input/output modes
and Ctrl+C handling; only its numeric record protocol is exercised without a
real console.

## Deterministic bounds tests

`tests/testthat/test-platform-bounds.R` sends UTF-8 combining, CJK, and ZWJ
emoji text through the input widget at 1x1, paints at 4096 columns, and applies
rapid changes between tiny and large sizes. The PTY harness repeats tiny and
wide resizes through the live POSIX driver and asserts that it reaches the final
size without hanging. Intermediate resize notifications may be coalesced by
the OS; the contract checked is a correct final size and usable redraw.

On a developer machine, run the headless and parser cases with:

```sh
Rscript -e "testthat::test_local(filter = 'windows-driver|posix-driver|mouse|paste|unicode|graphemes|platform-bounds')"
```

Run the real Unix terminal path with:

```sh
python3 tools/pty/pty-check.py
```

The PTY tests require Python's Unix `pty`, `fcntl`, and `termios` modules, `stty`,
and a working R installation. They cannot run on Windows. Interactive Windows
host testing must be done in an attached console, such as Windows Terminal
with PowerShell/Rterm. The RC1 real-console smoke above has passed; RStudio
Terminal remains NOT RUN. A restricted local Windows
sandbox may deny processx child pipes with `Access is denied`; the process
lifecycle test skips only that exact condition outside CI and is required to
run in CI.

The PTY matrix validates terminal mechanics but does not certify an SSH
client/server or terminal multiplexer. See [Running termr over SSH](ssh.md)
for the separate coverage matrix and required manual smoke test.
