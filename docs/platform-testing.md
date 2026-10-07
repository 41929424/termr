# Platform confidence and terminal testing

termr has deterministic input-parser and headless rendering tests, plus an
interactive PTY harness for Unix. These test kinds are deliberately distinct:
headless tests exercise event/render behavior, parser tests exercise driver
translation, and PTY tests exercise a real terminal interface and restoration.
A parser or headless pass does not certify a particular terminal host.

## CI coverage

| Platform / path | Automated coverage | Limits |
|---|---|---|
| Linux PTY, `/dev/tty` | `tools/pty/pty-check-ci.py` exercises raw mode, key input, UTF-8, mouse press/release/wheel/drag, bracketed paste, resize storms, Ctrl+C, handler errors, normal/error termios restoration, stdin-only fallback, and no-TTY errors. | Runs on GitHub-hosted Ubuntu; it does not cover every Linux terminal emulator. |
| macOS PTY, `/dev/tty` | Same harness on GitHub-hosted macOS. | Does not cover every terminal emulator or remote shell. |
| Windows input protocol | Windows CI runs deterministic `WindowsDriver` record parsing and shared mouse, paste, Unicode, bounds, and lifecycle tests. PowerShell helper records are covered through the parser contract. | GitHub Actions does not promise a Windows Terminal or interactive ConPTY session for this job. CI parses the helper script and tests its wire protocol, but does not launch it against a real console. |
| Windows Terminal / ConPTY | No automated interactive session currently. | Requires a real console host; the Windows CI shell may expose redirected pipes instead. |
| PowerShell console / conhost | No end-to-end console-mode test currently. | Requires an attached interactive console to verify native input modes and restoration. |
| RStudio Terminal | Not covered by CI. | Requires an installed RStudio desktop session and its integrated terminal. |

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
host testing must be done in Windows Terminal, PowerShell/conhost, or RStudio
Terminal; until such a host runner is available, those matrix entries remain
untested rather than inferred from parser tests. A restricted local Windows
sandbox may deny processx child pipes with `Access is denied`; the process
lifecycle test skips only that exact condition outside CI and is required to
run in CI.

The PTY matrix validates terminal mechanics but does not certify an SSH
client/server or terminal multiplexer. See [Running termr over SSH](ssh.md)
for the separate coverage matrix and required manual smoke test.
