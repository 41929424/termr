# Running termr over SSH

termr runs inside the remote R process. With an interactive SSH pseudo-terminal,
the existing POSIX driver reads terminal input and writes the rendered screen
through the SSH terminal stream. It does not open an SSH connection or transfer
R objects to the client.

Linux through a real `ssh -t` session was manually reported PASS for RC1 at
`1846527d3968cdd12fa9900bbe80a9f2a89a1a3e`. Linux and macOS PTY CI separately
passed terminal-mechanics checks. One successful SSH host does not certify
all clients, servers or multiplexers; tmux and screen have not been manually
validated. See the [platform matrix](platform-testing.md) for the full status.

## Start an interactive app

Install termr and any optional packages on the remote machine, then allocate a
TTY when starting a remote command:

```sh
ssh -t user@server
```

At the remote shell:

```sh
R
```

```r
library(termr)
run_example("data-explorer")
```

For a direct command such as `ssh user@server Rscript app.R`, use `-t` to
request a remote pseudo-terminal. `-tt` forces allocation even when the local
SSH client has no terminal. Without a TTY, termr rejects the app with an
interactive-terminal error; it does not attempt to render an interactive app
to a pipe. No SSH-specific hint is shown because termr does not infer that a
non-TTY invocation came from SSH.

## What runs and crosses the connection

R, widget state, data frames, database connections, SQL queries, workers, and
subprocesses run on the server. A regular `data_table()` formats its visible
viewport from the in-memory R object; a lazy `table_source()` can fetch only
requested rows from its backend. This avoids requiring a copy of the source
dataset on the client, but it is not a security boundary: the rows and values
shown on screen are terminal output and cross SSH.

More precisely, termr does not require transferring source datasets to the
client machine; the terminal output selected for display travels through the
SSH session. If OSC 52 is enabled, an explicit copy action can also send the
selected text toward the client clipboard. Exports such as `render_text()`,
`render_html()`, `render_svg()`, and `screen_snapshot_json()` return values in remote
R; file-writing helpers write to the remote filesystem unless the caller
explicitly transfers the file separately.

Workers start an Rscript child on the same server with stdin detached from the
terminal, and communicate through pipes and temporary files. They do not use
termr's terminal input driver or require a TTY. The PTY CI harness checks that
the app has a controlling terminal while a worker's stdin is not a TTY.
`run_process()` likewise executes its command on the server without a shell
and without inheriting the terminal as stdin; the caller supplies one
argument per vector element. No local Windows paths or client-side executables
are involved. The reported SSH smoke pass does not separately certify every
worker, clipboard or disconnect scenario in the extended checklist below.

## Terminal type and capabilities

SSH passes a terminal type in `TERM`; termr uses the remote process environment
and does not inspect the client's operating system. The current defaults are:

| Remote `TERM` | Colour default | SGR mouse / bracketed paste | Alternate screen | Synchronized output |
|---|---:|---|---|---|
| `xterm`, `xterm-256color` | 16 / 256 | enabled | enabled | off unless a modern direct terminal is identified |
| `screen`, `screen-256color` | 16 / 256 | enabled | enabled | disabled by default |
| `tmux`, `tmux-256color` | 16 / 256 | enabled | enabled | disabled by default |
| `linux` | 16 | disabled | enabled | disabled |
| `vt100` | none | disabled | disabled | disabled |

`NO_COLOR=1` disables colour. The `high-contrast` theme and monochrome styles
remain available. In an SSH session, true colour is selected from an explicit
remote `COLORTERM` value; otherwise term suffixes such as `-256color` select
256 colours. A forwarded local `WT_SESSION` marker does not promote the remote
terminal to true colour. Use a UTF-8 locale on the server for reliable
wide-grapheme display.

The feature table describes termr's defaults, not proof that every SSH client
or terminal implements each mode. Mouse forwarding, paste delimiters, and
resize propagation can be changed or filtered by the client and any
intermediate multiplexer. `TERM=vt100` is treated as monochrome and does not
enable SGR mouse, bracketed paste, or alternate-screen modes.

When inside tmux, the inner `$TERM` should be `screen`, `screen-256color`,
`tmux`, or `tmux-256color`, as appropriate for that installation. Mouse input
requires tmux's `mouse` option (`set -g mouse on`). termr does not configure
tmux or screen. Screen support has not been manually tested.

OSC 8 hyperlinks are not emitted by the current renderer. OSC 52 is write-only
and opt-in for recognised direct terminals. On SSH, `TERM_PROGRAM` or a local
terminal marker alone does not enable OSC 52, hyperlinks, or synchronized
output; a generic `xterm-256color` inside SSH leaves those features off. Set
`TERMR_OSC52=1` or `options(termr.osc52 = TRUE)` only when the complete path
supports it. A terminal or multiplexer may still ignore or filter the
sequence, and termr cannot confirm that the OS clipboard changed. tmux also
requires clipboard support, an `Ms` capability, and a compatible
`set-clipboard` setting; its default configuration may block applications
inside tmux from writing the outer clipboard. See the [tmux clipboard
guide](https://github.com/tmux/tmux/wiki/Clipboard).

If OSC 52 is unavailable or blocked, termr's in-app clipboard still works.
This failure does not prevent the copy action. Because OSC 52 can deliberately
send displayed values to the client, treat enabling it as a data-boundary
choice.

## Input, resize, and exit

The POSIX driver opens the remote `/dev/tty` when possible, saves its `stty`
state, enables raw input, and starts a `cat` reader for terminal bytes. If the
process has a TTY on stdin but cannot reopen `/dev/tty`, it uses `/dev/stdin`.
The output path remains R's stdout. SSH does not change this ownership model:
the PTY created by `sshd` is the terminal being controlled.

The driver polls `stty size` every 0.5 seconds. An SSH client resize reaches
termr only if the SSH PTY and any nested tmux/screen PTY report the new size.
The key parser handles terminal input bytes, SGR mouse reports, and
bracketed-paste markers; it has no local-OS dependency. Without bracketed
paste support, pasted characters arrive through the ordinary key stream.

The app's default `Ctrl+C` binding quits and normal app shutdown restores
terminal modes, stops the input reader, and cancels termr workers. In raw mode,
Ctrl+C is normally delivered as a key byte, not as the terminal's SIGINT
character. `Ctrl+D` has no default quit binding. An R interrupt/error also
runs app shutdown. If SSH or the remote process is forcibly killed (for
example, SIGKILL or power loss), R cannot run cleanup. Closing the PTY usually
ends the reader and lets the app unwind, but restoration cannot be guaranteed
after the terminal has disappeared. tmux/screen can keep the remote process
and its pane alive across a disconnected SSH client; reconnect and reattach to
inspect that state.

## Coverage and extended manual smoke test

The [PTY CI matrix](platform-testing.md) is configured for GitHub-hosted Ubuntu
and macOS. It covers the terminal mechanics below, but does not simulate SSH
transport or a particular remote host:

| Behavior | PTY CI harness | Requires real SSH smoke |
|---|---|---|
| `/dev/tty`, raw mode, stdin-TTY fallback, and no-TTY error | covered | confirm on remote host |
| Keyboard, UTF-8, SGR mouse click/wheel/drag, bracketed paste | covered | confirm client forwarding |
| Resize, including tiny/wide screens and resize storms | covered | confirm SSH/multiplexer propagation |
| Ctrl+C, handler error, and terminal restoration | covered | confirm remote shell behavior |
| Worker starts without terminal stdin | covered | confirm remote worker launch/cleanup |
| SSH server policy, OSC filtering, tmux/screen, disconnect | not covered | required for end-to-end claims |

These checks approximate the PTY layer. They do not replace the manual SSH
checklist below or add an SSH integration service or network dependency.

Basic Linux SSH smoke has passed for RC1. Use this extended checklist when
validating another client/server path or the optional workflows below; the
RC smoke report does not establish that every item was exercised:

```sh
ssh -t user@host
```

At the remote shell, verify:

1. Run `Rscript -e 'termr::run_example("data-explorer")'`; check initial
   rendering, keyboard navigation, sorting, scrolling, and resize while the
   table is open. Quit with Ctrl+C and check that the shell prompt returns.
2. Run `termr::run_example("sql-workspace")` with its optional packages
   installed; paste a multiline query, edit it, execute it, and inspect the
   result. Try `database-explorer` when DBI and RSQLite are available.
3. Check mouse click, wheel, and drag; bracketed multiline Unicode paste; and
   copy with OSC 52 only if intentionally enabled.
4. Repeat inside tmux with `mouse on`; check pane resize and paste. Repeat
   inside screen if screen support is a release requirement.
5. Start a worker or `process_view()`, exit normally, then test what happens
   when the SSH client disconnects. Do not expect cleanup after forced process
   termination.
6. Run an interactive app without a TTY (for example, `ssh
   user@host 'Rscript app.R'` without `-t`) and confirm the clear terminal
   error; then repeat with `ssh -t`.

An RStudio Server **Terminal**, VS Code Remote SSH integrated terminal, or
`docker exec -it` / `kubectl exec -it` can use the same PTY model, but these
hosts are not covered by the SSH checklist or CI. The RStudio Server Console
is not a terminal and is unsupported.

## SSH ANSI measurements

The renderer diffs each frame against its previous frame and writes only the
changed cells. The following byte counts came from a Windows 11 / R 4.5.2
headless-driver run at the listed sizes. They count the ANSI payload produced
by termr, not SSH framing, encryption, network latency, or server-side render
time; they are structural comparisons rather than an SSH benchmark.

| Action | Screen | ANSI bytes | Full repaint |
|---|---:|---:|---|
| DataTable move cursor one row | 120x40 | 270 | no |
| DataTable PageDown | 120x40 | 271 | no |
| Type one character in SQL editor | 80x20 | 175 | no |
| Update progress bar | 80x3 | 67 | no |
| One spinner timer tick | 80x3 | 18 | no |
| Initial 120x40 screen with 40 long rows | 120x40 | 3,767 | yes |

For comparison, the checked-in [recorded benchmark](performance.md) measured
286 bytes for a 1M-row DataTable cursor move and 1,108 bytes for PageDown at
120x40, both incremental. A slow SSH link adds transport latency to these
payloads, but termr does not add a separate network round trip per changed
cell.
