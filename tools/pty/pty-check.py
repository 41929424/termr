#!/usr/bin/env python3
"""Integration checks of termr's Unix driver in a real pseudo terminal.

Uses only the Python standard library (pty, termios, fcntl), so it runs on
any Linux or macOS machine with R and termr installed:

    python3 tools/pty/pty-check.py

Checks:
  * the app starts in raw mode on the alternate screen and draws;
  * typed characters, Tab and Enter (escape sequences included) arrive;
  * a resize (TIOCSWINSZ + SIGWINCH) is noticed and the layout follows;
  * Ctrl+C quits with exit status 0;
  * the terminal settings (termios) are restored after a normal exit,
    after an error in a handler, and the error message is printed after the
    alternate screen is left.
"""
import fcntl
import argparse
import errno
import os
import platform
import pty
import re
import select
import signal
import struct
import sys
import termios
import time
import shlex
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RSCRIPT = os.environ.get("RSCRIPT", "Rscript")
ANSI = re.compile(r"\x1b\[[0-9;?<>]*[ -/]*[@-~]|\x1b\][^\x07]*\x07|\x1b[()][0-9A-Za-z]")


def strip(text):
    return ANSI.sub("", text)


def require_current_install():
    """The apps run `library(termr)`: refuse to test an installed termr whose
    version differs from this checkout (e.g. an old release)."""
    import subprocess
    with open(os.path.join(ROOT, "DESCRIPTION"), encoding="utf-8") as f:
        wanted = next(line.split(":", 1)[1].strip() for line in f if line.startswith("Version:"))
    out = subprocess.run([RSCRIPT, "-e", "cat(as.character(packageVersion('termr')))"],
                         capture_output=True, text=True)
    found = out.stdout.strip()
    if out.returncode != 0 or found != wanted:
        sys.exit(f"Installed termr is {found or 'missing'}, this checkout is {wanted}. "
                 "Install the checkout first (R CMD INSTALL .) or set R_LIBS.")
    print(f"testing installed termr {found}")


class Session:
    active = []

    def __init__(self, script=None, cols=60, rows=20, tty_mode="controlling", term="xterm-256color", command=None,
                 extra_env=None):
        env = os.environ.copy()
        env.update(extra_env or {})
        for key in ("COLORTERM", "TERM_PROGRAM", "WT_SESSION", "KITTY_WINDOW_ID",
                    "WEZTERM_EXECUTABLE", "ALACRITTY_LOG", "VTE_VERSION"):
            env.pop(key, None)
        env["TERM"] = term
        print(f"PTY child: TERM={term!r} COLORTERM='' TERM_PROGRAM='' tty_mode={tty_mode}", flush=True)
        command = command or [RSCRIPT, script]
        # fork() copies Python's buffered output into the child. Flush both
        # streams so a previous case's failure cannot reappear in this PTY.
        sys.stdout.flush()
        sys.stderr.flush()
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
            if tty_mode == "controlling":
                try:
                    tty = os.open("/dev/tty", os.O_RDWR)
                    valid = os.isatty(0) and os.tcgetpgrp(tty) == os.getpgrp()
                    os.close(tty)
                except OSError:
                    valid = False
                os.write(1, b"__PTY_CTTY_OK__\n" if valid else b"__PTY_CTTY_MISSING__\n")
            elif tty_mode in ("stdin", "none"):
                # Detaching a session leader sends SIGHUP. Ignore it only
                # for this operation, then restore normal child semantics.
                old_hup = signal.signal(signal.SIGHUP, signal.SIG_IGN)
                fcntl.ioctl(0, termios.TIOCNOTTY, 0)
                signal.signal(signal.SIGHUP, old_hup)
                if tty_mode == "none":
                    null = os.open(os.devnull, os.O_RDONLY)
                    os.dup2(null, 0)
                    os.close(null)
                    os.write(1, b"__PTY_NO_TTY__\n")
                else:
                    try:
                        tty = os.open("/dev/tty", os.O_RDWR)
                        os.close(tty)
                        os.write(1, b"__PTY_STDIN_FALLBACK_INVALID__\n")
                    except OSError:
                        os.write(1, b"__PTY_STDIN_ONLY__\n" if os.isatty(0) else b"__PTY_STDIN_NOT_TTY__\n")
            os.execvpe(command[0], command, env)
        self.output = ""
        self.status = None
        self.eof = False
        self.pgid = None
        self.active.append(self)

    def identity(self):
        """pid / process group / session of the child. A pty child is a session
        leader (so pgid == pid), but that is verified, never assumed; the group
        is remembered because getpgid() fails once the child is reaped."""
        try:
            self.pgid = os.getpgid(self.pid)
            return f"pid={self.pid} pgid={self.pgid} sid={os.getsid(self.pid)}"
        except ProcessLookupError:
            return f"pid={self.pid} (gone) last-known pgid={self.pgid}"

    def resize(self, cols, rows):
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
        try:
            os.kill(self.pid, signal.SIGWINCH)
        except ProcessLookupError:
            pass

    def read(self, seconds):
        end = time.monotonic() + seconds
        while not self.eof and time.monotonic() < end:
            # Never block past the requested duration: a "10 ms" read between
            # fragments must not become a 50 ms gap.
            ready, _, _ = select.select([self.fd], [], [], max(0.0, min(0.05, end - time.monotonic())))
            if ready:
                try:
                    data = os.read(self.fd, 65536)
                except OSError as error:
                    if error.errno != errno.EIO:
                        raise
                    # PTY slave closure commonly surfaces as EIO. Reap the
                    # child below and report its status, not a driver guess.
                    self.eof = True
                    return
                if not data:
                    self.eof = True
                    return
                self.output += data.decode("utf-8", "replace")

    def wait_for(self, text, seconds=30):
        end = time.monotonic() + seconds
        while time.monotonic() < end:
            if text in strip(self.output):
                return True
            self.read(0.2)
            exited = self.exited()
            if self.eof or exited:
                break
        if text in strip(self.output):
            return True
        raise AssertionError(f"waiting for {text!r}; status={self.status}; output tail: {strip(self.output)[-400:]!r}")

    def send(self, keys, pause=0):
        try:
            os.write(self.fd, keys.encode("utf-8") if isinstance(keys, str) else keys)
        except OSError as error:
            self.read(0.1)
            self.exited()
            raise AssertionError(f"input write failed: {error}; status={self.status}; output={strip(self.output)[-400:]!r}") from error
        if pause:
            self.read(pause)

    def attrs(self):
        return termios.tcgetattr(self.fd)

    def exited(self):
        if self.status is None:
            pid, status = os.waitpid(self.pid, os.WNOHANG)
            if pid:
                self.status = os.waitstatus_to_exitcode(status)
        return self.status is not None

    def wait_exit(self, seconds=15):
        end = time.monotonic() + seconds
        while time.monotonic() < end:
            self.read(0.1)
            if self.exited():
                self.read(0.1)
                return self.status
        raise AssertionError(f"the app did not exit; output tail: {strip(self.output)[-400:]!r}")

    def close(self):
        """Kill what is left of the child's process group and reap it.

        Never assumes pid == pgid and never signals the harness' own group.
        Returns a list of unexpected problems (empty when clean) instead of
        raising, so one case's cleanup can not take the following cases down.
        """
        problems = []
        leader_alive = not self.exited()
        pgid = self.pgid
        if leader_alive:
            try:
                pgid = self.pgid = os.getpgid(self.pid)
            except ProcessLookupError:
                leader_alive = False
        if pgid and pgid > 1 and pgid != os.getpgrp():
            try:
                os.killpg(pgid, signal.SIGKILL)
            except ProcessLookupError:
                pass  # nothing left in the group
            except PermissionError as error:
                # macOS answers EPERM for a group whose only members are
                # already-exited zombies. That is expected once the leader is
                # gone; with a live leader it is a real problem.
                if leader_alive:
                    try:
                        os.kill(self.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    except PermissionError:
                        problems.append(f"cannot kill live child ({self.identity()}): {error}")
        elif leader_alive:
            try:
                os.kill(self.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
        if self.status is None:
            try:
                _, status = os.waitpid(self.pid, 0)
                self.status = os.waitstatus_to_exitcode(status)
            except ChildProcessError:
                self.status = self.status if self.status is not None else -1
        try:
            os.close(self.fd)
        except OSError:
            pass
        return problems


def teardown_modes(s):
    for name, mode in (("alternate screen", 1049), ("bracketed paste", 2004),
                       ("mouse reporting", 1000), ("SGR mouse", 1006)):
        enter, leave = f"\x1b[?{mode}h", f"\x1b[?{mode}l"
        if enter in s.output:
            check(name + " disabled at exit", s.output.rfind(leave) > s.output.rfind(enter))
    check("cursor shown again", "\x1b[?25h" in s.output and
          s.output.rfind("\x1b[?25h") > s.output.rfind("\x1b[?25l"))


def check(name, condition, detail=""):
    print(("PASS " if condition else "FAIL ") + name + (f": {detail}" if detail and not condition else ""))
    if not condition:
        check.failed += 1
        if os.environ.get("GITHUB_ACTIONS"):
            # Surface the failure in the run's annotations (logs need a login).
            text = f"{name}: {detail}".replace("%", "%25").replace(chr(13), "%0D").replace(chr(10), "%0A")
            print(f"::error title=PTY {platform.system()}::{text[:1800]}", flush=True)


check.failed = 0


def normal_session():
    with tempfile.TemporaryDirectory(prefix="termr-pty-hello-") as directory:
        log = os.path.join(directory, "events.log")
        os.environ["TERMR_PTY_LOG"] = log
        s = Session(os.path.join(HERE, "hello-app.R"))
        s.wait_for("termr demo", 60)
        print("PTY child identity:", s.identity(), flush=True)
        check("PTY harness provides a controlling terminal", "__PTY_CTTY_OK__" in s.output,
              "pty.fork child could not open /dev/tty as its controlling terminal")
        check("hello app ready", wait_until(lambda: "ready" in read_log(log), s, 60), repr(read_log(log)))
        attrs = s.attrs()
        check("raw mode while running (no ICANON, no ECHO)",
              not (attrs[3] & termios.ICANON) and not (attrs[3] & termios.ECHO))
        check("alternate screen entered", "\x1b[?1049h" in s.output)
        s.send("Ada")
        check("typing reaches input", wait_until(lambda: "input value=Ada" in read_log(log), s, 30),
              f"log={read_log(log)!r}; screen tail={strip(s.output)[-300:]!r}")
        s.send("\t")
        check("Tab focuses button", wait_until(lambda: "button focused" in read_log(log), s, 30),
              f"log={read_log(log)!r}; screen tail={strip(s.output)[-300:]!r}")
        s.send("\r")
        # Each event marker proves one transport stage; screen text is not
        # used because a diff renderer may split changed lines into runs.
        pressed = wait_until(lambda: "pressed name=Ada" in read_log(log), s, 30)
        check("Enter presses button", pressed,
              f"log={read_log(log)!r}; screen tail={strip(s.output)[-300:]!r}")
        print(f"INFO result label text contiguous on the stream: {'Hello, Ada' in strip(s.output)} "
              "(a diff renderer may split it into several cursor-addressed runs)", flush=True)
        s.send("\x1b[D")  # an arrow key escape sequence must not print
        s.read(0.3)
        check("escape sequences are parsed", "[D" not in strip(s.output[-200:]))
        before_resize = s.output.count("\x1b[2J")
        s.resize(40, 12)
        check("resize redraws", wait_until(lambda: s.output.count("\x1b[2J") > before_resize, s))
        s.send("\x03")  # Ctrl+C quits (raw mode: it is a key)
        code = s.wait_exit()
        check("Ctrl+C quits with status 0", code == 0, f"status {code}")
        teardown_modes(s)


def read_log(path):
    try:
        with open(path, encoding="utf-8") as f:
            return f.read().splitlines()
    except FileNotFoundError:
        return []


KEY_CASES = [
    ("enter", "\r"), ("tab", "\t"), ("shift+tab", "\x1b[Z"), ("escape", "\x1b"),
    ("up", "\x1b[A"), ("down", "\x1b[B"), ("right", "\x1b[C"), ("left", "\x1b[D"),
    ("home", "\x1b[H"), ("end", "\x1b[F"), ("home", "\x1b[1~"), ("end", "\x1b[4~"),
    ("delete", "\x1b[3~"), ("backspace", "\x7f"), ("pageup", "\x1b[5~"),
    ("pagedown", "\x1b[6~"), ("f1", "\x1bOP"), ("f5", "\x1b[15~"), ("f12", "\x1b[24~"),
    ("alt+x", "\x1bx"), ("ctrl+left", "\x1b[1;5D"), ("shift+up", "\x1b[1;2A"),
    ("ctrl+a", "\x01"),
]


def wait_until(predicate, session, seconds=3):
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if predicate():
            return True
        session.read(0.05)
        if session.eof or session.exited():
            break
    return predicate()


def keys_session():
    """Exact delivery, synchronized on events rather than timer/log noise."""
    with tempfile.TemporaryDirectory(prefix="termr-pty-") as directory:
        log = os.path.join(directory, "events.log")
        os.environ["TERMR_PTY_LOG"] = log
        s = Session(os.path.join(HERE, "keys-app.R"))
        check("keys app starts", wait_until(lambda: "ready" in read_log(log), s, 60))
        check("controlling terminal verified", "__PTY_CTTY_OK__" in s.output)
        attrs = s.attrs()
        check("ready marker follows raw mode", not (attrs[3] & (termios.ICANON | termios.ECHO)))
        s.wait_for("keys app")
        check("worker stdin detached",
              wait_until(lambda: "worker stdin is a TTY: FALSE" in read_log(log), s, 30),
              repr(read_log(log)))
        for name, mode in (("alternate screen", 1049), ("bracketed paste", 2004),
                           ("mouse reporting", 1000), ("SGR mouse", 1006)):
            check(name + " enabled for capable TERM", f"\x1b[?{mode}h" in s.output)

        def send_expect(name, parts, predicate):
            mark = len(read_log(log))
            for part in parts:
                s.send(part)
            observed = lambda: read_log(log)[mark:]
            check(name, wait_until(lambda: predicate(observed()), s), repr(observed()))

        send_expect("ASCII, accented and emoji characters", ["aé😀"],
                    lambda ev: all(line in ev for line in ("key a [a]", "key é [é]", "key 😀 [😀]")))
        for name, seq in KEY_CASES:
            send_expect("key " + name, [seq],
                        lambda ev, name=name: any(e.split(" [", 1)[0] == "key " + name for e in ev))

        send_expect("mouse press/release/wheel",
                    ["\x1b[<0;5;1M\x1b[<0;5;1m\x1b[<64;5;1M"],
                    lambda ev: all(line in ev for line in (
                        "mouse mouse.down left 5,1", "mouse mouse.up left 5,1",
                        "mouse mouse.scroll none 5,1 up")))
        send_expect("mouse drag press/move/release",
                    ["\x1b[<0;5;2M\x1b[<32;6;2M\x1b[<32;8;3M\x1b[<0;8;3m"],
                    lambda ev: all(line in ev for line in (
                        "mouse mouse.down left 5,2", "mouse mouse.move left 6,2",
                        "mouse mouse.move left 8,3", "mouse mouse.up left 8,3",
                        "mouse drag.start left 6,2", "mouse drag.end left 8,3")))
        # Observe these sizes separately; a storm can legitimately coalesce
        # intermediate sizes before the driver's next size poll.
        for cols, rows in ((1, 1), (2048, 40)):
            mark = len(read_log(log))
            s.resize(cols, rows)
            check(f"terminal size {cols}x{rows}",
                  wait_until(lambda: f"resize {cols}x{rows}" in read_log(log)[mark:], s),
                  repr(read_log(log)[mark:]))
        mark = len(read_log(log))
        for cols, rows in [(1, 1), (2048, 40), (2, 1), (120, 30), (1, 1), (50, 15)]:
            s.resize(cols, rows)
        check("resize storm reaches final size",
              wait_until(lambda: "resize 50x15" in read_log(log)[mark:], s),
              repr(read_log(log)[mark:]))
        mark = len(read_log(log))
        s.resize(60, 16)
        check("resize event", wait_until(lambda: "resize 60x16" in read_log(log)[mark:], s))
        check("timers fire", wait_until(lambda: "tick 1" in read_log(log), s))
        os.kill(s.pid, signal.SIGINT)
        code = s.wait_exit()
        check("SIGINT exits cleanly", code == 0, f"status {code}")
        teardown_modes(s)
        s.close()
        Session.active.remove(s)
def fragments_session():
    """Fragmented input, with the escape timeout widened for this session only.

    The runtime default (30 ms of silence) is what disambiguates a real Escape
    key, and it stays in force in every other session. Here the window is made
    wide enough that scheduler latency on a loaded CI runner can not cut a
    sequence between two deliberate, well separated writes; each fragment is a
    separate OS write and (with the pause between them) a separate read.
    """
    with tempfile.TemporaryDirectory(prefix="termr-pty-frag-") as directory:
        log = os.path.join(directory, "events.log")
        os.environ["TERMR_PTY_LOG"] = log
        s = Session(os.path.join(HERE, "keys-app.R"), extra_env={"TERMR_ESC_TIMEOUT_MS": "2500"})
        check("fragments app starts", wait_until(lambda: "ready" in read_log(log), s, 60))
        s.wait_for("keys app")
        for name, chunks, expected in (
            ("escape sequence split across reads", [b"\x1b", b"[1;", b"5C"], "key ctrl+right"),
            ("UTF-8 split across reads", [bytes([b]) for b in "€".encode()], "key € [€]"),
            ("bracketed paste fragmented", [b"\x1b[200~first line\nsec", "ond é\x1b[201~".encode()],
             "paste first line<LF>second é"),
        ):
            mark = len(read_log(log))
            for chunk in chunks:
                s.send(chunk)
                s.read(0.15)  # far below the 2.5 s window, far above one read cycle
            check(name, wait_until(lambda: expected in read_log(log)[mark:], s),
                  repr(read_log(log)[mark:]))
            events = read_log(log)[mark:]
            check(name + ": no stray Escape or bracket keys",
                  not any(e.startswith(("key escape", "key [", "key 1", "key ;")) for e in events), repr(events))
        # A lone Escape is still flushed as the Escape key once the window passes.
        mark = len(read_log(log))
        s.send(b"\x1b")
        check("lone Escape is flushed as the Escape key after the timeout",
              wait_until(lambda: "key escape" in read_log(log)[mark:], s, 8), repr(read_log(log)[mark:]))
        s.send("\x11")
        check("fragments session exits", s.wait_exit() == 0)
        teardown_modes(s)
        s.close()
        Session.active.remove(s)


def error_session():
    s = Session(os.path.join(HERE, "error-app.R"))
    s.wait_for("press x")
    s.send("x")
    code = s.wait_exit()
    check("an error exits with a non-zero status", code != 0, f"status {code}")
    tail = s.output[s.output.rfind("\x1b[?1049l"):]
    check("the error is printed after the screen is restored", "boom from handler" in tail)
    teardown_modes(s)


def stdin_fallback_session():
    with tempfile.TemporaryDirectory(prefix="termr-pty-stdin-") as directory:
        log = os.path.join(directory, "events.log")
        os.environ["TERMR_PTY_LOG"] = log
        s = Session(os.path.join(HERE, "keys-app.R"), tty_mode="stdin")
        s.wait_for("__PTY_STDIN_ONLY__")
        check("stdin-only PTY has no controlling terminal", "__PTY_STDIN_ONLY__" in s.output)
        check("stdin-only PTY app starts", wait_until(lambda: "ready" in read_log(log), s, 60))
        s.wait_for("keys app")
        s.send("a")
        check("stdin-only PTY key input works", wait_until(lambda: "key a [a]" in read_log(log), s))
        s.resize(50, 15)
        check("stdin-only PTY resize works", wait_until(lambda: "resize 50x15" in read_log(log), s))
        s.send("\x11")
        code = s.wait_exit()
        check("stdin-only PTY normal exit", code == 0, f"status {code}")
        teardown_modes(s)
        s.close()
        Session.active.remove(s)


def degraded_session():
    with tempfile.TemporaryDirectory(prefix="termr-pty-dumb-") as directory:
        log = os.path.join(directory, "events.log")
        os.environ["TERMR_PTY_LOG"] = log
        s = Session(os.path.join(HERE, "keys-app.R"), term="dumb")
        check("TERM=dumb starts", wait_until(lambda: "ready" in read_log(log), s, 60))
        s.send("a")
        check("TERM=dumb input usable", wait_until(lambda: "key a [a]" in read_log(log), s))
        s.send("\x11")
        check("TERM=dumb normal exit", s.wait_exit() == 0)
        for mode in (1049, 2004, 1000, 1006):
            check(f"TERM=dumb omits unsupported mode {mode}", f"\x1b[?{mode}h" not in s.output)
        teardown_modes(s)
        s.close()
        Session.active.remove(s)
def no_tty_session():
    script = os.path.join(HERE, "no-tty-app.R")
    s = Session(script, tty_mode="none")
    s.wait_for("termr needs an interactive terminal", 10)
    code = s.wait_exit()
    output = strip(s.output)
    check("no-TTY session fails with a termr error", code == 42 and "TERM_ERROR:" in output,
          f"status {code}: {output[-300:]!r}")
    check("no-TTY error is not opaque ENXIO", "system error 6" not in output and "No such device" not in output)


PENDIN = 0x20000000
pendin_control = {False: False, True: False}


def stty_settings(output):
    # `stty -g` prints colon-separated hex fields on Linux and
    # "gfmt1:name=hex:..." on macOS/BSD.
    return re.findall(r"^(gfmt1:\S+|[0-9a-f]+(?::[0-9a-f]+){8,})\s*$", strip(output), re.M)


def without_pendin(settings):
    if not settings.startswith("gfmt1:"):
        return settings
    return re.sub(r"(?<=:lflag=)[0-9a-fA-F]+",
                  lambda match: format(int(match.group(), 16) & ~PENDIN, "x"), settings, count=1)


def added_pendin(before, after):
    if platform.system() != "Darwin" or without_pendin(before) != without_pendin(after):
        return False
    flags = [re.search(r":lflag=([0-9a-fA-F]+)", x) for x in (before, after)]
    return (all(flags) and not (int(flags[0].group(1), 16) & PENDIN)
            and bool(int(flags[1].group(1), 16) & PENDIN))


def control_termios_session(stdin_only=False):
    """No termr/R process: test whether the PTY+stty lifecycle adds PENDIN."""
    cmd = 'saved=$(stty -g); printf "%s\\n" "$saved"; stty raw -echo; stty "$saved"; printf "\\n"; stty -g'
    s = Session(tty_mode="stdin" if stdin_only else "controlling", command=["sh", "-c", cmd])
    status = s.wait_exit()
    settings = stty_settings(s.output)
    check(f"no-termr stty control exits (stdin_only={stdin_only})", status == 0, f"status={status}; output={strip(s.output)[-400:]!r}")
    check(f"no-termr stty control snapshots (stdin_only={stdin_only})", len(settings) == 2, repr(settings))
    if len(settings) != 2:
        return
    before, after = settings
    pendin_control[stdin_only] = added_pendin(before, after)
    check(f"no-termr stty control changes only PENDIN (stdin_only={stdin_only})",
          before == after or pendin_control[stdin_only], repr(settings))
    print(f"INFO no-termr stty control stdin_only={stdin_only}: "
          f"{'PENDIN added' if pendin_control[stdin_only] else 'exact match' if before == after else 'other difference'}", flush=True)


def termios_restored(stdin_only=False, term="xterm-256color", handler_error=False):
    script = os.path.join(HERE, "error-app.R") if handler_error else os.path.join(ROOT, "inst", "examples", "hello.R")
    cmd = (f"stty -g; {shlex.quote(RSCRIPT)} {shlex.quote(script)}; app_status=$?; "
           "echo; stty -g; exit $app_status")
    s = Session(tty_mode="stdin" if stdin_only else "controlling", term=term,
                command=["sh", "-c", cmd])
    s.wait_for("press x" if handler_error else "termr demo", 60)
    s.send("x" if handler_error else "\x03")
    status = s.wait_exit()
    settings = stty_settings(s.output)
    matched = len(settings) >= 2 and (settings[0] == settings[-1] or
              (pendin_control[stdin_only] and added_pendin(settings[0], settings[-1])))
    check(f"stty restored (stdin_only={stdin_only}, TERM={term}, error={handler_error})",
          matched, f"found {settings}; control_PENDIN={pendin_control[stdin_only]}")
    check("restoration session status", status != 0 if handler_error else status == 0, f"status {status}")
    teardown_modes(s)


def close_sessions(label):
    """Clean up every live session; a cleanup failure is this case's failure
    only and never stops the remaining cases."""
    for session in list(Session.active):
        try:
            for problem in session.close():
                check(f"{label}: cleanup", False, problem)
        except Exception as error:  # noqa: BLE001 - keep later cases alive
            check(f"{label}: cleanup", False, f"{type(error).__name__}: {error}")
    Session.active.clear()


def run_cases(cases):
    for case in cases:
        label = getattr(case, "__name__", "case")
        try:
            case()
        except Exception as error:
            check(label, False, str(error))
        finally:
            close_sessions(label)
    if check.failed:
        print(f"{check.failed} check(s) failed")
        sys.exit(1)
    print("all PTY checks passed")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", choices=("stdin_fallback_session", "no_tty_session"),
                        help="run one failing session in an independent harness process")
    parser.add_argument("--exclude-case", action="append", default=[],
                        choices=("stdin_fallback_session", "no_tty_session"))
    options = parser.parse_args()
    require_current_install()
    cases = [normal_session, keys_session, fragments_session, error_session,
             control_termios_session, lambda: control_termios_session(stdin_only=True),
             termios_restored, stdin_fallback_session, no_tty_session, degraded_session,
             lambda: termios_restored(stdin_only=True),
             lambda: termios_restored(term="dumb"),
             lambda: termios_restored(handler_error=True)]
    if options.case:
        cases = [globals()[options.case]]
    else:
        cases = [case for case in cases if case.__name__ not in options.exclude_case]
    run_cases(cases)
