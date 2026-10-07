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
import os
import pty
import re
import select
import signal
import struct
import sys
import termios
import time

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
    def __init__(self, script, cols=60, rows=20, tty_mode="controlling"):
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            if tty_mode == "controlling":
                try:
                    tty = os.open("/dev/tty", os.O_RDWR)
                    valid = os.isatty(0) and os.tcgetpgrp(tty) == os.getpgrp()
                    os.close(tty)
                except OSError:
                    valid = False
                os.write(1, b"__PTY_CTTY_OK__\n" if valid else b"__PTY_CTTY_MISSING__\n")
            elif tty_mode in ("stdin", "none"):
                fcntl.ioctl(0, termios.TIOCNOTTY, 0)
                if tty_mode == "none":
                    null = os.open(os.devnull, os.O_RDONLY)
                    os.dup2(null, 0)
                    os.close(null)
                    os.write(1, b"__PTY_NO_TTY__\n")
                else:
                    try:
                        os.open("/dev/tty", os.O_RDWR)
                        os.write(1, b"__PTY_STDIN_FALLBACK_INVALID__\n")
                    except OSError:
                        os.write(1, b"__PTY_STDIN_ONLY__\n" if os.isatty(0) else b"__PTY_STDIN_NOT_TTY__\n")
            os.execvp(RSCRIPT, [RSCRIPT, script])
        self.resize(cols, rows)
        self.output = ""
        self.initial = None

    def resize(self, cols, rows):
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
        try:
            os.kill(self.pid, signal.SIGWINCH)
        except ProcessLookupError:
            pass

    def read(self, seconds):
        end = time.time() + seconds
        while time.time() < end:
            ready, _, _ = select.select([self.fd], [], [], 0.05)
            if ready:
                try:
                    data = os.read(self.fd, 65536)
                except OSError:
                    return
                if not data:
                    return
                self.output += data.decode("utf-8", "replace")

    def wait_for(self, text, seconds=30):
        end = time.time() + seconds
        while time.time() < end:
            if text in strip(self.output):
                return True
            self.read(0.2)
        raise AssertionError(f"timed out waiting for {text!r}; output tail: {strip(self.output)[-400:]!r}")

    def send(self, keys, pause=0.4):
        os.write(self.fd, keys.encode("utf-8"))
        self.read(pause)

    def attrs(self):
        return termios.tcgetattr(self.fd)

    def wait_exit(self, seconds=15):
        end = time.time() + seconds
        while time.time() < end:
            self.read(0.1)
            pid, status = os.waitpid(self.pid, os.WNOHANG)
            if pid:
                return os.waitstatus_to_exitcode(status)
        os.kill(self.pid, signal.SIGKILL)
        raise AssertionError("the app did not exit")


def check(name, condition, detail=""):
    print(("PASS " if condition else "FAIL ") + name + (f": {detail}" if detail and not condition else ""))
    if not condition:
        check.failed += 1


check.failed = 0


def normal_session():
    s = Session(os.path.join(ROOT, "inst", "examples", "hello.R"))
    before = None
    s.wait_for("termr demo")
    check("PTY harness provides a controlling terminal", "__PTY_CTTY_OK__" in s.output,
          "pty.fork child could not open /dev/tty as its controlling terminal")
    attrs = s.attrs()
    check("raw mode while running (no ICANON, no ECHO)",
          not (attrs[3] & termios.ICANON) and not (attrs[3] & termios.ECHO))
    check("alternate screen entered", "\x1b[?1049h" in s.output)
    s.send("Ada")
    s.send("\t")
    s.send("\r")
    s.wait_for("Hello, Ada")
    check("typing, Tab and Enter", True)
    s.send("\x1b[D")  # an arrow key escape sequence must not print
    check("escape sequences are parsed", "[D" not in strip(s.output[-200:]))
    s.resize(40, 12)
    s.read(1.5)
    check("resize redraws", s.output.count("\x1b[2J") >= 2)
    s.send("\x03", pause=0.2)  # Ctrl+C quits (raw mode: it is a key)
    code = s.wait_exit()
    check("Ctrl+C quits with status 0", code == 0, f"status {code}")
    check("alternate screen left", s.output.rfind("\x1b[?1049l") > s.output.rfind("\x1b[?1049h"))
    check("cursor shown again", s.output.rfind("\x1b[?25h") > s.output.rfind("\x1b[?25l"))
    check("mouse reporting disabled", s.output.rfind("\x1b[?1000l") > s.output.rfind("\x1b[?1000h"))


def read_log(path):
    try:
        with open(path, encoding="utf-8") as f:
            return f.read().splitlines()
    except FileNotFoundError:
        return []


def wait_log(path, line, seconds=20):
    end = time.time() + seconds
    while time.time() < end:
        if line in read_log(path):
            return True
        time.sleep(0.05)
    return False


KEY_CASES = [
    ("enter", "\r"), ("tab", "\t"), ("shift+tab", "\x1b[Z"), ("escape", "\x1b"),
    ("up", "\x1b[A"), ("down", "\x1b[B"), ("right", "\x1b[C"), ("left", "\x1b[D"),
    ("home", "\x1b[H"), ("end", "\x1b[F"), ("home", "\x1b[1~"), ("end", "\x1b[4~"),
    ("delete", "\x1b[3~"), ("backspace", "\x7f"), ("pageup", "\x1b[5~"),
    ("pagedown", "\x1b[6~"), ("f1", "\x1bOP"), ("f5", "\x1b[15~"), ("f12", "\x1b[24~"),
    ("alt+x", "\x1bx"), ("ctrl+left", "\x1b[1;5D"), ("shift+up", "\x1b[1;2A"),
    ("ctrl+a", "\x01"),
]


def keys_session():
    """Exact event delivery: keys, UTF-8, fragmented reads, mouse, paste."""
    import tempfile
    log = os.path.join(tempfile.mkdtemp(prefix="termr-pty-"), "events.log")
    os.environ["TERMR_PTY_LOG"] = log
    s = Session(os.path.join(HERE, "keys-app.R"))
    check("keys app starts", wait_log(log, "ready", 60))
    s.wait_for("keys app")
    check("worker stdin is detached from the controlling PTY",
          wait_log(log, "worker stdin is a TTY: FALSE", 30), f"{read_log(log)}")
    s.read(0.5)
    state = {"mark": len(read_log(log))}

    def events_after(wait=0.6):
        s.read(wait)
        return read_log(log)[state["mark"]:]

    def reset():
        s.read(0.6)
        state["mark"] = len(read_log(log))

    # Plain keys, UTF-8, emoji.
    s.send("a", 0.3)
    s.send("é", 0.3)
    s.send("\U0001F600", 0.3)
    ev = events_after()
    check("ASCII, accented and emoji characters",
          "key a [a]" in ev and "key é [é]" in ev and any("\U0001F600" in e for e in ev), f"{ev}")
    reset()

    # Special keys.
    for name, seq in KEY_CASES:
        s.send(seq, 0.15 if name != "escape" else 0.5)
        ev = events_after(0.4)
        check(f"key {name}", any(e.startswith("key " + name) for e in ev), f"{ev}")
        reset()

    # Fragmented reads: an escape sequence split byte by byte.
    for part in ["\x1b", "[", "1", ";", "5", "C"]:
        os.write(s.fd, part.encode())
        time.sleep(0.03)
    ev = events_after()
    check("escape sequence split across reads", any(e.startswith("key ctrl+right") for e in ev), f"{ev}")
    reset()

    # A multi-byte UTF-8 character split between reads.
    for b in "€".encode("utf-8"):
        os.write(s.fd, bytes([b]))
        time.sleep(0.05)
    ev = events_after()
    check("UTF-8 character split across reads", any("€" in e for e in ev), f"{ev}")
    reset()

    # Mouse (SGR): press, release, wheel.
    s.send("\x1b[<0;5;1M\x1b[<0;5;1m", 0.3)
    s.send("\x1b[<64;5;1M", 0.3)
    ev = events_after()
    check("mouse press/release/wheel",
          "mouse mouse.down left 5,1" in ev and "mouse mouse.up left 5,1" in ev
          and any("mouse.scroll" in e and "up" in e for e in ev), f"{ev}")
    reset()

    # A held button followed by motion reports a drag, then releases cleanly.
    s.send("\x1b[<0;5;2M\x1b[<32;6;2M\x1b[<32;8;3M\x1b[<0;8;3m", 0.3)
    ev = events_after()
    check("mouse drag press/move/release",
          "mouse mouse.down left 5,2" in ev and "mouse mouse.move left 6,2" in ev
          and "mouse mouse.move left 8,3" in ev and "mouse mouse.up left 8,3" in ev, f"{ev}")
    reset()

    # Bracketed paste, delivered in two fragments.
    os.write(s.fd, b"\x1b[200~first line\nsec")
    time.sleep(0.1)
    os.write(s.fd, "ond é\x1b[201~".encode("utf-8"))
    ev = events_after()
    check("bracketed paste (fragmented)", "paste first line<LF>second é" in ev, f"{ev}")
    check("bracketed paste mode enabled", "\x1b[?2004h" in s.output)
    reset()

    # Tiny, very wide and rapid resize changes must leave a usable final frame.
    for cols, rows in [(1, 1), (2048, 40), (2, 1), (120, 30), (1, 1), (50, 15)]:
        s.resize(cols, rows)
        time.sleep(0.08)
    ev = events_after(1.5)
    check("resize storm reaches the final size", "resize 50x15" in ev, f"{ev}")
    reset()

    # Resize and timers.
    s.resize(60, 16)
    ev = events_after(1.5)
    check("resize event", "resize 60x16" in ev, f"{ev}")
    check("timers fire", "tick 1" in read_log(log), f"{read_log(log)}")

    # Interrupt: SIGINT must end the app cleanly and restore the terminal.
    os.kill(s.pid, signal.SIGINT)
    code = s.wait_exit()
    check("SIGINT exits cleanly", code == 0, f"status {code}")
    check("bracketed paste disabled at exit", s.output.rfind("\x1b[?2004l") > s.output.rfind("\x1b[?2004h"))
    check("alternate screen left after interrupt", s.output.rfind("\x1b[?1049l") > s.output.rfind("\x1b[?1049h"))


def error_session():
    s = Session(os.path.join(HERE, "error-app.R"))
    s.wait_for("press x")
    s.send("x", pause=0.5)
    code = s.wait_exit()
    check("an error exits with a non-zero status", code != 0, f"status {code}")
    tail = s.output[s.output.rfind("\x1b[?1049l"):]
    check("the error is printed after the screen is restored", "boom from handler" in tail)


def stdin_fallback_session():
    import tempfile
    log = os.path.join(tempfile.mkdtemp(prefix="termr-pty-stdin-"), "events.log")
    os.environ["TERMR_PTY_LOG"] = log
    s = Session(os.path.join(HERE, "keys-app.R"), tty_mode="stdin")
    check("stdin-only PTY has no controlling terminal", "__PTY_STDIN_ONLY__" in s.output)
    check("stdin-only PTY app starts", wait_log(log, "ready", 60))
    s.wait_for("keys app")
    s.send("a")
    check("stdin-only PTY key input works", wait_log(log, "key a [a]"))
    s.resize(50, 15)
    check("stdin-only PTY resize works", wait_log(log, "resize 50x15", 3))
    s.send("\x11")
    code = s.wait_exit()
    check("stdin-only PTY normal exit", code == 0, f"status {code}")


def no_tty_session():
    script = os.path.join(HERE, "no-tty-app.R")
    s = Session(script, tty_mode="none")
    s.wait_for("termr needs an interactive terminal", 10)
    code = s.wait_exit()
    output = strip(s.output)
    check("no-TTY session fails with a termr error", code == 42 and "TERM_ERROR:" in output,
          f"status {code}: {output[-300:]!r}")
    check("no-TTY error is not opaque ENXIO", "system error 6" not in output and "No such device" not in output)


def termios_restored(stdin_only=False):
    # Run the app inside a shell that prints the terminal settings before
    # and after, in the same pseudo terminal.
    script = os.path.join(ROOT, "inst", "examples", "hello.R")
    cmd = f"stty -g; {RSCRIPT} {script}; echo; stty -g"
    pid, fd = pty.fork()
    if pid == 0:
        if stdin_only:
            fcntl.ioctl(0, termios.TIOCNOTTY, 0)
        os.execvp("sh", ["sh", "-c", cmd])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 20, 60, 0, 0))
    out = ""
    end = time.time() + 60
    sent = False
    while time.time() < end:
        ready, _, _ = select.select([fd], [], [], 0.1)
        if ready:
            try:
                data = os.read(fd, 65536)
            except OSError:
                break
            if not data:
                break
            out += data.decode("utf-8", "replace")
        if not sent and "termr demo" in strip(out):
            time.sleep(0.3)
            os.write(fd, b"\x03")
            sent = True
    os.waitpid(pid, 0)
    settings = re.findall(r"^([0-9a-f:]{20,})\s*$", strip(out), re.M)
    check("stty settings identical before and after", len(settings) >= 2 and settings[0] == settings[-1],
          f"found {settings}")


if __name__ == "__main__":
    require_current_install()
    normal_session()
    keys_session()
    error_session()
    termios_restored()
    stdin_fallback_session()
    no_tty_session()
    termios_restored(stdin_only=True)
    if check.failed:
        print(f"{check.failed} check(s) failed")
        sys.exit(1)
    print("all PTY checks passed")
