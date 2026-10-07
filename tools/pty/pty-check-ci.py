#!/usr/bin/env python3
"""Run Unix PTY checks, including TTY-detachment and restoration cases."""
import fcntl
import importlib.util
import os
import pty
import re
import select
import shlex
import signal
import struct
import sys
import termios
import time

HERE = os.path.dirname(os.path.abspath(__file__))
CHECK_PATH = os.path.join(HERE, "pty-check.py")
spec = importlib.util.spec_from_file_location("termr_pty_check", CHECK_PATH)
checks = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checks)

# TIOCNOTTY sends SIGHUP when a session leader detaches. Ignore it in this
# harness process so the deliberate stdin-only test can exec the R child.
signal.signal(signal.SIGHUP, signal.SIG_IGN)


def error_terminal_restoration():
    script = os.path.join(HERE, "error-app.R")
    cmd = f"stty -g; {shlex.quote(checks.RSCRIPT)} {shlex.quote(script)}; echo; stty -g"
    pid, fd = pty.fork()
    if pid == 0:
        os.execvp("sh", ["sh", "-c", cmd])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 10, 60, 0, 0))
    output = ""
    deadline = time.time() + 45
    sent = False
    while time.time() < deadline:
        ready, _, _ = select.select([fd], [], [], 0.1)
        if ready:
            try:
                chunk = os.read(fd, 65536)
            except OSError:
                break
            if not chunk:
                break
            output += chunk.decode("utf-8", "replace")
        if not sent and "press x" in checks.strip(output):
            os.write(fd, b"x")
            sent = True
    finished, status = os.waitpid(pid, os.WNOHANG)
    if finished == 0:
        os.kill(pid, signal.SIGKILL)
        _, status = os.waitpid(pid, 0)
    lines = re.findall(r"^([0-9a-f:]{20,})\s*$", checks.strip(output), re.M)
    checks.check(
        "stty settings restored after handler error",
        sent and os.waitstatus_to_exitcode(status) != 0 and len(lines) >= 2 and lines[0] == lines[-1],
        f"status {os.waitstatus_to_exitcode(status)}; settings {lines}",
    )


checks.normal_session()
checks.keys_session()
checks.error_session()
checks.termios_restored()
checks.stdin_fallback_session()
checks.no_tty_session()
checks.termios_restored(stdin_only=True)
error_terminal_restoration()
if checks.check.failed:
    sys.exit(1)
print("all PTY checks passed")
