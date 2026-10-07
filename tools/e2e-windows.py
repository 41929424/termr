"""End-to-end test of termr apps in a real Windows pseudo console (ConPTY).

Runs an R script in a ConPTY, sends keystrokes as a terminal would, and
prints what a VT100 emulator (pyte) shows. Windows only.

    python -m venv .venv && .venv/Scripts/pip install pywinpty pyte
    .venv/Scripts/python tools/e2e-windows.py tools/e2e/hello.json

Scenario file:
    {"rscript": "inst/examples/hello.R", "cols": 60, "rows": 16,
     "steps": [["wait", 6.0], ["keys", "Ada"], ["snap", "label"],
               ["resize", 70, 20], ["keys", ""]]}

Paths are relative to the repository root. Set RSCRIPT to choose the R
installation. Note: pyte does not emulate the alternate screen, so the
final snapshot shows the app screen with the shell output on top.
"""
import json, pathlib, sys, threading, time
sys.stdout.reconfigure(encoding="utf-8")
from winpty import PtyProcess
import pyte

spec = json.load(open(sys.argv[1], encoding="utf-8"))
cols, rows = spec.get("cols", 60), spec.get("rows", 16)
screen = pyte.Screen(cols, rows)
stream = pyte.Stream(screen)
raw = []
T0 = time.time()
chunks = []
lock = threading.Lock()
import os, glob
ROOT = pathlib.Path(__file__).resolve().parent.parent
RSCRIPT = os.environ.get("RSCRIPT") or (sorted(glob.glob("C:/Program Files/R/R-*/bin/Rscript.exe")) or ["Rscript"])[-1]
cmd = spec.get("cmd") or [RSCRIPT] + spec.get("rargs", []) + [str(ROOT / spec["rscript"])]

# The scenarios run `library(termr)`: refuse to test an installed termr whose
# version differs from this checkout (e.g. an old release in the user library).
import subprocess
wanted = next(line.split(":", 1)[1].strip()
              for line in (ROOT / "DESCRIPTION").read_text(encoding="utf-8").splitlines()
              if line.startswith("Version:"))
found = subprocess.run([RSCRIPT, "-e", "cat(as.character(packageVersion('termr')))"],
                       capture_output=True, text=True).stdout.strip()
if found != wanted:
    sys.exit(f"Installed termr is {found or 'missing'}, this checkout is {wanted}. "
             "Install the checkout first (R CMD INSTALL .) or set R_LIBS.")
proc = PtyProcess.spawn(cmd, dimensions=(rows, cols))

def reader():
    while True:
        try:
            data = proc.read(65536)
        except Exception:
            break
        if not data:
            if not proc.isalive():
                break
            continue
        with lock:
            raw.append(data)
            chunks.append((round(time.time()-T0, 2), len(data)))
            stream.feed(data)

t = threading.Thread(target=reader, daemon=True)
t.start()

def snap(label):
    with lock:
        print(f"===== {label} (cursor visible={not screen.cursor.hidden}) =====")
        for line in screen.display:
            print("|" + line + "|")
        sys.stdout.flush()

for number, step in enumerate(spec["steps"], start=1):
    kind = step[0]
    if kind in ("keys", "resize") and not proc.isalive():
        # Report an early exit with its status instead of dying in write().
        print(f"EXITED_EARLY: before step {number} {step!r}, "
              f"{time.time() - T0:.2f}s after start, exit status {proc.exitstatus}")
        break
    if kind == "wait":
        time.sleep(step[1])
    elif kind == "keys":
        proc.write(step[1])
        time.sleep(step[2] if len(step) > 2 else 0.3)
    elif kind == "snap":
        snap(step[1])
    elif kind == "resize":
        proc.setwinsize(step[2], step[1])
        with lock:
            screen.resize(step[2], step[1])
        time.sleep(1.0)
    elif kind == "attrs":
        with lock:
            y, x = step[1], step[2]
            c = screen.buffer[y][x]
            print(f"cell({x},{y}) = {c}")

deadline = time.time() + spec.get("exit_timeout", 5)
while proc.isalive() and time.time() < deadline:
    time.sleep(0.1)
alive = proc.isalive()
if alive:
    proc.terminate(force=True)
time.sleep(0.3)
snap("final")
total = "".join(raw)
print("CHUNKS(first 5, last 2):", chunks[:5], chunks[-2:], "n=", len(chunks))
print("ALIVE_AT_END:", alive, "| exit status:", proc.exitstatus)
print("BYTES:", len(total.encode("utf-8")))
if spec.get("dump_tail"):
    print("TAIL:", repr(total[-spec["dump_tail"]:]))
