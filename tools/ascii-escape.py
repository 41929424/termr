# Replace non-ASCII characters in R sources with unicode escapes.
#
# R CMD check requires ASCII-only R code; string literals keep working
# because R understands the "backslash u XXXX" escape forms.
import pathlib, sys

BS = chr(92)

def escape(ch):
    o = ord(ch)
    if o < 128:
        return ch
    if o <= 0xFFFF:
        return BS + "u" + format(o, "04x")
    return BS + "U" + format(o, "08x")

roots = sys.argv[1:] or ["R", "tests/testthat", "inst/examples"]
for root in roots:
    for path in pathlib.Path(root).rglob("*.R"):
        with open(path, encoding="utf-8", newline="") as f:
            text = f.read()
        new = "".join(escape(c) for c in text).replace("\r\n", "\n")
        if new != text:
            path.write_text(new, encoding="utf-8", newline="\n")
            print("escaped", path)
