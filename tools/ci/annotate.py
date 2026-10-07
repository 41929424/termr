#!/usr/bin/env python3
"""Print the tail of a file as one GitHub error annotation.

Step logs of a public repository need a login to download, annotations do
not: this makes the interesting part of a failed probe readable from the run
summary and the API.

    python3 tools/ci/annotate.py <title> <file> [max_chars]
"""
import os
import sys

title, path = sys.argv[1], sys.argv[2]
limit = int(sys.argv[3]) if len(sys.argv) > 3 else 6000
try:
    with open(path, encoding="utf-8", errors="replace") as handle:
        text = handle.read()[-limit:]
except OSError as error:
    text = f"cannot read {path}: {error}"
text = text.replace("%", "%25").replace(chr(13), "").replace(chr(10), "%0A")
print(f"::error title={title}::{text}")
