#!/usr/bin/env python3
"""Run the same bounded Unix PTY cases locally and in CI."""
import os
import runpy

runpy.run_path(os.path.join(os.path.dirname(__file__), "pty-check.py"), run_name="__main__")
