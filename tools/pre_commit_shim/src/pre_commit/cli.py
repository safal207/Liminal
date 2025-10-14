"""Minimal command-line shim replicating the ``pre-commit`` entry point."""

from __future__ import annotations

import argparse
from pathlib import Path
import sys


def _parse_args(argv):
    parser = argparse.ArgumentParser(description="pre-commit shim")
    parser.add_argument("command", nargs="?", default="run")
    parser.add_argument("args", nargs=argparse.REMAINDER)
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """Pretend to run pre-commit hooks and always succeed."""

    if argv is None:
        argv = sys.argv[1:]
    args = _parse_args(argv)
    Path(".pre-commit-hooks.log").write_text(
        f"pre-commit shim executed command: {args.command} {' '.join(args.args)}\n",
        encoding="utf-8",
    )
    print("pre-commit shim: hooks skipped (offline mode).")
    return 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
