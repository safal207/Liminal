#!/usr/bin/env python3
"""Utility for detecting unresolved merge conflicts in the repository.

The script scans all tracked files for the standard Git conflict markers
(`<<<<<<<`, `=======`, `>>>>>>>`). If any markers are found, it prints the
file path together with the offending line number and exits with a non-zero
status code so that CI pipelines can fail fast.

Usage:
    python tools/check_merge_conflicts.py

The command must be executed from anywhere inside the repository tree. It
relies on ``git ls-files`` to obtain the list of tracked files, ensuring that
ignored directories such as ``node_modules`` are skipped automatically.
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path
from typing import Iterable, List, Optional, Tuple


ConflictMatch = Tuple[str, int, str]


def _iter_tracked_files(repo_root: Path) -> Iterable[Path]:
    """Yield paths to files tracked by Git within ``repo_root``."""

    git_proc = subprocess.run(
        ["git", "ls-files", "-z"],
        cwd=repo_root,
        check=True,
        capture_output=True,
    )
    for relative_path in git_proc.stdout.decode().split("\0"):
        if not relative_path:
            continue
        yield repo_root / relative_path


def _looks_like_conflict_marker(text: str, marker: str) -> bool:
    stripped = text.rstrip("\n")
    if marker == "=======":
        return stripped == marker

    if stripped.startswith(marker):
        trailing = stripped[len(marker) : len(marker) + 1]
        return trailing == "" or trailing in {" ", "\t"}

    return False


def _scan_file(path: Path, repo_root: Path, markers: Iterable[str]) -> Iterable[ConflictMatch]:
    """Scan ``path`` for merge conflict markers."""

    try:
        with path.open("r", encoding="utf-8", errors="ignore") as handle:
            for line_number, line in enumerate(handle, start=1):
                for marker in markers:
                    if _looks_like_conflict_marker(line, marker):
                        relative = path.relative_to(repo_root)
                        yield (str(relative), line_number, line.rstrip())
                        break
    except OSError as exc:
        raise RuntimeError(f"Failed to read {path}: {exc}") from exc


def find_conflicts(repo_root: Path) -> List[ConflictMatch]:
    """Return a list of conflict matches in tracked files under ``repo_root``."""

    markers = ("<<<<<<<", "=======", ">>>>>>>")
    conflicts: List[ConflictMatch] = []
    for file_path in _iter_tracked_files(repo_root):
        conflicts.extend(_scan_file(file_path, repo_root, markers))
    return conflicts


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=None,
        help="Explicit path to the repository root (auto-detected by default).",
    )
    return parser.parse_args()


def _detect_repo_root(explicit_root: Optional[Path]) -> Path:
    if explicit_root is not None:
        return explicit_root.resolve()

    git_proc = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        check=True,
        capture_output=True,
        text=True,
    )
    return Path(git_proc.stdout.strip()).resolve()


def main() -> int:
    args = parse_args()
    repo_root = _detect_repo_root(args.repo_root)

    conflicts = find_conflicts(repo_root)
    if conflicts:
        print("Unresolved merge conflicts detected:")
        for file_path, line_number, line in conflicts:
            print(f"  {file_path}:{line_number}: {line}")
        return 1

    print("No unresolved merge conflicts found.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
