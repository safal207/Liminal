"""Command line entry point for the offline flake8 shim."""

from __future__ import annotations

import argparse
import ast
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Sequence, Set
import sys

_DEFAULT_EXCLUDES: Set[str] = {
    ".git",
    "__pycache__",
    "node_modules",
    "dist",
    "build",
    "venv",
    ".venv",
    "playwright-report",
}


@dataclass
class LintIssue:
    """Represents a lint finding produced by the shim."""

    path: Path
    line: int
    column: int
    message: str

    def format(self, root: Path) -> str:
        relative = self.path.relative_to(root)
        return f"{relative}:{self.line}:{self.column}: {self.message}"


def _iter_python_files(paths: Sequence[str], excludes: Set[str]) -> Iterable[Path]:
    if not paths:
        paths = ["."]
    for entry in paths:
        base = Path(entry).resolve()
        if base.is_file() and base.suffix == ".py":
            yield base
        elif base.is_dir():
            for candidate in base.rglob("*.py"):
                if any(part in excludes for part in candidate.parts):
                    continue
                yield candidate


def _check_syntax(path: Path) -> List[LintIssue]:
    try:
        source = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        return [
            LintIssue(path=path, line=1, column=0, message="unable to decode file as UTF-8"),
        ]
    try:
        ast.parse(source, filename=str(path))
    except SyntaxError as exc:  # pragma: no cover - executed in error scenarios
        line = exc.lineno or 1
        column = exc.offset or 0
        return [
            LintIssue(path=path, line=line, column=column, message=f"syntax error: {exc.msg}"),
        ]
    return []


def _check_trailing_whitespace(path: Path) -> List[LintIssue]:
    issues: List[LintIssue] = []
    for index, line in enumerate(path.read_text(encoding="utf-8", errors="ignore").splitlines(), start=1):
        stripped = line.rstrip(" \t")
        if stripped != line:
            column = len(stripped)
            issues.append(
                LintIssue(
                    path=path,
                    line=index,
                    column=column,
                    message="trailing whitespace",
                )
            )
    return issues


def _collect_issues(paths: Sequence[str], excludes: Set[str]) -> List[LintIssue]:
    issues: List[LintIssue] = []
    for file_path in _iter_python_files(paths, excludes):
        issues.extend(_check_syntax(file_path))
        issues.extend(_check_trailing_whitespace(file_path))
    return sorted(issues, key=lambda issue: (str(issue.path), issue.line, issue.column))


def _parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Minimal offline substitute for flake8")
    parser.add_argument(
        "paths",
        nargs="*",
        help="Files or directories to lint (default: current working directory)",
    )
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Additional directory names to exclude",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    """Run the shim and return an exit status compatible with flake8."""

    if argv is None:
        argv = sys.argv[1:]
    args = _parse_args(argv)
    excludes = set(_DEFAULT_EXCLUDES)
    excludes.update(args.exclude)

    root = Path.cwd()
    issues = _collect_issues(args.paths, excludes)
    for issue in issues:
        print(issue.format(root))

    if issues:
        print(f"\n{len(issues)} issue(s) detected.")
        return 1

    print("No issues detected by flake8 shim.")
    return 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
