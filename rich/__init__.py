"""Minimal stub for the :mod:`rich` package used by diagnostics tests.

Only the small subset of the API that the tests exercise is implemented
here so that the suite can run in environments where the real ``rich``
package is not available.  The goal is not to be feature complete but to
provide a tiny, well-behaved facade that mirrors the public signatures
we rely on.
"""
from __future__ import annotations

from typing import Any


def print(*objects: Any, sep: str = " ", end: str = "\n") -> None:
    """Drop-in proxy for :func:`rich.print`.

    We delegate to the built-in :func:`print` which keeps behaviour simple
    and predictable while satisfying the import expectations of the
    diagnostics module.
    """

    __builtins__["print"](*objects, sep=sep, end=end)


from .console import Console  # noqa: E402 - imported lazily to avoid cycles
from .panel import Panel  # noqa: E402
from .table import Table  # noqa: E402

__all__ = ["print", "Console", "Panel", "Table"]
