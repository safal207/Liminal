"""Minimal stub for :mod:`rich` used by ``tests/diagnostic_test.py`` only.

Named ``diagnostic_rich_stub`` so it does not shadow the real PyPI ``rich``
package on ``sys.path`` (Prefect and other tools require ``rich.traceback``).
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
