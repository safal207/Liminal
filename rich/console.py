"""Simplified :mod:`rich.console` implementation for testing purposes."""
from __future__ import annotations

from typing import Any


class Console:
    """Very small stand-in for :class:`rich.console.Console`.

    The diagnostic tests only rely on ``print`` and ``clear`` so we keep the
    implementation intentionally tiny.
    """

    def __init__(self, *, record: bool = False) -> None:  # pragma: no cover - trivial
        self._record = record
        self._buffer: list[str] = []

    def print(self, *objects: Any, sep: str = " ", end: str = "\n") -> None:
        message = sep.join(str(obj) for obj in objects)
        if self._record:
            self._buffer.append(message)
        __builtins__["print"](message, end=end)

    def clear(self) -> None:  # pragma: no cover - formatting helper
        if self._record:
            self._buffer.clear()
        __builtins__["print"]("", end="")

    def export_text(self) -> str:
        return "\n".join(self._buffer)
