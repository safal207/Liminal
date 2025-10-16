"""Tiny :mod:`rich.panel` stand-in used for diagnostic output."""
from __future__ import annotations

from dataclasses import dataclass
from typing import Any


@dataclass
class Panel:
    renderable: Any
    border_style: str | None = None

    @classmethod
    def fit(cls, renderable: Any, border_style: str | None = None) -> "Panel":
        return cls(renderable=renderable, border_style=border_style)

    def __str__(self) -> str:  # pragma: no cover - trivial formatting glue
        return str(self.renderable)
