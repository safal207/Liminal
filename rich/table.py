"""Small :mod:`rich.table` shim for human-friendly diagnostics."""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, List


@dataclass
class _Column:
    header: str
    style: str | None = None


@dataclass
class Table:
    title: str | None = None
    show_header: bool = False
    header_style: str | None = None
    _columns: List[_Column] = field(default_factory=list)
    _rows: List[List[str]] = field(default_factory=list)

    def add_column(self, header: str, style: str | None = None) -> None:
        self._columns.append(_Column(header=header, style=style))

    def add_row(self, *cells: Any) -> None:
        self._rows.append([str(cell) for cell in cells])

    def __str__(self) -> str:  # pragma: no cover - formatting helper
        lines: list[str] = []
        if self.title:
            lines.append(self.title)
        if self.show_header and self._columns:
            lines.append(" | ".join(column.header for column in self._columns))
        for row in self._rows:
            lines.append(" | ".join(row))
        return "\n".join(lines)
