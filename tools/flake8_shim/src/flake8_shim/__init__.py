"""Minimal offline shim for the :mod:`flake8` command."""

from __future__ import annotations

__all__ = ["main"]

from .cli import main  # noqa: F401  (re-export for "python -m flake8_shim")
