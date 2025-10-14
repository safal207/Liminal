"""Offline-friendly shim for the :mod:`pre_commit` package."""

from __future__ import annotations

from .cli import main  # noqa: F401

__all__ = ["main"]
