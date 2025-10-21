"""Legacy compatibility facade for Prometheus metrics utilities.

Older modules import symbols from a top-level :mod:`metrics` package.  The
refactored backend consolidated everything under :mod:`backend.metrics`.  To
keep those historical imports functioning we expose the same public surface by
re-exporting all names from :mod:`backend.metrics` while avoiding duplicate
registration side effects.
"""
from __future__ import annotations

from backend.metrics import *  # noqa: F401,F403 - re-exported API
from backend.metrics import __all__ as _backend_all

__all__ = list(_backend_all)
