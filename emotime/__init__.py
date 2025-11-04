"""Compatibility shim exposing ``backend.emotime`` at the top-level package.

This module makes the rich backend Emotime implementation importable both via
``backend.emotime`` (original location) and the legacy ``emotime`` namespace
that several tests and helper scripts expect.  It keeps the backend package as
source of truth while still allowing local utilities within ``emotime/`` to be
imported.
"""

from importlib import import_module
import os
from types import ModuleType
from typing import Any, Iterable

_backend_pkg: ModuleType = import_module("backend.emotime")

# Ensure submodule discovery works for both the backend implementation and the
# legacy helpers that live alongside this shim.
__path__ = [
    os.path.dirname(__file__),
    *getattr(_backend_pkg, "__path__", []),  # type: ignore[arg-type]
]

# Re-export the public API from the backend package.
__all__ = getattr(_backend_pkg, "__all__", [])


def __getattr__(name: str) -> Any:
    """Delegate attribute access to the backend implementation when possible."""

    if hasattr(_backend_pkg, name):
        return getattr(_backend_pkg, name)
    # Fall back to legacy helpers (e.g. emotime.api_simple).
    return import_module(f"emotime.{name}")


def __dir__() -> Iterable[str]:
    """Combine attributes from the shim and backend package for introspection."""

    return sorted(set(globals().keys()) | set(dir(_backend_pkg)))
