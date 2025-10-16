"""Compatibility layer for the legacy :mod:`backend.api` module.

Historically this module hosted the FastAPI application instance together
with a large amount of bootstrap code.  After the backend refactor the
canonical app lives in :mod:`backend.app.main`, but a significant portion
of the ecosystem – including older tests and CLI utilities – still imports
``app`` from ``backend.api``.  Rather than updating every call site, we keep
this thin wrapper to provide a stable facade.

The wrapper simply re-exports the modern application object and its
lifespan context while deliberately avoiding any side effects during
import (such as debug prints or Prometheus registration).  This prevents
duplicate metric registration during pytest collection and guarantees that
service initialisation flows through the unified dependency layer in
``backend.app``.
"""
from __future__ import annotations

from backend.app.main import app, lifespan

__all__ = ["app", "lifespan"]
