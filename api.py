"""Compatibility wrapper exposing the FastAPI application at the legacy module path.

Historically the project exposed ``app`` from a top-level ``api`` module.  The
backend refactor moved the real application into :mod:`backend.app.main` and
added :mod:`backend.api` as the canonical import location.  Some third-party
integrations and ad-hoc scripts, however, still ``import api`` directly.

To avoid breaking those environments we keep this tiny facade that simply
re-exports the modern application object and lifespan context.  Importing this
module has no side effects beyond importing :mod:`backend.api`.
"""
from __future__ import annotations

from backend.api import app, lifespan

__all__ = ["app", "lifespan"]
