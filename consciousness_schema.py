"""Compatibility shim for ``backend.consciousness_schema``.

The canonical implementation of the consciousness schema lives inside the
``backend`` package. This module re-exports *all* public symbols so that legacy
imports like ``from consciousness_schema import ...`` continue to function
after the backend package refactor.
"""

from backend.consciousness_schema import *  # noqa: F401,F403

# Re-export the backend module's ``__all__`` if it defines one. This keeps tools
# like ``pydoc`` consistent while remaining resilient when ``__all__`` is absent.
try:  # pragma: no cover - executed at import time
    from backend import consciousness_schema as _backend_schema

    __all__ = getattr(_backend_schema, "__all__", []) or [
        name
        for name in globals()
        if not name.startswith("_") and name not in {"_backend_schema"}
    ]
except Exception:  # pragma: no cover - fallback when backend import fails
    __all__ = [name for name in globals() if not name.startswith("_")]
