from __future__ import annotations
import os
from typing import Any

class _NoDatomic(Exception):
    pass

def _pydatomic_connect():
    try:
        from pydatomic.datomic import Datomic  # type: ignore
    except Exception as e:
        raise _NoDatomic(
            "Datomic client not installed. Install 'pydatomic' or provide service."
        ) from e
    base = os.getenv("DATOMIC_BASE_URL", "http://localhost:3000/")
    alias = os.getenv("DATOMIC_DB_ALIAS", "mem/test")
    return Datomic(base, alias)

def connect():
    """Возвращает минимальный клиент для операций, используемых в коде/тестах."""
    return _pydatomic_connect()
