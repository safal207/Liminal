#!/usr/bin/env python3
from __future__ import annotations

import os
import sys
from typing import Optional

# Ensure `src` is on sys.path for local execution
BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
SRC_DIR = os.path.join(BASE_DIR, "src")
if SRC_DIR not in sys.path:
    sys.path.insert(0, SRC_DIR)

from liminal.graphql_schema import graphql_app  # type: ignore  # noqa: E402


def main(host: str = "127.0.0.1", port: int = 8000, reload: bool = False) -> None:
    if graphql_app is None:
        raise RuntimeError(
            "GraphQL ASGI app is not available. Ensure strawberry-graphql is installed."
        )
    try:
        import uvicorn  # type: ignore
    except Exception as e:  # pragma: no cover
        raise RuntimeError(
            "uvicorn is required to run the GraphQL server. Install uvicorn."
        ) from e

    uvicorn.run(graphql_app, host=host, port=port, reload=reload, log_level="info")


if __name__ == "__main__":
    host = os.environ.get("GRAPHQL_HOST", "127.0.0.1")
    port = int(os.environ.get("GRAPHQL_PORT", "8000"))
    reload = os.environ.get("GRAPHQL_RELOAD", "0") == "1"
    main(host=host, port=port, reload=reload)
