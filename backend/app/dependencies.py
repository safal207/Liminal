"""Shared dependencies and service accessors for the Liminal backend app."""
from __future__ import annotations

import asyncio

from .services.memory import MemoryTimelineService
from .services.ml import MLService
from .services.neo4j import Neo4jService
from .services.websocket import ConnectionManagerService


_neo4j_service = Neo4jService()
_memory_service = MemoryTimelineService()
_ml_service = MLService()
_connection_manager_service = ConnectionManagerService()


def get_neo4j_service() -> Neo4jService:
    """Return the singleton Neo4j service."""
    return _neo4j_service


def get_neo4j_writer():
    """Provide direct access to the configured Neo4j writer."""
    return _neo4j_service.get_writer()


def get_memory_timeline():
    """Return the shared memory timeline instance."""
    return _memory_service.get_timeline()


def get_ml_service() -> MLService:
    """Return the ML integration service."""
    return _ml_service


def get_connection_manager():
    """Provide the configured WebSocket connection manager."""
    return _connection_manager_service.get_manager()


async def init_services() -> None:
    """Initialise asynchronous services on application startup."""
    manager = _connection_manager_service.get_manager()
    if hasattr(manager, "initialize"):
        maybe_coro = manager.initialize()
        if asyncio.iscoroutine(maybe_coro):
            await maybe_coro


async def shutdown_services() -> None:
    """Clean up services on application shutdown."""
    manager = _connection_manager_service.get_manager()
    if hasattr(manager, "shutdown"):
        maybe_coro = manager.shutdown()
        if asyncio.iscoroutine(maybe_coro):
            await maybe_coro
