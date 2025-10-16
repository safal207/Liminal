"""Shared dependencies and service accessors for the Liminal backend app."""
from __future__ import annotations

import asyncio

from .services import (
    AuthService,
    ConnectionManagerService,
    MemoryTimelineService,
    MLService,
    Neo4jService,
    TimelineWebSocketService,
)


_neo4j_service = Neo4jService()
_memory_service = MemoryTimelineService()
_ml_service = MLService()
_auth_service = AuthService()
_connection_manager_service = ConnectionManagerService()
_websocket_service = TimelineWebSocketService(
    manager_service=_connection_manager_service,
    memory_service=_memory_service,
    ml_service=_ml_service,
    auth_service=_auth_service,
)


def get_neo4j_service() -> Neo4jService:
    """Return the singleton Neo4j service."""
    return _neo4j_service


def get_neo4j_writer():
    """Provide direct access to the configured Neo4j writer."""
    return _neo4j_service.get_writer()


def get_memory_service() -> MemoryTimelineService:
    """Return the memory timeline service."""
    return _memory_service


def get_memory_timeline():
    """Return the shared memory timeline instance."""
    return _memory_service.get_timeline()


def get_ml_service() -> MLService:
    """Return the ML integration service."""
    return _ml_service


def get_auth_service() -> AuthService:
    """Return the authentication service."""
    return _auth_service


def get_connection_manager():
    """Provide the configured WebSocket connection manager."""
    return _connection_manager_service.get_manager()


def get_connection_manager_service() -> ConnectionManagerService:
    """Return the connection manager service wrapper."""
    return _connection_manager_service


def get_websocket_service() -> TimelineWebSocketService:
    """Return the orchestrator for timeline WebSocket connections."""
    return _websocket_service


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
