"""Shared dependencies and service accessors for the Liminal backend app."""
from __future__ import annotations

import asyncio

from .container import AppContainer, get_container
from .services.memory import MemoryTimelineService
from .services.ml import MLService
from .services.neo4j import Neo4jService
from .services.websocket import ConnectionManagerService


def _container() -> AppContainer:
    return get_container()


def get_neo4j_service() -> Neo4jService:
    """Return the singleton Neo4j service."""
    return _container().neo4j_service()


def get_neo4j_writer():
    """Provide direct access to the configured Neo4j writer."""
    return get_neo4j_service().get_writer()


def get_memory_timeline():
    """Return the shared memory timeline instance."""
    memory_service: MemoryTimelineService = _container().memory_timeline_service()
    return memory_service.get_timeline()


def get_ml_service() -> MLService:
    """Return the ML integration service."""
    return _container().ml_service()


def get_connection_manager():
    """Provide the configured WebSocket connection manager."""
    service: ConnectionManagerService = _container().connection_manager_service()
    return service.get_manager()


async def init_services() -> None:
    """Initialise asynchronous services on application startup."""
    service: ConnectionManagerService = _container().connection_manager_service()
    manager = service.get_manager()
    if hasattr(manager, "initialize"):
        maybe_coro = manager.initialize()
        if asyncio.iscoroutine(maybe_coro):
            await maybe_coro


async def shutdown_services() -> None:
    """Clean up services on application shutdown."""
    service: ConnectionManagerService = _container().connection_manager_service()
    manager = service.get_manager()
    if hasattr(manager, "shutdown"):
        maybe_coro = manager.shutdown()
        if asyncio.iscoroutine(maybe_coro):
            await maybe_coro
