"""Shared dependencies and service accessors for the Liminal backend app."""
from __future__ import annotations

import asyncio
from typing import Optional

from backend.infrastructure.neo4j import Neo4jGateway, get_default_gateway

from .services.memory import MemoryTimelineService
from .services.ml import MLService
from .services.neo4j import Neo4jService
from .services.timeline_bridge import TimelineGraphBridge
from .services.websocket import ConnectionManagerService


_memory_service = MemoryTimelineService()
_ml_service = MLService()
_connection_manager_service = ConnectionManagerService()

_neo4j_gateway: Optional[Neo4jGateway] = None
_neo4j_service: Optional[Neo4jService] = None
_timeline_bridge: Optional[TimelineGraphBridge] = None
_timeline_bridge_listener = None


def _bind_timeline_bridge(gateway: Neo4jGateway) -> None:
    global _timeline_bridge, _timeline_bridge_listener
    timeline = _memory_service.get_timeline()
    if _timeline_bridge_listener is not None:
        _memory_service.remove_listener(_timeline_bridge_listener)
    _timeline_bridge = TimelineGraphBridge(gateway)
    _timeline_bridge_listener = _timeline_bridge.handle_event
    _memory_service.register_listener(_timeline_bridge_listener)


def _ensure_neo4j_components() -> None:
    global _neo4j_gateway, _neo4j_service, _timeline_bridge_listener
    if _neo4j_gateway is None:
        _neo4j_gateway = get_default_gateway()
    if _neo4j_service is None:
        _neo4j_service = Neo4jService(_neo4j_gateway)
    if _timeline_bridge_listener is None:
        _bind_timeline_bridge(_neo4j_gateway)


def set_neo4j_gateway(gateway: Neo4jGateway) -> None:
    """Override the globally configured Neo4j gateway (useful for tests)."""

    global _neo4j_gateway, _neo4j_service
    _neo4j_gateway = gateway
    _neo4j_service = Neo4jService(gateway)
    _bind_timeline_bridge(gateway)


def reset_neo4j_gateway() -> None:
    """Reset the Neo4j gateway to its default implementation."""

    global _neo4j_gateway, _neo4j_service, _timeline_bridge, _timeline_bridge_listener
    _neo4j_gateway = None
    _neo4j_service = None
    _timeline_bridge = None
    if _timeline_bridge_listener is not None:
        _memory_service.remove_listener(_timeline_bridge_listener)
        _timeline_bridge_listener = None


def get_neo4j_service() -> Neo4jService:
    """Return the singleton Neo4j service."""

    _ensure_neo4j_components()
    assert _neo4j_service is not None  # Satisfy type-checkers
    return _neo4j_service


def get_memory_timeline():
    """Return the shared memory timeline instance."""

    _ensure_neo4j_components()
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
