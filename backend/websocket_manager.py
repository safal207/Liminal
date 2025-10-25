#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Production-ready WebSocket Management for LIMINAL.

Features:
- Horizontal scaling with load balancing
- Connection state management and persistence
- Message ordering guarantees
- Real-time consciousness state broadcasting
- Connection pooling and lifecycle management
- Performance monitoring and metrics
"""

import asyncio
import json
import logging
import time
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Dict, List, Optional, Set, Callable
from collections import defaultdict, deque

import redis.asyncio as redis
from fastapi import WebSocket, WebSocketDisconnect
from fastapi.websockets import WebSocketState

from config import get_websocket_settings
from monitoring import monitoring_service, record_business_metric
from resilience import circuit_breaker, with_bulkhead, LiminalException

logger = logging.getLogger(__name__)


class ConnectionState(Enum):
    """WebSocket connection states."""
    CONNECTING = "connecting"
    CONNECTED = "connected"
    AUTHENTICATED = "authenticated"
    DISCONNECTED = "disconnected"


class MessageType(Enum):
    """WebSocket message types."""
    AUTH = "auth"
    CONSCIOUSNESS_STATE = "consciousness_state"
    EMOTION_UPDATE = "emotion_update"
    HEARTBEAT = "heartbeat"
    ERROR = "error"
    SYNC_REQUEST = "sync_request"


class MessagePriority(Enum):
    """Message priority levels."""
    LOW = 1
    NORMAL = 2
    HIGH = 3
    CRITICAL = 4


@dataclass
class WebSocketMessage:
    """WebSocket message with metadata."""
    id: str
    type: MessageType
    payload: Dict[str, Any]
    priority: MessagePriority
    timestamp: datetime
    user_id: Optional[str] = None
    connection_id: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert message to dictionary."""
        return {
            "id": self.id,
            "type": self.type.value,
            "payload": self.payload,
            "timestamp": self.timestamp.isoformat(),
            "priority": self.priority.value
        }


@dataclass
class ConnectionInfo:
    """WebSocket connection information."""
    connection_id: str
    user_id: Optional[str]
    websocket: WebSocket
    state: ConnectionState
    created_at: datetime
    last_activity: datetime
    ip_address: str
    subscriptions: Set[str] = field(default_factory=set)
    message_count: int = 0
    
    def is_active(self) -> bool:
        """Check if connection is active."""
        return (
            self.state in [ConnectionState.CONNECTED, ConnectionState.AUTHENTICATED] and
            self.websocket.client_state == WebSocketState.CONNECTED
        )
    
    def update_activity(self):
        """Update last activity timestamp."""
        self.last_activity = datetime.utcnow()


class MessageQueue:
    """Priority-based message queue."""
    
    def __init__(self, max_size: int = 1000):
        self.queues: Dict[MessagePriority, deque] = {
            priority: deque(maxlen=max_size) for priority in MessagePriority
        }
        self._lock = asyncio.Lock()
    
    async def enqueue(self, message: WebSocketMessage) -> bool:
        """Add message to queue with priority."""
        async with self._lock:
            queue = self.queues[message.priority]
            if len(queue) >= queue.maxlen:
                return False
            queue.append(message)
            return True
    
    async def dequeue(self) -> Optional[WebSocketMessage]:
        """Get next message (highest priority first)."""
        async with self._lock:
            for priority in sorted(MessagePriority, key=lambda p: p.value, reverse=True):
                queue = self.queues[priority]
                if queue:
                    return queue.popleft()
            return None
    
    def get_stats(self) -> Dict[str, Any]:
        """Get queue statistics."""
        return {
            priority.name.lower(): len(queue)
            for priority, queue in self.queues.items()
        }


class ConnectionPool:
    """WebSocket connection pool with load balancing."""
    
    def __init__(self, redis_client: Optional[redis.Redis] = None):
        self.connections: Dict[str, ConnectionInfo] = {}
        self.user_connections: Dict[str, Set[str]] = defaultdict(set)
        self.subscription_groups: Dict[str, Set[str]] = defaultdict(set)
        self.redis_client = redis_client
        self._lock = asyncio.Lock()
        
        # Metrics
        self.total_connections = 0
        self.active_connections = 0
        self.messages_sent = 0
    
    async def add_connection(self, connection_info: ConnectionInfo) -> bool:
        """Add new connection to pool."""
        async with self._lock:
            connection_id = connection_info.connection_id
            
            if connection_id in self.connections:
                return False
            
            self.connections[connection_id] = connection_info
            
            if connection_info.user_id:
                self.user_connections[connection_info.user_id].add(connection_id)
            
            self.total_connections += 1
            self.active_connections += 1
            
            # Store in Redis for distributed scaling
            if self.redis_client:
                await self._store_connection_in_redis(connection_info)
            
            logger.info(f"Added WebSocket connection {connection_id}")
            return True
    
    async def remove_connection(self, connection_id: str) -> bool:
        """Remove connection from pool."""
        async with self._lock:
            connection_info = self.connections.get(connection_id)
            if not connection_info:
                return False
            
            # Clean up references
            if connection_info.user_id:
                self.user_connections[connection_info.user_id].discard(connection_id)
            
            for subscription in connection_info.subscriptions:
                self.subscription_groups[subscription].discard(connection_id)
            
            del self.connections[connection_id]
            self.active_connections -= 1
            
            # Remove from Redis
            if self.redis_client:
                await self._remove_connection_from_redis(connection_id)
            
            return True
    
    async def get_connections_for_user(self, user_id: str) -> List[ConnectionInfo]:
        """Get all connections for a user."""
        connection_ids = self.user_connections.get(user_id, set())
        return [
            self.connections[conn_id] 
            for conn_id in connection_ids 
            if conn_id in self.connections and self.connections[conn_id].is_active()
        ]
    
    async def get_connections_for_subscription(self, subscription: str) -> List[ConnectionInfo]:
        """Get all connections subscribed to a topic."""
        connection_ids = self.subscription_groups.get(subscription, set())
        return [
            self.connections[conn_id]
            for conn_id in connection_ids
            if conn_id in self.connections and self.connections[conn_id].is_active()
        ]
    
    async def subscribe_connection(self, connection_id: str, subscription: str) -> bool:
        """Subscribe connection to a topic."""
        async with self._lock:
            connection_info = self.connections.get(connection_id)
            if not connection_info:
                return False
            
            connection_info.subscriptions.add(subscription)
            self.subscription_groups[subscription].add(connection_id)
            return True
    
    async def _store_connection_in_redis(self, connection_info: ConnectionInfo):
        """Store connection info in Redis."""
        try:
            connection_data = {
                "connection_id": connection_info.connection_id,
                "user_id": connection_info.user_id or "",
                "state": connection_info.state.value,
                "created_at": connection_info.created_at.isoformat(),
                "subscriptions": ",".join(connection_info.subscriptions)
            }
            
            await self.redis_client.hset(
                f"ws_connection:{connection_info.connection_id}",
                mapping=connection_data
            )
            
            await self.redis_client.expire(
                f"ws_connection:{connection_info.connection_id}",
                3600  # 1 hour
            )
            
        except Exception as e:
            logger.error(f"Failed to store connection in Redis: {e}")
    
    async def _remove_connection_from_redis(self, connection_id: str):
        """Remove connection from Redis."""
        try:
            await self.redis_client.delete(f"ws_connection:{connection_id}")
        except Exception as e:
            logger.error(f"Failed to remove connection from Redis: {e}")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get connection pool statistics."""
        return {
            "total_connections": self.total_connections,
            "active_connections": self.active_connections,
            "user_connections": len(self.user_connections),
            "subscription_groups": len(self.subscription_groups),
            "messages_sent": self.messages_sent
        }


class WebSocketLoadBalancer:
    """Load balancer for WebSocket connections."""
    
    def __init__(self, redis_client: redis.Redis):
        self.redis_client = redis_client
        self.server_id = str(uuid.uuid4())
        self.server_capacity = 1000
        self.heartbeat_interval = 30
    
    async def register_server(self) -> bool:
        """Register this server with the load balancer."""
        try:
            server_info = {
                "server_id": self.server_id,
                "capacity": str(self.server_capacity),
                "current_load": "0",
                "last_heartbeat": str(time.time()),
                "status": "healthy"
            }
            
            await self.redis_client.hset(
                f"ws_server:{self.server_id}",
                mapping=server_info
            )
            
            await self.redis_client.expire(
                f"ws_server:{self.server_id}",
                self.heartbeat_interval * 3
            )
            
            # Start heartbeat
            asyncio.create_task(self._heartbeat_loop())
            
            logger.info(f"Registered WebSocket server {self.server_id}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to register server: {e}")
            return False
    
    async def update_server_load(self, current_load: int):
        """Update current server load."""
        try:
            await self.redis_client.hset(
                f"ws_server:{self.server_id}",
                "current_load", str(current_load)
            )
        except Exception as e:
            logger.error(f"Failed to update server load: {e}")
    
    async def _heartbeat_loop(self):
        """Send periodic heartbeats."""
        while True:
            try:
                await asyncio.sleep(self.heartbeat_interval)
                await self.redis_client.hset(
                    f"ws_server:{self.server_id}",
                    "last_heartbeat", str(time.time())
                )
            except Exception as e:
                logger.error(f"Heartbeat failed: {e}")


class WebSocketManager:
    """Main WebSocket management system."""
    
    def __init__(self):
        self.settings = get_websocket_settings()
        self.connection_pool = ConnectionPool()
        self.message_queue = MessageQueue(max_size=self.settings.max_queue_size)
        self.load_balancer: Optional[WebSocketLoadBalancer] = None
        self._running_tasks: Set[asyncio.Task] = set()
        self.start_time = datetime.utcnow()
        
        # Message handlers
        self.message_handlers: Dict[MessageType, Callable] = {
            MessageType.AUTH: self._handle_auth,
            MessageType.HEARTBEAT: self._handle_heartbeat,
            MessageType.CONSCIOUSNESS_STATE: self._handle_consciousness,
        }
    
    async def initialize(self) -> bool:
        """Initialize WebSocket manager."""
        try:
            # Initialize Redis for distributed scaling
            if self.settings.redis_enabled:
                redis_client = redis.from_url(
                    self.settings.redis_url,
                    max_connections=self.settings.redis_max_connections
                )
                self.connection_pool.redis_client = redis_client
                
                # Initialize load balancer
                self.load_balancer = WebSocketLoadBalancer(redis_client)
                await self.load_balancer.register_server()
            
            # Start background tasks
            self._start_background_tasks()
            
            logger.info("WebSocket manager initialized")
            return True
            
        except Exception as e:
            logger.error(f"Failed to initialize WebSocket manager: {e}")
            return False
    
    def _start_background_tasks(self):
        """Start background maintenance tasks."""
        tasks = [
            self._message_processor_loop(),
            self._connection_cleanup_loop(),
            self._metrics_update_loop()
        ]
        
        for task_coro in tasks:
            task = asyncio.create_task(task_coro)
            self._running_tasks.add(task)
    
    @circuit_breaker(name="websocket_connect", failure_threshold=10)
    async def handle_connection(
        self,
        websocket: WebSocket,
        user_id: Optional[str] = None,
        ip_address: str = "unknown"
    ) -> str:
        """Handle new WebSocket connection."""
        connection_id = str(uuid.uuid4())
        
        try:
            await websocket.accept()
            
            connection_info = ConnectionInfo(
                connection_id=connection_id,
                user_id=user_id,
                websocket=websocket,
                state=ConnectionState.CONNECTED,
                created_at=datetime.utcnow(),
                last_activity=datetime.utcnow(),
                ip_address=ip_address
            )
            
            await self.connection_pool.add_connection(connection_info)
            
            # Send welcome message
            welcome_msg = WebSocketMessage(
                id=str(uuid.uuid4()),
                type=MessageType.AUTH,
                payload={
                    "message": "Connected to LIMINAL",
                    "connection_id": connection_id
                },
                priority=MessagePriority.HIGH,
                timestamp=datetime.utcnow()
            )
            await self._send_message(connection_id, welcome_msg)
            
            # Update metrics
            monitoring_service.metrics.websocket_connections_total.labels(
                status="connected"
            ).inc()
            
            logger.info(f"WebSocket connection established: {connection_id}")
            return connection_id
            
        except Exception as e:
            logger.error(f"Failed to handle WebSocket connection: {e}")
            raise LiminalException("Failed to establish WebSocket connection")
    
    async def handle_disconnection(self, connection_id: str) -> bool:
        """Handle WebSocket disconnection."""
        try:
            await self.connection_pool.remove_connection(connection_id)
            
            monitoring_service.metrics.websocket_connections_total.labels(
                status="disconnected"
            ).inc()
            
            logger.info(f"WebSocket connection closed: {connection_id}")
            return True
            
        except Exception as e:
            logger.error(f"Error handling disconnection: {e}")
            return False
    
    @with_bulkhead(name="websocket_message", max_concurrent=100)
    async def handle_message(self, connection_id: str, raw_message: str) -> bool:
        """Handle incoming WebSocket message."""
        try:
            message_data = json.loads(raw_message)
            message_type = MessageType(message_data.get("type", "unknown"))
            
            message = WebSocketMessage(
                id=message_data.get("id", str(uuid.uuid4())),
                type=message_type,
                payload=message_data.get("payload", {}),
                priority=MessagePriority(message_data.get("priority", MessagePriority.NORMAL.value)),
                timestamp=datetime.utcnow(),
                connection_id=connection_id
            )
            
            # Update connection activity
            connection_info = self.connection_pool.connections.get(connection_id)
            if connection_info:
                connection_info.update_activity()
                connection_info.message_count += 1
            
            # Handle message
            if message_type in self.message_handlers:
                await self.message_handlers[message_type](connection_id, message)
            
            return True
            
        except Exception as e:
            logger.error(f"Error handling message: {e}")
            await self._send_error(connection_id, str(e))
            return False
    
    async def broadcast_to_subscription(
        self,
        subscription: str,
        message: WebSocketMessage
    ) -> int:
        """Broadcast message to subscription."""
        connections = await self.connection_pool.get_connections_for_subscription(subscription)
        
        successful_sends = 0
        for connection in connections:
            try:
                await self._send_message(connection.connection_id, message)
                successful_sends += 1
            except Exception as e:
                logger.error(f"Failed to send to {connection.connection_id}: {e}")
        
        return successful_sends
    
    async def _send_message(self, connection_id: str, message: WebSocketMessage):
        """Send message to specific connection."""
        connection_info = self.connection_pool.connections.get(connection_id)
        if not connection_info or not connection_info.is_active():
            raise LiminalException(f"Connection {connection_id} not active")
        
        await connection_info.websocket.send_text(json.dumps(message.to_dict()))
        self.connection_pool.messages_sent += 1
    
    async def _send_error(self, connection_id: str, error_message: str):
        """Send error message."""
        error_msg = WebSocketMessage(
            id=str(uuid.uuid4()),
            type=MessageType.ERROR,
            payload={"error": error_message},
            priority=MessagePriority.HIGH,
            timestamp=datetime.utcnow()
        )
        
        try:
            await self._send_message(connection_id, error_msg)
        except Exception as e:
            logger.error(f"Failed to send error: {e}")
    
    # Message handlers
    async def _handle_auth(self, connection_id: str, message: WebSocketMessage):
        """Handle authentication."""
        connection_info = self.connection_pool.connections.get(connection_id)
        if connection_info:
            connection_info.state = ConnectionState.AUTHENTICATED
            connection_info.user_id = message.payload.get("user_id")
    
    async def _handle_heartbeat(self, connection_id: str, message: WebSocketMessage):
        """Handle heartbeat."""
        pong_msg = WebSocketMessage(
            id=str(uuid.uuid4()),
            type=MessageType.HEARTBEAT,
            payload={"pong": True},
            priority=MessagePriority.NORMAL,
            timestamp=datetime.utcnow()
        )
        await self._send_message(connection_id, pong_msg)
    
    async def _handle_consciousness(self, connection_id: str, message: WebSocketMessage):
        """Handle consciousness state broadcast."""
        await self.broadcast_to_subscription("consciousness_updates", message)
    
    # Background loops
    async def _message_processor_loop(self):
        """Process queued messages."""
        while True:
            try:
                await asyncio.sleep(0.01)
                message = await self.message_queue.dequeue()
                if message and message.connection_id:
                    await self._send_message(message.connection_id, message)
            except Exception as e:
                logger.error(f"Error in message processor: {e}")
    
    async def _connection_cleanup_loop(self):
        """Clean up stale connections."""
        while True:
            try:
                await asyncio.sleep(60)
                
                current_time = datetime.utcnow()
                stale_connections = []
                
                for connection_id, connection_info in self.connection_pool.connections.items():
                    if (current_time - connection_info.last_activity).total_seconds() > 300:
                        stale_connections.append(connection_id)
                
                for connection_id in stale_connections:
                    await self.handle_disconnection(connection_id)
                
            except Exception as e:
                logger.error(f"Error in connection cleanup: {e}")
    
    async def _metrics_update_loop(self):
        """Update metrics."""
        while True:
            try:
                await asyncio.sleep(30)
                
                if self.load_balancer:
                    await self.load_balancer.update_server_load(
                        self.connection_pool.active_connections
                    )
                
                monitoring_service.metrics.websocket_active_connections.set(
                    self.connection_pool.active_connections
                )
                
            except Exception as e:
                logger.error(f"Error updating metrics: {e}")
    
    async def get_status(self) -> Dict[str, Any]:
        """Get WebSocket status."""
        uptime = (datetime.utcnow() - self.start_time).total_seconds()
        
        return {
            "uptime_seconds": uptime,
            "connection_pool": self.connection_pool.get_stats(),
            "message_queue": self.message_queue.get_stats(),
            "server_id": self.load_balancer.server_id if self.load_balancer else None
        }


# Global instance
websocket_manager = WebSocketManager()


# FastAPI endpoint helper
async def websocket_endpoint(websocket: WebSocket, user_id: Optional[str] = None):
    """WebSocket endpoint for FastAPI."""
    connection_id = None
    
    try:
        client_ip = websocket.client.host if websocket.client else "unknown"
        connection_id = await websocket_manager.handle_connection(websocket, user_id, client_ip)
        
        # Message handling loop
        while True:
            try:
                message = await websocket.receive_text()
                await websocket_manager.handle_message(connection_id, message)
            except WebSocketDisconnect:
                break
            except Exception as e:
                logger.error(f"Error in message loop: {e}")
                break
                
    except Exception as e:
        logger.error(f"WebSocket error: {e}")
    finally:
        if connection_id:
            await websocket_manager.handle_disconnection(connection_id)


if __name__ == "__main__":
    async def test_websocket_manager():
        """Test WebSocket manager."""
        print("🌐 Testing WebSocket Management System...")
        
        await websocket_manager.initialize()
        
        status = await websocket_manager.get_status()
        print(f"📊 WebSocket Status: {json.dumps(status, indent=2)}")
        
        print("✅ WebSocket management test completed!")
    
    asyncio.run(test_websocket_manager())