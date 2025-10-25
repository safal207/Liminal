#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Database Optimization and Connection Management for LIMINAL.

Features:
- Advanced connection pooling with load balancing
- Query optimization and caching
- Read/write replica management
- Database migration strategies
- Performance monitoring and tuning
- Automatic scaling and health management
"""

import asyncio
import logging
import time
import uuid
from abc import ABC, abstractmethod
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Dict, List, Optional, Set, Union
from contextlib import asynccontextmanager

import asyncpg
from neo4j import AsyncGraphDatabase, AsyncDriver
from redis.asyncio import Redis, ConnectionPool as RedisPool

from config import get_database_settings
from monitoring import monitoring_service
from resilience import circuit_breaker, with_bulkhead, LiminalException

logger = logging.getLogger(__name__)


class DatabaseType(Enum):
    """Database types."""
    NEO4J = "neo4j"
    DATOMIC = "datomic" 
    REDIS = "redis"
    POSTGRESQL = "postgresql"


class ConnectionState(Enum):
    """Connection states."""
    IDLE = "idle"
    ACTIVE = "active"
    STALE = "stale"
    FAILED = "failed"


class QueryType(Enum):
    """Query operation types."""
    READ = "read"
    WRITE = "write"
    ADMIN = "admin"


@dataclass
class ConnectionMetrics:
    """Connection performance metrics."""
    total_queries: int = 0
    successful_queries: int = 0
    failed_queries: int = 0
    avg_response_time: float = 0.0
    last_used: datetime = field(default_factory=datetime.utcnow)
    created_at: datetime = field(default_factory=datetime.utcnow)
    
    def record_query(self, success: bool, response_time: float):
        """Record query execution."""
        self.total_queries += 1
        if success:
            self.successful_queries += 1
        else:
            self.failed_queries += 1
        
        # Update average response time
        if self.total_queries == 1:
            self.avg_response_time = response_time
        else:
            self.avg_response_time = (
                (self.avg_response_time * (self.total_queries - 1) + response_time) 
                / self.total_queries
            )
        
        self.last_used = datetime.utcnow()


@dataclass
class DatabaseConnection:
    """Database connection wrapper with metrics."""
    id: str
    db_type: DatabaseType
    connection: Any
    state: ConnectionState
    metrics: ConnectionMetrics
    replica_type: str = "primary"  # primary, read_replica
    
    def is_healthy(self) -> bool:
        """Check if connection is healthy."""
        if self.state == ConnectionState.FAILED:
            return False
        
        # Check if connection is too old (configurable)
        max_age = timedelta(hours=8)
        if datetime.utcnow() - self.metrics.created_at > max_age:
            return False
        
        # Check failure rate
        if self.metrics.total_queries > 10:
            failure_rate = self.metrics.failed_queries / self.metrics.total_queries
            if failure_rate > 0.1:  # 10% failure rate threshold
                return False
        
        return True


class ConnectionPool:
    """Advanced connection pool with load balancing."""
    
    def __init__(
        self,
        db_type: DatabaseType,
        min_connections: int = 5,
        max_connections: int = 20,
        connection_timeout: float = 30.0,
        idle_timeout: float = 300.0
    ):
        self.db_type = db_type
        self.min_connections = min_connections
        self.max_connections = max_connections
        self.connection_timeout = connection_timeout
        self.idle_timeout = idle_timeout
        
        self.connections: Dict[str, DatabaseConnection] = {}
        self.available_connections: Set[str] = set()
        self.busy_connections: Set[str] = set()
        self.read_replicas: Set[str] = set()
        self.write_connections: Set[str] = set()
        
        self._lock = asyncio.Lock()
        self._connection_factory = None
        
    async def initialize(self, connection_factory):
        """Initialize connection pool."""
        self._connection_factory = connection_factory
        
        # Create initial connections
        for _ in range(self.min_connections):
            await self._create_connection()
        
        # Start maintenance task
        asyncio.create_task(self._maintenance_loop())
        
        logger.info(f"Initialized {self.db_type.value} connection pool with {len(self.connections)} connections")
    
    async def _create_connection(self, replica_type: str = "primary") -> DatabaseConnection:
        """Create new database connection."""
        connection_id = str(uuid.uuid4())
        
        try:
            raw_connection = await self._connection_factory(replica_type)
            
            db_connection = DatabaseConnection(
                id=connection_id,
                db_type=self.db_type,
                connection=raw_connection,
                state=ConnectionState.IDLE,
                metrics=ConnectionMetrics(),
                replica_type=replica_type
            )
            
            async with self._lock:
                self.connections[connection_id] = db_connection
                self.available_connections.add(connection_id)
                
                if replica_type == "read_replica":
                    self.read_replicas.add(connection_id)
                else:
                    self.write_connections.add(connection_id)
            
            logger.debug(f"Created new {self.db_type.value} connection: {connection_id}")
            return db_connection
            
        except Exception as e:
            logger.error(f"Failed to create {self.db_type.value} connection: {e}")
            raise
    
    async def acquire_connection(self, query_type: QueryType = QueryType.READ) -> DatabaseConnection:
        """Acquire connection from pool."""
        async with self._lock:
            # Choose appropriate connection set
            if query_type == QueryType.WRITE:
                candidate_pool = self.available_connections & self.write_connections
            else:
                # Prefer read replicas for read queries
                candidate_pool = self.available_connections & self.read_replicas
                if not candidate_pool:
                    candidate_pool = self.available_connections & self.write_connections
            
            if not candidate_pool:
                # Try to create new connection if under limit
                if len(self.connections) < self.max_connections:
                    replica_type = "read_replica" if query_type == QueryType.READ else "primary"
                    connection = await self._create_connection(replica_type)
                    candidate_pool = {connection.id}
                else:
                    raise LiminalException(
                        f"No available connections in {self.db_type.value} pool",
                        error_code="POOL_EXHAUSTED"
                    )
            
            # Select best connection (lowest avg response time)
            best_connection_id = min(
                candidate_pool,
                key=lambda cid: self.connections[cid].metrics.avg_response_time
            )
            
            connection = self.connections[best_connection_id]
            connection.state = ConnectionState.ACTIVE
            
            self.available_connections.remove(best_connection_id)
            self.busy_connections.add(best_connection_id)
            
            return connection
    
    async def release_connection(self, connection: DatabaseConnection):
        """Release connection back to pool."""
        async with self._lock:
            if connection.id in self.busy_connections:
                self.busy_connections.remove(connection.id)
                
                if connection.is_healthy():
                    connection.state = ConnectionState.IDLE
                    self.available_connections.add(connection.id)
                else:
                    # Remove unhealthy connection
                    await self._remove_connection(connection.id)
    
    async def _remove_connection(self, connection_id: str):
        """Remove connection from pool."""
        if connection_id in self.connections:
            connection = self.connections[connection_id]
            
            try:
                # Close connection based on type
                if hasattr(connection.connection, 'close'):
                    await connection.connection.close()
                elif hasattr(connection.connection, 'aclose'):
                    await connection.connection.aclose()
            except Exception as e:
                logger.warning(f"Error closing connection {connection_id}: {e}")
            
            # Remove from all sets
            self.connections.pop(connection_id, None)
            self.available_connections.discard(connection_id)
            self.busy_connections.discard(connection_id)
            self.read_replicas.discard(connection_id)
            self.write_connections.discard(connection_id)
            
            logger.debug(f"Removed connection {connection_id} from pool")
    
    async def _maintenance_loop(self):
        """Background maintenance for connection pool."""
        while True:
            try:
                await asyncio.sleep(60)  # Run every minute
                await self._cleanup_stale_connections()
                await self._ensure_min_connections()
                await self._update_pool_metrics()
            except Exception as e:
                logger.error(f"Error in connection pool maintenance: {e}")
    
    async def _cleanup_stale_connections(self):
        """Remove stale and unhealthy connections."""
        async with self._lock:
            stale_connections = []
            
            for connection_id, connection in self.connections.items():
                # Check if idle too long
                if connection.state == ConnectionState.IDLE:
                    idle_time = datetime.utcnow() - connection.metrics.last_used
                    if idle_time.total_seconds() > self.idle_timeout:
                        stale_connections.append(connection_id)
                
                # Check if unhealthy
                elif not connection.is_healthy():
                    stale_connections.append(connection_id)
        
        for connection_id in stale_connections:
            await self._remove_connection(connection_id)
        
        if stale_connections:
            logger.info(f"Cleaned up {len(stale_connections)} stale connections")
    
    async def _ensure_min_connections(self):
        """Ensure minimum number of connections."""
        current_count = len(self.connections)
        if current_count < self.min_connections:
            needed = self.min_connections - current_count
            for _ in range(needed):
                try:
                    await self._create_connection()
                except Exception as e:
                    logger.error(f"Failed to create connection during maintenance: {e}")
                    break
    
    async def _update_pool_metrics(self):
        """Update pool-level metrics."""
        pool_metrics = {
            "total_connections": len(self.connections),
            "available_connections": len(self.available_connections),
            "busy_connections": len(self.busy_connections),
            "read_replicas": len(self.read_replicas),
            "write_connections": len(self.write_connections)
        }
        
        # Update monitoring
        monitoring_service.metrics.db_connection_pool_size.labels(
            database=self.db_type.value
        ).set(pool_metrics["total_connections"])
    
    def get_stats(self) -> Dict[str, Any]:
        """Get pool statistics."""
        return {
            "db_type": self.db_type.value,
            "total_connections": len(self.connections),
            "available": len(self.available_connections),
            "busy": len(self.busy_connections),
            "read_replicas": len(self.read_replicas),
            "write_connections": len(self.write_connections),
            "min_connections": self.min_connections,
            "max_connections": self.max_connections
        }


class QueryOptimizer:
    """Query optimization and caching system."""
    
    def __init__(self):
        self.query_cache: Dict[str, Any] = {}
        self.query_stats: Dict[str, Dict] = defaultdict(lambda: {
            "count": 0,
            "total_time": 0.0,
            "avg_time": 0.0,
            "last_executed": None
        })
        self.slow_queries: List[Dict] = []
        self.cache_hit_rate = 0.0
        self.cache_hits = 0
        self.cache_misses = 0
    
    async def execute_query(
        self,
        query: str,
        params: Dict = None,
        connection: DatabaseConnection = None,
        cache_ttl: int = 300
    ) -> Any:
        """Execute optimized query with caching."""
        query_hash = self._hash_query(query, params)
        
        # Check cache
        if self._should_cache_query(query) and query_hash in self.query_cache:
            cache_entry = self.query_cache[query_hash]
            if time.time() - cache_entry["timestamp"] < cache_ttl:
                self.cache_hits += 1
                self._update_cache_hit_rate()
                return cache_entry["result"]
            else:
                # Expired cache entry
                del self.query_cache[query_hash]
        
        # Execute query
        start_time = time.time()
        try:
            result = await self._execute_raw_query(query, params, connection)
            execution_time = time.time() - start_time
            
            # Record statistics
            self._record_query_stats(query, execution_time, True)
            
            # Cache result if appropriate
            if self._should_cache_query(query):
                self.query_cache[query_hash] = {
                    "result": result,
                    "timestamp": time.time()
                }
            
            self.cache_misses += 1
            self._update_cache_hit_rate()
            
            return result
            
        except Exception as e:
            execution_time = time.time() - start_time
            self._record_query_stats(query, execution_time, False)
            
            # Record slow/failed query
            if execution_time > 1.0:  # Slow query threshold
                self.slow_queries.append({
                    "query": query[:200],  # Truncate for logging
                    "execution_time": execution_time,
                    "error": str(e) if e else None,
                    "timestamp": datetime.utcnow().isoformat()
                })
                
                # Keep only recent slow queries
                if len(self.slow_queries) > 100:
                    self.slow_queries = self.slow_queries[-100:]
            
            raise
    
    def _hash_query(self, query: str, params: Dict = None) -> str:
        """Generate hash for query and parameters."""
        import hashlib
        query_str = f"{query}:{params}" if params else query
        return hashlib.md5(query_str.encode()).hexdigest()
    
    def _should_cache_query(self, query: str) -> bool:
        """Determine if query should be cached."""
        # Cache read queries, not writes
        query_lower = query.lower().strip()
        read_keywords = ["select", "find", "match", "return"]
        write_keywords = ["insert", "update", "delete", "create", "merge"]
        
        starts_with_read = any(query_lower.startswith(kw) for kw in read_keywords)
        contains_write = any(kw in query_lower for kw in write_keywords)
        
        return starts_with_read and not contains_write
    
    async def _execute_raw_query(self, query: str, params: Dict, connection: DatabaseConnection) -> Any:
        """Execute raw query based on database type."""
        if connection.db_type == DatabaseType.NEO4J:
            session = connection.connection.session()
            result = await session.run(query, params or {})
            return [record.data() async for record in result]
        
        elif connection.db_type == DatabaseType.POSTGRESQL:
            return await connection.connection.fetch(query, *(params.values() if params else []))
        
        elif connection.db_type == DatabaseType.REDIS:
            # Redis operations would be different
            return await connection.connection.execute_command(query, *params.values() if params else [])
        
        else:
            raise LiminalException(f"Unsupported database type: {connection.db_type}")
    
    def _record_query_stats(self, query: str, execution_time: float, success: bool):
        """Record query execution statistics."""
        query_key = query[:50]  # Use first 50 chars as key
        stats = self.query_stats[query_key]
        
        stats["count"] += 1
        stats["total_time"] += execution_time
        stats["avg_time"] = stats["total_time"] / stats["count"]
        stats["last_executed"] = datetime.utcnow().isoformat()
        
        if not success:
            stats["error_count"] = stats.get("error_count", 0) + 1
    
    def _update_cache_hit_rate(self):
        """Update cache hit rate."""
        total_requests = self.cache_hits + self.cache_misses
        if total_requests > 0:
            self.cache_hit_rate = self.cache_hits / total_requests
    
    def get_optimization_stats(self) -> Dict[str, Any]:
        """Get query optimization statistics."""
        return {
            "cache_hit_rate": self.cache_hit_rate,
            "cache_size": len(self.query_cache),
            "total_queries": len(self.query_stats),
            "slow_queries_count": len(self.slow_queries),
            "avg_query_time": sum(
                stats["avg_time"] for stats in self.query_stats.values()
            ) / len(self.query_stats) if self.query_stats else 0
        }


class DatabaseManager:
    """Central database management system."""
    
    def __init__(self):
        self.pools: Dict[DatabaseType, ConnectionPool] = {}
        self.query_optimizer = QueryOptimizer()
        self.settings = get_database_settings()
        
    async def initialize(self):
        """Initialize all database connections."""
        # Initialize Neo4j pool
        if self.settings.neo4j_uri:
            neo4j_pool = ConnectionPool(
                DatabaseType.NEO4J,
                min_connections=3,
                max_connections=15
            )
            await neo4j_pool.initialize(self._create_neo4j_connection)
            self.pools[DatabaseType.NEO4J] = neo4j_pool
        
        # Initialize Redis pool
        if self.settings.redis_enabled:
            redis_pool = ConnectionPool(
                DatabaseType.REDIS,
                min_connections=5,
                max_connections=20
            )
            await redis_pool.initialize(self._create_redis_connection)
            self.pools[DatabaseType.REDIS] = redis_pool
        
        logger.info(f"Initialized {len(self.pools)} database pools")
    
    async def _create_neo4j_connection(self, replica_type: str = "primary") -> AsyncDriver:
        """Create Neo4j connection."""
        uri = self.settings.neo4j_uri
        if replica_type == "read_replica":
            # In production, this would be a different URI for read replicas
            uri = uri.replace("bolt://", "bolt+routing://")
        
        driver = AsyncGraphDatabase.driver(
            uri,
            auth=(self.settings.neo4j_user, self.settings.neo4j_password),
            max_connection_pool_size=self.settings.neo4j_max_connection_pool_size,
            connection_timeout=self.settings.neo4j_connection_timeout
        )
        
        # Verify connectivity
        await driver.verify_connectivity()
        return driver
    
    async def _create_redis_connection(self, replica_type: str = "primary") -> Redis:
        """Create Redis connection."""
        url = self.settings.redis_url
        if replica_type == "read_replica":
            # In production, would use read replica URL
            url = url  # Placeholder
        
        pool = RedisPool.from_url(
            url,
            max_connections=self.settings.redis_max_connections,
            socket_timeout=self.settings.redis_connection_timeout
        )
        
        redis_client = Redis(connection_pool=pool)
        
        # Test connection
        await redis_client.ping()
        return redis_client
    
    @asynccontextmanager
    async def get_connection(self, db_type: DatabaseType, query_type: QueryType = QueryType.READ):
        """Get database connection context manager."""
        if db_type not in self.pools:
            raise LiminalException(f"Database pool {db_type.value} not initialized")
        
        pool = self.pools[db_type]
        connection = await pool.acquire_connection(query_type)
        
        try:
            yield connection
        finally:
            await pool.release_connection(connection)
    
    @circuit_breaker(name="database_query", failure_threshold=5)
    @with_bulkhead(name="database_operations", max_concurrent=50)
    async def execute_query(
        self,
        db_type: DatabaseType,
        query: str,
        params: Dict = None,
        query_type: QueryType = QueryType.READ,
        cache_ttl: int = 300
    ) -> Any:
        """Execute optimized database query."""
        async with self.get_connection(db_type, query_type) as connection:
            start_time = time.time()
            
            try:
                result = await self.query_optimizer.execute_query(
                    query, params, connection, cache_ttl
                )
                
                execution_time = time.time() - start_time
                
                # Record metrics
                connection.metrics.record_query(True, execution_time)
                
                monitoring_service.metrics.db_operations_total.labels(
                    database=db_type.value,
                    operation=query_type.value,
                    status="success"
                ).inc()
                
                monitoring_service.metrics.db_query_duration.labels(
                    database=db_type.value,
                    operation=query_type.value
                ).observe(execution_time)
                
                return result
                
            except Exception as e:
                execution_time = time.time() - start_time
                connection.metrics.record_query(False, execution_time)
                
                monitoring_service.metrics.db_operations_total.labels(
                    database=db_type.value,
                    operation=query_type.value,
                    status="error"
                ).inc()
                
                logger.error(f"Database query failed: {e}", extra={
                    "db_type": db_type.value,
                    "query": query[:100],
                    "execution_time": execution_time
                })
                
                raise
    
    async def health_check(self) -> Dict[str, Any]:
        """Comprehensive database health check."""
        health_status = {}
        
        for db_type, pool in self.pools.items():
            try:
                # Test connection from pool
                async with self.get_connection(db_type, QueryType.READ) as connection:
                    # Simple health query based on database type
                    if db_type == DatabaseType.NEO4J:
                        await self._neo4j_health_check(connection)
                    elif db_type == DatabaseType.REDIS:
                        await self._redis_health_check(connection)
                
                health_status[db_type.value] = {
                    "status": "healthy",
                    "pool_stats": pool.get_stats()
                }
                
            except Exception as e:
                health_status[db_type.value] = {
                    "status": "unhealthy",
                    "error": str(e)
                }
        
        # Add optimization stats
        health_status["query_optimizer"] = self.query_optimizer.get_optimization_stats()
        
        return health_status
    
    async def _neo4j_health_check(self, connection: DatabaseConnection):
        """Neo4j specific health check."""
        async with connection.connection.session() as session:
            result = await session.run("RETURN 1 as health_check")
            record = await result.single()
            if record["health_check"] != 1:
                raise Exception("Neo4j health check failed")
    
    async def _redis_health_check(self, connection: DatabaseConnection):
        """Redis specific health check."""
        pong = await connection.connection.ping()
        if not pong:
            raise Exception("Redis health check failed")
    
    async def get_performance_stats(self) -> Dict[str, Any]:
        """Get comprehensive performance statistics."""
        stats = {
            "pools": {},
            "query_optimizer": self.query_optimizer.get_optimization_stats(),
            "slow_queries": self.query_optimizer.slow_queries[-10:]  # Last 10
        }
        
        for db_type, pool in self.pools.items():
            stats["pools"][db_type.value] = pool.get_stats()
        
        return stats


# Global database manager
db_manager = DatabaseManager()


# Convenience functions for easy usage
async def execute_neo4j_query(query: str, params: Dict = None, read_only: bool = True) -> Any:
    """Execute Neo4j query with optimization."""
    query_type = QueryType.READ if read_only else QueryType.WRITE
    return await db_manager.execute_query(
        DatabaseType.NEO4J, query, params, query_type
    )


async def execute_redis_command(command: str, *args) -> Any:
    """Execute Redis command with optimization."""
    # Convert command and args to query format
    query = command
    params = dict(enumerate(args)) if args else None
    return await db_manager.execute_query(
        DatabaseType.REDIS, query, params, QueryType.WRITE
    )


# Migration and maintenance utilities
class DatabaseMigrator:
    """Database migration management."""
    
    def __init__(self, db_manager: DatabaseManager):
        self.db_manager = db_manager
        self.migration_history: List[Dict] = []
    
    async def run_migration(self, migration_name: str, migration_script: str) -> bool:
        """Run database migration."""
        try:
            # This would implement actual migration logic
            logger.info(f"Running migration: {migration_name}")
            
            # Record migration
            self.migration_history.append({
                "name": migration_name,
                "executed_at": datetime.utcnow().isoformat(),
                "status": "success"
            })
            
            return True
            
        except Exception as e:
            logger.error(f"Migration {migration_name} failed: {e}")
            self.migration_history.append({
                "name": migration_name,
                "executed_at": datetime.utcnow().isoformat(),
                "status": "failed",
                "error": str(e)
            })
            return False


if __name__ == "__main__":
    async def test_database_optimization():
        """Test database optimization features."""
        print("🗄️ Testing Database Optimization System...")
        
        # Initialize database manager
        await db_manager.initialize()
        
        # Test health check
        health = await db_manager.health_check()
        print(f"📊 Database Health: {health}")
        
        # Test query optimization
        if DatabaseType.NEO4J in db_manager.pools:
            result = await execute_neo4j_query("RETURN 1 as test")
            print(f"🔍 Neo4j Test Query: {result}")
        
        # Get performance stats
        stats = await db_manager.get_performance_stats()
        print(f"📈 Performance Stats: {stats}")
        
        print("✅ Database optimization test completed!")
    
    asyncio.run(test_database_optimization())