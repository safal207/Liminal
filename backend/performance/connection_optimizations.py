"""
Connection Pooling Optimizations
Оптимизация пулов соединений для лучшей производительности.
"""

import asyncio
from typing import Optional, Dict, Any
from contextlib import asynccontextmanager

# Заглушки для aiohttp если не установлен
try:
    import aiohttp
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False
    aiohttp = None

class OptimizedConnectionManager:
    """Оптимизированный менеджер соединений."""
    
    def __init__(self):
        self.session: Optional = None
        self.connector: Optional = None
        
    async def initialize(self):
        """Инициализирует оптимизированные соединения."""
        if not AIOHTTP_AVAILABLE:
            # Fallback для простых случаев
            return
            
        if self.session is None:
            # Оптимизированный TCP коннектор
            self.connector = aiohttp.TCPConnector(
                limit=100,              # Общий лимит соединений
                limit_per_host=30,      # Лимит на хост
                ttl_dns_cache=300,      # DNS кэш на 5 минут
                use_dns_cache=True,
                keepalive_timeout=30,   # Keep-alive timeout
                enable_cleanup_closed=True
            )
            
            # Оптимизированная сессия
            timeout = aiohttp.ClientTimeout(
                total=10,               # Общий timeout
                connect=2,              # Timeout подключения
                sock_read=5             # Timeout чтения
            )
            
            self.session = aiohttp.ClientSession(
                connector=self.connector,
                timeout=timeout,
                headers={
                    'Connection': 'keep-alive',
                    'Accept-Encoding': 'gzip, deflate'
                }
            )
    
    async def close(self):
        """Закрывает соединения."""
        if self.session:
            await self.session.close()
        if self.connector:
            await self.connector.close()
    
    @asynccontextmanager
    async def get_session(self):
        """Context manager для получения сессии."""
        await self.initialize()
        try:
            yield self.session
        finally:
            # Сессия остается открытой для переиспользования
            pass


# Глобальный менеджер соединений
connection_manager = OptimizedConnectionManager()


class RedisConnectionOptimizer:
    """Оптимизатор для Redis соединений."""
    
    @staticmethod
    def get_optimized_redis_config() -> Dict[str, Any]:
        """Возвращает оптимизированную конфигурацию Redis."""
        return {
            # Connection pooling
            "connection_pool_max_connections": 50,
            "connection_pool_retry_on_timeout": True,
            
            # Timeouts
            "socket_connect_timeout": 2,
            "socket_timeout": 5,
            "socket_keepalive": True,
            "socket_keepalive_options": {},
            
            # Protocol optimizations
            "protocol": 3,
            "encoding": "utf-8",
            "decode_responses": True,
            
            # Performance settings
            "max_connections": 50,
            "retry_on_timeout": True,
            "health_check_interval": 30
        }
    
    @staticmethod
    def get_redis_lua_scripts() -> Dict[str, str]:
        """Возвращает оптимизированные Lua скрипты для Redis."""
        return {
            # Атомарная операция rate limiting
            "rate_limit": """
local key = KEYS[1]
local window = tonumber(ARGV[1])
local limit = tonumber(ARGV[2])
local current_time = tonumber(ARGV[3])

local current = redis.call('GET', key)
if current == false then
    redis.call('SET', key, 1)
    redis.call('EXPIRE', key, window)
    return {1, limit}
end

current = tonumber(current)
if current < limit then
    current = redis.call('INCR', key)
    local ttl = redis.call('TTL', key)
    if ttl == -1 then
        redis.call('EXPIRE', key, window)
    end
    return {current, limit}
else
    local ttl = redis.call('TTL', key)
    return {current, limit, ttl}
end
            """,
            
            # Batch операции для метрик
            "batch_metrics": """
local metrics = cjson.decode(ARGV[1])
local results = {}

for key, value in pairs(metrics) do
    local result = redis.call('INCRBY', key, value)
    table.insert(results, {key, result})
end

return results
            """
        }


class DatabaseOptimizer:
    """Оптимизатор для базы данных."""
    
    @staticmethod
    def get_neo4j_optimized_config() -> Dict[str, Any]:
        """Оптимизированная конфигурация Neo4j."""
        return {
            "uri": "bolt://localhost:7687",
            "max_connection_pool_size": 50,
            "max_transaction_retry_time": 30,
            "connection_acquisition_timeout": 5,
            "trust": "TRUST_ALL_CERTIFICATES",
            "encrypted": False,
            
            # Performance settings
            "fetch_size": 1000,
            "max_connection_lifetime": 300,  # 5 minutes
            "connection_timeout": 5,
            "keep_alive": True
        }
    
    @staticmethod
    def get_optimized_queries() -> Dict[str, str]:
        """Оптимизированные Cypher запросы."""
        return {
            "create_emotional_point": """
MERGE (u:User {user_id: $user_id})
CREATE (p:EmotionalPoint {
    point_id: $point_id,
    valence: $valence,
    arousal: $arousal,
    dominance: $dominance,
    timestamp: $timestamp,
    session_id: $session_id
})
CREATE (u)-[:HAS_EMOTIONAL_STATE]->(p)
RETURN p.point_id as point_id
            """,
            
            "get_user_emotional_history": """
MATCH (u:User {user_id: $user_id})-[:HAS_EMOTIONAL_STATE]->(p:EmotionalPoint)
WHERE p.timestamp > $since
RETURN p
ORDER BY p.timestamp DESC
LIMIT $limit
            """
        }