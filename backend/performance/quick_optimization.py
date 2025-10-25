"""
Quick Performance Optimizations for LIMINAL
Быстрые оптимизации на основе выявленных проблем.
"""

import asyncio
import time
import requests
from datetime import datetime


class QuickPerformanceTest:
    """Быстрое тестирование производительности."""
    
    def __init__(self, base_url: str = "http://localhost:8000"):
        self.base_url = base_url
    
    def run_quick_analysis(self):
        """Быстрый анализ основных проблем."""
        print("QUICK PERFORMANCE ANALYSIS")
        print("=" * 40)
        
        # 1. Response time test
        print("1. Response Time Analysis")
        print("-" * 25)
        
        endpoints = ["/health", "/emotime/status", "/metrics"]
        for endpoint in endpoints:
            avg_time = self._measure_response_time(endpoint)
            status = "OK" if avg_time < 2000 else "SLOW"
            print(f"   {endpoint}: {avg_time:.0f}ms [{status}]")
        
        # 2. Emotime confidence test
        print(f"\n2. Emotime Confidence Test")
        print("-" * 25)
        
        confidence = self._test_emotime_confidence()
        status = "OK" if confidence > 0.5 else "LOW"
        print(f"   Average confidence: {confidence:.2f} [{status}]")
        
        # 3. Simple throughput test
        print(f"\n3. Basic Throughput Test")
        print("-" * 25)
        
        throughput = self._test_basic_throughput()
        status = "OK" if throughput > 10 else "LOW"
        print(f"   Throughput: {throughput:.1f} req/sec [{status}]")
        
        print(f"\nAnalysis completed: {datetime.now().strftime('%H:%M:%S')}")
    
    def _measure_response_time(self, endpoint: str, samples: int = 3) -> float:
        """Измеряет среднее время ответа endpoint."""
        times = []
        
        for _ in range(samples):
            start = time.time()
            try:
                response = requests.get(f"{self.base_url}{endpoint}", timeout=5)
                if response.status_code == 200:
                    times.append((time.time() - start) * 1000)
                else:
                    times.append(5000)  # Penalty for errors
            except:
                times.append(5000)  # Penalty for timeout
        
        return sum(times) / len(times) if times else 5000
    
    def _test_emotime_confidence(self) -> float:
        """Тестирует уверенность Emotime обработки."""
        confidences = []
        
        test_texts = [
            "I feel happy and energetic!",
            "This makes me sad and worried",
            "I'm in a peaceful state of mind"
        ]
        
        for text in test_texts:
            try:
                response = requests.post(
                    f"{self.base_url}/emotime/text",
                    json={
                        "text": text,
                        "user_id": "quick_test_user",
                        "session_id": "quick_session"
                    },
                    timeout=8
                )
                
                if response.status_code == 200:
                    data = response.json()
                    confidence = data.get("confidence", 0.0)
                    confidences.append(confidence)
                else:
                    confidences.append(0.0)
                    
            except Exception:
                confidences.append(0.0)
        
        return sum(confidences) / len(confidences) if confidences else 0.0
    
    def _test_basic_throughput(self) -> float:
        """Базовый тест пропускной способности."""
        start_time = time.time()
        request_count = 0
        success_count = 0
        test_duration = 5.0  # 5 seconds
        
        while (time.time() - start_time) < test_duration:
            try:
                response = requests.get(f"{self.base_url}/health", timeout=1)
                request_count += 1
                if response.status_code == 200:
                    success_count += 1
            except:
                request_count += 1
            
            # Small delay to prevent overwhelming
            time.sleep(0.1)
        
        actual_duration = time.time() - start_time
        return success_count / actual_duration


def create_performance_optimizations():
    """Создает файлы с оптимизациями производительности."""
    
    # 1. Caching layer для Emotime
    caching_code = '''"""
Emotime Performance Optimizations - Caching Layer
Кэширование для улучшения производительности Emotime.
"""

import hashlib
import time
from typing import Dict, Optional, Any
from dataclasses import dataclass
from datetime import datetime, timedelta

@dataclass
class CacheEntry:
    """Запись в кэше."""
    data: Any
    timestamp: datetime
    ttl_seconds: int
    
    @property
    def is_expired(self) -> bool:
        return datetime.now() > (self.timestamp + timedelta(seconds=self.ttl_seconds))


class EmotimeCacheManager:
    """Менеджер кэша для Emotime обработки."""
    
    def __init__(self, max_size: int = 1000, default_ttl: int = 300):
        self.cache: Dict[str, CacheEntry] = {}
        self.max_size = max_size
        self.default_ttl = default_ttl
        self.hits = 0
        self.misses = 0
    
    def get_cache_key(self, text: str, user_id: str) -> str:
        """Генерирует ключ кэша для текста и пользователя."""
        content = f"{user_id}:{text}".encode('utf-8')
        return hashlib.md5(content).hexdigest()
    
    def get(self, cache_key: str) -> Optional[Any]:
        """Получает значение из кэша."""
        entry = self.cache.get(cache_key)
        
        if entry is None:
            self.misses += 1
            return None
        
        if entry.is_expired:
            del self.cache[cache_key]
            self.misses += 1
            return None
        
        self.hits += 1
        return entry.data
    
    def set(self, cache_key: str, data: Any, ttl: Optional[int] = None) -> None:
        """Сохраняет значение в кэш."""
        if len(self.cache) >= self.max_size:
            self._evict_oldest()
        
        entry = CacheEntry(
            data=data,
            timestamp=datetime.now(),
            ttl_seconds=ttl or self.default_ttl
        )
        
        self.cache[cache_key] = entry
    
    def _evict_oldest(self):
        """Удаляет самую старую запись."""
        if self.cache:
            oldest_key = min(self.cache.keys(), key=lambda k: self.cache[k].timestamp)
            del self.cache[oldest_key]
    
    def get_stats(self) -> Dict[str, Any]:
        """Возвращает статистику кэша."""
        total_requests = self.hits + self.misses
        hit_rate = (self.hits / total_requests * 100) if total_requests > 0 else 0
        
        return {
            "size": len(self.cache),
            "max_size": self.max_size,
            "hits": self.hits,
            "misses": self.misses,
            "hit_rate_percent": hit_rate,
            "memory_usage_percent": (len(self.cache) / self.max_size * 100)
        }
    
    def clear_expired(self):
        """Очищает истекшие записи."""
        expired_keys = [k for k, v in self.cache.items() if v.is_expired]
        for key in expired_keys:
            del self.cache[key]
        return len(expired_keys)


# Глобальный экземпляр кэша
emotime_cache = EmotimeCacheManager()
'''
    
    with open("C:\\Users\\safal\\OneDrive\\Documente\\GitHub\\resonance-liminal\\backend\\emotime\\cache.py", "w") as f:
        f.write(caching_code)
    
    print("✅ Created: emotime/cache.py")
    
    # 2. Batch processing для улучшения throughput
    batch_processing_code = '''"""
Batch Processing Optimizations
Пакетная обработка для повышения пропускной способности.
"""

import asyncio
from typing import List, Dict, Any, Optional
from datetime import datetime
from dataclasses import dataclass

@dataclass
class BatchRequest:
    """Запрос в батче."""
    id: str
    data: Dict[str, Any]
    timestamp: datetime
    future: asyncio.Future


class BatchProcessor:
    """Процессор пакетной обработки."""
    
    def __init__(self, batch_size: int = 10, batch_timeout: float = 1.0):
        self.batch_size = batch_size
        self.batch_timeout = batch_timeout
        self.pending_requests: List[BatchRequest] = []
        self._processing_lock = asyncio.Lock()
        self._batch_task: Optional[asyncio.Task] = None
    
    async def add_request(self, request_id: str, data: Dict[str, Any]) -> Any:
        """Добавляет запрос в батч и ожидает результат."""
        future = asyncio.Future()
        
        request = BatchRequest(
            id=request_id,
            data=data,
            timestamp=datetime.now(),
            future=future
        )
        
        async with self._processing_lock:
            self.pending_requests.append(request)
            
            # Запускаем обработку батча если достигли лимита
            if len(self.pending_requests) >= self.batch_size:
                await self._process_batch()
            elif self._batch_task is None:
                # Запускаем таймер для обработки по timeout
                self._batch_task = asyncio.create_task(self._batch_timeout_handler())
        
        # Ожидаем результат
        return await future
    
    async def _batch_timeout_handler(self):
        """Обработчик timeout для батча."""
        await asyncio.sleep(self.batch_timeout)
        
        async with self._processing_lock:
            if self.pending_requests:
                await self._process_batch()
    
    async def _process_batch(self):
        """Обрабатывает текущий батч запросов."""
        if not self.pending_requests:
            return
        
        batch_to_process = self.pending_requests.copy()
        self.pending_requests.clear()
        
        if self._batch_task:
            self._batch_task.cancel()
            self._batch_task = None
        
        try:
            # Обработка всего батча одновременно
            results = await self._process_batch_data(batch_to_process)
            
            # Возвращаем результаты каждому запросу
            for request, result in zip(batch_to_process, results):
                if not request.future.done():
                    request.future.set_result(result)
                    
        except Exception as e:
            # В случае ошибки возвращаем ошибку всем запросам
            for request in batch_to_process:
                if not request.future.done():
                    request.future.set_exception(e)
    
    async def _process_batch_data(self, batch: List[BatchRequest]) -> List[Any]:
        """Обрабатывает данные батча. Переопределяется в наследниках."""
        # Заглушка - в реальности здесь будет обработка Emotime
        results = []
        for request in batch:
            # Симуляция обработки
            await asyncio.sleep(0.1)
            results.append({
                "id": request.id,
                "processed_at": datetime.now().isoformat(),
                "data": request.data
            })
        return results


class EmotimeBatchProcessor(BatchProcessor):
    """Специализированный батч процессор для Emotime."""
    
    def __init__(self, emotime_engine=None, batch_size: int = 5, batch_timeout: float = 0.5):
        super().__init__(batch_size, batch_timeout)
        self.emotime_engine = emotime_engine
    
    async def _process_batch_data(self, batch: List[BatchRequest]) -> List[Any]:
        """Обрабатывает батч эмоциональных данных."""
        if not self.emotime_engine:
            # Fallback без батчевой обработки
            return await super()._process_batch_data(batch)
        
        results = []
        
        # Группируем по пользователям для более эффективной обработки
        user_groups = {}
        for request in batch:
            user_id = request.data.get("user_id", "default")
            if user_id not in user_groups:
                user_groups[user_id] = []
            user_groups[user_id].append(request)
        
        # Обрабатываем каждую группу
        for user_id, user_requests in user_groups.items():
            for request in user_requests:
                try:
                    # Здесь будет реальная обработка через Emotime
                    result = await self._process_single_emotime_request(request)
                    results.append(result)
                except Exception as e:
                    results.append({"error": str(e), "id": request.id})
        
        return results
    
    async def _process_single_emotime_request(self, request: BatchRequest) -> Dict[str, Any]:
        """Обрабатывает одиночный Emotime запрос."""
        # Заглушка для интеграции с реальным Emotime движком
        text = request.data.get("text", "")
        user_id = request.data.get("user_id", "")
        
        # Здесь будет вызов реального Emotime API
        return {
            "id": request.id,
            "emotional_features": {
                "valence": 0.7,  # Заглушка
                "arousal": 0.5,
                "dominance": 0.6
            },
            "confidence": 0.8,
            "processed_at": datetime.now().isoformat()
        }


# Глобальный батч процессор
emotime_batch_processor = EmotimeBatchProcessor()
'''
    
    with open("C:\\Users\\safal\\OneDrive\\Documente\\GitHub\\resonance-liminal\\backend\\performance\\batch_processor.py", "w") as f:
        f.write(batch_processing_code)
    
    print("✅ Created: performance/batch_processor.py")
    
    # 3. Connection pooling оптимизации
    connection_pool_code = '''"""
Connection Pooling Optimizations
Оптимизация пулов соединений для лучшей производительности.
"""

import asyncio
import aiohttp
import time
from typing import Optional, Dict, Any
from contextlib import asynccontextmanager

class OptimizedConnectionManager:
    """Оптимизированный менеджер соединений."""
    
    def __init__(self):
        self.session: Optional[aiohttp.ClientSession] = None
        self.connector: Optional[aiohttp.TCPConnector] = None
        
    async def initialize(self):
        """Инициализирует оптимизированные соединения."""
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
'''
    
    with open("C:\\Users\\safal\\OneDrive\\Documente\\GitHub\\resonance-liminal\\backend\\performance\\connection_optimizations.py", "w") as f:
        f.write(connection_pool_code)
    
    print("✅ Created: performance/connection_optimizations.py")


def main():
    """Создает оптимизации и запускает быстрый тест."""
    print("PERFORMANCE OPTIMIZATION SETUP")
    print("=" * 40)
    
    # Создаем файлы оптимизаций
    print("\n1. Creating optimization files...")
    create_performance_optimizations()
    
    print("\n2. Running quick performance test...")
    tester = QuickPerformanceTest()
    tester.run_quick_analysis()
    
    print(f"\n3. OPTIMIZATION RECOMMENDATIONS:")
    print("   1. ✅ Implement emotime/cache.py for response caching")
    print("   2. ✅ Use performance/batch_processor.py for throughput") 
    print("   3. ✅ Apply connection_optimizations.py for DB performance")
    print("   4. 🔧 Configure Redis connection pooling")
    print("   5. 🔧 Enable Neo4j query optimization")
    print("   6. 🔧 Add response compression (gzip)")
    
    print(f"\nNext steps:")
    print("   - Integrate caching in Emotime API endpoints")
    print("   - Replace single requests with batch processing")
    print("   - Apply connection pool configurations")
    print("   - Test improvements with load testing")


if __name__ == "__main__":
    main()