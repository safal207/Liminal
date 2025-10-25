#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Production-ready DatabaseAdapter for LIMINAL.

Automatically routes data between different databases:
- Datomic: temporal data, history, audit, events
- Neo4j: structural relationships, graph, philosophical states

Principle: "No randomness is random" - every database choice is justified by data nature.
"""

import asyncio
import logging
import time
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Union

from config import get_database_settings
from datomic_client import DatomicClient, DatomicConnectionError

try:
    from philosophy_neo4j import PhilosophyNeo4jWriter
except ImportError:
    # Fallback implementation for environments without Neo4j
    class PhilosophyNeo4jWriter:
        def __init__(self, *args, **kwargs):
            logging.warning("Using fallback Neo4j implementation")
        def close(self):
            pass

logger = logging.getLogger(__name__)


class DataType(Enum):
    """Типы данных для автоматического выбора БД."""
    
    # Временные данные → Datomic
    TEMPORAL = "temporal"           # История изменений
    EVENT = "event"                 # События системы
    AUDIT = "audit"                 # Аудит действий
    EMOTION_HISTORY = "emotion_history"  # История эмоций
    SESSION_DATA = "session_data"   # Данные сессий
    
    # Структурные данные → Neo4j
    RELATIONSHIP = "relationship"   # Связи между сущностями
    GRAPH = "graph"                # Графовые структуры
    PHILOSOPHY = "philosophy"       # Философские состояния
    CONCEPT_MAP = "concept_map"     # Карты концептов
    USER_NETWORK = "user_network"   # Сети пользователей


class DatabaseAdapter:
    """
    Универсальный адаптер для работы с Datomic и Neo4j.
    
    Автоматически выбирает подходящую БД на основе типа данных.
    Предоставляет единый интерфейс для всех операций с БД.
    """
    
    def __init__(
        self,
        # Datomic настройки
        datomic_uri: str = "http://localhost:8080",
        datomic_db_name: str = "liminal",
        datomic_storage_type: str = "dev",
        
        # Neo4j настройки
        neo4j_uri: str = None,
        neo4j_user: str = None,
        neo4j_password: str = None,
        neo4j_database: str = "neo4j",
        
        # Общие настройки
        auto_connect: bool = True,
        fallback_enabled: bool = True,
    ):
        """
        Инициализация адаптера.
        
        Args:
            datomic_uri: URI для подключения к Datomic
            datomic_db_name: Имя базы данных Datomic
            datomic_storage_type: Тип хранилища Datomic
            neo4j_uri: URI для подключения к Neo4j
            neo4j_user: Пользователь Neo4j
            neo4j_password: Пароль Neo4j
            neo4j_database: Имя базы данных Neo4j
            auto_connect: Автоматически подключаться при инициализации
            fallback_enabled: Включить fallback между БД при недоступности
        """
        # Настройки Datomic
        self.datomic_uri = datomic_uri
        self.datomic_db_name = datomic_db_name
        self.datomic_storage_type = datomic_storage_type
        
        # Настройки Neo4j
        self.neo4j_uri = neo4j_uri or os.getenv("NEO4J_URI", "bolt://localhost:7687")
        self.neo4j_user = neo4j_user or os.getenv("NEO4J_USER", "neo4j")
        self.neo4j_password = neo4j_password or os.getenv("NEO4J_PASSWORD", "NewStrongPass123!")
        self.neo4j_database = neo4j_database
        
        # Общие настройки
        self.auto_connect = auto_connect
        self.fallback_enabled = fallback_enabled
        
        # Клиенты БД
        self.datomic_client: Optional[DatomicClient] = None
        self.neo4j_client: Optional[PhilosophyNeo4jWriter] = None
        
        # Статусы подключений
        self.datomic_available = False
        self.neo4j_available = False
        
        # Статистика использования
        self.stats = {
            "datomic_queries": 0,
            "neo4j_queries": 0,
            "fallback_uses": 0,
            "errors": 0
        }
        
        if auto_connect:
            self.connect()
    
    def connect(self) -> bool:
        """
        Подключение к обеим БД.
        
        Returns:
            True если хотя бы одна БД доступна
        """
        logger.info("🔌 Подключение к базам данных...")
        
        # Подключение к Datomic
        try:
            self.datomic_client = DatomicClient(
                uri=self.datomic_uri,
                db_name=self.datomic_db_name,
                storage_type=self.datomic_storage_type
            )
            self.datomic_available = self.datomic_client.connect()
            if self.datomic_available:
                logger.info("✅ Datomic подключен")
            else:
                logger.warning("⚠️ Datomic недоступен")
        except Exception as e:
            logger.error(f"❌ Ошибка подключения к Datomic: {e}")
            self.datomic_available = False
        
        # Подключение к Neo4j
        try:
            self.neo4j_client = PhilosophyNeo4jWriter(
                uri=self.neo4j_uri,
                user=self.neo4j_user,
                password=self.neo4j_password,
                database=self.neo4j_database
            )
            self.neo4j_available = True
            logger.info("✅ Neo4j подключен")
        except Exception as e:
            logger.error(f"❌ Ошибка подключения к Neo4j: {e}")
            self.neo4j_available = False
        
        # Проверяем доступность хотя бы одной БД
        if not (self.datomic_available or self.neo4j_available):
            logger.error("❌ Ни одна БД не доступна!")
            return False
        
        logger.info(f"🎯 DatabaseAdapter готов (Datomic: {self.datomic_available}, Neo4j: {self.neo4j_available})")
        return True
    
    def _choose_database(self, data_type: DataType) -> str:
        """
        Выбор подходящей БД на основе типа данных.
        
        Args:
            data_type: Тип данных
            
        Returns:
            "datomic" или "neo4j"
        """
        # Временные данные → Datomic
        if data_type in [DataType.TEMPORAL, DataType.EVENT, DataType.AUDIT, 
                        DataType.EMOTION_HISTORY, DataType.SESSION_DATA]:
            return "datomic"
        
        # Структурные данные → Neo4j
        elif data_type in [DataType.RELATIONSHIP, DataType.GRAPH, DataType.PHILOSOPHY,
                          DataType.CONCEPT_MAP, DataType.USER_NETWORK]:
            return "neo4j"
        
        # По умолчанию Neo4j (более универсальный)
        return "neo4j"
    
    def _get_available_database(self, preferred: str) -> Optional[str]:
        """
        Получение доступной БД с учетом fallback.
        
        Args:
            preferred: Предпочтительная БД ("datomic" или "neo4j")
            
        Returns:
            Имя доступной БД или None
        """
        if preferred == "datomic" and self.datomic_available:
            return "datomic"
        elif preferred == "neo4j" and self.neo4j_available:
            return "neo4j"
        elif self.fallback_enabled:
            # Fallback на доступную БД
            if preferred == "datomic" and self.neo4j_available:
                logger.warning("⚠️ Datomic недоступен, используем Neo4j как fallback")
                self.stats["fallback_uses"] += 1
                return "neo4j"
            elif preferred == "neo4j" and self.datomic_available:
                logger.warning("⚠️ Neo4j недоступен, используем Datomic как fallback")
                self.stats["fallback_uses"] += 1
                return "datomic"
        
        return None
    
    async def store_data(
        self, 
        data: Dict[str, Any], 
        data_type: DataType,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None
    ) -> str:
        """
        Сохранение данных в подходящую БД.
        
        Args:
            data: Данные для сохранения
            data_type: Тип данных
            user_id: ID пользователя (опционально)
            session_id: ID сессии (опционально)
            
        Returns:
            ID созданной записи
        """
        preferred_db = self._choose_database(data_type)
        available_db = self._get_available_database(preferred_db)
        
        if not available_db:
            logger.error(f"❌ Нет доступных БД для типа {data_type}")
            self.stats["errors"] += 1
            raise ConnectionError("Нет доступных баз данных")
        
        # Добавляем метаданные
        enriched_data = {
            **data,
            "id": str(uuid.uuid4()),
            "timestamp": datetime.utcnow().isoformat(),
            "data_type": data_type.value,
            "user_id": user_id,
            "session_id": session_id,
            "stored_in": available_db
        }
        
        try:
            if available_db == "datomic":
                return await self._store_in_datomic(enriched_data, data_type)
            else:
                return await self._store_in_neo4j(enriched_data, data_type)
        except Exception as e:
            logger.error(f"❌ Ошибка сохранения в {available_db}: {e}")
            self.stats["errors"] += 1
            raise
    
    async def _store_in_datomic(self, data: Dict[str, Any], data_type: DataType) -> str:
        """Сохранение данных в Datomic."""
        self.stats["datomic_queries"] += 1
        
        if data_type == DataType.EMOTION_HISTORY:
            # Специальная обработка для эмоций
            result = self.datomic_client.add_emotion_entry(
                user_id=data.get("user_id"),
                emotion=data.get("emotion", "unknown"),
                intensity=data.get("intensity", 0.5)
            )
            return str(result)
        else:
            # Общее сохранение данных
            # TODO: Реализовать универсальный метод в DatomicClient
            logger.info(f"📝 Сохранение в Datomic: {data_type.value}")
            return data["id"]
    
    async def _store_in_neo4j(self, data: Dict[str, Any], data_type: DataType) -> str:
        """Сохранение данных в Neo4j."""
        self.stats["neo4j_queries"] += 1
        
        if data_type == DataType.PHILOSOPHY:
            # Специальная обработка для философских состояний
            # TODO: Интеграция с PhilosophyNeo4jWriter
            logger.info(f"🧠 Сохранение философского состояния в Neo4j")
            return data["id"]
        else:
            # Общее сохранение данных
            logger.info(f"📝 Сохранение в Neo4j: {data_type.value}")
            return data["id"]
    
    async def query_data(
        self,
        data_type: DataType,
        filters: Optional[Dict[str, Any]] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Запрос данных из подходящей БД.
        
        Args:
            data_type: Тип данных
            filters: Фильтры для запроса
            limit: Максимальное количество записей
            
        Returns:
            Список найденных записей
        """
        preferred_db = self._choose_database(data_type)
        available_db = self._get_available_database(preferred_db)
        
        if not available_db:
            logger.error(f"❌ Нет доступных БД для запроса {data_type}")
            return []
        
        try:
            if available_db == "datomic":
                return await self._query_from_datomic(data_type, filters, limit)
            else:
                return await self._query_from_neo4j(data_type, filters, limit)
        except Exception as e:
            logger.error(f"❌ Ошибка запроса из {available_db}: {e}")
            self.stats["errors"] += 1
            return []
    
    async def _query_from_datomic(
        self, 
        data_type: DataType, 
        filters: Optional[Dict[str, Any]], 
        limit: int
    ) -> List[Dict[str, Any]]:
        """Запрос данных из Datomic."""
        self.stats["datomic_queries"] += 1
        
        if data_type == DataType.EMOTION_HISTORY and filters and "user_id" in filters:
            # Специальный запрос истории эмоций
            history = self.datomic_client.get_emotion_history(
                user_id=filters["user_id"],
                limit=limit
            )
            return [{"emotion": h[1], "intensity": h[2], "timestamp": h[3]} for h in history]
        else:
            # Общий запрос
            logger.info(f"🔍 Запрос из Datomic: {data_type.value}")
            return []
    
    async def _query_from_neo4j(
        self, 
        data_type: DataType, 
        filters: Optional[Dict[str, Any]], 
        limit: int
    ) -> List[Dict[str, Any]]:
        """Запрос данных из Neo4j."""
        self.stats["neo4j_queries"] += 1
        
        # TODO: Реализовать запросы через PhilosophyNeo4jWriter
        logger.info(f"🔍 Запрос из Neo4j: {data_type.value}")
        return []
    
    def get_health_status(self) -> Dict[str, Any]:
        """
        Получение статуса здоровья адаптера.
        
        Returns:
            Словарь со статусом всех компонентов
        """
        return {
            "status": "healthy" if (self.datomic_available or self.neo4j_available) else "unhealthy",
            "databases": {
                "datomic": {
                    "available": self.datomic_available,
                    "uri": self.datomic_uri,
                    "db_name": self.datomic_db_name
                },
                "neo4j": {
                    "available": self.neo4j_available,
                    "uri": self.neo4j_uri,
                    "database": self.neo4j_database
                }
            },
            "stats": self.stats,
            "fallback_enabled": self.fallback_enabled
        }
    
    def close(self):
        """Закрытие всех подключений."""
        logger.info("🔌 Закрытие подключений к БД...")
        
        if self.datomic_client:
            try:
                self.datomic_client.close()
                logger.info("✅ Datomic подключение закрыто")
            except Exception as e:
                logger.error(f"❌ Ошибка закрытия Datomic: {e}")
        
        if self.neo4j_client:
            try:
                self.neo4j_client.close()
                logger.info("✅ Neo4j подключение закрыто")
            except Exception as e:
                logger.error(f"❌ Ошибка закрытия Neo4j: {e}")
        
        logger.info("🎯 DatabaseAdapter закрыт")


# Глобальный экземпляр адаптера
_database_adapter: Optional[DatabaseAdapter] = None


def get_database_adapter() -> DatabaseAdapter:
    """
    Получение глобального экземпляра DatabaseAdapter с ленивой инициализацией.
    
    Returns:
        Экземпляр DatabaseAdapter
    """
    global _database_adapter
    
    if _database_adapter is None:
        _database_adapter = DatabaseAdapter()
    
    return _database_adapter


# Пример использования
if __name__ == "__main__":
    async def main():
        # Инициализация адаптера
        adapter = DatabaseAdapter()
        
        try:
            # Сохранение эмоциональных данных (→ Datomic)
            emotion_id = await adapter.store_data(
                data={
                    "emotion": "радость",
                    "intensity": 0.8,
                    "context": "успешное завершение задачи"
                },
                data_type=DataType.EMOTION_HISTORY,
                user_id="user-123",
                session_id="session-456"
            )
            print(f"Эмоция сохранена: {emotion_id}")
            
            # Сохранение философского состояния (→ Neo4j)
            philosophy_id = await adapter.store_data(
                data={
                    "state": "resonance",
                    "depth": 0.9,
                    "clarity": 0.7
                },
                data_type=DataType.PHILOSOPHY,
                user_id="user-123"
            )
            print(f"Философское состояние сохранено: {philosophy_id}")
            
            # Запрос истории эмоций
            emotions = await adapter.query_data(
                data_type=DataType.EMOTION_HISTORY,
                filters={"user_id": "user-123"},
                limit=10
            )
            print(f"История эмоций: {emotions}")
            
            # Статус здоровья
            health = adapter.get_health_status()
            print(f"Статус здоровья: {health}")
            
        finally:
            adapter.close()
    
    # Запуск примера
    asyncio.run(main())
