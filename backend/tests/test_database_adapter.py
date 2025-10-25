#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тесты для DatabaseAdapter - универсального адаптера БД.

Проверяет автоматический выбор между Datomic и Neo4j,
fallback механизмы и корректность работы с разными типами данных.
"""

import asyncio
import pytest
from unittest.mock import AsyncMock, MagicMock, patch

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database_adapter import DatabaseAdapter, DataType


class TestDatabaseAdapter:
    """Тесты для DatabaseAdapter."""
    
    @pytest.fixture
    def mock_datomic_client(self):
        """Мок для DatomicClient."""
        mock = MagicMock()
        mock.connect.return_value = True
        mock.add_emotion_entry.return_value = "emotion-123"
        mock.get_emotion_history.return_value = [
            ("entry-1", "радость", 0.8, "2025-08-21T19:30:00"),
            ("entry-2", "спокойствие", 0.6, "2025-08-21T19:25:00")
        ]
        return mock
    
    @pytest.fixture
    def mock_neo4j_client(self):
        """Мок для PhilosophyNeo4jWriter."""
        mock = MagicMock()
        return mock
    
    @pytest.fixture
    def adapter_with_mocks(self, mock_datomic_client, mock_neo4j_client):
        """DatabaseAdapter с моками."""
        with patch('database_adapter.DatomicClient', return_value=mock_datomic_client), \
             patch('database_adapter.PhilosophyNeo4jWriter', return_value=mock_neo4j_client):
            
            adapter = DatabaseAdapter(auto_connect=False)
            adapter.datomic_client = mock_datomic_client
            adapter.neo4j_client = mock_neo4j_client
            adapter.datomic_available = True
            adapter.neo4j_available = True
            
            return adapter
    
    def test_data_type_routing(self, adapter_with_mocks):
        """Тест автоматического выбора БД по типу данных."""
        adapter = adapter_with_mocks
        
        # Временные данные → Datomic
        assert adapter._choose_database(DataType.TEMPORAL) == "datomic"
        assert adapter._choose_database(DataType.EVENT) == "datomic"
        assert adapter._choose_database(DataType.AUDIT) == "datomic"
        assert adapter._choose_database(DataType.EMOTION_HISTORY) == "datomic"
        assert adapter._choose_database(DataType.SESSION_DATA) == "datomic"
        
        # Структурные данные → Neo4j
        assert adapter._choose_database(DataType.RELATIONSHIP) == "neo4j"
        assert adapter._choose_database(DataType.GRAPH) == "neo4j"
        assert adapter._choose_database(DataType.PHILOSOPHY) == "neo4j"
        assert adapter._choose_database(DataType.CONCEPT_MAP) == "neo4j"
        assert adapter._choose_database(DataType.USER_NETWORK) == "neo4j"
    
    def test_fallback_mechanism(self, adapter_with_mocks):
        """Тест fallback механизма при недоступности БД."""
        adapter = adapter_with_mocks
        adapter.fallback_enabled = True
        
        # Datomic недоступен → fallback на Neo4j
        adapter.datomic_available = False
        adapter.neo4j_available = True
        
        available_db = adapter._get_available_database("datomic")
        assert available_db == "neo4j"
        assert adapter.stats["fallback_uses"] == 1
        
        # Neo4j недоступен → fallback на Datomic
        adapter.datomic_available = True
        adapter.neo4j_available = False
        adapter.stats["fallback_uses"] = 0  # Сброс счетчика
        
        available_db = adapter._get_available_database("neo4j")
        assert available_db == "datomic"
        assert adapter.stats["fallback_uses"] == 1
    
    def test_fallback_disabled(self, adapter_with_mocks):
        """Тест отключенного fallback."""
        adapter = adapter_with_mocks
        adapter.fallback_enabled = False
        adapter.datomic_available = False
        adapter.neo4j_available = True
        
        # Fallback отключен → None при недоступности предпочтительной БД
        available_db = adapter._get_available_database("datomic")
        assert available_db is None
    
    @pytest.mark.asyncio
    async def test_store_emotion_data(self, adapter_with_mocks):
        """Тест сохранения эмоциональных данных в Datomic."""
        adapter = adapter_with_mocks
        
        emotion_data = {
            "emotion": "радость",
            "intensity": 0.8,
            "context": "успешное завершение задачи"
        }
        
        result_id = await adapter.store_data(
            data=emotion_data,
            data_type=DataType.EMOTION_HISTORY,
            user_id="user-123",
            session_id="session-456"
        )
        
        # Проверяем, что данные отправлены в Datomic
        assert adapter.stats["datomic_queries"] == 1
        assert adapter.stats["neo4j_queries"] == 0
        assert result_id == "emotion-123"
        
        # Проверяем вызов метода Datomic клиента
        adapter.datomic_client.add_emotion_entry.assert_called_once_with(
            user_id="user-123",
            emotion="радость",
            intensity=0.8
        )
    
    @pytest.mark.asyncio
    async def test_store_philosophy_data(self, adapter_with_mocks):
        """Тест сохранения философских данных в Neo4j."""
        adapter = adapter_with_mocks
        
        philosophy_data = {
            "state": "resonance",
            "depth": 0.9,
            "clarity": 0.7
        }
        
        result_id = await adapter.store_data(
            data=philosophy_data,
            data_type=DataType.PHILOSOPHY,
            user_id="user-123"
        )
        
        # Проверяем, что данные отправлены в Neo4j
        assert adapter.stats["neo4j_queries"] == 1
        assert adapter.stats["datomic_queries"] == 0
        assert result_id is not None
    
    @pytest.mark.asyncio
    async def test_query_emotion_history(self, adapter_with_mocks):
        """Тест запроса истории эмоций из Datomic."""
        adapter = adapter_with_mocks
        
        emotions = await adapter.query_data(
            data_type=DataType.EMOTION_HISTORY,
            filters={"user_id": "user-123"},
            limit=10
        )
        
        # Проверяем результат
        assert len(emotions) == 2
        assert emotions[0]["emotion"] == "радость"
        assert emotions[0]["intensity"] == 0.8
        assert emotions[1]["emotion"] == "спокойствие"
        assert emotions[1]["intensity"] == 0.6
        
        # Проверяем вызов метода Datomic клиента
        adapter.datomic_client.get_emotion_history.assert_called_once_with(
            user_id="user-123",
            limit=10
        )
    
    @pytest.mark.asyncio
    async def test_connection_error_handling(self, adapter_with_mocks):
        """Тест обработки ошибок подключения."""
        adapter = adapter_with_mocks
        adapter.datomic_available = False
        adapter.neo4j_available = False
        adapter.fallback_enabled = False
        
        # Должно вызвать ConnectionError при недоступности всех БД
        with pytest.raises(ConnectionError, match="Нет доступных баз данных"):
            await adapter.store_data(
                data={"test": "data"},
                data_type=DataType.EMOTION_HISTORY
            )
        
        assert adapter.stats["errors"] == 1
    
    def test_health_status(self, adapter_with_mocks):
        """Тест получения статуса здоровья."""
        adapter = adapter_with_mocks
        
        health = adapter.get_health_status()
        
        assert health["status"] == "healthy"
        assert health["databases"]["datomic"]["available"] is True
        assert health["databases"]["neo4j"]["available"] is True
        assert "stats" in health
        assert health["fallback_enabled"] is True
    
    def test_health_status_unhealthy(self, adapter_with_mocks):
        """Тест статуса при недоступности всех БД."""
        adapter = adapter_with_mocks
        adapter.datomic_available = False
        adapter.neo4j_available = False
        
        health = adapter.get_health_status()
        
        assert health["status"] == "unhealthy"
        assert health["databases"]["datomic"]["available"] is False
        assert health["databases"]["neo4j"]["available"] is False
    
    def test_close_connections(self, adapter_with_mocks):
        """Тест закрытия подключений."""
        adapter = adapter_with_mocks
        
        adapter.close()
        
        # Проверяем, что методы close были вызваны
        adapter.datomic_client.close.assert_called_once()
        adapter.neo4j_client.close.assert_called_once()
    
    def test_global_adapter_instance(self):
        """Тест глобального экземпляра адаптера."""
        from database_adapter import get_database_adapter
        
        # Первый вызов создает экземпляр
        adapter1 = get_database_adapter()
        assert adapter1 is not None
        
        # Второй вызов возвращает тот же экземпляр
        adapter2 = get_database_adapter()
        assert adapter1 is adapter2
    
    @pytest.mark.asyncio
    async def test_data_enrichment(self, adapter_with_mocks):
        """Тест обогащения данных метаданными."""
        adapter = adapter_with_mocks
        
        original_data = {
            "emotion": "спокойствие",
            "intensity": 0.6
        }
        
        # Перехватываем вызов _store_in_datomic для проверки обогащенных данных
        async def mock_store_in_datomic(enriched_data, data_type):
            # Проверяем, что данные обогащены
            assert "id" in enriched_data
            assert "timestamp" in enriched_data
            assert "data_type" in enriched_data
            assert "user_id" in enriched_data
            assert "session_id" in enriched_data
            assert "stored_in" in enriched_data
            
            assert enriched_data["emotion"] == "спокойствие"
            assert enriched_data["intensity"] == 0.6
            assert enriched_data["data_type"] == "emotion_history"
            assert enriched_data["user_id"] == "user-456"
            assert enriched_data["session_id"] == "session-789"
            assert enriched_data["stored_in"] == "datomic"
            
            return enriched_data["id"]
        
        adapter._store_in_datomic = mock_store_in_datomic
        
        result_id = await adapter.store_data(
            data=original_data,
            data_type=DataType.EMOTION_HISTORY,
            user_id="user-456",
            session_id="session-789"
        )
        
        assert result_id is not None


# Интеграционные тесты (требуют реальных БД)
class TestDatabaseAdapterIntegration:
    """Интеграционные тесты для DatabaseAdapter."""
    
    @pytest.mark.integration
    @pytest.mark.asyncio
    async def test_real_database_connection(self):
        """Тест подключения к реальным БД (только если они доступны)."""
        adapter = DatabaseAdapter(auto_connect=False)
        
        # Пытаемся подключиться
        connected = adapter.connect()
        
        if connected:
            # Если подключение успешно, проверяем статус
            health = adapter.get_health_status()
            assert health["status"] == "healthy"
            
            # Закрываем подключения
            adapter.close()
        else:
            # Если подключение не удалось, это нормально для тестовой среды
            pytest.skip("Реальные БД недоступны для интеграционного тестирования")


if __name__ == "__main__":
    # Запуск тестов
    pytest.main([__file__, "-v"])
