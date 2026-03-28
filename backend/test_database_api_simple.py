#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Простой тест Database API без внешних зависимостей.

Проверяет основную функциональность DatabaseAdapter и API endpoints
через прямые вызовы функций.
"""

import asyncio
import os
import sys
from unittest.mock import AsyncMock, MagicMock

# Добавляем путь к модулям
sys.path.append(os.path.dirname(os.path.abspath(__file__)))


def test_database_adapter_routing():
    """Тест автоматического выбора БД по типу данных."""
    print("🧪 Тестирование роутинга DatabaseAdapter...")

    try:
        from database_adapter import DatabaseAdapter, DataType

        # Создаем адаптер без подключения к реальным БД
        adapter = DatabaseAdapter(auto_connect=False)

        # Тестируем роутинг временных данных → Datomic
        temporal_types = [
            DataType.TEMPORAL,
            DataType.EVENT,
            DataType.AUDIT,
            DataType.EMOTION_HISTORY,
            DataType.SESSION_DATA,
        ]

        for data_type in temporal_types:
            db = adapter._choose_database(data_type)
            assert (
                db == "datomic"
            ), f"❌ {data_type.value} должен направляться в Datomic, получен {db}"

        # Тестируем роутинг структурных данных → Neo4j
        structural_types = [
            DataType.RELATIONSHIP,
            DataType.GRAPH,
            DataType.PHILOSOPHY,
            DataType.CONCEPT_MAP,
            DataType.USER_NETWORK,
        ]

        for data_type in structural_types:
            db = adapter._choose_database(data_type)
            assert (
                db == "neo4j"
            ), f"❌ {data_type.value} должен направляться в Neo4j, получен {db}"

        print("✅ Роутинг работает корректно")
        return True

    except Exception as e:
        print(f"❌ Ошибка тестирования роутинга: {e}")
        return False


def test_database_adapter_fallback():
    """Тест fallback механизма."""
    print("🧪 Тестирование fallback механизма...")

    try:
        from database_adapter import DatabaseAdapter

        adapter = DatabaseAdapter(auto_connect=False, fallback_enabled=True)

        # Тест 1: Datomic недоступен → fallback на Neo4j
        adapter.datomic_available = False
        adapter.neo4j_available = True

        available_db = adapter._get_available_database("datomic")
        assert (
            available_db == "neo4j"
        ), f"❌ Ожидался fallback на Neo4j, получен {available_db}"

        # Тест 2: Neo4j недоступен → fallback на Datomic
        adapter.datomic_available = True
        adapter.neo4j_available = False

        available_db = adapter._get_available_database("neo4j")
        assert (
            available_db == "datomic"
        ), f"❌ Ожидался fallback на Datomic, получен {available_db}"

        # Тест 3: Fallback отключен
        adapter.fallback_enabled = False
        adapter.datomic_available = False
        adapter.neo4j_available = True

        available_db = adapter._get_available_database("datomic")
        assert (
            available_db is None
        ), f"❌ При отключенном fallback должен быть None, получен {available_db}"

        print("✅ Fallback механизм работает корректно")
        return True

    except Exception as e:
        print(f"❌ Ошибка тестирования fallback: {e}")
        return False


async def test_database_adapter_store_data():
    """Тест сохранения данных."""
    print("🧪 Тестирование сохранения данных...")

    try:
        from database_adapter import DatabaseAdapter, DataType

        # Создаем адаптер с моками
        adapter = DatabaseAdapter(auto_connect=False)

        # Мокаем клиенты БД
        mock_datomic = MagicMock()
        mock_datomic.add_emotion_entry.return_value = "emotion-123"

        mock_neo4j = MagicMock()

        adapter.datomic_client = mock_datomic
        adapter.neo4j_client = mock_neo4j
        adapter.datomic_available = True
        adapter.neo4j_available = True

        # Тест сохранения эмоциональных данных
        emotion_data = {
            "emotion": "радость",
            "intensity": 0.8,
            "context": "успешное завершение задачи",
        }

        result_id = await adapter.store_data(
            data=emotion_data,
            data_type=DataType.EMOTION_HISTORY,
            user_id="user-123",
            session_id="session-456",
        )

        assert result_id == "emotion-123", f"❌ Неожиданный ID результата: {result_id}"
        assert (
            adapter.stats["datomic_queries"] == 1
        ), "❌ Счетчик Datomic запросов не увеличился"

        # Проверяем вызов метода Datomic
        mock_datomic.add_emotion_entry.assert_called_once_with(
            user_id="user-123", emotion="радость", intensity=0.8
        )

        print("✅ Сохранение данных работает корректно")
        return True

    except Exception as e:
        print(f"❌ Ошибка тестирования сохранения данных: {e}")
        return False


async def test_database_adapter_query_data():
    """Тест запроса данных."""
    print("🧪 Тестирование запроса данных...")

    try:
        from database_adapter import DatabaseAdapter, DataType

        # Создаем адаптер с моками
        adapter = DatabaseAdapter(auto_connect=False)

        # Мокаем клиенты БД
        mock_datomic = MagicMock()
        mock_datomic.get_emotion_history.return_value = [
            ("entry-1", "радость", 0.8, "2025-08-21T19:30:00"),
            ("entry-2", "спокойствие", 0.6, "2025-08-21T19:25:00"),
        ]

        adapter.datomic_client = mock_datomic
        adapter.datomic_available = True
        adapter.neo4j_available = True

        # Тест запроса истории эмоций
        emotions = await adapter.query_data(
            data_type=DataType.EMOTION_HISTORY,
            filters={"user_id": "user-123"},
            limit=10,
        )

        assert len(emotions) == 2, f"❌ Ожидалось 2 записи, получено {len(emotions)}"
        assert (
            emotions[0]["emotion"] == "радость"
        ), f"❌ Неожиданная эмоция: {emotions[0]['emotion']}"
        assert (
            emotions[1]["emotion"] == "спокойствие"
        ), f"❌ Неожиданная эмоция: {emotions[1]['emotion']}"

        # Проверяем вызов метода
        mock_datomic.get_emotion_history.assert_called_once_with(
            user_id="user-123", limit=10
        )

        print("✅ Запрос данных работает корректно")
        return True

    except Exception as e:
        print(f"❌ Ошибка тестирования запроса данных: {e}")
        return False


def test_database_adapter_health():
    """Тест проверки здоровья."""
    print("🧪 Тестирование проверки здоровья...")

    try:
        from database_adapter import DatabaseAdapter

        adapter = DatabaseAdapter(auto_connect=False)
        adapter.datomic_available = True
        adapter.neo4j_available = True
        adapter.stats = {
            "datomic_queries": 5,
            "neo4j_queries": 3,
            "fallback_uses": 0,
            "errors": 0,
        }

        health = adapter.get_health_status()

        assert (
            health["status"] == "healthy"
        ), f"❌ Неожиданный статус: {health['status']}"
        assert (
            health["databases"]["datomic"]["available"] is True
        ), "❌ Datomic должен быть доступен"
        assert (
            health["databases"]["neo4j"]["available"] is True
        ), "❌ Neo4j должен быть доступен"
        assert (
            health["stats"]["datomic_queries"] == 5
        ), "❌ Неожиданное количество Datomic запросов"

        # Тест нездорового состояния
        adapter.datomic_available = False
        adapter.neo4j_available = False

        health = adapter.get_health_status()
        assert (
            health["status"] == "unhealthy"
        ), f"❌ При недоступности БД статус должен быть unhealthy"

        print("✅ Проверка здоровья работает корректно")
        return True

    except Exception as e:
        print(f"❌ Ошибка тестирования здоровья: {e}")
        return False


def test_api_endpoints_structure():
    """Тест структуры API endpoints."""
    print("🧪 Тестирование структуры API endpoints...")

    try:
        from database_api import router

        # Проверяем, что роутер создан
        assert router is not None, "❌ Router не создан"
        assert (
            router.prefix == "/api/database"
        ), f"❌ Неожиданный prefix: {router.prefix}"

        # Проверяем, что есть нужные routes
        route_paths = [route.path for route in router.routes]

        expected_paths = ["/store", "/query", "/health", "/data-types", "/examples"]
        for path in expected_paths:
            assert path in route_paths, f"❌ Отсутствует route: {path}"

        print("✅ Структура API endpoints корректна")
        return True

    except Exception as e:
        print(f"❌ Ошибка тестирования API структуры: {e}")
        return False


async def run_all_tests():
    """Запуск всех тестов."""
    print("🚀 Запуск тестов Database API...")
    print("=" * 50)

    tests = [
        ("Роутинг DatabaseAdapter", test_database_adapter_routing),
        ("Fallback механизм", test_database_adapter_fallback),
        ("Сохранение данных", test_database_adapter_store_data),
        ("Запрос данных", test_database_adapter_query_data),
        ("Проверка здоровья", test_database_adapter_health),
        ("Структура API", test_api_endpoints_structure),
    ]

    passed = 0
    failed = 0

    for test_name, test_func in tests:
        print(f"\n📋 {test_name}:")
        try:
            if asyncio.iscoroutinefunction(test_func):
                result = await test_func()
            else:
                result = test_func()

            if result:
                passed += 1
            else:
                failed += 1
        except Exception as e:
            print(f"❌ Критическая ошибка в тесте {test_name}: {e}")
            failed += 1

    print("\n" + "=" * 50)
    print(f"📊 Результаты тестирования:")
    print(f"✅ Пройдено: {passed}")
    print(f"❌ Провалено: {failed}")
    print(f"📈 Успешность: {passed/(passed+failed)*100:.1f}%")

    if failed == 0:
        print("🎉 Все тесты пройдены успешно!")
        print("🚀 Database API готов к использованию!")
    else:
        print("⚠️ Некоторые тесты провалились. Требуется доработка.")

    return failed == 0


if __name__ == "__main__":
    # Запуск тестов
    try:
        success = asyncio.run(run_all_tests())
        sys.exit(0 if success else 1)
    except KeyboardInterrupt:
        print("\n⏹️ Тестирование прервано пользователем")
        sys.exit(1)
    except Exception as e:
        print(f"\n💥 Критическая ошибка: {e}")
        sys.exit(1)
