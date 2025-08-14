"""
Простой скрипт для проверки подключения к Neo4j
"""

import os
import sys
import time

from neo4j import GraphDatabase


def test_neo4j_connection(timeout=10):
    start_time = time.time()
    uri = os.getenv("NEO4J_URI", "bolt://localhost:7687")
    user = os.getenv("NEO4J_USER", "neo4j")
    password = os.getenv("NEO4J_PASSWORD", "NewStrongPass123!")

    print(f"🔌 Попытка подключения к Neo4j: {uri}")
    print(f"👤 Пользователь: {user}")
    print(f"🔑 Пароль: {'*' * len(password)}")

    try:
        # Устанавливаем таймаут для подключения
        print("Попытка подключения с таймаутом...")
        driver = GraphDatabase.driver(
            uri, auth=(user, password), connection_timeout=timeout
        )

        print("Driver создан, проверка подключения...")
        driver.verify_connectivity()

        print("Подключение проверено, выполнение запроса...")
        with driver.session() as session:
            result = session.run("MATCH (n) RETURN count(n) as count")
            count = result.single()["count"]
            print(f"✅ Neo4j подключение успешно! Найдено {count} узлов.")
        driver.close()
        return True
    except Exception as e:
        elapsed = time.time() - start_time
        print(f"❌ Ошибка подключения к Neo4j через {elapsed:.2f} сек: {str(e)}")
        return False


def check_neo4j_service():
    """Проверка статуса службы Neo4j"""
    try:
        # Проверяем запущена ли служба Neo4j на Windows
        print("Проверка статуса службы Neo4j...")
        os.system('sc query "Neo4j" >nul 2>&1')
        print("Проверка завершена")
    except Exception as e:
        print(f"Ошибка при проверке службы Neo4j: {e}")


if __name__ == "__main__":
    print("=== Диагностика Neo4j ===\n")
    check_neo4j_service()
    result = test_neo4j_connection(timeout=5)

    if not result:
        print("\nВозможные причины проблемы:")
        print("1. Neo4j не запущен - проверьте службу")
        print("2. Неверные учетные данные")
        print("3. Блокировка порта или сетевая проблема")

    print("\n=== Диагностика завершена ===\n")
    sys.exit(0 if result else 1)
