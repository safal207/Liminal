#!/usr/bin/env python3

"""
Тест подключения к Neo4j Docker с записью в файл
"""

import os
import sys
import traceback
from datetime import datetime

from neo4j import GraphDatabase

# Путь для файла журнала
log_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), "neo4j_test_output.log")


def log(message):
    """Записать сообщение в файл и вывести на экран"""
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    log_message = f"[{timestamp}] {message}"
    print(log_message)

    with open(log_file, "a", encoding="utf-8") as f:
        f.write(log_message + "\n")


def main():
    log("=== Neo4j Docker Connection Test ===")
    log(f"Python версия: {sys.version}")
    log(f"Текущая директория: {os.getcwd()}")

    try:
        uri = "bolt://localhost:7687"
        user = "neo4j"
        password = "NewStrongPass123!"

        log(f"Попытка подключения к Neo4j: {uri}")
        driver = GraphDatabase.driver(uri, auth=(user, password))

        # Проверка подключения
        driver.verify_connectivity()
        log("✓ Подключение установлено успешно!")

        # Простой запрос
        with driver.session() as session:
            result = session.run("RETURN 1 as num")
            value = result.single()["num"]
            log(f"✓ Результат запроса: {value}")

        # Проверка версии Neo4j
        log("Проверка версии Neo4j...")
        with driver.session() as session:
            result = session.run(
                "CALL dbms.components() YIELD name, versions RETURN name, versions"
            )
            for record in result:
                log(f"✓ {record['name']}: {record['versions'][0]}")

        # Закрытие соединения
        driver.close()
        log("✓ Тест успешно завершен!")
        return True

    except Exception as e:
        log(f"❌ ОШИБКА: {e}")
        log(traceback.format_exc())
        return False


if __name__ == "__main__":
    success = main()
    log(f"Результаты теста записаны в: {log_file}")
    if not success:
        sys.exit(1)
