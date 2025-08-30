#!/usr/bin/env python3

"""
Тест подключения к Neo4j Docker контейнеру
"""

import sys

from neo4j import GraphDatabase

print("=== Neo4j Docker Connection Test ===")

try:
    print("Попытка подключения к Neo4j на localhost:7687...")
    driver = GraphDatabase.driver("bolt://localhost:7687", auth=("neo4j", "NewStrongPass123!"))
    print("Подключение установлено!")

    with driver.session() as session:
        result = session.run("RETURN 1 as num")
        value = result.single()["num"]
        print(f"Результат запроса: {value}")

    print("Проверка версии Neo4j...")
    with driver.session() as session:
        result = session.run("CALL dbms.components() YIELD name, versions RETURN name, versions")
        for record in result:
            print(f"{record['name']}: {record['versions'][0]}")

    driver.close()
    print("Тест успешно завершен!")

except Exception as e:
    print(f"ОШИБКА: {e}")
    import traceback

    traceback.print_exc()
    sys.exit(1)
