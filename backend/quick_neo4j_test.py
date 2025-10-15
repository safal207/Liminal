#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Тест подключения к Neo4j Docker контейнеру
"""

import sys
from typing import Optional

from neo4j import GraphDatabase


def run_check(uri: str = "bolt://localhost:7687", password: str = "NewStrongPass123!") -> bool:
    """Attempt to connect to Neo4j and run a simple query.

    Returns True when the query executes successfully.  Any failure is
    surfaced to the caller so CI can decide how to react.
    """

    print("=== Neo4j Docker Connection Test ===")
    print(f"Попытка подключения к Neo4j на {uri.split('://')[-1]}...")

    driver = GraphDatabase.driver(uri, auth=("neo4j", password))
    try:
        with driver.session() as session:
            result = session.run("RETURN 1 as num")
            value = result.single()["num"]
            print(f"Результат запроса: {value}")

        print("Проверка версии Neo4j...")
        with driver.session() as session:
            result = session.run(
                "CALL dbms.components() YIELD name, versions RETURN name, versions"
            )
            for record in result:
                print(f"{record['name']}: {record['versions'][0]}")
    finally:
        driver.close()

    print("Тест успешно завершен!")
    return True


def main(argv: Optional[list[str]] = None) -> int:
    try:
        run_check()
    except Exception as exc:  # pragma: no cover - diagnostic helper
        print(f"ОШИБКА: {exc}")
        import traceback

        traceback.print_exc()
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
