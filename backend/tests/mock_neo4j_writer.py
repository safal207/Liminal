"""
Mock implementation of neo4j_writer for testing purposes.
This module will be used instead of the real neo4j_writer when running tests.
"""

from datetime import datetime
from typing import Any

print("MOCK: Using mock Neo4jWriter implementation")


class Neo4jDateTime:
    def __init__(self, year, month, day, hour=0, minute=0, second=0, nanosecond=0):
        self.year = year
        self.month = month
        self.day = day
        self.hour = hour
        self.minute = minute
        self.second = second
        self.nanosecond = nanosecond

    def to_native(self):
        return datetime(
            self.year,
            self.month,
            self.day,
            self.hour,
            self.minute,
            self.second,
            self.nanosecond // 1000,
        )


class Neo4jWriter:
    def __init__(self, uri=None, user=None, password=None):
        print("MOCK: Initializing mock Neo4jWriter")
        self.uri = uri or "mock://localhost:7687"
        self.user = user or "neo4j"
        self.password = password or "password"
        self.connected = False

    def connect(self):
        print("MOCK: Connecting to mock Neo4j")
        self.connected = True
        return self

    def close(self):
        print("MOCK: Closing mock Neo4j connection")
        self.connected = False

    def execute_query(self, query: str, parameters: dict | None = None, **kwargs) -> Any:
        print(f"MOCK: Executing query: {query[:100]}...")
        print(f"MOCK: With parameters: {parameters}")
        return []

    def __enter__(self):
        return self.connect()

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
        return False
