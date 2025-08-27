"""
Модуль для работы с Neo4j (мок для тестирования).
"""

from datetime import datetime
from typing import Any, Dict, List, Optional


class Neo4jDateTime:
    """Мок-класс для имитации даты/времени Neo4j."""

    def __init__(self, value: datetime = None):
        self._value = value if value is not None else datetime.utcnow()

    def isoformat(self) -> str:
        """Возвращает ISO-форматированную строку времени."""
        # Возвращаем время в формате ISO 8601 с 'Z' для UTC
        return self._value.isoformat(timespec="milliseconds") + "Z"

    @classmethod
    def now(cls):
        return cls(datetime.utcnow())


class Neo4jWriter:
    """Мок-класс для работы с Neo4j."""

    def __init__(self, uri: str = None, user: str = None, password: str = None):
        self.uri = uri or "bolt://localhost:7687"
        self.user = user or "neo4j"
        self.password = password or "password"
        self._connected = True
        self.nodes = []
        self.relationships = []

    def create_dunewave_node(self, wave_data: Dict[str, Any]) -> Optional[Dict]:
        """Создает мок-узел DuneWave."""
        node = {
            "id": wave_data.get("id", f"wave_{len(self.nodes) + 1}"),
            "type": "DuneWave",
            "timestamp": Neo4jDateTime.now().isoformat(),
            **wave_data,
        }
        self.nodes.append(node)
        return node

    def create_memory_fragment_node(
        self, memory_data: Dict[str, Any]
    ) -> Optional[Dict]:
        """Создает мок-узел MemoryFragment."""
        node = {
            "id": memory_data.get("id", f"mem_{len(self.nodes) + 1}"),
            "type": "MemoryFragment",
            "timestamp": Neo4jDateTime.now().isoformat(),
            **memory_data,
        }
        self.nodes.append(node)
        return node

    def link_dunewave_to_memory(self, wave_id: str, memory_id: str) -> bool:
        """Создает мок-связь между DuneWave и MemoryFragment."""
        self.relationships.append(
            {"from": wave_id, "to": memory_id, "type": "CONTAINS"}
        )
        return True

    def get_memories(self, limit: int = 10) -> List[Dict]:
        """Возвращает список воспоминаний."""
        return [n for n in self.nodes if n.get("type") == "MemoryFragment"][:limit]

    def get_waves(self, limit: int = 10) -> List[Dict]:
        """Возвращает список волн."""
        return [n for n in self.nodes if n.get("type") == "DuneWave"][:limit]

    def create_mentorship(self, younger_id: str, mentor_id: str) -> bool:
        """Создает мок-связь наставничества."""
        self.relationships.append(
            {"from": mentor_id, "to": younger_id, "type": "MENTORS"}
        )
        return True

    def close(self):
        """Закрывает соединение (заглушка)."""
        self._connected = False


# Класс с поддержкой времени для обратной совместимости
class Neo4jWriterTime(Neo4jWriter):
    """Алиас для обратной совместимости."""
