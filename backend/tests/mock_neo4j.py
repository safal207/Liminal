"""
Мок для neo4j_writer для тестирования.
"""

from datetime import datetime
from typing import Any, Dict, Optional


class MockNeo4jWriter:
    """Мок-класс для Neo4jWriter."""

    def __init__(self, *args, **kwargs):
        self.nodes = []
        self.relationships = []

    def create_dunewave_node(self, wave_data: Dict[str, Any]) -> Optional[Dict]:
        """Создает мок-узел DuneWave."""
        node = {
            "id": wave_data.get("id", f"wave_{len(self.nodes) + 1}"),
            "type": "DuneWave",
            "timestamp": datetime.utcnow().isoformat(),
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
            "timestamp": datetime.utcnow().isoformat(),
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

    def close(self):
        """Закрывает соединение (заглушка)."""


# Глобальный экземпляр мока
mock_writer = MockNeo4jWriter()
