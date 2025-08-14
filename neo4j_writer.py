# backend/neo4j_writer.py

import json
from datetime import datetime
from typing import Any, Dict, List, Optional

from neo4j.time import DateTime as Neo4jDateTime

from neo4j import GraphDatabase


# Функция для сериализации объектов Neo4j
def neo4j_serializer(obj):
    if hasattr(obj, "isoformat"):
        return obj.isoformat()
    raise TypeError(f"Type {type(obj)} not serializable")


# Функция для преобразования записи Neo4j в словарь
def record_to_dict(record) -> Dict[str, Any]:
    if not record:
        return {}
    if isinstance(record, (list, tuple)):
        return [record_to_dict(r) for r in record]
    if hasattr(record, "items"):
        return {key: record_to_dict(value) for key, value in record.items()}
    if hasattr(record, "isoformat"):
        return record.isoformat()
    return record


class Neo4jWriter:
    def __init__(self, uri, user, password):
        self.driver = GraphDatabase.driver(uri, auth=(user, password))

    def create_indexes(self) -> None:
        """Создает индексы для ускорения поиска"""
        try:
            with self.driver.session() as session:
                # Индексы для ID
                session.run("CREATE INDEX IF NOT EXISTS FOR (d:DuneWave) ON (d.id)")
                session.run(
                    "CREATE INDEX IF NOT EXISTS FOR (m:MemoryFragment) ON (m.id)"
                )

                # Индексы для часто используемых полей
                session.run(
                    "CREATE INDEX IF NOT EXISTS FOR (d:DuneWave) ON (d.emotion)"
                )
                session.run(
                    "CREATE INDEX IF NOT EXISTS FOR (d:DuneWave) ON (d.timestamp)"
                )
                session.run(
                    "CREATE INDEX IF NOT EXISTS FOR (m:MemoryFragment) ON (m.type)"
                )

                # Ограничения уникальности
                session.run(
                    "CREATE CONSTRAINT IF NOT EXISTS FOR (d:DuneWave) REQUIRE d.id IS UNIQUE"
                )
                session.run(
                    "CREATE CONSTRAINT IF NOT EXISTS FOR (m:MemoryFragment) REQUIRE m.id IS UNIQUE"
                )

        except Exception as e:
            print(f"Ошибка при создании индексов: {e}")
            raise

    def close(self):
        self.driver.close()

    def create_dunewave_node(
        self, wave_data: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """Создает или обновляет узел DuneWave в Neo4j"""
        try:
            with self.driver.session() as session:
                query = """
                MERGE (dw:DuneWave {id: $id})
                SET dw.phase = $phase,
                    dw.emotion = $emotion,
                    dw.intensity = $intensity,
                    dw.context = $context,
                    dw.source = $source,
                    dw.timestamp = datetime($timestamp)
                RETURN dw
                """
                result = session.run(query, **wave_data)
                record = result.single()
                if record and "dw" in record:
                    return dict(record["dw"])
                return None
        except Exception as e:
            print(f"Ошибка при создании DuneWave: {e}")
            return None

    def create_memory_fragment_node(
        self, memory_data: Dict[str, Any]
    ) -> Optional[Dict[str, Any]]:
        """Создает или обновляет узел MemoryFragment в Neo4j"""
        try:
            with self.driver.session() as session:
                query = """
                MERGE (mf:MemoryFragment {id: $id})
                SET mf.content = $content,
                    mf.type = $type,
                    mf.growth_stage = $growth_stage,
                    mf.timestamp = datetime($timestamp)
                RETURN mf
                """
                result = session.run(query, **memory_data)
                record = result.single()
                if record and "mf" in record:
                    return dict(record["mf"])
                return None
        except Exception as e:
            print(f"Ошибка при создании MemoryFragment: {e}")
            return None

    def link_dunewave_to_memory(self, wave_id: str, memory_id: str) -> bool:
        """Создает связь между DuneWave и MemoryFragment"""
        try:
            with self.driver.session() as session:
                query = """
                MATCH (dw:DuneWave {id: $wave_id})
                MATCH (mf:MemoryFragment {id: $memory_id})
                MERGE (dw)-[r:CONTAINS]->(mf)
                SET r.linked_at = datetime()
                RETURN r
                """
                result = session.run(query, wave_id=wave_id, memory_id=memory_id)
                return result.single() is not None
        except Exception as e:
            print(f"Ошибка при связывании DuneWave и MemoryFragment: {e}")
            return False

    def create_mentorship(self, younger_id: str, mentor_id: str) -> bool:
        """Создает связь наставничества между MemoryFragment"""
        try:
            with self.driver.session() as session:
                query = """
                MATCH (young:MemoryFragment {id: $younger_id})
                MATCH (mentor:MemoryFragment {id: $mentor_id})
                MERGE (young)-[r:MENTORED_BY]->(mentor)
                SET r.created_at = datetime()
                RETURN r
                """
                result = session.run(query, younger_id=younger_id, mentor_id=mentor_id)
                return result.single() is not None
        except Exception as e:
            print(f"Ошибка при создании связи наставничества: {e}")
            return False

    def find_wisdom_fragments(
        self, emotion: Optional[str] = None, limit: int = 10
    ) -> List[Dict[str, Any]]:
        """Находит фрагменты мудрости по эмоции"""
        try:
            with self.driver.session() as session:
                query = """
                MATCH (d:DuneWave)
                WHERE $emotion IS NULL OR d.emotion = $emotion
                MATCH (d)-[:CONTAINS]->(m:MemoryFragment)
                RETURN m, d.emotion as emotion
                ORDER BY d.timestamp DESC
                LIMIT $limit
                """
                result = session.run(query, emotion=emotion, limit=limit)
                fragments = []
                for record in result:
                    fragment = dict(record["m"])
                    fragment["emotion"] = record["emotion"]
                    # Преобразуем объекты Neo4j DateTime в строки
                    for key, value in fragment.items():
                        if hasattr(value, "isoformat"):
                            fragment[key] = value.isoformat()
                    fragments.append(fragment)
                return fragments
        except Exception as e:
            print(f"Ошибка при поиске фрагментов мудрости: {e}")
            return []


# Пример использования
if __name__ == "__main__":
    # Инициализация подключения
    writer = Neo4jWriter(
        uri="bolt://localhost:7687", user="neo4j", password="NewStrongPass123!"
    )

    try:
        # Создаем индексы
        print("Создание индексов...")
        writer.create_indexes()

        # Примеры DuneWave
        waves = [
            {
                "id": "wave_001",
                "phase": "Synthesis",
                "emotion": "shame",
                "intensity": 0.72,
                "context": "внутренний конфликт",
                "source": "agent/aliosha",
                "timestamp": datetime.utcnow().isoformat(),
            },
            {
                "id": "wave_002",
                "phase": "Analysis",
                "emotion": "joy",
                "intensity": 0.85,
                "context": "достижение цели",
                "source": "user/123",
                "timestamp": datetime.utcnow().isoformat(),
            },
            {
                "id": "wave_003",
                "phase": "Reflection",
                "emotion": "curiosity",
                "intensity": 0.65,
                "context": "изучение нового",
                "source": "agent/mentor",
                "timestamp": datetime.utcnow().isoformat(),
            },
        ]

        # Создаем узлы DuneWave
        for wave in waves:
            node = writer.create_dunewave_node(wave)
            if node:
                print(f"Создан узел: {node['id']} - {node['emotion']}")
            else:
                print(f"Не удалось создать узел: {wave['id']}")

        # Примеры MemoryFragment
        memories = [
            {
                "id": "mem_001",
                "content": "Стыд трансформировался в принятие",
                "type": "emotion",
                "growth_stage": "sprout",
                "timestamp": datetime.utcnow().isoformat(),
            },
            {
                "id": "mem_002",
                "content": "Радость от достижения цели",
                "type": "achievement",
                "growth_stage": "bloom",
                "timestamp": datetime.utcnow().isoformat(),
            },
            {
                "id": "mem_003",
                "content": "Исследование новых возможностей",
                "type": "learning",
                "growth_stage": "seed",
                "timestamp": datetime.utcnow().isoformat(),
            },
        ]

        # Создаем узлы MemoryFragment
        for memory in memories:
            node = writer.create_memory_fragment_node(memory)
            if node:
                print(f"Создан MemoryFragment: {node['id']} - {node['type']}")
            else:
                print(f"Не удалось создать MemoryFragment: {memory['id']}")

        # Связываем узлы
        relationships = [
            ("wave_001", "mem_001"),
            ("wave_002", "mem_002"),
            ("wave_003", "mem_003"),
        ]

        for wave_id, mem_id in relationships:
            result = writer.link_dunewave_to_memory(wave_id, mem_id)
            if result:
                print(f"Связаны: {wave_id} -> {mem_id}")
            else:
                print(f"Не удалось связать: {wave_id} -> {mem_id}")

        # Создаем связи наставничества
        writer.create_mentorship("mem_001", "mem_002")
        writer.create_mentorship("mem_002", "mem_003")

        # Находим фрагменты мудрости
        print("\nПоиск фрагментов мудрости:")
        mentors = writer.find_wisdom_fragments(emotion="shame")
        for mentor in mentors:
            print(f"Найден наставник: {mentor['content']}")

    except Exception as e:
        print(f"Произошла ошибка: {e}")
    finally:
        writer.close()
