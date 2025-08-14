# LIMINAL Backend Prototype
# Полный поток: пульс -> Redis -> InfluxDB -> анализ -> Neo4j/Datomic

import asyncio
import json
import logging
import random
import time
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional


# Mock dependencies для прототипа
class MockRedis:
    def __init__(self):
        self.data = {}
        self.pubsub_channels = {}

    async def set(self, key: str, value: str):
        self.data[key] = value
        # Публикуем в канал если есть подписчики
        if key.startswith("biometric:"):
            await self.publish("biometric_stream", value)

    async def get(self, key: str) -> Optional[str]:
        return self.data.get(key)

    async def publish(self, channel: str, message: str):
        if channel in self.pubsub_channels:
            for callback in self.pubsub_channels[channel]:
                await callback(message)

    def subscribe(self, channel: str, callback):
        if channel not in self.pubsub_channels:
            self.pubsub_channels[channel] = []
        self.pubsub_channels[channel].append(callback)


class MockInfluxDB:
    def __init__(self):
        self.points = []

    async def write_point(
        self, measurement: str, tags: Dict, fields: Dict, timestamp: datetime
    ):
        point = {
            "measurement": measurement,
            "tags": tags,
            "fields": fields,
            "timestamp": timestamp.isoformat(),
        }
        self.points.append(point)
        print(f"📊 InfluxDB: {measurement} - {fields}")

    async def query_recent(self, measurement: str, limit: int = 10) -> List[Dict]:
        # Возвращаем последние точки для измерения
        recent = [p for p in self.points if p["measurement"] == measurement][-limit:]
        return recent


class MockNeo4j:
    def __init__(self):
        self.nodes = {}
        self.relationships = []

    async def create_node(self, label: str, properties: Dict):
        node_id = f"{label}_{len(self.nodes)}"
        self.nodes[node_id] = {"label": label, "properties": properties}
        print(f"🧠 Neo4j: Создан узел {label} - {properties}")
        return node_id

    async def create_relationship(
        self, from_node: str, to_node: str, rel_type: str, properties: Dict = None
    ):
        rel = {
            "from": from_node,
            "to": to_node,
            "type": rel_type,
            "properties": properties or {},
        }
        self.relationships.append(rel)
        print(f"🔗 Neo4j: Связь {rel_type}: {from_node} -> {to_node}")


class MockDatomic:
    def __init__(self):
        self.facts = []
        self.transaction_id = 0

    async def transact(self, facts: List[Dict]):
        self.transaction_id += 1
        timestamp = datetime.now(timezone.utc)

        for fact in facts:
            fact["tx_id"] = self.transaction_id
            fact["tx_time"] = timestamp
            self.facts.append(fact)

        print(f"🕰️ Datomic: Транзакция {self.transaction_id} - {len(facts)} фактов")

    async def query_entity(self, entity_id: str) -> List[Dict]:
        return [f for f in self.facts if f.get("entity") == entity_id]


# Core Services


class BiometricService:
    """💓 Biometric Service - обработка пульса и биометрики"""

    def __init__(self, redis: MockRedis, influx: MockInfluxDB):
        self.redis = redis
        self.influx = influx
        self.user_sessions = {}

    async def process_heartrate(
        self, user_id: str, heart_rate: int, timestamp: datetime = None
    ):
        if timestamp is None:
            timestamp = datetime.now(timezone.utc)

        # Сохраняем в Redis для быстрого доступа
        session_key = f"biometric:{user_id}:current"
        biometric_data = {
            "user_id": user_id,
            "heart_rate": heart_rate,
            "timestamp": timestamp.isoformat(),
            "type": "heartrate",
        }

        await self.redis.set(session_key, json.dumps(biometric_data))

        # Сохраняем в InfluxDB для анализа временных рядов
        await self.influx.write_point(
            measurement="biometrics",
            tags={"user_id": user_id, "type": "heartrate"},
            fields={"value": heart_rate},
            timestamp=timestamp,
        )

        # Вычисляем дополнительные метрики
        await self._calculate_derived_metrics(user_id, heart_rate, timestamp)

        return biometric_data

    async def _calculate_derived_metrics(
        self, user_id: str, heart_rate: int, timestamp: datetime
    ):
        # Простой анализ состояния на основе пульса
        if heart_rate < 60:
            state = "relaxed"
            stress_level = max(0, 40 - heart_rate)
        elif heart_rate > 90:
            state = "excited"
            stress_level = min(100, heart_rate - 60)
        else:
            state = "normal"
            stress_level = abs(heart_rate - 75) * 2

        # Сохраняем производные метрики
        await self.influx.write_point(
            measurement="psychological_state",
            tags={"user_id": user_id},
            fields={
                "state": state,
                "stress_level": stress_level,
                "coherence": random.uniform(0.3, 0.9),  # Заглушка для HRV
            },
            timestamp=timestamp,
        )


class PythiaAI:
    """🧠 Пифия AI - анализ паттернов и генерация инсайтов"""

    def __init__(self, influx: MockInfluxDB, neo4j: MockNeo4j, datomic: MockDatomic):
        self.influx = influx
        self.neo4j = neo4j
        self.datomic = datomic
        self.pattern_threshold = 3  # Минимум точек для паттерна

    async def analyze_biometric_patterns(self, user_id: str) -> List[Dict]:
        # Получаем последние биометрические данные
        recent_bio = await self.influx.query_recent("biometrics", limit=10)
        recent_psych = await self.influx.query_recent("psychological_state", limit=10)

        insights = []

        if len(recent_bio) >= self.pattern_threshold:
            # Анализируем тренды пульса
            heart_rates = [p["fields"]["value"] for p in recent_bio]
            insight = await self._analyze_heartrate_trend(user_id, heart_rates)
            if insight:
                insights.append(insight)

        if len(recent_psych) >= self.pattern_threshold:
            # Анализируем психологические состояния
            states = [p["fields"]["state"] for p in recent_psych]
            insight = await self._analyze_state_pattern(user_id, states)
            if insight:
                insights.append(insight)

        # Сохраняем инсайты в граф и временную базу
        for insight in insights:
            await self._store_insight(user_id, insight)

        return insights

    async def _analyze_heartrate_trend(
        self, user_id: str, heart_rates: List[int]
    ) -> Optional[Dict]:
        if len(heart_rates) < 3:
            return None

        # Простой анализ тренда
        recent_avg = sum(heart_rates[-3:]) / 3
        older_avg = sum(heart_rates[:-3]) / max(1, len(heart_rates) - 3)

        if recent_avg - older_avg > 10:
            return {
                "type": "heartrate_spike",
                "message": "Замечен резкий рост пульса. Возможно, стоит сделать глубокий вдох?",
                "confidence": 0.8,
                "data": {"trend": "increasing", "delta": recent_avg - older_avg},
            }
        elif older_avg - recent_avg > 10:
            return {
                "type": "heartrate_calm",
                "message": "Пульс успокаивается. Вы входите в состояние глубокого покоя.",
                "confidence": 0.9,
                "data": {"trend": "decreasing", "delta": older_avg - recent_avg},
            }

        return None

    async def _analyze_state_pattern(
        self, user_id: str, states: List[str]
    ) -> Optional[Dict]:
        # Анализ паттернов состояний
        if len(states) < 3:
            return None

        # Ищем циклы или устойчивые состояния
        if states[-3:] == ["excited", "normal", "relaxed"]:
            return {
                "type": "natural_cycle",
                "message": "Вы проходите естественный цикл возбуждение → норма → покой. Отличная саморегуляция!",
                "confidence": 0.95,
                "data": {"pattern": "descending_cycle"},
            }

        if all(s == "relaxed" for s in states[-3:]):
            return {
                "type": "deep_relaxation",
                "message": "Достигнуто состояние глубокого расслабления. Идеальное время для медитации или творчества.",
                "confidence": 0.85,
                "data": {"pattern": "sustained_relaxation"},
            }

        return None

    async def _store_insight(self, user_id: str, insight: Dict):
        # Создаем узел инсайта в Neo4j
        insight_node = await self.neo4j.create_node(
            "Insight",
            {
                "type": insight["type"],
                "message": insight["message"],
                "confidence": insight["confidence"],
                "timestamp": datetime.now(timezone.utc).isoformat(),
            },
        )

        # Связываем с пользователем
        user_node = f"Person_{user_id}"
        await self.neo4j.create_relationship(
            user_node,
            insight_node,
            "HAS_INSIGHT",
            {"created_at": datetime.now(timezone.utc).isoformat()},
        )

        # Сохраняем в Datomic для временного анализа
        await self.datomic.transact(
            [
                {
                    "entity": f"insight_{insight_node}",
                    "attribute": "insight/type",
                    "value": insight["type"],
                },
                {
                    "entity": f"insight_{insight_node}",
                    "attribute": "insight/message",
                    "value": insight["message"],
                },
                {
                    "entity": f"insight_{insight_node}",
                    "attribute": "insight/user",
                    "value": user_id,
                },
                {
                    "entity": f"insight_{insight_node}",
                    "attribute": "insight/confidence",
                    "value": insight["confidence"],
                },
            ]
        )


class MorpheusNavigator:
    """🌌 Морфеус - проводник переходов"""

    def __init__(self, neo4j: MockNeo4j, datomic: MockDatomic):
        self.neo4j = neo4j
        self.datomic = datomic

    async def suggest_transition(
        self, user_id: str, current_insights: List[Dict]
    ) -> Optional[Dict]:
        if not current_insights:
            return None

        # На основе инсайтов предлагаем переходы
        latest_insight = current_insights[-1]

        transitions = {
            "heartrate_spike": {
                "action": "breathing_exercise",
                "message": "Рекомендую технику 4-7-8: вдох на 4, задержка на 7, выдох на 8",
                "duration_minutes": 5,
            },
            "heartrate_calm": {
                "action": "mindful_moment",
                "message": "Отличное время для mindfulness практики или творческой работы",
                "duration_minutes": 10,
            },
            "deep_relaxation": {
                "action": "meditation_or_creation",
                "message": "Входите в поток. Время для глубокой медитации или творческого процесса",
                "duration_minutes": 20,
            },
            "natural_cycle": {
                "action": "observe_and_integrate",
                "message": "Понаблюдайте за своим естественным ритмом. Какие паттерны вы замечаете?",
                "duration_minutes": 3,
            },
        }

        transition = transitions.get(latest_insight["type"])
        if transition:
            # Сохраняем предложенный переход
            await self._store_transition(user_id, transition, latest_insight)

        return transition

    async def _store_transition(
        self, user_id: str, transition: Dict, triggering_insight: Dict
    ):
        # Создаем узел перехода в Neo4j
        transition_node = await self.neo4j.create_node(
            "Transition",
            {
                "action": transition["action"],
                "message": transition["message"],
                "duration_minutes": transition["duration_minutes"],
                "timestamp": datetime.now(timezone.utc).isoformat(),
            },
        )

        # Связываем переход с пользователем и инсайтом
        user_node = f"Person_{user_id}"
        await self.neo4j.create_relationship(
            user_node, transition_node, "SUGGESTED_TRANSITION"
        )


# API Gateway Mock
class LiminalGateway:
    """🌉 API Gateway - точка входа в систему"""

    def __init__(self):
        # Инициализация всех сервисов
        self.redis = MockRedis()
        self.influx = MockInfluxDB()
        self.neo4j = MockNeo4j()
        self.datomic = MockDatomic()

        self.biometric = BiometricService(self.redis, self.influx)
        self.pythia = PythiaAI(self.influx, self.neo4j, self.datomic)
        self.morpheus = MorpheusNavigator(self.neo4j, self.datomic)

        # Подписываемся на биометрический поток
        self.redis.subscribe("biometric_stream", self._on_biometric_data)

    async def _on_biometric_data(self, message: str):
        """Обработчик новых биометрических данных"""
        data = json.loads(message)
        user_id = data["user_id"]

        print(f"🔔 Новые биометрические данные для {user_id}")

        # Запускаем анализ через 2 секунды (имитация real-time обработки)
        await asyncio.sleep(2)
        await self._trigger_analysis(user_id)

    async def _trigger_analysis(self, user_id: str):
        """Запуск полного цикла анализа и генерации инсайтов"""
        print(f"\n🧠 Запуск анализа для пользователя {user_id}")

        # Пифия анализирует паттерны
        insights = await self.pythia.analyze_biometric_patterns(user_id)

        if insights:
            print(f"💡 Сгенерировано инсайтов: {len(insights)}")
            for insight in insights:
                print(f"   - {insight['message']}")

            # Морфеус предлагает переходы
            transition = await self.morpheus.suggest_transition(user_id, insights)
            if transition:
                print(f"🚀 Предложен переход: {transition['action']}")
                print(f"   {transition['message']}")
        else:
            print("📊 Недостаточно данных для анализа")

    # Public API methods

    async def submit_heartrate(self, user_id: str, heart_rate: int):
        """Внешний API для отправки пульса"""
        print(f"\n📱 Получен пульс от пользователя {user_id}: {heart_rate} BPM")
        return await self.biometric.process_heartrate(user_id, heart_rate)

    async def get_user_insights(self, user_id: str) -> List[Dict]:
        """Получить последние инсайты пользователя"""
        return await self.datomic.query_entity(f"insight_{user_id}")


# Демонстрация полного потока
async def demo_full_flow():
    """🎭 Демонстрация работы полного потока LIMINAL"""

    print("🏗️ Инициализация LIMINAL Gateway...")
    gateway = LiminalGateway()

    print("\n" + "=" * 60)
    print("🎯 ДЕМОНСТРАЦИЯ ПОЛНОГО ПОТОКА: пульс -> инсайт -> переход")
    print("=" * 60)

    user_id = "demo_user"

    # Создаем узел пользователя
    await gateway.neo4j.create_node("Person", {"user_id": user_id, "name": "Demo User"})

    # Симулируем последовательность измерений пульса
    heartrate_sequence = [
        (85, "Начальное состояние"),
        (95, "Небольшое возбуждение"),
        (105, "Пик активности"),
        (90, "Начало успокоения"),
        (75, "Возвращение к норме"),
        (65, "Глубокое расслабление"),
        (60, "Состояние покоя"),
    ]

    for i, (hr, description) in enumerate(heartrate_sequence):
        print(f"\n📍 Шаг {i+1}: {description}")
        await gateway.submit_heartrate(user_id, hr)

        # Даем время на обработку
        await asyncio.sleep(3)

        if i >= 2:  # Начинаем анализ после накопления данных
            print("⏳ Ожидание анализа...")
            await asyncio.sleep(2)

    print(f"\n🏁 Демонстрация завершена!")
    print(f"📊 Всего точек в InfluxDB: {len(gateway.influx.points)}")
    print(f"🧠 Узлов в Neo4j: {len(gateway.neo4j.nodes)}")
    print(f"🕰️ Фактов в Datomic: {len(gateway.datomic.facts)}")


if __name__ == "__main__":
    # Настройка логирования
    logging.basicConfig(level=logging.INFO)

    # Запуск демонстрации
    asyncio.run(demo_full_flow())
