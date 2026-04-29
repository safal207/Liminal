"""
GraphQL схема для PersonalityAdapter.
Определяет типы данных и запросы для персонализации и рекомендаций.
"""

from datetime import datetime
from typing import Any, AsyncIterator, Dict, List, Optional

import strawberry
from strawberry.schema.config import StrawberryConfig


def _parse_datetime(value: Any) -> datetime:
    if isinstance(value, datetime):
        return value
    if isinstance(value, str):
        s = value.strip()
        if s.endswith("Z"):
            s = s[:-1] + "+00:00"
        return datetime.fromisoformat(s)
    return datetime.utcnow()


def emotion_from_payload(payload: Dict[str, Any]) -> "Emotion":
    return Emotion(
        type=str(payload.get("type", "нейтральность")),
        intensity=float(payload.get("intensity", 0.0)),
        context=payload.get("context"),
        timestamp=_parse_datetime(payload.get("timestamp")),
    )


def preference_from_payload(payload: Dict[str, Any]) -> "Preference":
    return Preference(
        category=str(payload["category"]),
        value=str(payload["value"]),
        strength=float(payload["strength"]),
        last_updated=_parse_datetime(payload.get("last_updated")),
    )


def recommendation_from_payload(payload: Dict[str, Any]) -> "Recommendation":
    return Recommendation(
        id=str(payload["id"]),
        content=str(payload["content"]),
        confidence=float(payload["confidence"]),
        source=str(payload["source"]),
        context=payload.get("context"),
        created_at=_parse_datetime(payload.get("created_at")),
    )


def personality_profile_from_payload(data: Dict[str, Any]) -> "PersonalityProfile":
    return PersonalityProfile(
        user_id=str(data["user_id"]),
        emotional_history=[
            emotion_from_payload(x) for x in data.get("emotional_history", [])
        ],
        preferences=[preference_from_payload(x) for x in data.get("preferences", [])],
        recommendations=[
            recommendation_from_payload(x) for x in data.get("recommendations", [])
        ],
    )


@strawberry.type
class Emotion:
    """Эмоциональное состояние пользователя в определенный момент времени."""

    type: str
    intensity: float
    context: Optional[str] = None
    timestamp: datetime


@strawberry.type
class Preference:
    """Предпочтение пользователя в определенной категории."""

    category: str
    value: str
    strength: float
    last_updated: datetime


@strawberry.type
class Recommendation:
    """Рекомендация, сгенерированная на основе профиля пользователя."""

    id: str
    content: str
    confidence: float
    source: str  # "emotional", "preference" или "hybrid"
    context: Optional[str] = None
    created_at: datetime = strawberry.field(default_factory=datetime.now)


@strawberry.type
class RelationshipHealth:
    """Метрика здоровья отношений."""

    score: float


@strawberry.type
class PersonalityProfile:
    """Полный профиль личности пользователя."""

    user_id: str
    emotional_history: List[Emotion]
    preferences: List[Preference]
    recommendations: List[Recommendation]

    @strawberry.field
    def current_mood(self) -> Optional[Emotion]:
        """Текущее эмоциональное состояние пользователя."""
        return self.emotional_history[-1] if self.emotional_history else None

    @strawberry.field
    def dominant_preferences(self, limit: int = 3) -> List[Preference]:
        """Наиболее сильные предпочтения пользователя."""
        sorted_prefs = sorted(self.preferences, key=lambda p: p.strength, reverse=True)
        return sorted_prefs[:limit]


@strawberry.input
class PreferenceInput:
    """Входные данные для обновления предпочтения."""

    category: str
    value: str
    strength: float


@strawberry.type
class Query:
    @strawberry.field
    async def personality_profile(self, user_id: str) -> PersonalityProfile:
        """Получить полный профиль личности пользователя."""
        # Резолвер будет реализован в resolvers.py
        from .adapter import PersonalityAdapter

        adapter = PersonalityAdapter(user_id)
        raw = await adapter.get_profile()
        return personality_profile_from_payload(raw)

    @strawberry.field
    async def recommendations(
        self, user_id: str, limit: int = 5, context: Optional[str] = None
    ) -> List[Recommendation]:
        """Получить рекомендации для пользователя."""
        # Резолвер будет реализован в resolvers.py
        from .adapter import PersonalityAdapter

        adapter = PersonalityAdapter(user_id)
        raw_list = await adapter.get_recommendations(limit, context)
        return [recommendation_from_payload(x) for x in raw_list]

    @strawberry.field
    async def compute_relationship_health(
        self,
        source_id: str,
        target_id: str,
    ) -> "RelationshipHealth":
        """Вычислить здоровье отношений между двумя узлами.

        Имя поля в GraphQL будет computeRelationshipHealth благодаря auto_camel_case.
        """
        # Импортируем сервис на уровне функции, чтобы избежать циклических зависимостей при импорте
        from backend.metrics.relationship_health import (
            compute_relationship_health as compute,
        )

        score = compute(source_id, target_id)
        return RelationshipHealth(score=score)


@strawberry.type
class Mutation:
    @strawberry.mutation
    async def store_emotion(
        self,
        user_id: str,
        emotion_type: str,
        intensity: float,
        context: Optional[str] = None,
    ) -> Emotion:
        """Сохранить эмоциональное состояние пользователя."""
        from .adapter import PersonalityAdapter

        adapter = PersonalityAdapter(user_id)
        raw = await adapter.store_emotion(emotion_type, intensity, context)
        return emotion_from_payload(raw)

    @strawberry.mutation
    async def update_preference(
        self, user_id: str, preference: PreferenceInput
    ) -> Preference:
        """Обновить предпочтение пользователя."""
        from .adapter import PersonalityAdapter

        adapter = PersonalityAdapter(user_id)
        raw = await adapter.update_preference(
            preference.category,
            preference.value,
            preference.strength,
        )
        return preference_from_payload(raw)


@strawberry.type
class Subscription:
    @strawberry.subscription
    async def recommendation_updates(
        self, user_id: str
    ) -> AsyncIterator[Recommendation]:
        """Подписка-заглушка (событий пока нет)."""
        if False:  # pragma: no cover
            yield Recommendation(
                id="_",
                content="_",
                confidence=0.0,
                source="_",
                context=None,
                created_at=datetime.utcnow(),
            )


# Создаем схему с поддержкой запросов, мутаций и подписок
schema = strawberry.Schema(
    query=Query,
    mutation=Mutation,
    subscription=Subscription,
    config=StrawberryConfig(auto_camel_case=True),
)
