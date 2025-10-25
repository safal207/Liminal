"""
GraphQL схема для PersonalityAdapter.
Определяет типы данных и запросы для персонализации и рекомендаций.
"""
import strawberry
from typing import List, Optional
from datetime import datetime

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
        return await adapter.get_profile()

    @strawberry.field
    async def recommendations(
        self,
        user_id: str, 
        limit: int = 5,
        context: Optional[str] = None
    ) -> List[Recommendation]:
        """Получить рекомендации для пользователя."""
        # Резолвер будет реализован в resolvers.py
        from .adapter import PersonalityAdapter
        adapter = PersonalityAdapter(user_id)
        return await adapter.get_recommendations(limit, context)

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
        context: Optional[str] = None
    ) -> Emotion:
        """Сохранить эмоциональное состояние пользователя."""
        from .adapter import PersonalityAdapter
        adapter = PersonalityAdapter(user_id)
        return await adapter.store_emotion(emotion_type, intensity, context)

    @strawberry.mutation
    async def update_preference(
        self,
        user_id: str,
        preference: PreferenceInput
    ) -> Preference:
        """Обновить предпочтение пользователя."""
        from .adapter import PersonalityAdapter
        adapter = PersonalityAdapter(user_id)
        return await adapter.update_preference(
            preference.category, 
            preference.value, 
            preference.strength
        )

@strawberry.type
class Subscription:
    @strawberry.subscription
    async def recommendation_updates(self, user_id: str) -> Recommendation:
        """Подписка на обновления рекомендаций в реальном времени."""
        # Будет реализовано позже с использованием асинхронных генераторов
        pass

# Создаем схему с поддержкой запросов, мутаций и подписок
schema = strawberry.Schema(
    query=Query, 
    mutation=Mutation,
    subscription=Subscription
)
