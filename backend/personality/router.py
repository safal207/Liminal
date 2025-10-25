"""
FastAPI роутер для интеграции GraphQL схемы PersonalityAdapter.
"""
from fastapi import APIRouter, Depends
graphql_app = None
try:  # Защита CI от отсутствия extra-зависимостей strawberry[fastapi]
    from strawberry.fastapi import GraphQLRouter
    from strawberry.schema.config import StrawberryConfig
    from .schema import schema

    # Создаем GraphQL роутер с настройками
    graphql_app = GraphQLRouter(
        schema,
        graphiql=True,  # Включаем GraphiQL интерфейс для тестирования
        context_getter=lambda: {"request": None},  # Будет расширено для передачи контекста
        config=StrawberryConfig(auto_camel_case=True),
    )
except Exception:  # pragma: no cover
    graphql_app = None
from ..auth.jwt_utils import get_current_user, User

# Создаем FastAPI роутер
router = APIRouter(prefix="/personality", tags=["Personality"])

# Добавляем GraphQL эндпоинт, если зависимости доступны
if graphql_app is not None:
    router.include_router(graphql_app, prefix="/graphql")

# Добавляем REST эндпоинты для совместимости
@router.post("/emotion")
async def store_emotion(
    emotion_type: str,
    intensity: float,
    context: str = None,
    user: User = Depends(get_current_user)
):
    """
    REST эндпоинт для сохранения эмоционального состояния.
    
    Args:
        emotion_type: Тип эмоции
        intensity: Интенсивность от 0.0 до 1.0
        context: Контекст эмоции (опционально)
        user: Текущий пользователь (из JWT)
        
    Returns:
        Сохраненная эмоция
    """
    from .adapter import PersonalityAdapter
    adapter = PersonalityAdapter(user.id)
    return await adapter.store_emotion(emotion_type, intensity, context)

@router.get("/profile")
async def get_profile(user: User = Depends(get_current_user)):
    """
    REST эндпоинт для получения профиля пользователя.
    
    Args:
        user: Текущий пользователь (из JWT)
        
    Returns:
        Профиль пользователя
    """
    from .adapter import PersonalityAdapter
    adapter = PersonalityAdapter(user.id)
    return await adapter.get_profile()

@router.get("/recommendations")
async def get_recommendations(
    limit: int = 5,
    context: str = None,
    user: User = Depends(get_current_user)
):
    """
    REST эндпоинт для получения рекомендаций.
    
    Args:
        limit: Максимальное количество рекомендаций
        context: Контекст для фильтрации рекомендаций
        user: Текущий пользователь (из JWT)
        
    Returns:
        Список рекомендаций
    """
    from .adapter import PersonalityAdapter
    adapter = PersonalityAdapter(user.id)
    return await adapter.get_recommendations(limit, context)
