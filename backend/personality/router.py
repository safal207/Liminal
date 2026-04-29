"""FastAPI роутер для интеграции GraphQL схемы PersonalityAdapter."""

from __future__ import annotations

import logging
from dataclasses import dataclass
from importlib import import_module
from typing import TYPE_CHECKING, Optional

from fastapi import APIRouter, Depends, HTTPException, status

from backend.auth.dependencies import token_verifier


@dataclass
class PersonalityUser:
    """Минимальная модель пользователя для маршрутов персонализации."""

    id: str


async def personality_current_user(
    payload: dict = Depends(token_verifier),
) -> PersonalityUser:
    sub = payload.get("sub")
    if not sub:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Недействительный токен",
        )
    return PersonalityUser(id=str(sub))


if TYPE_CHECKING:  # pragma: no cover
    from strawberry.fastapi import GraphQLRouter as _GraphQLRouter


logger = logging.getLogger(__name__)


def _create_graphql_router() -> Optional["_GraphQLRouter"]:
    """Создает GraphQL роутер, если доступна зависимость strawberry."""

    try:
        strawberry_fastapi = import_module("strawberry.fastapi")
        GraphQLRouter = getattr(strawberry_fastapi, "GraphQLRouter")
        from .schema import schema

        return GraphQLRouter(
            schema,
            context_getter=lambda: {"request": None},
        )
    except ModuleNotFoundError as exc:  # pragma: no cover - зависит от окружения CI
        logger.warning(
            "Strawberry GraphQL dependency is missing. Personality GraphQL endpoint disabled: %s",
            exc,
        )
    except Exception as exc:  # pragma: no cover - защитный сценарий
        logger.error("Failed to initialize Strawberry GraphQL router: %s", exc)

    return None


graphql_app = _create_graphql_router()

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
    user: PersonalityUser = Depends(personality_current_user),
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
async def get_profile(user: PersonalityUser = Depends(personality_current_user)):
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
    user: PersonalityUser = Depends(personality_current_user),
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
