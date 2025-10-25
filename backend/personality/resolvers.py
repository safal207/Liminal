"""
Резолверы для GraphQL схемы PersonalityAdapter.
Реализуют бизнес-логику для запросов и мутаций.
"""
from typing import List, Dict, Any, Optional
import logging
from datetime import datetime

# Импорт адаптера
from .adapter import PersonalityAdapter

# Настройка логирования
logger = logging.getLogger("personality.resolvers")

async def resolve_personality_profile(user_id: str) -> Dict[str, Any]:
    """
    Резолвер для получения полного профиля пользователя.
    
    Args:
        user_id: Идентификатор пользователя
        
    Returns:
        Полный профиль пользователя
    """
    try:
        adapter = PersonalityAdapter(user_id)
        return await adapter.get_profile()
    except Exception as e:
        # "Случайности не случайны" - обрабатываем ошибки как точки роста
        logger.error(f"Ошибка при получении профиля: {str(e)}")
        return {
            "user_id": user_id,
            "emotional_history": [],
            "preferences": [],
            "recommendations": []
        }

async def resolve_recommendations(user_id: str, limit: int = 5, 
                               context: Optional[str] = None) -> List[Dict[str, Any]]:
    """
    Резолвер для получения рекомендаций.
    
    Args:
        user_id: Идентификатор пользователя
        limit: Максимальное количество рекомендаций
        context: Контекст для фильтрации рекомендаций
        
    Returns:
        Список рекомендаций
    """
    try:
        adapter = PersonalityAdapter(user_id)
        return await adapter.get_recommendations(limit, context)
    except Exception as e:
        logger.error(f"Ошибка при получении рекомендаций: {str(e)}")
        return []

async def resolve_store_emotion(user_id: str, emotion_type: str, 
                             intensity: float, context: Optional[str] = None) -> Dict[str, Any]:
    """
    Резолвер для сохранения эмоционального состояния.
    
    Args:
        user_id: Идентификатор пользователя
        emotion_type: Тип эмоции
        intensity: Интенсивность
        context: Контекст эмоции
        
    Returns:
        Сохраненная эмоция
    """
    try:
        adapter = PersonalityAdapter(user_id)
        return await adapter.store_emotion(emotion_type, intensity, context)
    except Exception as e:
        logger.error(f"Ошибка при сохранении эмоции: {str(e)}")
        return {
            "type": emotion_type,
            "intensity": intensity,
            "context": context or "",
            "timestamp": datetime.now().isoformat()
        }

async def resolve_update_preference(user_id: str, category: str, 
                                 value: str, strength: float) -> Dict[str, Any]:
    """
    Резолвер для обновления предпочтения.
    
    Args:
        user_id: Идентификатор пользователя
        category: Категория предпочтения
        value: Значение предпочтения
        strength: Сила предпочтения
        
    Returns:
        Обновленное предпочтение
    """
    try:
        adapter = PersonalityAdapter(user_id)
        return await adapter.update_preference(category, value, strength)
    except Exception as e:
        logger.error(f"Ошибка при обновлении предпочтения: {str(e)}")
        return {
            "category": category,
            "value": value,
            "strength": strength,
            "last_updated": datetime.now().isoformat()
        }

# Функция для подписки на обновления рекомендаций
async def recommendation_generator(user_id: str):
    """
    Генератор для подписки на обновления рекомендаций.
    
    Args:
        user_id: Идентификатор пользователя
        
    Yields:
        Новые рекомендации по мере их появления
    """
    # В реальной реализации здесь будет подписка на события
    # Для примера просто возвращаем одну рекомендацию
    yield {
        "id": f"rec-subscription",
        "content": "Рекомендация из подписки",
        "confidence": 0.95,
        "source": "subscription",
        "created_at": datetime.now().isoformat()
    }
