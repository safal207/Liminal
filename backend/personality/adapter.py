"""
PersonalityAdapter - универсальный адаптер для хранения и анализа персональных данных.
Интегрируется с DatabaseAdapter для сохранения данных в Datomic (временные данные) и Neo4j (связи).
Поддерживает ML-анализ эмоций в тексте пользователя.
"""
from datetime import datetime
from typing import List, Optional, Dict, Any, Union
import logging
from enum import Enum

# Импортируем существующие компоненты
from ..database_adapter import DatabaseAdapter, DataType
from .ml_adapter import emotion_ml_adapter, EmotionMLAdapter

class PersonalityDataType(Enum):
    """Типы данных для PersonalityAdapter."""
    EMOTION = "emotion"
    PREFERENCE = "preference"
    RECOMMENDATION = "recommendation"
    PROFILE = "profile"

class PersonalityAdapter:
    """
    Универсальный адаптер для работы с персональными данными пользователя.
    
    Позволяет:
    - Сохранять эмоциональные состояния
    - Управлять предпочтениями
    - Получать рекомендации
    - Анализировать историю изменений
    
    Интегрируется с DatabaseAdapter для хранения данных в Datomic и Neo4j.
    """
    
    def __init__(self, user_id: str):
        """
        Инициализация адаптера для конкретного пользователя.
        
        Args:
            user_id: Уникальный идентификатор пользователя
        """
        self.user_id = user_id
        self.db = DatabaseAdapter()
        self.logger = logging.getLogger("personality.adapter")
        self.ml_adapter = emotion_ml_adapter
        
    async def get_profile(self) -> Dict[str, Any]:
        """
        Получить полный профиль пользователя.
        
        Returns:
            Dict с данными профиля
        """
        try:
            # Получаем эмоциональную историю из Datomic
            emotions = await self._load_emotions()
            
            # Получаем предпочтения из Neo4j
            preferences = await self._load_preferences()
            
            # Генерируем рекомендации на основе профиля
            recommendations = await self.get_recommendations()
            
            return {
                "user_id": self.user_id,
                "emotional_history": emotions,
                "preferences": preferences,
                "recommendations": recommendations
            }
        except Exception as e:
            # "Случайности не случайны" - обрабатываем ошибки как точки роста
            self.logger.error(f"Ошибка при получении профиля: {str(e)}")
            self.logger.info("Возвращаем пустой профиль как точку роста")
            return {
                "user_id": self.user_id,
                "emotional_history": [],
                "preferences": [],
                "recommendations": []
            }
    
    async def store_emotion(self, emotion_type: str, intensity: float, 
                           context: Optional[str] = None) -> Dict[str, Any]:
        """
        Сохранить эмоциональное состояние пользователя.
        
        Args:
            emotion_type: Тип эмоции (радость, грусть и т.д.)
            intensity: Интенсивность от 0.0 до 1.0
            context: Контекст эмоции (опционально)
            
        Returns:
            Сохраненная эмоция
        """
        timestamp = datetime.now().isoformat()
        emotion_data = {
            "type": emotion_type,
            "intensity": intensity,
            "context": context or "",
            "timestamp": timestamp
        }
        
        # Сохраняем в Datomic через DatabaseAdapter
        await self.db.store_data(
            data=emotion_data,
            data_type=DataType.EMOTION,
            user_id=self.user_id
        )
        
        return emotion_data
        
    async def analyze_text_emotion(self, text: str, context: Optional[str] = None) -> Dict[str, Any]:
        """
        Анализирует эмоции в тексте пользователя и сохраняет результат.
        
        Args:
            text: Текст для анализа
            context: Контекст анализа (опционально)
            
        Returns:
            Сохраненная эмоция с дополнительными данными ML-анализа
        """
        try:
            # Анализируем текст с помощью ML-адаптера
            emotion_analysis = await self.ml_adapter.analyze_text(text)
            
            # Извлекаем основные параметры для сохранения
            emotion_type = emotion_analysis["emotion_type"]
            intensity = emotion_analysis["intensity"]
            
            # Добавляем контекст, если он предоставлен
            if context:
                context_info = context
            else:
                # Используем первые 50 символов текста как контекст
                context_info = text[:50] + "..." if len(text) > 50 else text
            
            # Сохраняем эмоцию
            emotion_data = await self.store_emotion(
                emotion_type=emotion_type,
                intensity=intensity,
                context=context_info
            )
            
            # Добавляем данные ML-анализа к результату
            emotion_data["ml_analysis"] = {
                "dimensions": emotion_analysis["dimensions"],
                "confidence": emotion_analysis["confidence"],
                "features": emotion_analysis["features"]
            }
            
            return emotion_data
        except Exception as e:
            self.logger.error(f"Ошибка при анализе эмоций в тексте: {str(e)}")
            # Случайности не случайны - возвращаем нейтральную эмоцию как точку роста
            return await self.store_emotion(
                emotion_type="нейтральность",
                intensity=0.5,
                context=f"Ошибка анализа: {context or 'текст'}"
            )
            
    async def get_emotion_suggestions(self, context: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Получает предложения по эмоциям на основе контекста.
        
        Args:
            context: Контекст для анализа (опционально)
            
        Returns:
            Список предложений по эмоциям
        """
        try:
            # Получаем предложения от ML-адаптера
            suggestions = await self.ml_adapter.get_emotion_suggestions(self.user_id, context)
            return suggestions
        except Exception as e:
            self.logger.error(f"Ошибка при получении предложений по эмоциям: {str(e)}")
            # Возвращаем базовый набор эмоций
            return [
                {"emotion_type": "радость", "description": "Позитивное чувство"},
                {"emotion_type": "интерес", "description": "Любопытство"},
                {"emotion_type": "спокойствие", "description": "Умиротворение"}
            ]
    
    async def update_preference(self, category: str, value: str, 
                              strength: float) -> Dict[str, Any]:
        """
        Обновить предпочтение пользователя.
        
        Args:
            category: Категория предпочтения
            value: Значение предпочтения
            strength: Сила предпочтения от 0.0 до 1.0
            
        Returns:
            Обновленное предпочтение
        """
        timestamp = datetime.now().isoformat()
        preference_data = {
            "category": category,
            "value": value,
            "strength": strength,
            "last_updated": timestamp
        }
        
        # Сохраняем в Neo4j через DatabaseAdapter
        await self.db.store_data(
            data=preference_data,
            data_type=DataType.PREFERENCE,
            user_id=self.user_id
        )
        
        return preference_data
    
    async def get_recommendations(self, limit: int = 5, 
                                context: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Получить рекомендации для пользователя.
        
        Args:
            limit: Максимальное количество рекомендаций
            context: Контекст для фильтрации рекомендаций
            
        Returns:
            Список рекомендаций
        """
        try:
            # В реальной реализации здесь будет сложная логика на основе Neo4j
            # Для примера возвращаем заглушку
            recommendations = []
            
            # Пример запроса к Neo4j через DatabaseAdapter
            query = f"""
            MATCH (u:User {{id: '{self.user_id}'}})-[:HAS_PREFERENCE]->(p)
            WHERE p.context = '{context}' OR '{context}' IS NULL
            RETURN p
            LIMIT {limit}
            """
            
            # В будущем здесь будет реальный запрос
            # results = await self.db.query_neo4j(query)
            
            # Заглушка для примера
            for i in range(min(3, limit)):
                recommendations.append({
                    "id": f"rec-{i}",
                    "content": f"Рекомендация {i} для контекста {context or 'общий'}",
                    "confidence": 0.7 + (i * 0.1),
                    "source": "hybrid",
                    "context": context,
                    "created_at": datetime.now().isoformat()
                })
                
            return recommendations
        except Exception as e:
            self.logger.error(f"Ошибка при получении рекомендаций: {str(e)}")
            return []
    
    async def _load_emotions(self) -> List[Dict[str, Any]]:
        """
        Загрузить историю эмоций пользователя.
        
        Returns:
            Список эмоций
        """
        # В реальной реализации здесь будет запрос к Datomic
        # Для примера возвращаем заглушку
        return [
            {
                "type": "радость",
                "intensity": 0.8,
                "context": "завершение задачи",
                "timestamp": datetime.now().isoformat()
            }
        ]
    
    async def _load_preferences(self) -> List[Dict[str, Any]]:
        """
        Загрузить предпочтения пользователя.
        
        Returns:
            Список предпочтений
        """
        # В реальной реализации здесь будет запрос к Neo4j
        # Для примера возвращаем заглушку
        return [
            {
                "category": "обучение",
                "value": "визуальный стиль",
                "strength": 0.9,
                "last_updated": datetime.now().isoformat()
            }
        ]
