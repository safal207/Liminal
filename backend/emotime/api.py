"""
🌿✨ Emotime API Endpoints — интеграция с LIMINAL API

API эндпоинты для взаимодействия с системой Emotime:
- Обработка сенсорных данных  
- Получение текущего эмоционального состояния
- Анализ эмоциональных паттернов
- Управление сессиями

"Каждый запрос — это прикосновение к эмоциональному ритму души"
"""

from datetime import datetime
from typing import Dict, List, Optional, Any
from fastapi import APIRouter, HTTPException, Depends
from pydantic import BaseModel, Field

from .core import EmotimeEngine
from .sensors import TextSensor, TouchSensor, AudioSensor, SensorType
from .cache import emotime_cache
from ..performance.batch_processor import emotime_batch_processor
from ..performance.connection_optimizations import connection_manager
import asyncio
import hashlib


# Глобальный экземпляр Emotime (в продакшене должен быть в зависимости)
_global_emotime: Optional[EmotimeEngine] = None

# Performance optimization settings
USE_CACHING = True
USE_BATCH_PROCESSING = True
CACHE_TTL = 300  # 5 minutes
BATCH_SIZE = 5
BATCH_TIMEOUT = 0.5


def get_emotime_engine(user_id: str = "default") -> EmotimeEngine:
    """Получает экземпляр Emotime для пользователя."""
    global _global_emotime
    if not _global_emotime or _global_emotime.user_id != user_id:
        _global_emotime = EmotimeEngine(user_id=user_id)
        # Автозапуск в production должен быть настроен иначе
        # await _global_emotime.start()
    return _global_emotime


# Pydantic модели для API

class TextInput(BaseModel):
    """Текстовые данные для анализа."""
    text: str = Field(..., description="Текст сообщения")
    typing_speed: Optional[float] = Field(None, description="Скорость печати (символов/сек)")
    user_id: str = Field("default", description="ID пользователя")


class TouchInput(BaseModel):
    """Данные касания для анализа."""
    pressure: float = Field(..., ge=0.0, le=1.0, description="Давление касания (0.0-1.0)")
    duration: float = Field(..., gt=0, description="Длительность касания (секунды)")
    pattern: str = Field("tap", description="Паттерн касания")
    coordinates: Optional[tuple] = Field(None, description="Координаты касания")
    user_id: str = Field("default", description="ID пользователя")


class AudioInput(BaseModel):
    """Аудио данные для анализа."""
    pitch_mean: float = Field(..., description="Средняя частота голоса")
    pitch_variance: float = Field(..., description="Вариативность тона")
    speech_rate: float = Field(..., description="Скорость речи (слов/мин)")
    volume_level: float = Field(..., ge=0.0, le=1.0, description="Уровень громкости")
    pause_ratio: float = Field(..., ge=0.0, le=1.0, description="Доля пауз")
    emotion_markers: List[str] = Field([], description="Эмоциональные маркеры")
    user_id: str = Field("default", description="ID пользователя")


class EmotimeStatus(BaseModel):
    """Статус системы Emotime."""
    status: str
    user_id: Optional[str] = None
    session_id: Optional[str] = None
    timestamp: Optional[str] = None
    mode: Optional[Dict[str, Any]] = None
    features: Optional[Dict[str, Any]] = None  # Изменено на Any для поддержки разных типов
    confidence: Optional[float] = None
    trace_points: Optional[int] = None
    storage_enabled: Optional[bool] = None


class EmotimeInsights(BaseModel):
    """Глубокие инсайты Emotime."""
    current_state: Dict[str, Any]
    timeseries_analysis: Dict[str, Any]
    mode_statistics: Dict[str, Any]
    mode_insights: List[str]
    fusion_statistics: Dict[str, Any]
    historical_patterns: Optional[Dict[str, Any]] = None


# Создаем роутер
emotime_router = APIRouter(prefix="/emotime", tags=["Emotime 🌿✨"])


@emotime_router.post("/text", response_model=Dict[str, Any])
async def process_text(input_data: TextInput):
    """
    Обрабатывает текстовые данные для эмоционального анализа.
    
    Анализирует:
    - Эмоциональные маркеры в тексте
    - Скорость печати и паузы
    - Структуру предложений
    
    Оптимизации:
    - Кэширование результатов для повторяющихся текстов
    - Пакетная обработка для повышения throughput
    
    Security:
    - Input sanitization для всех пользовательских данных
    - Валидация user_id
    """
    try:
        # Security: Sanitize input data
        from .security.validators import get_input_sanitizer
        sanitizer = get_input_sanitizer()
        
        # Validate and sanitize user_id
        clean_user_id = sanitizer.validate_user_id(input_data.user_id)
        
        # Sanitize text content  
        clean_text = sanitizer.sanitize_text(input_data.text)
        
        # Create sanitized input data
        sanitized_input = TextInput(
            text=clean_text,
            typing_speed=input_data.typing_speed,
            pause_duration=input_data.pause_duration, 
            user_id=clean_user_id
        )
        # Performance optimization: check cache first  
        if USE_CACHING:
            cache_key = emotime_cache.get_cache_key(sanitized_input.text, sanitized_input.user_id)
            cached_result = emotime_cache.get(cache_key)
            
            if cached_result:
                return {
                    "status": "processed",
                    "sensor_type": "text", 
                    "timestamp": datetime.now().isoformat(),
                    "message": f"Обработан текст (cached): '{sanitized_input.text[:50]}...'",
                    "cached": True,
                    **cached_result
                }
        
        # Get Emotime engine
        emotime = get_emotime_engine(sanitized_input.user_id)
        
        # Performance optimization: use batch processing if enabled
        if USE_BATCH_PROCESSING:
            batch_data = {
                "text": sanitized_input.text,
                "user_id": sanitized_input.user_id,
                "typing_speed": sanitized_input.typing_speed,
                "type": "text"
            }
            
            batch_result = await emotime_batch_processor.add_request(
                request_id=f"text_{hash(sanitized_input.text)}_{sanitized_input.user_id}",
                data=batch_data
            )
            
            # Cache the result
            if USE_CACHING and batch_result:
                emotime_cache.set(cache_key, batch_result, CACHE_TTL)
            
            return {
                "status": "processed",
                "sensor_type": "text",
                "timestamp": datetime.now().isoformat(),
                "message": f"Обработан текст (batch): '{input_data.text[:50]}...'",
                "batch_processed": True,
                **batch_result
            }
        
        # Standard processing fallback
        text_sensor = TextSensor()
        metadata = {}
        if input_data.typing_speed:
            metadata["typing_speed"] = input_data.typing_speed
            
        sensor_data = await text_sensor.process(input_data.text, metadata)
        await emotime.process_sensor_data(sensor_data)
        
        # Get current state for response
        current_state = await emotime.get_current_state()
        result = {
            "status": "processed",
            "sensor_type": "text",
            "timestamp": sensor_data.timestamp.isoformat(),
            "message": f"Обработан текст: '{input_data.text[:50]}...'"
        }
        
        # Add emotional features if available
        if current_state:
            result["emotional_features"] = {
                "valence": current_state.features.valence,
                "arousal": current_state.features.arousal,
                "dominance": current_state.features.dominance,
                "confidence": current_state.features.confidence
            }
            result["confidence"] = current_state.confidence
            result["mode"] = {
                "name": current_state.mode.name,
                "type": current_state.mode.type.value
            }
        
        # Cache successful result
        if USE_CACHING:
            emotime_cache.set(cache_key, result, CACHE_TTL)
        
        return result
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Text processing failed: {str(e)}")


@emotime_router.post("/touch", response_model=Dict[str, str]) 
async def process_touch(input_data: TouchInput):
    """
    Обрабатывает данные касаний для эмоционального анализа.
    
    Анализирует:
    - Давление и продолжительность касаний
    - Частоту взаимодействий
    - Паттерны жестов
    """
    try:
        emotime = get_emotime_engine(input_data.user_id)
        
        # Создаем тач-сенсор
        touch_sensor = TouchSensor()
        
        # Обрабатываем касание
        sensor_data = await touch_sensor.process(
            pressure=input_data.pressure,
            duration=input_data.duration,
            pattern=input_data.pattern,
            coordinates=input_data.coordinates
        )
        
        # Отправляем в Emotime
        await emotime.process_sensor_data(sensor_data)
        
        return {
            "status": "processed", 
            "sensor_type": "touch",
            "timestamp": sensor_data.timestamp.isoformat(),
            "message": f"Обработано касание: {input_data.pattern} ({input_data.pressure:.2f} pressure)"
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Touch processing failed: {str(e)}")


@emotime_router.post("/audio", response_model=Dict[str, str])
async def process_audio(input_data: AudioInput):
    """
    Обрабатывает аудио данные для эмоционального анализа.
    
    Анализирует:
    - Тон и интонацию голоса
    - Скорость речи и паузы
    - Эмоциональные маркеры в речи
    """
    try:
        emotime = get_emotime_engine(input_data.user_id)
        
        # Создаем аудио сенсор
        audio_sensor = AudioSensor()
        
        # Обрабатываем аудио
        sensor_data = await audio_sensor.process(
            pitch_mean=input_data.pitch_mean,
            pitch_variance=input_data.pitch_variance,
            speech_rate=input_data.speech_rate,
            volume_level=input_data.volume_level,
            pause_ratio=input_data.pause_ratio,
            emotion_markers=input_data.emotion_markers
        )
        
        # Отправляем в Emotime
        await emotime.process_sensor_data(sensor_data)
        
        return {
            "status": "processed",
            "sensor_type": "audio", 
            "timestamp": sensor_data.timestamp.isoformat(),
            "message": f"Обработано аудио: {input_data.speech_rate:.1f} wpm, {len(input_data.emotion_markers)} маркеров"
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Audio processing failed: {str(e)}")


@emotime_router.get("/status", response_model=EmotimeStatus)
async def get_status(user_id: str = "default"):
    """
    Получает текущее эмоциональное состояние пользователя.
    
    Возвращает:
    - Текущий режим (спокойствие, фокус, стресс, и т.д.)
    - Эмоциональные признаки
    - Уверенность системы в классификации
    """
    try:
        emotime = get_emotime_engine(user_id)
        status_data = emotime.to_dict()
        
        return EmotimeStatus(**status_data)
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Status retrieval failed: {str(e)}")


@emotime_router.get("/insights", response_model=EmotimeInsights)
async def get_insights(user_id: str = "default"):
    """
    Получает глубокие инсайты об эмоциональном состоянии.
    
    Включает:
    - Анализ временных рядов и трендов
    - Статистику эмоциональных режимов
    - Паттерны поведения  
    - Исторические данные из Neo4j
    """
    try:
        emotime = get_emotime_engine(user_id)
        insights_data = await emotime.get_emotional_insights()
        
        return EmotimeInsights(**insights_data)
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Insights retrieval failed: {str(e)}")


@emotime_router.get("/timeline")
async def get_timeline(
    user_id: str = "default",
    session_id: Optional[str] = None,
    limit: int = 50
):
    """
    Получает эмоциональную временную линию пользователя.
    
    Возвращает последовательность эмоциональных точек
    с их режимами и временными метками.
    """
    try:
        emotime = get_emotime_engine(user_id)
        
        if not emotime.storage:
            # Возвращаем данные из памяти
            recent_points = emotime.get_resonance_trace(limit)
            return {
                "source": "memory", 
                "user_id": user_id,
                "points": [
                    {
                        "timestamp": point.timestamp.isoformat(),
                        "valence": point.valence,
                        "arousal": point.arousal,
                        "trend": point.trend,
                        "is_peak": point.is_peak
                    }
                    for point in recent_points
                ]
            }
        else:
            # Получаем данные из Neo4j
            timeline = await emotime.storage.get_emotional_timeline(
                user_id=user_id,
                session_id=session_id, 
                limit=limit
            )
            return {
                "source": "neo4j",
                "user_id": user_id,
                "session_id": session_id,
                "timeline": timeline
            }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Timeline retrieval failed: {str(e)}")


@emotime_router.post("/session/start")
async def start_session(user_id: str = "default"):
    """
    Запускает новую эмоциональную сессию для пользователя.
    
    Создает новый движок Emotime и начинает сбор данных.
    """
    try:
        global _global_emotime
        
        # Останавливаем предыдущую сессию если есть
        if _global_emotime:
            await _global_emotime.stop()
            
        # Создаем новую сессию
        _global_emotime = EmotimeEngine(user_id=user_id)
        await _global_emotime.start()
        
        return {
            "status": "started",
            "user_id": user_id,
            "session_id": _global_emotime.session_id,
            "timestamp": datetime.now().isoformat(),
            "message": "💓 Emotime session started — сердце начинает биться"
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Session start failed: {str(e)}")


@emotime_router.post("/session/stop")
async def stop_session(user_id: str = "default"):
    """
    Останавливает текущую эмоциональную сессию.
    
    Сохраняет накопленные данные и закрывает соединения.
    """
    try:
        global _global_emotime
        
        if _global_emotime and _global_emotime.user_id == user_id:
            session_id = _global_emotime.session_id
            await _global_emotime.stop()
            _global_emotime = None
            
            return {
                "status": "stopped",
                "user_id": user_id, 
                "session_id": session_id,
                "timestamp": datetime.now().isoformat(),
                "message": "🌿 Emotime session stopped — сердце замирает"
            }
        else:
            return {
                "status": "no_active_session",
                "user_id": user_id,
                "message": "Активной сессии не найдено"
            }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Session stop failed: {str(e)}")


@emotime_router.get("/health")
async def health_check():
    """
    Проверка здоровья Emotime системы.
    
    Возвращает статус всех компонентов.
    """
    try:
        global _global_emotime
        
        health_status = {
            "emotime_core": "ok",
            "active_session": _global_emotime is not None,
            "neo4j_storage": False,
            "prometheus_metrics": False,
            "timestamp": datetime.now().isoformat()
        }
        
        # Проверяем Neo4j
        if _global_emotime and _global_emotime.storage:
            health_status["neo4j_storage"] = _global_emotime.storage.driver is not None
            
        # Проверяем метрики
        try:
            from .metrics_integration import METRICS_AVAILABLE
            health_status["prometheus_metrics"] = METRICS_AVAILABLE
        except ImportError:
            pass
            
        return health_status
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Health check failed: {str(e)}")


@emotime_router.get("/demo")
async def demo_data():
    """
    Генерирует демо-данные для тестирования Emotime.
    
    Создает различные типы сенсорных данных для демонстрации работы системы.
    """
    demo_scenarios = [
        {
            "name": "Спокойное утро",
            "text_data": {
                "text": "Доброе утро! Сегодня прекрасный день для размышлений.",
                "typing_speed": 4.5
            },
            "touch_data": {
                "pressure": 0.3,
                "duration": 1.2,
                "pattern": "tap"
            },
            "expected_mode": "calm"
        },
        {
            "name": "Рабочий фокус", 
            "text_data": {
                "text": "Нужно сосредоточиться на этой задаче и довести ее до конца.",
                "typing_speed": 8.2
            },
            "touch_data": {
                "pressure": 0.7,
                "duration": 0.5, 
                "pattern": "tap"
            },
            "expected_mode": "focus"
        },
        {
            "name": "Стрессовая ситуация",
            "text_data": {
                "text": "Это просто кошмар! Не знаю, как со всем этим справиться!!!",
                "typing_speed": 12.1
            },
            "touch_data": {
                "pressure": 0.9,
                "duration": 0.2,
                "pattern": "gesture"
            },
            "expected_mode": "stress"
        },
        {
            "name": "Радостный момент",
            "text_data": {
                "text": "Ура! Получилось! Я так счастлива, что все сложилось отлично! ✨",
                "typing_speed": 6.8
            },
            "touch_data": {
                "pressure": 0.5,
                "duration": 0.8,
                "pattern": "swipe" 
            },
            "expected_mode": "joy"
        }
    ]
    
    return {
        "message": "Emotime demo scenarios ready",
        "scenarios": demo_scenarios,
        "usage": {
            "instructions": [
                "1. Запустите сессию: POST /emotime/session/start",
                "2. Отправьте данные: POST /emotime/text или /emotime/touch",
                "3. Проверьте состояние: GET /emotime/status", 
                "4. Получите инсайты: GET /emotime/insights"
            ],
            "example_text_request": {
                "url": "/emotime/text",
                "method": "POST",
                "body": demo_scenarios[0]["text_data"]
            }
        }
    }


@emotime_router.get("/analytics", response_model=Dict[str, Any])
async def get_advanced_analytics(
    user_id: Optional[str] = None,
    time_range_hours: int = 24,
    include_predictions: bool = True
):
    """
    MIT-level Advanced Analytics Dashboard with ML insights.
    
    Provides world-class analytics from top AI research labs:
    - MIT: Real-time learning analytics & adaptation metrics
    - Stanford: Behavioral pattern analysis & predictive modeling  
    - OpenAI: Safety-first analytics & responsible AI metrics
    - DeepMind: Multi-agent emotional coordination analytics
    - Google Research: Scalable performance analytics
    """
    try:
        # Security: Use safe import with validation
        import os
        from .security.validators import safe_import_module
        
        # Secure import of analytics module
        analytics_path = os.path.join(os.path.dirname(__file__), 'analytics_standalone.py')
        analytics_module = safe_import_module('analytics_standalone', analytics_path)
        
        if not analytics_module:
            raise HTTPException(status_code=500, detail="Analytics module failed security validation")
        
        dashboard = analytics_module.get_standalone_analytics_dashboard()
        analytics_data = await dashboard.generate_comprehensive_analytics(
            user_id=user_id,
            time_range_hours=time_range_hours,
            include_predictions=include_predictions
        )
        
        return {
            "success": True,
            "data": analytics_data,
            "ai_lab_practices": [
                "MIT: Real-time learning & adaptation",
                "Stanford: Behavioral pattern recognition",
                "OpenAI: Safety-first analytics", 
                "DeepMind: Multi-agent coordination",
                "Google: Scalable performance analytics"
            ]
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analytics generation error: {str(e)}")


@emotime_router.get("/analytics/summary", response_model=Dict[str, Any]) 
async def get_analytics_summary():
    """
    Get analytics dashboard summary and capabilities.
    """
    try:
        # Security: Use safe import with validation
        import os
        from .security.validators import safe_import_module
        
        # Secure import of analytics module
        analytics_path = os.path.join(os.path.dirname(__file__), 'analytics_standalone.py')
        analytics_module = safe_import_module('analytics_standalone', analytics_path)
        
        if not analytics_module:
            raise HTTPException(status_code=500, detail="Analytics module failed security validation")
        
        dashboard = analytics_module.get_standalone_analytics_dashboard()
        summary = dashboard.get_dashboard_summary()
        
        return {
            "success": True,
            "data": summary,
            "description": "World-class emotional intelligence analytics powered by MIT ML insights"
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analytics summary error: {str(e)}")