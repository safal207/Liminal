"""
🚀🛡️ BurnoutGuard Utils — вспомогательные функции

Утилиты для BurnoutGuard:
- Безопасное логирование
- Форматирование данных
- Валидация
- Конвертеры
"""

import logging
import json
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Union

# Безопасный logger
safe_logger = logging.getLogger("burnout_guard")
if not safe_logger.handlers:
    handler = logging.StreamHandler()
    handler.setFormatter(logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    ))
    safe_logger.addHandler(handler)
    safe_logger.setLevel(logging.INFO)


def format_risk_score(score: float) -> str:
    """Форматирует скор риска в читаемый вид."""
    percentage = int(score * 100)
    
    if score <= 0.2:
        return f"🟢 {percentage}% - Очень низкий риск"
    elif score <= 0.4:
        return f"🟡 {percentage}% - Низкий риск"
    elif score <= 0.6:
        return f"🟠 {percentage}% - Средний риск"
    elif score <= 0.8:
        return f"🔴 {percentage}% - Высокий риск"
    else:
        return f"🚨 {percentage}% - КРИТИЧЕСКИЙ риск"


def format_time_duration(hours: float) -> str:
    """Форматирует длительность в читаемый вид."""
    if hours < 1:
        minutes = int(hours * 60)
        return f"{minutes} мин"
    elif hours < 24:
        return f"{hours:.1f} ч"
    else:
        days = int(hours // 24)
        remaining_hours = int(hours % 24)
        return f"{days}д {remaining_hours}ч"


def validate_risk_score(score: float) -> float:
    """Валидирует и нормализует скор риска."""
    if not isinstance(score, (int, float)):
        return 0.0
    return max(0.0, min(1.0, float(score)))


def safe_divide(numerator: float, denominator: float, default: float = 0.0) -> float:
    """Безопасное деление с обработкой деления на ноль."""
    if denominator == 0:
        return default
    return numerator / denominator


def calculate_percentage_change(old_value: float, new_value: float) -> float:
    """Вычисляет процентное изменение."""
    if old_value == 0:
        return 0.0 if new_value == 0 else 100.0
    return ((new_value - old_value) / old_value) * 100.0


def get_time_of_day_category(hour: int) -> str:
    """Категоризирует время дня."""
    if 6 <= hour < 12:
        return "morning"
    elif 12 <= hour < 18:
        return "afternoon"
    elif 18 <= hour < 22:
        return "evening"
    else:
        return "night"


def is_working_hours(hour: int, working_start: int = 9, working_end: int = 18) -> bool:
    """Проверяет, является ли время рабочими часами."""
    return working_start <= hour < working_end


def serialize_burnout_data(data: Any) -> str:
    """Сериализует данные BurnoutGuard в JSON."""
    def serialize_helper(obj):
        if isinstance(obj, datetime):
            return obj.isoformat()
        elif hasattr(obj, '__dict__'):
            return {key: serialize_helper(value) for key, value in obj.__dict__.items()}
        elif isinstance(obj, list):
            return [serialize_helper(item) for item in obj]
        elif isinstance(obj, dict):
            return {key: serialize_helper(value) for key, value in obj.items()}
        elif hasattr(obj, 'value'):  # для Enum
            return obj.value
        else:
            return obj
    
    try:
        return json.dumps(serialize_helper(data), ensure_ascii=False, indent=2)
    except Exception as e:
        safe_logger.error(f"Serialization error: {e}")
        return "{}"


def create_alert_message(risk_score: float, indicators: List[str]) -> str:
    """Создает сообщение предупреждения."""
    risk_emoji = "🚨" if risk_score > 0.8 else "⚠️" if risk_score > 0.6 else "ℹ️"
    
    message = f"{risk_emoji} Уровень риска выгорания: {format_risk_score(risk_score)}\n"
    
    if indicators:
        message += "\nОсновные индикаторы:\n"
        for indicator in indicators[:5]:  # показываем максимум 5
            message += f"• {indicator}\n"
    
    return message.strip()


def calculate_work_intensity(session_hours: float, break_count: int = 0) -> str:
    """Вычисляет интенсивность работы."""
    if session_hours <= 2:
        return "low"
    elif session_hours <= 4:
        return "normal"
    elif session_hours <= 8:
        intensity = "high" if break_count < 2 else "normal"
        return intensity
    else:
        return "extreme"


def get_intervention_urgency(risk_score: float, trend_risk: float) -> str:
    """Определяет срочность вмешательства."""
    if risk_score > 0.8 or (risk_score > 0.6 and trend_risk > 0.4):
        return "immediate"
    elif risk_score > 0.6 or trend_risk > 0.3:
        return "soon"
    elif risk_score > 0.4:
        return "planned"
    else:
        return "none"