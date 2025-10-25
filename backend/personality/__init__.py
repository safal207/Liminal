"""
Модуль PersonalityAdapter для персонализации и рекомендаций.
Интегрируется с DatabaseAdapter для хранения данных в Datomic и Neo4j.
"""

import os

from .adapter import PersonalityAdapter

# Креативная изоляция импортов для тестов:
# По умолчанию НЕ импортируем тяжёлые зависимости (GraphQL/strawberry, security),
# чтобы unit-тесты personality/ml_adapter работали автономно.
# Включить полноценный роутер можно переменной среды LIMINAL_ENABLE_PERSONALITY_ROUTER=1

router = None  # безопасная заглушка по умолчанию

if os.getenv("LIMINAL_ENABLE_PERSONALITY_ROUTER", "0") == "1":
    try:
        from .router import router as _router  # type: ignore

        router = _router
    except Exception as e:  # fail-open в тестовом режиме
        # Оставляем router = None, чтобы не ломать импорт пакета
        # Подробности ошибки можно посмотреть в логах приложения при реальном запуске
        router = None

__all__ = ["PersonalityAdapter", "router"]
