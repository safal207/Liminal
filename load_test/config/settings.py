"""
Настройки нагрузочного тестирования.

Этот файл содержит конфигурацию для различных сценариев тестирования.
"""

import os
from typing import Dict, List, Optional, Tuple


class TestSettings:
    # Базовые настройки
    HOST = os.getenv("TARGET_HOST", "http://localhost:8000")
    TEST_TIMEOUT = int(os.getenv("TEST_TIMEOUT", "300"))  # 5 минут по умолчанию

    # Настройки нагрузки
    LOAD_PROFILES: Dict[str, Dict[str, int]] = {
        "smoke": {"users": 10, "spawn_rate": 1, "duration_sec": 60},
        "load": {"users": 100, "spawn_rate": 10, "duration_sec": 300},
        "stress": {"users": 1000, "spawn_rate": 50, "duration_sec": 600},
        "soak": {"users": 50, "spawn_rate": 5, "duration_sec": 3600},  # 1 час
    }

    # Настройки WebSocket
    WS_RECONNECT_DELAY = 5  # секунды
    WS_TIMEOUT = 30  # секунды

    # Настройки HTTP
    HTTP_TIMEOUT = 10  # секунды

    # Настройки логирования
    LOG_LEVEL = os.getenv("LOG_LEVEL", "INFO")
    LOG_FORMAT = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"

    # Настройки отчетов
    REPORT_DIR = "reports"
    REPORT_FORMATS = ["html", "csv"]

    # Настройки мониторинга
    ENABLE_PROMETHEUS = os.getenv("ENABLE_PROMETHEUS", "false").lower() == "true"
    PROMETHEUS_PORT = int(os.getenv("PROMETHEUS_PORT", "9090"))

    @classmethod
    def get_load_profile(cls, profile_name: str) -> Dict[str, int]:
        """Получить настройки профиля нагрузки."""
        return cls.LOAD_PROFILES.get(profile_name, cls.LOAD_PROFILES["load"])


# Создаем директорию для отчетов, если её нет
os.makedirs(TestSettings.REPORT_DIR, exist_ok=True)
