"""
ML Feature Extraction для Resonance Liminal.
Модуль содержит функции для извлечения ML-признаков из логов, метрик и пользовательских данных.
Используется для:
1. Advanced anomaly detection
2. Intelligent rate limiting
3. Smart message routing
4. Predictive scaling
"""

import json
import logging
import math
import os
import time
from collections import Counter, defaultdict
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Tuple, Union

import numpy as np
import prometheus_client as prom

# Инициализация логгера
logger = logging.getLogger(__name__)

# Проверка включения ML-функциональности
ML_ENABLED = os.environ.get("ML_ENABLED", "false").lower() == "true"
logger.info(f"ML Features module loaded. ML_ENABLED={ML_ENABLED}")

# Инициализация Prometheus метрик
feature_gauge = prom.Gauge("ml_feature_value", "Значение ML-признака", ["feature_name"])

anomaly_gauge = prom.Gauge("ml_anomaly_score", "Оценка аномальности на основе ML")


class MLFeatureExtractor:
    """Класс для извлечения ML-признаков из данных логов и метрик"""

    def __init__(self):
        """Инициализация экстрактора признаков"""
        # Временные метки запросов для расчета скорости
        self.request_timestamps = []
        # Данные о запросах пользователей [timestamp, user_id]
        self.user_request_data = []
        # Данные о IP адресах [timestamp, ip_address]
        self.request_ip_data = []
        # Максимальное количество хранимых временных меток
        self.max_timestamps = 1000

        # Метрики Prometheus
        self.ml_feature_gauge = prom.Gauge(
            "ml_feature_value", "Значение ML-признака", ["feature_name"]
        )
        self.anomaly_score_gauge = prom.Gauge(
            "ml_anomaly_score", "Оценка аномальности на основе ML"
        )

    @staticmethod
    def extract_user_patterns() -> Dict[str, Any]:
        """Извлекает паттерны использования по пользователям"""
        if not ML_ENABLED:
            return {"enabled": False}

        result = {
            "enabled": True,
            "requests_per_user": MLFeatureExtractor._calculate_requests_per_user(),
            "unique_users": len(MLFeatureExtractor._get_user_requests()),
            "login_intervals": MLFeatureExtractor._calculate_login_intervals(),
        }
        return result

    @staticmethod
    def extract_traffic_features() -> Dict[str, Any]:
        """Извлекает признаки трафика"""
        if not ML_ENABLED:
            return {"enabled": False}

        result = {
            "enabled": True,
            "average_request_rate": MLFeatureExtractor._calculate_request_rate(),
            "burstiness_score": MLFeatureExtractor._calculate_burstiness(),
            "ip_entropy": MLFeatureExtractor._calculate_ip_entropy(),
            "channel_activity": MLFeatureExtractor._get_channel_activity(),
        }
        return result

    @staticmethod
    def get_current_anomaly_scores() -> Dict[str, Any]:
        """Вычисляет текущие аномальные скоры"""
        if not ML_ENABLED:
            return {"enabled": False}

        # Вычисление скоров аномалий на основе извлеченных признаков
        burstiness = MLFeatureExtractor._calculate_burstiness()
        ip_entropy = MLFeatureExtractor._calculate_ip_entropy()

        # Простая эвристика для аномальности
        traffic_anomaly = 0
        if burstiness > 0.7:  # Высокая неравномерность - потенциально аномально
            traffic_anomaly = (burstiness - 0.7) * 3.33  # Нормализация до 0-1

        auth_anomaly = 0
        # Определяем аномалии в авторизации (много неудачных попыток и т.д.)
        if len(MLFeatureExtractor._get_auth_events()) > 50:
            failures = sum(
                1
                for event in MLFeatureExtractor._get_auth_events()[-50:]
                if not event.get("success", True)
            )
            if failures > 15:  # Более 30% неудачных попыток авторизации
                auth_anomaly = failures / 50  # Нормализация до 0-1

        # Общий скор аномалии как комбинация различных факторов
        total_anomaly = max(
            traffic_anomaly,
            auth_anomaly,
            (1.0 - ip_entropy) if ip_entropy is not None else 0,
        )

        return {
            "enabled": True,
            "traffic_anomaly_score": round(traffic_anomaly, 3),
            "auth_anomaly_score": round(auth_anomaly, 3),
            "ip_diversity_anomaly": round(
                (1.0 - ip_entropy) if ip_entropy is not None else 0, 3
            ),
            "total_anomaly_score": round(total_anomaly, 3),
        }

    @staticmethod
    def get_prediction_features() -> Dict[str, Any]:
        """Предсказательные признаки для ML-моделей"""
        if not ML_ENABLED:
            return {"enabled": False}

        # Извлечение признаков для предсказаний нагрузки и поведения
        curr_time = datetime.now()
        hour_of_day = curr_time.hour
        day_of_week = curr_time.weekday()

        # На основе исторических данных предсказываем нагрузку
        predicted_load = MLFeatureExtractor._predict_user_load(hour_of_day, day_of_week)

        result = {
            "enabled": True,
            "predicted_user_load": predicted_load,
            "jwt_usage_patterns": MLFeatureExtractor._extract_jwt_patterns(),
            "timestamp_features": {
                "hour": hour_of_day,
                "day_of_week": day_of_week,
                "is_weekend": day_of_week >= 5,
            },
        }
        return result

    # Вспомогательные методы для вычисления конкретных метрик

    @staticmethod
    def _calculate_login_intervals() -> Dict[str, float]:
        """Вычисляет интервалы между логинами пользователей"""
        # Заглушка для демонстрации
        return {
            "median_interval": 3600,  # секунды
            "min_interval": 60,
            "max_interval": 86400,
        }

    @staticmethod
    def _calculate_request_rate() -> float:
        """Вычисляет среднюю скорость запросов"""
        # Пример: среднее число запросов в минуту за последний час
        total_requests = len(MLFeatureExtractor._get_user_requests())
        return total_requests / 60 if total_requests else 0

    @staticmethod
    def _get_recent_ip_addresses(window_seconds: int = 300) -> List[str]:
        """Получает список IP-адресов из последних запросов за указанное окно времени.

        Args:
            window_seconds: Временное окно в секундах для анализа (по умолчанию 5 минут)

        Returns:
            Список IP-адресов из последних запросов
        """
        # Заглушка для демонстрации - в реальной системе здесь будет получение данных из логов или БД
        # В продакшн-версии этот метод должен читать IP из логов или из Redis
        current_time = time.time()
        cutoff_time = current_time - window_seconds

        # Фильтрация запросов по времени
        recent_ips = []
        for timestamp, ip in MLFeatureExtractor._get_request_ip_data():
            if timestamp >= cutoff_time:
                recent_ips.append(ip)

        return recent_ips

    @staticmethod
    def _get_user_requests(window_seconds: int = 300) -> Dict[str, int]:
        """Получает количество запросов по пользователям за указанное окно времени.

        Args:
            window_seconds: Временное окно в секундах для анализа (по умолчанию 5 минут)

        Returns:
            Словарь {user_id: request_count}
        """
        # Заглушка для демонстрации - в реальной системе здесь будет получение данных из логов или Redis
        current_time = time.time()
        cutoff_time = current_time - window_seconds

        # Фильтрация запросов по времени и подсчёт по пользователям
        user_counts = defaultdict(int)
        for timestamp, user_id in MLFeatureExtractor._get_user_request_data():
            if timestamp >= cutoff_time:
                user_counts[user_id] += 1

        return dict(user_counts)

    @staticmethod
    def _calculate_requests_per_user(user_requests: Dict[str, int]) -> Dict[str, Any]:
        """Вычисляет статистику по запросам пользователей.

        Эта метрика позволяет выявлять аномальную активность отдельных пользователей,
        которые могут превышать нормальные паттерны использования системы.

        Args:
            user_requests: Словарь {user_id: request_count}

        Returns:
            Словарь со статистикой запросов по пользователям
        """
        if not user_requests:
            return {
                "avg_requests": 0,
                "max_requests": 0,
                "total_requests": 0,
                "active_users": 0,
                "top_users": [],
            }

        request_counts = list(user_requests.values())
        active_users = len(user_requests)
        total_requests = sum(request_counts)
        avg_requests = total_requests / active_users if active_users > 0 else 0
        max_requests = max(request_counts) if request_counts else 0

        # Определение топ-5 пользователей по количеству запросов
        top_users = sorted(
            [(user_id, count) for user_id, count in user_requests.items()],
            key=lambda x: x[1],
            reverse=True,
        )[:5]

        return {
            "avg_requests": avg_requests,
            "max_requests": max_requests,
            "total_requests": total_requests,
            "active_users": active_users,
            "top_users": top_users,
        }

    @staticmethod
    def _calculate_burstiness() -> float:
        """Вычисляет коэффициент всплесков активности.

        Returns:
            Коэффициент от 0 до 1, где 1 - максимальная неравномерность
        """
        # Получаем временные метки запросов
        request_timestamps = MLFeatureExtractor._get_request_timestamps()
        if not request_timestamps or len(request_timestamps) < 2:
            return 0.0

        # Вычисляем интервалы между запросами
        intervals = [
            request_timestamps[i] - request_timestamps[i - 1]
            for i in range(1, len(request_timestamps))
        ]

        if not intervals:
            return 0.0

        # Вычисляем среднее и стандартное отклонение
        mean_interval = sum(intervals) / len(intervals)
        if mean_interval == 0:
            return 1.0  # Максимальная неравномерность при нулевом интервале

        variance = sum((x - mean_interval) ** 2 for x in intervals) / len(intervals)
        std_dev = math.sqrt(variance)

        # Коэффициент вариации как мера неравномерности (burstiness)
        cv = std_dev / mean_interval if mean_interval else 1.0

        # Нормализуем до 0-1, где 1 - максимальная неравномерность
        burstiness = min(1.0, cv / 2.0)
        return burstiness

    @staticmethod
    def _calculate_ip_entropy() -> Optional[float]:
        """Вычисляет энтропию IP адресов для обнаружения ботов.

        Высокая энтропия (ближе к 1) означает равномерное распределение IP-адресов.
        Низкая энтропия (ближе к 0) означает концентрацию трафика от небольшого числа IP.
        Внезапное понижение энтропии может указывать на DDoS-атаку.

        Returns:
            Значение энтропии от 0 до 1, где 1 - максимальная энтропия
        """
        recent_ips = MLFeatureExtractor._get_recent_ip_addresses()
        if not recent_ips:
            return None

        # Считаем частоту каждого IP
        ip_counts = Counter(recent_ips)
        total = len(recent_ips)

        # Вычисляем энтропию Шеннона
        entropy = 0.0
        for ip, count in ip_counts.items():
            probability = count / total
            entropy -= probability * math.log2(probability)

        # Нормализуем энтропию к диапазону [0, 1]
        # Максимальная энтропия для n уникальных IP = log2(n)
        max_entropy = math.log2(len(ip_counts)) if len(ip_counts) > 1 else 1.0
        normalized_entropy = entropy / max_entropy if max_entropy > 0 else 0.0

        return normalized_entropy

    @staticmethod
    def _predict_user_load(hour: int, day: int) -> float:
        """Предсказывает нагрузку пользователей на основе времени суток и дня недели"""
        # Заглушка для демонстрации - в реальности должна использоваться ML-модель
        base_load = 50  # Базовая нагрузка

        # Повышение в рабочие часы (9-18) в будние дни
        hour_factor = 1.5 if 9 <= hour <= 18 else 0.7
        day_factor = 1.2 if day < 5 else 0.8  # Больше в будни, меньше в выходные

        # Пиковые часы - утро и вечер в будни
        peak_boost = 1.0
        if day < 5:  # Будний день
            if 9 <= hour <= 11 or 14 <= hour <= 17:
                peak_boost = 1.8

        predicted_load = base_load * hour_factor * day_factor * peak_boost
        return round(predicted_load, 1)

    @staticmethod
    def _extract_jwt_patterns() -> Dict[str, Any]:
        """Извлекает паттерны использования JWT токенов"""
        # Заглушка для демонстрации
        return {
            "average_token_lifetime": 3600,  # секунды
            "token_usage_frequency": 0.8,  # использований в минуту
            "multi_device_usage": 0.3,  # доля мульти-девайс использования
        }

    @staticmethod
    def _get_request_timestamps() -> List[float]:
        """Возвращает временные метки запросов"""
        global _request_timestamps
        return _request_timestamps

    @staticmethod
    def _get_user_request_data() -> List[Tuple[float, str]]:
        """Возвращает данные о запросах пользователей"""
        global _user_request_data
        return _user_request_data

    @staticmethod
    def _get_request_ip_data() -> List[Tuple[float, str]]:
        """Возвращает данные об IP-адресах запросов"""
        global _request_ip_data
        return _request_ip_data

    @staticmethod
    def _get_auth_events() -> List[Dict[str, Any]]:
        """Возвращает события авторизации"""
        global _auth_events
        return _auth_events

    @staticmethod
    def _get_channel_activity() -> Dict[str, int]:
        """Возвращает активность по каналам"""
        global _channel_activity
        return dict(_channel_activity)


# Функции для обновления данных для ML-анализа

# Глобальные хранилища данных для ML-анализа
_request_timestamps = []
_user_request_data = []
_request_ip_data = []
_auth_events = []
_channel_activity = defaultdict(int)


def register_request(
    timestamp: float = None, user_id: str = None, ip_address: str = None
):
    """Регистрирует новый запрос для ML-анализа

    Args:
        timestamp: Временная метка запроса, если None - текущее время
        user_id: ID пользователя, сделавшего запрос
        ip_address: IP-адрес, с которого пришел запрос
    """
    if not ML_ENABLED:
        return

    if timestamp is None:
        timestamp = time.time()

    # Добавляем временную метку и обрезаем историю при необходимости
    global _request_timestamps
    _request_timestamps.append(timestamp)
    if len(_request_timestamps) > 1000:
        _request_timestamps = _request_timestamps[-1000:]

    # Если указан пользователь, добавляем в историю пользовательских запросов
    if user_id:
        global _user_request_data
        _user_request_data.append((timestamp, user_id))
        if len(_user_request_data) > 1000:
            _user_request_data = _user_request_data[-1000:]

    # Если указан IP-адрес, добавляем в историю IP
    if ip_address:
        global _request_ip_data
        _request_ip_data.append((timestamp, ip_address))
        if len(_request_ip_data) > 1000:
            _request_ip_data = _request_ip_data[-1000:]


def register_ip_address(ip: str) -> None:
    """Регистрирует IP адрес для анализа

    Note: Устаревшая функция, используйте register_request с параметром ip_address
    """
    register_request(timestamp=time.time(), ip_address=ip)


def register_channel_activity(channel: str) -> None:
    """Регистрирует активность в канале"""
    if not ML_ENABLED:
        return

    global _channel_activity
    _channel_activity[channel] += 1


def register_auth_event(user_id: str, success: bool) -> None:
    """Регистрирует событие авторизации"""
    if not ML_ENABLED:
        return

    global _auth_events
    _auth_events.append(
        {"user_id": user_id, "timestamp": time.time(), "success": success}
    )

    # Ограничиваем размер истории событий
    if len(_auth_events) > 1000:
        _auth_events = _auth_events[-1000:]


# Экспортируем функции извлечения признаков для использования в API
extract_user_patterns = MLFeatureExtractor.extract_user_patterns
extract_traffic_features = MLFeatureExtractor.extract_traffic_features
get_current_anomaly_scores = MLFeatureExtractor.get_current_anomaly_scores
get_prediction_features = MLFeatureExtractor.get_prediction_features
