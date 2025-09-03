"""
ML Metrics Exporter для Prometheus.
Собирает расширенные фичи для ML-моделей из различных источников.
"""

import asyncio
import json
import math
import time
from collections import defaultdict, deque
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, Request
from loguru import logger
# Prometheus метрики
from prometheus_client import (CONTENT_TYPE_LATEST, Counter, Gauge, Histogram,
                               generate_latest)

router = APIRouter(prefix="/ml_metrics", tags=["ml-metrics"])


@dataclass
class MLMetrics:
    """Структура для ML-метрик."""

    timestamp: float
    requests_per_user: Dict[str, float]
    average_request_rate: float
    burstiness_score: float
    ip_entropy: float
    channel_activity: Dict[str, float]
    geographic_distribution: Dict[str, int]
    jwt_usage_patterns: Dict[str, Any]
    connection_patterns: Dict[str, Any]


class MLMetricsCollector:
    """
    Коллектор расширенных ML-метрик для Prometheus.
    Агрегирует данные из WebSocket, Redis, логов для feature engineering.
    """

    def __init__(self):
        # Временные окна для анализа
        self.time_windows = {
            "1m": deque(maxlen=60),  # 1 минута
            "5m": deque(maxlen=300),  # 5 минут
            "15m": deque(maxlen=900),  # 15 минут
            "1h": deque(maxlen=3600),  # 1 час
        }

        # Счетчики для расчета метрик
        self.user_requests = defaultdict(list)  # {user_id: [timestamps]}
        self.ip_requests = defaultdict(int)  # {ip: count}
        self.channel_activity = defaultdict(list)  # {channel: [timestamps]}
        self.jwt_events = defaultdict(list)  # {user_id: [jwt_events]}
        self.connection_events = []  # [(timestamp, event_type, user_id)]

        # Prometheus метрики для ML
        self.ml_requests_per_user = Gauge(
            "ml_requests_per_user_rate",
            "Requests per user rate for ML analysis",
            ["user_id", "time_window"],
        )

        self.ml_burstiness_score = Gauge(
            "ml_burstiness_score", "Traffic burstiness score for anomaly detection"
        )

        self.ml_ip_entropy = Gauge(
            "ml_ip_entropy", "IP address entropy for geographic analysis"
        )

        self.ml_channel_hotness = Gauge(
            "ml_channel_hotness", "Channel activity hotness score", ["channel"]
        )

        self.ml_connection_patterns = Gauge(
            "ml_connection_patterns", "Connection pattern analysis", ["pattern_type"]
        )

        self.ml_jwt_anomalies = Counter(
            "ml_jwt_anomalies_total", "JWT usage anomalies detected", ["anomaly_type"]
        )

    def track_request(self, user_id: str, ip_address: str, channel: str = None) -> None:
        """
        Отслеживает запрос для ML-анализа.

        Args:
            user_id: ID пользователя
            ip_address: IP адрес
            channel: Канал (опционально)
        """
        current_time = time.time()

        # Добавляем в временные окна
        for window in self.time_windows.values():
            window.append(current_time)

        # Отслеживаем по пользователям
        self.user_requests[user_id].append(current_time)
        # Ограничиваем историю (последние 1000 запросов на пользователя)
        if len(self.user_requests[user_id]) > 1000:
            self.user_requests[user_id] = self.user_requests[user_id][-1000:]

        # Отслеживаем по IP
        self.ip_requests[ip_address] += 1

        # Отслеживаем активность каналов
        if channel:
            self.channel_activity[channel].append(current_time)
            if len(self.channel_activity[channel]) > 1000:
                self.channel_activity[channel] = self.channel_activity[channel][-1000:]

    def track_connection_event(self, user_id: str, event_type: str) -> None:
        """
        Отслеживает события подключения/отключения.

        Args:
            user_id: ID пользователя
            event_type: connect/disconnect/auth_success/auth_failure
        """
        current_time = time.time()
        self.connection_events.append((current_time, event_type, user_id))

        # Ограничиваем историю
        if len(self.connection_events) > 10000:
            self.connection_events = self.connection_events[-10000:]

    def track_jwt_event(self, user_id: str, event_data: Dict[str, Any]) -> None:
        """
        Отслеживает события JWT для анализа паттернов.

        Args:
            user_id: ID пользователя
            event_data: Данные события (token_age, source_ip, etc.)
        """
        current_time = time.time()
        event_data["timestamp"] = current_time

        self.jwt_events[user_id].append(event_data)
        if len(self.jwt_events[user_id]) > 100:
            self.jwt_events[user_id] = self.jwt_events[user_id][-100:]

    def calculate_requests_per_user(self, time_window: str = "1m") -> Dict[str, float]:
        """
        Вычисляет количество запросов на пользователя за временное окно.

        Args:
            time_window: Временное окно (1m, 5m, 15m, 1h)

        Returns:
            Словарь {user_id: requests_per_minute}
        """
        current_time = time.time()
        window_seconds = {"1m": 60, "5m": 300, "15m": 900, "1h": 3600}
        window_duration = window_seconds.get(time_window, 60)

        result = {}
        for user_id, timestamps in self.user_requests.items():
            # Считаем запросы за последнее временное окно
            recent_requests = [
                ts for ts in timestamps if current_time - ts <= window_duration
            ]
            requests_per_minute = len(recent_requests) * (60 / window_duration)
            result[user_id] = requests_per_minute

            # Обновляем Prometheus метрику
            self.ml_requests_per_user.labels(
                user_id=user_id, time_window=time_window
            ).set(requests_per_minute)

        return result

    def calculate_burstiness_score(self, time_window: str = "5m") -> float:
        """
        Вычисляет коэффициент "всплесковости" трафика.
        Высокий score указывает на возможную DDoS-атаку.

        Returns:
            Burstiness score (0-1, где 1 = максимальная всплесковость)
        """
        window_data = self.time_windows.get(time_window, self.time_windows["5m"])
        if len(window_data) < 10:
            return 0.0

        # Разбиваем временное окно на интервалы
        current_time = time.time()
        interval_duration = 30  # 30-секундные интервалы
        intervals = defaultdict(int)

        for timestamp in window_data:
            interval = int((current_time - timestamp) // interval_duration)
            intervals[interval] += 1

        # Вычисляем коэффициент вариации
        if not intervals:
            return 0.0

        values = list(intervals.values())
        mean_val = sum(values) / len(values)
        if mean_val == 0:
            return 0.0

        variance = sum((x - mean_val) ** 2 for x in values) / len(values)
        cv = math.sqrt(variance) / mean_val  # Coefficient of Variation

        # Нормализуем в диапазон 0-1
        burstiness = min(cv / 2.0, 1.0)

        # Обновляем Prometheus метрику
        self.ml_burstiness_score.set(burstiness)

        return burstiness

    def calculate_ip_entropy(self) -> float:
        """
        Вычисляет энтропию распределения IP-адресов.
        Низкая энтропия может указывать на ботнет или координированную атаку.

        Returns:
            IP entropy score (0-1, где 1 = максимальное разнообразие)
        """
        if not self.ip_requests:
            return 0.0

        total_requests = sum(self.ip_requests.values())
        if total_requests == 0:
            return 0.0

        # Вычисляем энтропию Шеннона
        entropy = 0.0
        for count in self.ip_requests.values():
            if count > 0:
                probability = count / total_requests
                entropy -= probability * math.log2(probability)

        # Нормализуем относительно максимальной возможной энтропии
        max_entropy = (
            math.log2(len(self.ip_requests)) if len(self.ip_requests) > 1 else 1
        )
        normalized_entropy = entropy / max_entropy if max_entropy > 0 else 0

        # Обновляем Prometheus метрику
        self.ml_ip_entropy.set(normalized_entropy)

        return normalized_entropy

    def calculate_channel_hotness(self) -> Dict[str, float]:
        """
        Вычисляет "горячность" каналов для предсказания нагрузки.

        Returns:
            Словарь {channel: hotness_score}
        """
        current_time = time.time()
        hotness_scores = {}

        for channel, timestamps in self.channel_activity.items():
            # Активность за последние 5 минут
            recent_activity = [ts for ts in timestamps if current_time - ts <= 300]

            # Простой score: запросы в минуту с весом по времени
            if recent_activity:
                # Более свежие запросы имеют больший вес
                weighted_score = sum(
                    1.0 / (1.0 + (current_time - ts) / 60)  # Экспоненциальный спад
                    for ts in recent_activity
                )
                hotness_scores[channel] = weighted_score
            else:
                hotness_scores[channel] = 0.0

            # Обновляем Prometheus метрику
            self.ml_channel_hotness.labels(channel=channel).set(hotness_scores[channel])

        return hotness_scores

    def analyze_connection_patterns(self) -> Dict[str, float]:
        """
        Анализирует паттерны подключений для выявления аномалий.

        Returns:
            Словарь с различными метриками паттернов
        """
        current_time = time.time()
        recent_events = [
            event
            for event in self.connection_events
            if current_time - event[0] <= 300  # Последние 5 минут
        ]

        if not recent_events:
            return {}

        # Анализируем различные паттерны
        patterns = {
            "connection_rate": 0.0,
            "auth_failure_rate": 0.0,
            "rapid_reconnects": 0.0,
            "unique_users": 0.0,
        }

        # Подсчитываем события по типам
        event_counts = defaultdict(int)
        unique_users = set()

        for timestamp, event_type, user_id in recent_events:
            event_counts[event_type] += 1
            unique_users.add(user_id)

        total_events = len(recent_events)
        if total_events > 0:
            patterns["connection_rate"] = (
                event_counts.get("connect", 0) / 5.0
            )  # за 5 минут
            patterns["auth_failure_rate"] = event_counts.get("auth_failure", 0) / max(
                event_counts.get("connect", 1), 1
            )
            patterns["unique_users"] = len(unique_users)

            # Быстрые переподключения (connect -> disconnect -> connect)
            rapid_reconnects = 0
            user_events = defaultdict(list)
            for timestamp, event_type, user_id in recent_events:
                user_events[user_id].append((timestamp, event_type))

            for user_id, events in user_events.items():
                events.sort()  # Сортируем по времени
                for i in range(len(events) - 2):
                    if (
                        events[i][1] == "connect"
                        and events[i + 1][1] == "disconnect"
                        and events[i + 2][1] == "connect"
                        and events[i + 2][0] - events[i][0] < 60
                    ):  # В течение минуты
                        rapid_reconnects += 1

            patterns["rapid_reconnects"] = rapid_reconnects

        # Обновляем Prometheus метрики
        for pattern_type, value in patterns.items():
            self.ml_connection_patterns.labels(pattern_type=pattern_type).set(value)

        return patterns

    def detect_jwt_anomalies(self) -> Dict[str, int]:
        """
        Обнаруживает аномалии в использовании JWT токенов.

        Returns:
            Словарь с количеством различных типов аномалий
        """
        anomalies = defaultdict(int)
        current_time = time.time()

        for user_id, events in self.jwt_events.items():
            recent_events = [
                event
                for event in events
                if current_time - event["timestamp"] <= 3600  # Последний час
            ]

            if len(recent_events) < 2:
                continue

            # Анализируем различные аномалии

            # 1. Слишком частое обновление токенов
            if len(recent_events) > 10:  # Более 10 обновлений за час
                anomalies["frequent_token_refresh"] += 1
                self.ml_jwt_anomalies.labels(
                    anomaly_type="frequent_token_refresh"
                ).inc()

            # 2. Использование токенов с разных IP
            unique_ips = set(event.get("source_ip", "") for event in recent_events)
            if len(unique_ips) > 3:  # Более 3 разных IP за час
                anomalies["multiple_ip_usage"] += 1
                self.ml_jwt_anomalies.labels(anomaly_type="multiple_ip_usage").inc()

            # 3. Использование старых токенов
            for event in recent_events:
                token_age = event.get("token_age", 0)
                if token_age > 86400:  # Токен старше суток
                    anomalies["old_token_usage"] += 1
                    self.ml_jwt_anomalies.labels(anomaly_type="old_token_usage").inc()
                    break

        return dict(anomalies)

    def get_ml_metrics(self) -> MLMetrics:
        """
        Собирает все ML-метрики в единую структуру.

        Returns:
            MLMetrics объект со всеми вычисленными метриками
        """
        current_time = time.time()

        return MLMetrics(
            timestamp=current_time,
            requests_per_user=self.calculate_requests_per_user("1m"),
            average_request_rate=len(self.time_windows["1m"]),
            burstiness_score=self.calculate_burstiness_score(),
            ip_entropy=self.calculate_ip_entropy(),
            channel_activity=self.calculate_channel_hotness(),
            geographic_distribution={},  # Заглушка для будущей реализации
            jwt_usage_patterns=self.detect_jwt_anomalies(),
            connection_patterns=self.analyze_connection_patterns(),
        )


# Глобальный экземпляр коллектора
ml_metrics_collector = MLMetricsCollector()


@router.get("/")
async def get_ml_metrics_summary() -> Dict[str, Any]:
    """
    Возвращает сводку ML-метрик в JSON формате.
    """
    try:
        metrics = ml_metrics_collector.get_ml_metrics()
        return {
            "timestamp": metrics.timestamp,
            "summary": {
                "total_users": len(metrics.requests_per_user),
                "average_request_rate": metrics.average_request_rate,
                "burstiness_score": metrics.burstiness_score,
                "ip_entropy": metrics.ip_entropy,
                "active_channels": len(metrics.channel_activity),
                "connection_anomalies": sum(metrics.connection_patterns.values()),
                "jwt_anomalies": sum(metrics.jwt_usage_patterns.values()),
            },
            "details": {
                "top_active_users": dict(
                    sorted(
                        metrics.requests_per_user.items(),
                        key=lambda x: x[1],
                        reverse=True,
                    )[:10]
                ),
                "hot_channels": dict(
                    sorted(
                        metrics.channel_activity.items(),
                        key=lambda x: x[1],
                        reverse=True,
                    )[:10]
                ),
                "connection_patterns": metrics.connection_patterns,
                "jwt_anomalies": metrics.jwt_usage_patterns,
            },
        }
    except Exception as e:
        logger.error(f"Ошибка получения ML-метрик: {e}")
        return {"error": str(e), "timestamp": time.time()}


@router.get("/prometheus")
async def get_ml_metrics_prometheus():
    """
    Возвращает ML-метрики в формате Prometheus.
    """
    try:
        # Обновляем все метрики перед экспортом
        ml_metrics_collector.get_ml_metrics()

        # Возвращаем метрики в формате Prometheus
        return generate_latest()
    except Exception as e:
        logger.error(f"Ошибка экспорта Prometheus метрик: {e}")
        return f"# ERROR: {str(e)}\n"
