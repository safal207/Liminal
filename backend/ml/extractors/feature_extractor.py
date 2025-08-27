#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Экстрактор фичей для ML-моделей с интеграцией Kenning
"""

import argparse
import json
import logging
import os
import sys
import time
from typing import Any, Dict

import requests

import redis

# Настройка логгирования
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# Константы
REDIS_HOST = os.environ.get("REDIS_HOST", "localhost")
REDIS_PORT = int(os.environ.get("REDIS_PORT", "6379"))
BACKEND_URL = os.environ.get("BACKEND_URL", "http://localhost:8000")
KENNING_URL = os.environ.get("KENNING_URL", "http://kenning-ml:5000")
EXTRACT_INTERVAL = int(os.environ.get("EXTRACT_INTERVAL", "30"))  # секунд

# Подключение к Redis
try:
    redis_client = redis.Redis(host=REDIS_HOST, port=REDIS_PORT)
    redis_client.ping()  # Проверка соединения
    logger.info(f"Connected to Redis at {REDIS_HOST}:{REDIS_PORT}")
except Exception as e:
    logger.error(f"Failed to connect to Redis: {e}")
    redis_client = None


class FeatureExtractor:
    """Класс для извлечения признаков и отправки их в Kenning"""

    def __init__(self):
        """Инициализация экстрактора фичей"""
        self.features = {}
        self.last_extract_time = 0

    def extract_features_from_backend(self) -> Dict[str, Any]:
        """
        Получение признаков из бэкенда через API

        Returns:
            Словарь с извлеченными признаками
        """
        try:
            # Запрос к API бэкенда для получения ML-метрик
            response = requests.get(f"{BACKEND_URL}/ml_metrics")

            if response.status_code == 200:
                data = response.json()
                logger.info(f"Received features from backend: {list(data.keys())}")
                return data
            else:
                logger.error(
                    f"Failed to get features from backend: {response.status_code}"
                )
                return {}
        except Exception as e:
            logger.error(f"Error getting features from backend: {e}")
            return {}

    def store_features_in_redis(self, features: Dict[str, Any]) -> None:
        """
        Сохранение признаков в Redis для использования Kenning

        Args:
            features: Словарь с признаками
        """
        if not redis_client:
            logger.error("Redis client not available")
            return

        timestamp = time.time()

        try:
            # Сохраняем каждый признак отдельно для удобства доступа
            for feature_name, value in features.items():
                if isinstance(value, (int, float)):
                    feature_data = {"timestamp": timestamp, "value": value}
                    redis_client.set(
                        f"ml_feature:{feature_name}", json.dumps(feature_data)
                    )
                    redis_client.expire(
                        f"ml_feature:{feature_name}", 86400
                    )  # TTL 1 день
                elif isinstance(value, dict):
                    # Для сложных метрик (словарей), сохраняем каждую подметрику
                    for subfeature, subvalue in value.items():
                        if isinstance(subvalue, (int, float)):
                            feature_data = {"timestamp": timestamp, "value": subvalue}
                            redis_client.set(
                                f"ml_feature:{feature_name}_{subfeature}",
                                json.dumps(feature_data),
                            )
                            redis_client.expire(
                                f"ml_feature:{feature_name}_{subfeature}", 86400
                            )

            # Также сохраняем полный набор признаков
            redis_client.set(
                "ml_features:latest",
                json.dumps({"timestamp": timestamp, "features": features}),
            )
            redis_client.expire("ml_features:latest", 86400)

            logger.info("Features stored in Redis")
        except Exception as e:
            logger.error(f"Failed to store features in Redis: {e}")

    def send_features_to_kenning(self, features: Dict[str, Any]) -> Dict[str, Any]:
        """
        Отправка признаков в Kenning для анализа и предсказаний

        Args:
            features: Словарь с признаками

        Returns:
            Результат предсказания от Kenning
        """
        try:
            # Готовим данные для отправки в формате, понятном Kenning
            kenning_data = {
                "timestamp": time.time(),
                "features": features,
            }

            # Отправляем запрос на предсказание
            response = requests.post(
                f"{KENNING_URL}/predict",
                json=kenning_data,
                headers={"Content-Type": "application/json"},
            )

            if response.status_code == 200:
                result = response.json()
                logger.info("Prediction received from Kenning")
                return result
            else:
                logger.error(
                    f"Failed to get prediction from Kenning: {response.status_code}"
                )
                return {}
        except Exception as e:
            logger.error(f"Error sending features to Kenning: {e}")
            return {}

    def store_predictions(self, predictions: Dict[str, Any]) -> None:
        """
        Сохранение результатов предсказаний в Redis

        Args:
            predictions: Словарь с предсказаниями
        """
        if not redis_client or not predictions:
            return

        try:
            timestamp = time.time()

            # Сохраняем предсказания
            redis_client.set(
                "ml_predictions:latest",
                json.dumps({"timestamp": timestamp, "predictions": predictions}),
            )
            redis_client.expire("ml_predictions:latest", 86400)

            # Отдельно сохраняем score аномалий для быстрого доступа
            if "anomaly_score" in predictions:
                redis_client.set("ml_anomaly_score", predictions["anomaly_score"])
                redis_client.expire("ml_anomaly_score", 86400)

            logger.info("Predictions stored in Redis")
        except Exception as e:
            logger.error(f"Failed to store predictions in Redis: {e}")

    def run_extraction_loop(self) -> None:
        """Основной цикл извлечения признаков и отправки в Kenning"""
        logger.info(
            f"Starting feature extraction loop with interval {EXTRACT_INTERVAL}s"
        )

        while True:
            try:
                # Извлекаем признаки из бэкенда
                features = self.extract_features_from_backend()

                if features:
                    # Сохраняем признаки в Redis
                    self.store_features_in_redis(features)

                    # Отправляем признаки в Kenning и получаем предсказания
                    predictions = self.send_features_to_kenning(features)

                    # Сохраняем предсказания
                    self.store_predictions(predictions)
                else:
                    logger.warning("No features extracted")

            except Exception as e:
                logger.error(f"Error in extraction loop: {e}")

            # Ждем до следующего извлечения
            time.sleep(EXTRACT_INTERVAL)


def main():
    """Точка входа для запуска экстрактора фичей"""
    parser = argparse.ArgumentParser(
        description="ML Feature Extractor for Kenning integration"
    )
    parser.add_argument(
        "--once", action="store_true", help="Run extraction once and exit"
    )
    parser.add_argument(
        "--interval",
        type=int,
        default=EXTRACT_INTERVAL,
        help=f"Extraction interval in seconds (default: {EXTRACT_INTERVAL})",
    )

    args = parser.parse_args()

    global EXTRACT_INTERVAL
    EXTRACT_INTERVAL = args.interval

    extractor = FeatureExtractor()

    if args.once:
        # Запуск однократного извлечения
        features = extractor.extract_features_from_backend()

        if features:
            extractor.store_features_in_redis(features)
            predictions = extractor.send_features_to_kenning(features)
            extractor.store_predictions(predictions)

            logger.info("Single extraction completed")
            sys.exit(0)
        else:
            logger.error("Failed to extract features")
            sys.exit(1)
    else:
        # Запуск в режиме постоянной работы
        try:
            extractor.run_extraction_loop()
        except KeyboardInterrupt:
            logger.info("Extraction loop stopped by user")
            sys.exit(0)


if __name__ == "__main__":
    main()
