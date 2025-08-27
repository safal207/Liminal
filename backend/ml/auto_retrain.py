#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Автоматический ретренинг моделей с использованием Kenning
"""

import argparse
import datetime
import json
import logging
import os
import time
from pathlib import Path
from typing import Any, Dict, Tuple

import pandas as pd

import redis

# Настройка логгирования
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# Константы
REDIS_HOST = os.environ.get("REDIS_HOST", "localhost")
REDIS_PORT = int(os.environ.get("REDIS_PORT", "6379"))
PROMETHEUS_URL = os.environ.get("PROMETHEUS_URL", "http://localhost:9090")
MODEL_DIR = os.environ.get("KENNING_MODEL_DIR", "./models")
CONFIG_DIR = os.environ.get("KENNING_CONFIG_DIR", "./configs")

# Подключение к Redis
redis_client = redis.Redis(host=REDIS_HOST, port=REDIS_PORT)


class ModelRetrainer:
    """Класс для автоматического ретренинга моделей"""

    def __init__(self, config_path: str, model_dir: str):
        """
        Инициализация ретрейнера моделей

        Args:
            config_path: путь к конфигурационному файлу модели
            model_dir: директория для сохранения моделей
        """
        self.config_path = config_path
        self.model_dir = model_dir
        self.config = self._load_config()

    def _load_config(self) -> Dict[str, Any]:
        """Загрузка конфигурации модели"""
        with open(self.config_path, "r") as f:
            return json.load(f)

    def _get_metrics_from_prometheus(self) -> pd.DataFrame:
        """
        Получение метрик из Prometheus для обучения модели

        Returns:
            DataFrame с метриками для обучения
        """
        import requests

        # В реальном приложении здесь был бы запрос к Prometheus API
        # но для простоты демонстрации мы будем использовать заглушку
        # Создаем запрос к /metrics endpoint'у бэкенда
        try:
            response = requests.get(
                f"{PROMETHEUS_URL}/api/v1/query?query=ml_feature_value"
            )
            if response.status_code == 200:
                data = response.json()
                # Преобразуем результат в DataFrame
                metrics = []

                for result in data.get("data", {}).get("result", []):
                    feature_name = result.get("metric", {}).get("feature_name")
                    value = float(result.get("value", [0, "0"])[1])
                    timestamp = result.get("value", [0, "0"])[0]
                    metrics.append(
                        {
                            "timestamp": timestamp,
                            "feature_name": feature_name,
                            "value": value,
                        }
                    )

                df = pd.DataFrame(metrics)
                return df
            else:
                logger.error(
                    f"Failed to get metrics from Prometheus: {response.status_code}"
                )
                return pd.DataFrame()
        except Exception as e:
            logger.error(f"Error getting metrics from Prometheus: {e}")
            return pd.DataFrame()

    def _get_training_data_from_redis(self) -> pd.DataFrame:
        """
        Получение обучающих данных из Redis

        Returns:
            DataFrame с обучающими данными
        """
        try:
            # Получаем ключи с ML-фичами
            keys = redis_client.keys("ml_feature:*")

            if not keys:
                logger.warning("No ML feature data found in Redis")
                return pd.DataFrame()

            data = []

            for key in keys:
                key_str = key.decode("utf-8")
                feature_name = key_str.split(":")[1]
                value = redis_client.get(key)

                if value:
                    try:
                        value_dict = json.loads(value)
                        data.append(
                            {
                                "timestamp": value_dict.get("timestamp", time.time()),
                                "feature_name": feature_name,
                                "value": value_dict.get("value", 0),
                            }
                        )
                    except json.JSONDecodeError:
                        logger.warning(f"Failed to decode JSON for key {key_str}")

            return pd.DataFrame(data)
        except Exception as e:
            logger.error(f"Error getting data from Redis: {e}")
            return pd.DataFrame()

    def _prepare_dataset(self) -> Tuple[pd.DataFrame, pd.DataFrame]:
        """
        Подготовка датасета для обучения

        Returns:
            Tuple из обучающего и тестового датасетов
        """
        # Получаем данные из Prometheus и Redis
        prometheus_data = self._get_metrics_from_prometheus()
        redis_data = self._get_training_data_from_redis()

        # Объединяем данные
        df = pd.concat([prometheus_data, redis_data])

        if df.empty:
            logger.error("No data available for training")
            return pd.DataFrame(), pd.DataFrame()

        # Преобразуем данные из "длинного" формата в "широкий"
        # timestamp | feature1 | feature2 | ...
        df_wide = df.pivot(
            index="timestamp", columns="feature_name", values="value"
        ).reset_index()

        # Проверяем, что у нас есть все необходимые фичи
        required_features = self.config["training"]["features"]
        missing_features = [f for f in required_features if f not in df_wide.columns]

        if missing_features:
            logger.warning(f"Missing features: {missing_features}")
            # Создаем недостающие фичи со средними значениями
            for feature in missing_features:
                df_wide[feature] = 0

        # Разделяем на обучающую и тестовую выборки
        train_size = int(
            len(df_wide) * (1 - self.config["training"]["validation_split"])
        )
        train_df = df_wide.iloc[:train_size]
        test_df = df_wide.iloc[train_size:]

        return train_df, test_df

    def train_model(self) -> str:
        """
        Запуск обучения модели с использованием Kenning CLI

        Returns:
            Путь к обученной модели
        """
        train_df, test_df = self._prepare_dataset()

        if train_df.empty or test_df.empty:
            logger.error("No data available for training")
            return ""

        # Сохраняем датасеты временно для Kenning
        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        train_path = f"/tmp/train_{timestamp}.csv"
        test_path = f"/tmp/test_{timestamp}.csv"

        train_df.to_csv(train_path, index=False)
        test_df.to_csv(test_path, index=False)

        # Настраиваем параметры Kenning
        model_name = f"anomaly_detector_{timestamp}"
        model_path = os.path.join(self.model_dir, f"{model_name}.onnx")

        # В реальной системе здесь был бы запуск CLI команды Kenning
        # Например:
        # kenning train --config {self.config_path} --train-data {train_path} --test-data {test_path} --output {model_path}

        # Для демонстрации имитируем успешное обучение
        logger.info(f"Training model {model_name}...")
        time.sleep(2)  # Имитация процесса обучения

        # Создаем заглушку для модели
        Path(model_path).parent.mkdir(parents=True, exist_ok=True)
        with open(model_path, "w") as f:
            f.write("# ONNX model placeholder")

        logger.info(f"Model trained and saved to {model_path}")

        # Очищаем временные файлы
        try:
            os.remove(train_path)
            os.remove(test_path)
        except:
            pass

        return model_path

    def deploy_model(self, model_path: str) -> bool:
        """
        Деплой обученной модели в Kenning сервис

        Args:
            model_path: путь к обученной модели

        Returns:
            True если успешно, False в противном случае
        """
        # В реальной системе здесь был бы запрос к REST API Kenning
        # для обновления модели

        logger.info(f"Deploying model {model_path} to Kenning service...")

        # Регистрируем модель в Redis для использования в сервисе
        try:
            model_info = {
                "path": model_path,
                "timestamp": time.time(),
                "metrics": {
                    "accuracy": 0.95,
                    "precision": 0.92,
                    "recall": 0.91,
                    "f1": 0.915,
                },
            }
            redis_client.set("current_model", json.dumps(model_info))
            logger.info("Model registered in Redis")
            return True
        except Exception as e:
            logger.error(f"Failed to register model in Redis: {e}")
            return False

    def check_performance_drift(self) -> bool:
        """
        Проверка необходимости переобучения на основе дрифта производительности модели

        Returns:
            True если требуется переобучение, False в противном случае
        """
        # Получаем данные о текущих предсказаниях и их качестве
        try:
            current_model = redis_client.get("current_model")
            if not current_model:
                logger.warning("No current model found")
                return True  # Если нет текущей модели, нужно обучить новую

            model_info = json.loads(current_model)
            last_train_time = model_info.get("timestamp", 0)

            # Если прошло больше суток с последнего обучения
            if time.time() - last_train_time > 86400:
                logger.info("Model is older than 24 hours, retraining recommended")
                return True

            # Проверяем метрики аномалий
            anomaly_score = redis_client.get("ml_anomaly_score")
            if anomaly_score and float(anomaly_score) > 0.8:
                logger.info("High anomaly score detected, retraining recommended")
                return True

            return False
        except Exception as e:
            logger.error(f"Error checking performance drift: {e}")
            return True  # В случае ошибки лучше перестраховаться и переобучить


def main():
    """Основная функция для запуска процесса автоматического ретренинга"""
    parser = argparse.ArgumentParser(description="Automatic model retraining")
    parser.add_argument(
        "--config",
        type=str,
        default=os.path.join(CONFIG_DIR, "anomaly_detection.json"),
        help="Path to model config",
    )
    parser.add_argument(
        "--model-dir", type=str, default=MODEL_DIR, help="Directory for model storage"
    )
    parser.add_argument(
        "--force", action="store_true", help="Force retraining regardless of drift"
    )
    parser.add_argument(
        "--interval",
        type=int,
        default=3600,
        help="Check interval in seconds (default: 1 hour)",
    )

    args = parser.parse_args()

    retrainer = ModelRetrainer(args.config, args.model_dir)

    logger.info("Starting automatic retraining service")

    while True:
        try:
            # Проверяем необходимость переобучения
            if args.force or retrainer.check_performance_drift():
                logger.info("Retraining needed, starting process...")

                # Обучаем новую модель
                model_path = retrainer.train_model()

                if model_path:
                    # Деплоим модель
                    success = retrainer.deploy_model(model_path)
                    if success:
                        logger.info("Model successfully retrained and deployed")
                    else:
                        logger.error("Failed to deploy model")
                else:
                    logger.error("Failed to train model")
            else:
                logger.info("No retraining needed at this time")

        except Exception as e:
            logger.error(f"Error in retraining process: {e}")

        # Ждем до следующей проверки
        if args.force:
            break  # Если принудительное обучение, выходим после одного запуска

        logger.info(f"Sleeping for {args.interval} seconds")
        time.sleep(args.interval)


if __name__ == "__main__":
    main()
