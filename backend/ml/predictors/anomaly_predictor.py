#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Предиктор аномалий с использованием Kenning
"""

import argparse
import datetime
import json
import logging
import os
import sys
import threading
import time
from typing import Any, Dict, List, Optional, Tuple

import numpy as np
import requests
from flask import Flask, jsonify, request

import redis

# Настройка логгирования
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# Константы
REDIS_HOST = os.environ.get("REDIS_HOST", "localhost")
REDIS_PORT = int(os.environ.get("REDIS_PORT", "6379"))
KENNING_URL = os.environ.get("KENNING_URL", "http://kenning-ml:5000")
UPDATE_INTERVAL = int(os.environ.get("UPDATE_INTERVAL", "30"))  # секунд
HOST = os.environ.get("HOST", "0.0.0.0")
PORT = int(os.environ.get("PORT", "8002"))

# Подключение к Redis
try:
    redis_client = redis.Redis(host=REDIS_HOST, port=REDIS_PORT)
    redis_client.ping()  # Проверка соединения
    logger.info(f"Connected to Redis at {REDIS_HOST}:{REDIS_PORT}")
except Exception as e:
    logger.error(f"Failed to connect to Redis: {e}")
    redis_client = None

# Создание Flask приложения
app = Flask(__name__)


class AnomalyPredictor:
    """Класс для предсказания аномалий с использованием Kenning"""

    def __init__(self):
        """Инициализация предиктора аномалий"""
        self.latest_predictions = {}
        self.latest_features = {}
        self.model_info = {}
        self.model_loaded = False
        self.last_update_time = 0
        self.explanations = {}
        self.prediction_store = {}  # хранилище предсказаний с ID
        self.explanation_store = {}  # хранилище объяснений с ID

    def get_model_info(self) -> Dict[str, Any]:
        """
        Получение информации о текущей модели из Redis

        Returns:
            Словарь с информацией о модели
        """
        if not redis_client:
            return {}

        try:
            model_info = redis_client.get("current_model")
            if model_info:
                return json.loads(model_info)
            return {}
        except Exception as e:
            logger.error(f"Failed to get model info from Redis: {e}")
            return {}

    def get_latest_features(self) -> Dict[str, Any]:
        """
        Получение последних извлеченных признаков из Redis

        Returns:
            Словарь с последними признаками
        """
        if not redis_client:
            return {}

        try:
            features_data = redis_client.get("ml_features:latest")
            if features_data:
                return json.loads(features_data).get("features", {})
            return {}
        except Exception as e:
            logger.error(f"Failed to get features from Redis: {e}")
            return {}

    def get_latest_predictions(self) -> Dict[str, Any]:
        """
        Получение последних предсказаний из Redis

        Returns:
            Словарь с последними предсказаниями
        """
        if not redis_client:
            return {}

        try:
            predictions_data = redis_client.get("ml_predictions:latest")
            if predictions_data:
                return json.loads(predictions_data).get("predictions", {})
            return {}
        except Exception as e:
            logger.error(f"Failed to get predictions from Redis: {e}")
            return {}

    def make_prediction(self, features: Dict[str, Any]) -> Dict[str, Any]:
        """
        Выполнение предсказания аномалий на основе признаков

        Args:
            features: Словарь с признаками

        Returns:
            Словарь с результатами предсказания и ID
        """
        try:
            # Отправляем запрос к Kenning ML сервису
            response = requests.post(
                f"{KENNING_URL}/predict",
                json={"model_name": "anomaly_detection", "features": features},
                headers={"Content-Type": "application/json"},
            )

            if response.status_code == 200:
                prediction_data = response.json()
                prediction = prediction_data.get("prediction", {})

                # Создаем уникальный ID для предсказания
                timestamp = int(time.time())
                prediction_id = f"pred_{timestamp}_{hash(json.dumps(features, sort_keys=True)) % 10000}"

                # Добавляем ID к предсказанию
                prediction["prediction_id"] = prediction_id

                # Сохраняем предсказание в локальном хранилище
                self.prediction_store[prediction_id] = {
                    "timestamp": timestamp,
                    "features": features,
                    "prediction": prediction,
                }

                # Сохраняем предсказание в Redis, если подключен
                if redis_client:
                    result_key = f"anomaly:prediction:{prediction_id}"
                    redis_client.set(
                        result_key,
                        json.dumps(
                            {
                                "timestamp": timestamp,
                                "features": features,
                                "prediction": prediction,
                            }
                        ),
                    )

                    # Также сохраняем в индексе предсказаний
                    redis_client.zadd(
                        "anomaly:prediction:index", {prediction_id: timestamp}
                    )

                return prediction
            else:
                logger.error(
                    f"Error from Kenning ML: {response.status_code} - {response.text}"
                )
                return {"error": f"ML service returned status {response.status_code}"}

        except Exception as e:
            logger.error(f"Failed to make prediction: {e}")
            return {"error": str(e)}

    def get_explanation(
        self,
        features: Dict[str, Any],
        prediction: Dict[str, Any],
        explanation_method: str = "shap",
        comparison_model: str = None,
        include_natural_language: bool = False,
    ) -> Dict[str, Any]:
        """
        Получение объяснения предсказания с использованием различных XAI методов

        Args:
            features: Словарь с признаками
            prediction: Словарь с предсказанием
            explanation_method: Метод объяснения ("shap", "lime", "eli5", "anchors", "natural_language")
            comparison_model: Опциональная модель для сравнения объяснений
            include_natural_language: Whether to include natural language explanation

        Returns:
            Словарь с объяснением предсказания
        """
        try:
            # Получаем ID предсказания, если есть
            prediction_id = prediction.get(
                "prediction_id",
                f"pred_{int(time.time())}_{hash(json.dumps(features, sort_keys=True)) % 10000}",
            )

            explanation_data = {
                "timestamp": time.time(),
                "features": features,
                "prediction": prediction,
                "explanation_method": explanation_method,
                "comparison_model": comparison_model,
                "include_natural_language": include_natural_language,
            }

            response = requests.post(
                f"{KENNING_URL}/explain",
                json=explanation_data,
                headers={"Content-Type": "application/json"},
            )

            if response.status_code == 200:
                explanation_result = response.json()
                explanation = explanation_result.get("explanation", {})

                # Если это сравнение моделей, структура ответа будет другой
                if comparison_model:
                    # Добавляем ID предсказания к объяснениям и сравнению
                    if "primary_explanation" in explanation:
                        explanation["primary_explanation"][
                            "prediction_id"
                        ] = prediction_id
                    if "comparison_explanation" in explanation:
                        explanation["comparison_explanation"][
                            "prediction_id"
                        ] = prediction_id

                    # Сохраняем объяснение в локальном хранилище
                    self.explanation_store[prediction_id] = {
                        "timestamp": int(time.time()),
                        "features": features,
                        "prediction": prediction,
                        "explanation": explanation,
                        "comparison_model": comparison_model,
                        "explanation_method": explanation_method,
                    }
                else:
                    # Стандартное объяснение без сравнения
                    explanation["prediction_id"] = prediction_id

                    # Сохраняем объяснение в локальном хранилище
                    self.explanation_store[prediction_id] = {
                        "timestamp": int(time.time()),
                        "features": features,
                        "prediction": prediction,
                        "explanation": explanation,
                        "explanation_method": explanation_method,
                        "visualization": explanation_result.get("explanation", {}).get(
                            "visualization", None
                        ),
                    }

                # Сохраняем объяснение в Redis, если подключен
                if redis_client:
                    result_key = (
                        f"anomaly:explanation:{prediction_id}:{explanation_method}"
                    )
                    redis_client.set(
                        result_key,
                        json.dumps(
                            {
                                "timestamp": int(time.time()),
                                "prediction_id": prediction_id,
                                "explanation": explanation,
                                "explanation_method": explanation_method,
                                "comparison_model": comparison_model,
                            }
                        ),
                    )

                return explanation
            else:
                logger.error(
                    f"Error from Kenning ML explanation: {response.status_code} - {response.text}"
                )
                return {
                    "error": f"ML explanation service returned status {response.status_code}"
                }

        except Exception as e:
            logger.error(f"Failed to get explanation: {e}")
            return {"error": str(e)}

    def update_state(self) -> None:
        """Обновление состояния предиктора"""
        # Получаем информацию о модели
        model_info = self.get_model_info()
        if model_info:
            self.model_info = model_info
            self.model_loaded = True

        # Получаем последние признаки
        features = self.get_latest_features()
        if features:
            self.latest_features = features

        # Получаем последние предсказания
        predictions = self.get_latest_predictions()
        if predictions:
            self.latest_predictions = predictions

        self.last_update_time = time.time()

    def run_update_loop(self) -> None:
        """Запуск цикла обновления состояния"""
        logger.info(f"Starting update loop with interval {UPDATE_INTERVAL}s")

        while True:
            try:
                self.update_state()
            except Exception as e:
                logger.error(f"Error in update loop: {e}")

            # Ждем до следующего обновления
            time.sleep(UPDATE_INTERVAL)

    def get_explanation_by_id(
        self,
        prediction_id: str,
        explanation_method: str = "shap",
        comparison_model: str = None,
        include_natural_language: bool = False,
    ) -> Dict[str, Any]:
        """
        Получение объяснения по ID предсказания

        Args:
            prediction_id: ID предсказания
            explanation_method: Метод объяснения ("shap", "lime", "eli5", "anchors", "natural_language")
            comparison_model: Опциональная модель для сравнения объяснений
            include_natural_language: Whether to include natural language explanation

        Returns:
            Объяснение или сообщение об ошибке
        """
        try:
            # Если указана модель для сравнения, формируем специальный ключ
            if comparison_model:
                cache_key = (
                    f"{prediction_id}_{explanation_method}_vs_{comparison_model}"
                )
            else:
                cache_key = f"{prediction_id}_{explanation_method}"

            # Сначала проверяем локальное хранилище
            # Ищем по конкретному методу объяснения и опциональной модели сравнения
            for store_key, store_data in self.explanation_store.items():
                if (
                    store_key == prediction_id
                    and store_data.get("explanation_method") == explanation_method
                    and store_data.get("comparison_model") == comparison_model
                ):
                    return store_data["explanation"]

            # Затем проверяем Redis, если доступен
            if redis_client:
                # Специальный ключ для Redis с учетом метода объяснения
                redis_key = f"anomaly:explanation:{prediction_id}:{explanation_method}"
                redis_data = redis_client.get(redis_key)
                if redis_data:
                    explanation_data = json.loads(redis_data)
                    # Проверяем, что это объяснение с нужной моделью сравнения
                    if explanation_data.get("comparison_model") == comparison_model:
                        return explanation_data.get("explanation", {})

            # Если объяснения нет, но есть предсказание, пытаемся сгенерировать объяснение
            prediction = self.get_prediction_by_id(prediction_id)
            if prediction and "error" not in prediction:
                features = prediction.get("features", {})
                if features:
                    # Создаем объяснение "on-demand" с указанным методом и опциональной моделью сравнения
                    explanation = self.get_explanation(
                        features,
                        prediction,
                        explanation_method,
                        comparison_model,
                        include_natural_language,
                    )
                    if "error" not in explanation:
                        return explanation

            return {
                "error": f"Explanation with ID {prediction_id} using method {explanation_method} not found"
            }
        except Exception as e:
            logger.error(f"Error retrieving explanation by ID: {e}")
            return {"error": str(e)}

    def get_prediction_by_id(self, prediction_id: str) -> Dict[str, Any]:
        """
        Получение сохраненного предсказания по ID

        Args:
            prediction_id: Уникальный идентификатор предсказания

        Returns:
            Словарь с предсказанием или ошибкой
        """
        # Сначала проверяем локальное хранилище
        if prediction_id in self.prediction_store:
            return self.prediction_store[prediction_id]

        # Если не нашли, проверяем Redis
        if redis_client:
            try:
                prediction_data = redis_client.get(
                    f"anomaly:prediction:{prediction_id}"
                )
                if prediction_data:
                    return json.loads(prediction_data)
            except Exception as e:
                logger.error(f"Error retrieving prediction from Redis: {e}")

        return {"error": f"Prediction not found for prediction_id: {prediction_id}"}


# Создание экземпляра предиктора
predictor = AnomalyPredictor()


@app.route("/health", methods=["GET"])
def health():
    """Проверка здоровья сервиса"""
    return jsonify(
        {
            "status": "ok",
            "service": "anomaly_predictor",
            "model_loaded": predictor.model_loaded,
            "last_update": predictor.last_update_time,
        }
    )


@app.route("/ml/explain/<prediction_id>", methods=["GET"])
def get_explanation_by_id(prediction_id):
    """
    Получение объяснения предсказания по его ID

    Args:
        prediction_id: Уникальный идентификатор предсказания

    Returns:
        JSON с объяснением или ошибкой
    """
    try:
        # Получаем параметры из запроса
        explanation_method = request.args.get("method", "shap")
        comparison_model = request.args.get("comparison_model", None)
        include_natural_language = request.args.get("include_natural_language", False)

        # Проверяем, что метод объяснения корректный
        valid_methods = ["shap", "lime", "eli5", "anchors", "natural_language"]
        if explanation_method not in valid_methods:
            return (
                jsonify(
                    {
                        "error": f"Invalid explanation method. Must be one of: {valid_methods}"
                    }
                ),
                400,
            )

        # Получаем объяснение с указанным методом и опционально моделью для сравнения
        explanation = predictor.get_explanation_by_id(
            prediction_id,
            explanation_method=explanation_method,
            comparison_model=comparison_model,
            include_natural_language=include_natural_language,
        )

        return jsonify(explanation)
    except Exception as e:
        logger.error(f"Error getting explanation by ID {prediction_id}: {e}")
        return jsonify({"error": str(e)}), 500


@app.route("/predict", methods=["POST"])
def predict():
    """Endpoint для выполнения предсказания"""
    try:
        data = request.get_json()

        if not data or "features" not in data:
            return jsonify({"error": "Features data is required"}), 400

        features = data["features"]
        prediction = predictor.make_prediction(features)

        return jsonify({"timestamp": time.time(), "prediction": prediction})

    except Exception as e:
        logger.error(f"Error in prediction endpoint: {e}")
        return jsonify({"error": str(e)}), 500


@app.route("/explain", methods=["POST"])
def explain():
    """Endpoint для получения объяснения предсказания"""
    try:
        data = request.get_json()

        if not data or "features" not in data:
            return (
                jsonify({"error": "Invalid request format, 'features' field required"}),
                400,
            )

        # Получаем параметры из запроса
        explanation_method = data.get("explanation_method", "shap")
        comparison_model = data.get("comparison_model", None)
        include_natural_language = data.get("include_natural_language", False)

        # Проверяем, что метод объяснения корректный
        valid_methods = ["shap", "lime", "eli5", "anchors", "natural_language"]
        if explanation_method not in valid_methods:
            return (
                jsonify(
                    {
                        "error": f"Invalid explanation method. Must be one of: {valid_methods}"
                    }
                ),
                400,
            )

        # Выполняем предсказание, если его нет в запросе
        prediction = data.get("prediction", predictor.make_prediction(data["features"]))

        # Получаем объяснение с указанными параметрами
        explanation = predictor.get_explanation(
            data["features"],
            prediction,
            explanation_method=explanation_method,
            comparison_model=comparison_model,
        )

        # Возвращаем результат
        return jsonify(
            {
                "timestamp": time.time(),
                "explanation": explanation,
                "explanation_method": explanation_method,
                "comparison_model": comparison_model,
            }
        )
    except Exception as e:
        logger.error(f"Error in explain endpoint: {e}")
        return jsonify({"error": str(e)}), 500


@app.route("/status", methods=["GET"])
def status():
    """Endpoint для получения текущего статуса предиктора"""
    return jsonify(
        {
            "timestamp": time.time(),
            "model_info": predictor.model_info,
            "latest_features": predictor.latest_features,
            "latest_predictions": predictor.latest_predictions,
        }
    )


def main():
    """Точка входа для запуска сервиса предсказания"""
    parser = argparse.ArgumentParser(description="ML Anomaly Predictor Service")
    parser.add_argument(
        "--host", type=str, default=HOST, help=f"Host to bind (default: {HOST})"
    )
    parser.add_argument(
        "--port", type=int, default=PORT, help=f"Port to bind (default: {PORT})"
    )
    parser.add_argument(
        "--update-interval",
        type=int,
        default=UPDATE_INTERVAL,
        help=f"Update interval in seconds (default: {UPDATE_INTERVAL})",
    )

    args = parser.parse_args()

    global HOST, PORT, UPDATE_INTERVAL
    HOST = args.host
    PORT = args.port
    UPDATE_INTERVAL = args.update_interval

    # Запускаем поток обновления состояния
    update_thread = threading.Thread(target=predictor.run_update_loop, daemon=True)
    update_thread.start()

    # Запускаем Flask сервер
    app.run(host=HOST, port=PORT)


if __name__ == "__main__":
    main()
