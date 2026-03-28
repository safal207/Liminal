"""
Kenning ML Service для Resonance Liminal.
Отдельный микросервис для обучения и inference ML-моделей.
"""

import asyncio
import json
import logging
import os
import time
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

import eli5
import lime
import lime.lime_tabular
import matplotlib
import numpy as np

# XAI библиотеки
import shap
from alibi.explainers import AnchorTabular
from fastapi import BackgroundTasks, FastAPI, HTTPException
from loguru import logger
from pydantic import BaseModel

import redis

matplotlib.use("Agg")  # Не-интерактивный бэкенд для matplotlib
import base64
import io

import matplotlib.pyplot as plt
import pandas as pd
from ml.openai_service import openai_service

# Настройка логирования
logging.basicConfig(level=logging.INFO)

app = FastAPI(
    title="Resonance Liminal ML Service",
    description="Kenning-based ML service for intelligent WebSocket optimization",
    version="1.0.0",
)

# Конфигурация из переменных окружения
REDIS_HOST = os.getenv("REDIS_HOST", "localhost")
MODEL_STORAGE = os.getenv("MODEL_STORAGE", "/models")
DATA_STORAGE = os.getenv("DATA_STORAGE", "/data")
PROMETHEUS_URL = os.getenv("PROMETHEUS_URL", "http://localhost:9090")

# Подключение к Redis
redis_client = redis.Redis(host=REDIS_HOST, port=6379, decode_responses=True)


# Pydantic модели
class TrainingRequest(BaseModel):
    model_name: str
    config: Dict[str, Any]
    data_source: str = "redis"
    auto_deploy: bool = True


class InferenceRequest(BaseModel):
    model_name: str
    features: Dict[str, Any]


class ExplanationRequest(BaseModel):
    model_name: str
    features: Dict[str, Any]
    prediction: Optional[Dict[str, Any]] = None
    explanation_method: str = (
        "shap"  # 'shap', 'lime', 'eli5', 'anchors', or 'natural_language'
    )
    comparison_model: Optional[str] = (
        None  # Optional second model to compare explanations with
    )
    include_natural_language: bool = (
        False  # Whether to include natural language explanation
    )


class ModelStatus(BaseModel):
    name: str
    status: str
    accuracy: Optional[float] = None
    last_trained: Optional[str] = None
    version: str = "1.0"


class MLService:
    """
    Основной класс ML-сервиса на базе Kenning.
    """

    def __init__(self):
        self.models = {}  # Загруженные модели
        self.model_configs = {}  # Конфигурации моделей
        self.training_jobs = {}  # Активные задачи обучения
        self.explainers = {}  # Объекты XAI для объяснения моделей

        # Создаем директории
        Path(MODEL_STORAGE).mkdir(parents=True, exist_ok=True)
        Path(DATA_STORAGE).mkdir(parents=True, exist_ok=True)

        logger.info(
            f"ML Service инициализирован. Models: {MODEL_STORAGE}, Data: {DATA_STORAGE}"
        )

    async def load_data_from_redis(
        self, key_pattern: str = "ml:features:*"
    ) -> List[Dict[str, Any]]:
        """
        Загружает данные для обучения из Redis.

        Args:
            key_pattern: Паттерн ключей для поиска данных

        Returns:
            Список записей с фичами
        """
        try:
            keys = redis_client.keys(key_pattern)
            data = []

            for key in keys:
                raw_data = redis_client.get(key)
                if raw_data:
                    try:
                        record = json.loads(raw_data)
                        data.append(record)
                    except json.JSONDecodeError:
                        logger.warning(f"Не удалось декодировать данные из ключа {key}")

            logger.info(f"Загружено {len(data)} записей из Redis")
            return data

        except Exception as e:
            logger.error(f"Ошибка загрузки данных из Redis: {e}")
            return []

    async def prepare_training_data(
        self, model_name: str, raw_data: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        """
        Подготавливает данные для обучения конкретной модели.

        Args:
            model_name: Название модели
            raw_data: Сырые данные

        Returns:
            Подготовленные данные для обучения
        """
        if model_name == "anomaly_detection":
            return self._prepare_anomaly_data(raw_data)
        elif model_name == "load_prediction":
            return self._prepare_load_prediction_data(raw_data)
        elif model_name == "user_behavior":
            return self._prepare_user_behavior_data(raw_data)
        else:
            raise ValueError(f"Неизвестная модель: {model_name}")

    def _prepare_anomaly_data(self, raw_data: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Подготовка данных для anomaly detection."""
        features = []
        labels = []

        for record in raw_data:
            # Извлекаем фичи
            feature_vector = [
                record.get("messages_per_minute", 0),
                record.get("avg_message_size", 0),
                record.get("error_rate", 0),
                record.get("rate_limit_violations", 0),
                record.get("channels_count", 0),
                record.get("connection_duration", 0),
            ]
            features.append(feature_vector)

            # Простая эвристика для лейблов (в реальности нужна разметка)
            is_anomaly = (
                record.get("messages_per_minute", 0) > 50
                or record.get("error_rate", 0) > 0.3
                or record.get("rate_limit_violations", 0) > 5
            )
            labels.append(1 if is_anomaly else 0)

        return {
            "features": np.array(features),
            "labels": np.array(labels),
            "feature_names": [
                "messages_per_minute",
                "avg_message_size",
                "error_rate",
                "rate_limit_violations",
                "channels_count",
                "connection_duration",
            ],
        }

    def _prepare_load_prediction_data(
        self, raw_data: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        """Подготовка данных для load prediction."""
        features = []
        targets = []

        # Сортируем по времени для time-series анализа
        sorted_data = sorted(raw_data, key=lambda x: x.get("timestamp", 0))

        for i in range(len(sorted_data) - 1):
            current = sorted_data[i]
            next_record = sorted_data[i + 1]

            # Фичи текущего состояния
            feature_vector = [
                current.get("messages_per_minute", 0),
                current.get("connection_duration", 0),
                current.get("channels_count", 0),
                len(raw_data),  # Общая активность
            ]
            features.append(feature_vector)

            # Целевая переменная - нагрузка в следующий момент
            future_load = next_record.get("messages_per_minute", 0)
            targets.append(future_load)

        return {
            "features": np.array(features),
            "targets": np.array(targets),
            "feature_names": [
                "current_messages_per_minute",
                "connection_duration",
                "channels_count",
                "total_activity",
            ],
        }

    def _prepare_user_behavior_data(
        self, raw_data: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        """Подготовка данных для user behavior clustering."""
        features = []
        user_ids = []

        # Агрегируем данные по пользователям
        user_aggregates = {}
        for record in raw_data:
            user_id = record.get("user_id", "unknown")
            if user_id not in user_aggregates:
                user_aggregates[user_id] = {
                    "total_messages": 0,
                    "total_duration": 0,
                    "total_channels": set(),
                    "total_errors": 0,
                    "sessions": 0,
                }

            agg = user_aggregates[user_id]
            agg["total_messages"] += record.get("messages_per_minute", 0)
            agg["total_duration"] += record.get("connection_duration", 0)
            agg["total_channels"].update([record.get("channel", "default")])
            agg["total_errors"] += record.get("error_rate", 0)
            agg["sessions"] += 1

        # Создаем фичи для кластеризации
        for user_id, agg in user_aggregates.items():
            if agg["sessions"] > 0:
                feature_vector = [
                    agg["total_messages"]
                    / agg["sessions"],  # Среднее сообщений за сессию
                    agg["total_duration"]
                    / agg["sessions"],  # Средняя длительность сессии
                    len(agg["total_channels"]),  # Уникальные каналы
                    agg["total_errors"] / agg["sessions"],  # Средний уровень ошибок
                    agg["sessions"],  # Количество сессий
                ]
                features.append(feature_vector)
                user_ids.append(user_id)

        return {
            "features": np.array(features),
            "user_ids": user_ids,
            "feature_names": [
                "avg_messages_per_session",
                "avg_session_duration",
                "unique_channels",
                "avg_error_rate",
                "total_sessions",
            ],
        }

    async def train_model_with_kenning(
        self, model_name: str, training_data: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Обучает модель используя Kenning framework.

        Args:
            model_name: Название модели
            training_data: Подготовленные данные для обучения

        Returns:
            Результат обучения
        """
        try:
            logger.info(f"Начинаем обучение модели {model_name}")

            # Сохраняем данные для Kenning
            data_path = Path(DATA_STORAGE) / f"{model_name}_training_data.npz"
            np.savez(data_path, **training_data)

            # Создаем конфигурацию Kenning
            config = self._create_kenning_config(model_name, str(data_path))
            config_path = Path(DATA_STORAGE) / f"{model_name}_config.json"

            with open(config_path, "w") as f:
                json.dump(config, f, indent=2)

            # Симуляция обучения (в реальности здесь будет вызов Kenning CLI)
            await asyncio.sleep(5)  # Имитация времени обучения

            # Создаем "обученную" модель (заглушка)
            model_path = Path(MODEL_STORAGE) / f"{model_name}.onnx"
            model_path.touch()  # Создаем пустой файл для демонстрации

            # Сохраняем метаданные модели
            metadata = {
                "name": model_name,
                "version": "1.0",
                "trained_at": datetime.now().isoformat(),
                "accuracy": 0.85
                + np.random.random() * 0.1,  # Случайная точность для демо
                "features": training_data.get("feature_names", []),
                "data_size": len(training_data.get("features", [])),
                "config": config,
            }

            metadata_path = Path(MODEL_STORAGE) / f"{model_name}_metadata.json"
            with open(metadata_path, "w") as f:
                json.dump(metadata, f, indent=2)

            # Сохраняем в Redis для быстрого доступа
            redis_client.set(f"model:metadata:{model_name}", json.dumps(metadata))

            logger.info(
                f"Модель {model_name} успешно обучена. Точность: {metadata['accuracy']:.3f}"
            )

            return {
                "status": "success",
                "model_name": model_name,
                "accuracy": metadata["accuracy"],
                "model_path": str(model_path),
                "metadata": metadata,
            }

        except Exception as e:
            logger.error(f"Ошибка обучения модели {model_name}: {e}")
            return {"status": "error", "model_name": model_name, "error": str(e)}

    def _create_kenning_config(self, model_name: str, data_path: str) -> Dict[str, Any]:
        """Создает конфигурацию для Kenning."""
        base_config = {
            "model_name": model_name,
            "data_path": data_path,
            "output_dir": MODEL_STORAGE,
            "optimization": {
                "target": "accuracy",
                "time_budget": 300,
                "memory_limit": "1GB",
            },
        }

        if model_name == "anomaly_detection":
            base_config.update(
                {
                    "model_type": "classification",
                    "algorithm": "random_forest",
                    "hyperparameters": {"n_estimators": 100, "max_depth": 10},
                }
            )
        elif model_name == "load_prediction":
            base_config.update(
                {
                    "model_type": "regression",
                    "algorithm": "gradient_boosting",
                    "hyperparameters": {"n_estimators": 100, "learning_rate": 0.1},
                }
            )
        elif model_name == "user_behavior":
            base_config.update(
                {
                    "model_type": "clustering",
                    "algorithm": "kmeans",
                    "hyperparameters": {"n_clusters": 5},
                }
            )

        return base_config

    async def load_model(self, model_name: str) -> bool:
        """Загружает обученную модель."""
        try:
            # Проверяем наличие модели
            model_path = Path(MODEL_STORAGE) / f"{model_name}.onnx"

            if not model_path.exists():
                logger.error(f"Модель {model_name} не найдена по пути {model_path}")
                return False

            # В реальном приложении здесь загрузка модели через ONNX runtime
            # Для демо используем простую заглушку
            class SimpleModel:
                def predict(self, x):
                    if model_name == "anomaly_detection":
                        # Для демо генерируем предсказания на основе входных данных
                        # В реальности здесь был бы вызов ONNX модели
                        return np.mean(x, axis=1) * 1.5
                    elif model_name == "load_prediction":
                        return np.mean(x, axis=1) * 2.0 + 5
                    else:
                        return np.random.random(x.shape[0])

            self.models[model_name] = SimpleModel()

            logger.info(f"Модель {model_name} успешно загружена")
            return True

        except Exception as e:
            logger.error(f"Ошибка загрузки модели {model_name}: {e}")
            return False

    async def predict(
        self, model_name: str, features: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Выполняет предсказание.
        """
        if model_name not in self.models:
            await self.load_model(model_name)

            if model_name not in self.models:
                logger.error(f"Не удалось загрузить модель {model_name}")
                return None

        try:
            # Преобразуем фичи в нужный формат
            feature_vector = []
            feature_names = []

            for name, value in features.items():
                feature_vector.append(float(value))
                feature_names.append(name)

            # Делаем предсказание
            model = self.models[model_name]
            prediction = model.predict(np.array([feature_vector]))[0]

            # Для разных типов моделей разные выходы
            result = {}

            if model_name == "anomaly_detection":
                # Возвращаем вероятность аномалии
                result = {
                    "anomaly_score": float(prediction),
                    "is_anomaly": bool(
                        prediction > 0.7
                    ),  # Порог определяется на основе модели
                    "threshold": 0.7,
                }
            elif model_name == "load_prediction":
                # Предсказание нагрузки
                result = {
                    "predicted_load": float(prediction),
                    "confidence": 0.85,  # Уверенность в предсказании
                }
            elif model_name == "user_behavior":
                # Кластеризация пользователей
                result = {
                    "cluster": int(prediction),
                    "description": self._get_cluster_description(int(prediction)),
                }
            else:
                # Общий случай
                result = {"prediction": float(prediction)}

            # Сохраняем предсказание в Redis
            prediction_key = f"ml_predictions:{int(time.time())}_{model_name}"
            prediction_data = {
                "timestamp": time.time(),
                "model_name": model_name,
                "features": features,
                "prediction": result,
            }

            redis_client.set(prediction_key, json.dumps(prediction_data))
            redis_client.set(
                f"ml_predictions:latest",
                json.dumps({"predictions": result, "timestamp": time.time()}),
            )

            # Инициализируем explainer для XAI, если его еще нет
            if model_name not in self.explainers:
                await self._init_explainer(model_name, feature_names)

            return result

        except Exception as e:
            logger.error(f"Ошибка предсказания модели {model_name}: {e}")
            return None

    async def _init_explainer(self, model_name: str, features: Dict[str, Any]):
        """Initialize SHAP, LIME, ELI5, or Anchors explainer for a given model

        Args:
            model_name: Name of the model to explain
            features: Features to use for initialization

        Returns:
            Tuple of (shap_explainer, lime_explainer, eli5_model, anchors_explainer)
        """
        # For this simple example, we'll just use a simple model that returns feature sum
        # In a real scenario, you would load the actual model
        model = lambda x: np.sum(x, axis=1).reshape(-1, 1)

        # Convert features to numpy array for explanations
        feature_names = list(features.keys())
        feature_values = list(features.values())

        # For SHAP, we need a background dataset
        # In a real scenario, this would be a representative dataset
        background_data = np.random.random(
            (100, len(feature_names))
        )  # Random background data

        # Initialize SHAP explainer
        shap_explainer = shap.KernelExplainer(model, background_data)

        # Initialize LIME explainer
        lime_explainer = lime.lime_tabular.LimeTabularExplainer(
            background_data,
            feature_names=feature_names,
            verbose=True,
            mode="regression",  # or 'classification'
        )

        # For ELI5, we'll use the same model
        # ELI5 works with scikit-learn models, but for this example we'll just use the model directly
        eli5_model = model

        # For Anchors, we need a prediction function that returns class indices
        # In a real scenario, this would be the actual model's predict function
        predict_fn = lambda x: (model(x) > 0.5).astype(int)  # Binarize output

        # Initialize Anchors explainer
        anchors_explainer = AnchorTabular(predict_fn, feature_names)
        # Fit the explainer with background data
        anchors_explainer.fit(background_data)

        return shap_explainer, lime_explainer, eli5_model, anchors_explainer

    async def get_explanation(
        self,
        model_name: str,
        features: Dict[str, Any],
        prediction: Dict[str, Any],
        explanation_method: str = "shap",
        comparison_model: Optional[str] = None,
        include_natural_language: bool = False,
    ):
        """Generate explanation for a prediction

        Args:
            model_name: Name of the model to explain
            features: Features used for prediction
            prediction: Prediction to explain
            explanation_method: Method to use for explanation ("shap", "lime", "eli5", or "anchors")
            comparison_model: Optional second model name to compare explanations with
            include_natural_language: Whether to include natural language explanation

        Returns:
            Dict with explanation details
        """
        feature_names = list(features.keys())
        feature_values = np.array([list(features.values())])

        # Special case for natural language explanation
        if explanation_method.lower() == "natural_language":
            return await self._generate_natural_language_explanation(
                model_name, features, prediction
            )

        # Initialize explainers
        shap_explainer, lime_explainer, eli5_model, anchors_explainer = (
            await self._init_explainer(model_name, features)
        )

        # Generate primary explanation
        primary_explanation = self._generate_explanation(
            model_name,
            features,
            prediction,
            explanation_method,
            shap_explainer,
            lime_explainer,
            eli5_model,
            anchors_explainer,
        )

        # Add natural language explanation if requested
        if include_natural_language:
            nl_explanation = await self._generate_natural_language_explanation(
                model_name, features, prediction, xai_explanation=primary_explanation
            )
            primary_explanation["natural_language_explanation"] = nl_explanation

        # If comparison model is provided, generate explanation for that model too
        if comparison_model:
            # In a real scenario, you would initialize a different model here
            # For this example, we'll just use the same explainers with a slight variation
            comparison_explanation = self._generate_explanation(
                comparison_model,
                features,
                prediction,
                explanation_method,
                shap_explainer,
                lime_explainer,
                eli5_model,
                anchors_explainer,
            )

            # Compare the explanations
            comparison = self._compare_explanations(
                primary_explanation, comparison_explanation
            )

            # Return both explanations and comparison
            return {
                "primary_model": model_name,
                "comparison_model": comparison_model,
                "primary_explanation": primary_explanation,
                "comparison_explanation": comparison_explanation,
                "comparison": comparison,
            }

        return primary_explanation

    def _generate_explanation(
        self,
        model_name: str,
        features: Dict[str, Any],
        prediction: Dict[str, Any],
        explanation_method: str,
        shap_explainer,
        lime_explainer,
        eli5_model,
        anchors_explainer,
    ):
        """Generate explanation using the specified method

        Args:
            model_name: Name of the model
            features: Features used for prediction
            prediction: Prediction to explain
            explanation_method: Method to use for explanation
            shap_explainer: Initialized SHAP explainer
            lime_explainer: Initialized LIME explainer
            eli5_model: Model for ELI5
            anchors_explainer: Initialized Anchors explainer

        Returns:
            Dict with explanation details
        """
        feature_names = list(features.keys())
        feature_values = np.array([list(features.values())])

        if explanation_method.lower() == "shap":
            # Generate SHAP values
            shap_values = shap_explainer.shap_values(feature_values)

            # Get feature importance
            feature_importance = dict(zip(feature_names, np.abs(shap_values[0][0])))

            # Sort features by importance
            sorted_features = sorted(
                feature_importance.items(), key=lambda x: x[1], reverse=True
            )
            top_features = [k for k, v in sorted_features[:5]]  # Top 5 features

            # Generate SHAP visualization
            plt.figure(figsize=(10, 6))
            shap.summary_plot(
                shap_values[0], feature_values, feature_names=feature_names, show=False
            )

            # Save plot to base64 string
            buf = io.BytesIO()
            plt.savefig(buf, format="png", bbox_inches="tight", dpi=150)
            plt.close()
            buf.seek(0)
            img_str = base64.b64encode(buf.read()).decode("utf-8")

            return {
                "explanation_method": "shap",
                "feature_importance": feature_importance,
                "top_features": top_features,
                "visualization": img_str,
            }

        elif explanation_method.lower() == "lime":
            # Generate LIME explanation
            exp = lime_explainer.explain_instance(
                feature_values[0],
                lambda x: np.sum(x, axis=1).reshape(-1, 1),  # Same simple model
                num_features=len(feature_names),
            )

            # Extract feature importance from LIME
            lime_weights = exp.as_list()
            feature_importance = {feature: weight for feature, weight in lime_weights}

            # Sort features by importance
            sorted_features = sorted(
                feature_importance.items(), key=lambda x: abs(x[1]), reverse=True
            )
            top_features = [k for k, v in sorted_features[:5]]  # Top 5 features

            # Generate LIME visualization
            plt.figure(figsize=(10, 6))
            exp.as_pyplot_figure()

            # Save plot to base64 string
            buf = io.BytesIO()
            plt.savefig(buf, format="png", bbox_inches="tight", dpi=150)
            plt.close()
            buf.seek(0)
            img_str = base64.b64encode(buf.read()).decode("utf-8")

            return {
                "explanation_method": "lime",
                "feature_importance": feature_importance,
                "top_features": top_features,
                "visualization": img_str,
            }

        elif explanation_method.lower() == "eli5":
            # Convert features to DataFrame for ELI5
            features_df = pd.DataFrame([features])

            # Generate ELI5 explanation
            # For this example, we're using a simple permutation importance
            # In a real scenario, you would use the actual model
            weights = np.abs(np.array(list(features.values())))
            feature_importance = dict(zip(feature_names, weights / np.sum(weights)))

            # Sort features by importance
            sorted_features = sorted(
                feature_importance.items(), key=lambda x: x[1], reverse=True
            )
            top_features = [k for k, v in sorted_features[:5]]  # Top 5 features

            # Generate ELI5 visualization
            plt.figure(figsize=(10, 6))
            # Create a bar chart of feature importance
            plt.barh(feature_names, list(feature_importance.values()))
            plt.title("Feature Importance (ELI5)")
            plt.xlabel("Importance")
            plt.tight_layout()

            # Save plot to base64 string
            buf = io.BytesIO()
            plt.savefig(buf, format="png", bbox_inches="tight", dpi=150)
            plt.close()
            buf.seek(0)
            img_str = base64.b64encode(buf.read()).decode("utf-8")

            return {
                "explanation_method": "eli5",
                "feature_importance": feature_importance,
                "top_features": top_features,
                "visualization": img_str,
            }

        elif explanation_method.lower() == "anchors":
            # Convert features to numpy array for Anchors
            # Generate Anchors explanation
            explanation = anchors_explainer.explain(feature_values[0])

            # Extract anchor rules
            anchor_rules = explanation.anchor
            precision = explanation.precision
            coverage = explanation.coverage

            # Create feature importance based on the anchor rules
            # Features in the anchor rules are the most important
            feature_importance = {feature: 0.0 for feature in feature_names}
            for rule in anchor_rules:
                feature = feature_names[int(rule.split("=")[0].strip())]
                feature_importance[feature] = 1.0

            # Sort features by importance
            sorted_features = sorted(
                feature_importance.items(), key=lambda x: x[1], reverse=True
            )
            top_features = [k for k, v in sorted_features if v > 0][
                :5
            ]  # Top 5 features in anchors

            # Generate visualization for Anchors
            plt.figure(figsize=(10, 6))
            plt.barh(list(feature_importance.keys()), list(feature_importance.values()))
            plt.title(
                f"Anchor Explanation (Precision: {precision:.2f}, Coverage: {coverage:.2f})"
            )
            plt.xlabel("Importance in Anchor")
            plt.tight_layout()

            # Save plot to base64 string
            buf = io.BytesIO()
            plt.savefig(buf, format="png", bbox_inches="tight", dpi=150)
            plt.close()
            buf.seek(0)
            img_str = base64.b64encode(buf.read()).decode("utf-8")

            return {
                "explanation_method": "anchors",
                "anchor_rules": anchor_rules,
                "precision": precision,
                "coverage": coverage,
                "feature_importance": feature_importance,
                "top_features": top_features,
                "visualization": img_str,
            }

        else:
            return {"error": f"Unsupported explanation method: {explanation_method}"}

    def _compare_explanations(
        self, explanation1: Dict[str, Any], explanation2: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Compare two explanations to identify differences

        Args:
            explanation1: First explanation
            explanation2: Second explanation

        Returns:
            Dict with comparison results
        """
        method = explanation1.get("explanation_method")

        if method != explanation2.get("explanation_method"):
            return {"error": "Cannot compare explanations with different methods"}

        # Compare feature importance
        feature_importance1 = explanation1.get("feature_importance", {})
        feature_importance2 = explanation2.get("feature_importance", {})

        # Get all unique features
        all_features = set(feature_importance1.keys()).union(
            set(feature_importance2.keys())
        )

        # Calculate differences in importance
        importance_diff = {}
        for feature in all_features:
            val1 = feature_importance1.get(feature, 0)
            val2 = feature_importance2.get(feature, 0)
            importance_diff[feature] = val1 - val2

        # Sort by absolute difference
        sorted_diff = sorted(
            importance_diff.items(), key=lambda x: abs(x[1]), reverse=True
        )

        # Generate visualization comparing the two explanations
        plt.figure(figsize=(12, 8))

        # Get top N features with biggest difference
        top_diff_features = [
            k for k, v in sorted_diff[:10]
        ]  # Top 10 different features

        # Get values for these features
        feat1_values = [feature_importance1.get(f, 0) for f in top_diff_features]
        feat2_values = [feature_importance2.get(f, 0) for f in top_diff_features]

        # Plot comparison
        x = np.arange(len(top_diff_features))
        width = 0.35

        plt.bar(x - width / 2, feat1_values, width, label="Model 1")
        plt.bar(x + width / 2, feat2_values, width, label="Model 2")

        plt.xlabel("Features")
        plt.ylabel("Importance")
        plt.title("Feature Importance Comparison")
        plt.xticks(x, top_diff_features, rotation=45, ha="right")
        plt.legend()
        plt.tight_layout()

        # Save plot to base64 string
        buf = io.BytesIO()
        plt.savefig(buf, format="png", bbox_inches="tight", dpi=150)
        plt.close()
        buf.seek(0)
        img_str = base64.b64encode(buf.read()).decode("utf-8")

        return {
            "importance_differences": importance_diff,
            "top_different_features": top_diff_features,
            "visualization": img_str,
        }

    def _get_model_type(self, model_name: str) -> str:
        """Return model type based on model name"""
        if "anomaly" in model_name:
            return "anomaly_detection"
        elif "load" in model_name:
            return "regression"
        elif "user" in model_name or "cluster" in model_name:
            return "clustering"
        else:
            return "classification"

    def _get_cluster_description(self, cluster_id: int) -> str:
        """Возвращает описание кластера пользователей."""
        descriptions = [
            "Normal user",
            "Power user",
            "Potential bot",
            "Anomalous behavior",
            "New user",
        ]

        if 0 <= cluster_id < len(descriptions):
            return descriptions[cluster_id]
        else:
            return "Unknown cluster"

    async def _generate_natural_language_explanation(
        self,
        model_name: str,
        features: Dict[str, Any],
        prediction: Dict[str, Any],
        xai_explanation: Dict[str, Any] = None,
    ) -> Dict[str, Any]:
        """
        Generate a natural language explanation of a prediction using OpenAI.

        Args:
            model_name: Name of the model
            features: Features used for prediction
            prediction: Prediction to explain
            xai_explanation: Optional XAI explanation to enhance the natural language explanation

        Returns:
            Natural language explanation
        """
        try:
            # Determine model type
            model_type = "classification"
            if "anomaly_score" in prediction:
                model_type = "anomaly_detection"
            elif "predicted_load" in prediction:
                model_type = "regression"
            elif "cluster" in prediction:
                model_type = "clustering"

            # Generate natural language explanation using OpenAI
            if openai_service.client is None:
                return {
                    "error": "OpenAI service not available",
                    "message": "Configure the OpenAI API key to enable natural language explanations",
                }

            # Call OpenAI service to get natural language explanation
            explanation = await openai_service.explain_ml_prediction(
                prediction=prediction,
                features=features,
                model_type=model_type,
                xai_explanation=xai_explanation,
            )

            # Extract explanation data
            return {
                "explanation": explanation.analysis,
                "summary": explanation.summary,
                "technical_details": explanation.technical_details,
                "recommendations": explanation.recommendations,
                "confidence": explanation.confidence,
            }

        except Exception as e:
            logger.error(f"Error generating natural language explanation: {e}")
            return {
                "error": str(e),
                "message": "Failed to generate natural language explanation",
            }


# Глобальный экземпляр сервиса
ml_service = MLService()


# API эндпоинты
@app.get("/")
async def root():
    """Корневой эндпоинт."""
    return {
        "service": "Resonance Liminal ML Service",
        "version": "1.0.0",
        "status": "running",
        "models_loaded": len(ml_service.models),
        "timestamp": datetime.now().isoformat(),
    }


@app.get("/health")
async def health_check():
    """Health check для ML сервиса."""
    try:
        # Проверяем подключение к Redis
        redis_client.ping()

        return {
            "status": "healthy",
            "redis": "connected",
            "models": len(ml_service.models),
            "timestamp": datetime.now().isoformat(),
        }
    except Exception as e:
        return {
            "status": "unhealthy",
            "error": str(e),
            "timestamp": datetime.now().isoformat(),
        }


@app.post("/train")
async def train_model(request: TrainingRequest, background_tasks: BackgroundTasks):
    """Запускает обучение модели."""
    try:
        # Загружаем данные
        raw_data = await ml_service.load_data_from_redis()
        if len(raw_data) < 10:
            raise HTTPException(
                status_code=400, detail="Недостаточно данных для обучения"
            )

        # Подготавливаем данные
        training_data = await ml_service.prepare_training_data(
            request.model_name, raw_data
        )

        # Запускаем обучение в фоне
        background_tasks.add_task(
            ml_service.train_model_with_kenning, request.model_name, training_data
        )

        return {
            "message": f"Обучение модели {request.model_name} запущено",
            "data_size": len(raw_data),
            "model_name": request.model_name,
        }

    except Exception as e:
        logger.error(f"Ошибка запуска обучения: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/load/{model_name}")
async def load_model_endpoint(model_name: str):
    """Загружает модель для inference."""
    success = await ml_service.load_model(model_name)

    if success:
        return {"message": f"Модель {model_name} загружена", "model_name": model_name}
    else:
        raise HTTPException(status_code=404, detail=f"Модель {model_name} не найдена")


@app.post("/predict")
async def predict_endpoint(request: InferenceRequest):
    """Выполняет предсказание."""
    result = await ml_service.predict(request.model_name, request.features)

    if result is not None:
        return {
            "model_name": request.model_name,
            "prediction": result,
            "timestamp": datetime.now().isoformat(),
        }
    else:
        raise HTTPException(
            status_code=400, detail=f"Ошибка предсказания модели {request.model_name}"
        )


@app.get("/ml_models")
async def list_models():
    """List available ML models"""
    try:
        models = []
        for model_name, model in ml_service.models.items():
            models.append(
                {
                    "name": model_name,
                    "type": ml_service._get_model_type(model_name),
                    "supports_explanation": True,
                    "explanation_methods": [
                        "shap",
                        "lime",
                        "eli5",
                        "anchors",
                        "natural_language",
                    ],
                }
            )

        return {
            "models": models,
            "count": len(models),
            "timestamp": datetime.now().isoformat(),
        }
    except Exception as e:
        logger.error(f"Error listing models: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/explain")
async def explain_prediction(request: ExplanationRequest):
    """Explain a prediction using SHAP, LIME, ELI5, Anchors, or natural language"""
    try:
        # Get or make prediction if not provided
        if request.prediction is None:
            prediction = await ml_service.predict(request.model_name, request.features)
        else:
            prediction = request.prediction

        # Generate explanation
        explanation = await ml_service.get_explanation(
            request.model_name,
            request.features,
            prediction,
            request.explanation_method,
            request.comparison_model,
            request.include_natural_language,
        )

        return {
            "timestamp": time.time(),
            "model_name": request.model_name,
            "features": request.features,
            "prediction": prediction,
            "explanation": explanation,
            "explanation_method": request.explanation_method,
            "comparison_model": request.comparison_model,
        }

    except Exception as e:
        logger.error(f"Error explaining prediction: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/models")
async def list_models():
    """Возвращает список доступных моделей."""
    available_models = []

    # Проверяем файлы в MODEL_STORAGE
    model_dir = Path(MODEL_STORAGE)
    for model_file in model_dir.glob("*.onnx"):
        model_name = model_file.stem
        metadata_file = model_dir / f"{model_name}_metadata.json"

        if metadata_file.exists():
            with open(metadata_file, "r") as f:
                metadata = json.load(f)

            available_models.append(
                {
                    "name": model_name,
                    "status": (
                        "loaded" if model_name in ml_service.models else "available"
                    ),
                    "accuracy": metadata.get("accuracy"),
                    "trained_at": metadata.get("trained_at"),
                    "version": metadata.get("version", "1.0"),
                }
            )

    return {
        "models": available_models,
        "total": len(available_models),
        "loaded": len(ml_service.models),
    }


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=5000, log_level="info")
