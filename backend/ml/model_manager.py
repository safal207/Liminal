"""
Model Manager для интеграции с AutoML for Embedded (Kenning).
Управляет ML-моделями для оптимизации WebSocket производительности.
"""

import asyncio
import json
import os
from pathlib import Path
from typing import Any, Dict, List, Optional

import yaml
from loguru import logger


class ModelManager:
    """
    Управляет ML-моделями через интеграцию с Kenning framework.
    Поддерживает автоматическое обучение и деплой моделей.
    """

    def __init__(
        self, models_dir: str = "ml/models", kenning_config_dir: str = "ml/configs"
    ):
        self.models_dir = Path(models_dir)
        self.kenning_config_dir = Path(kenning_config_dir)
        self.models_dir.mkdir(parents=True, exist_ok=True)
        self.kenning_config_dir.mkdir(parents=True, exist_ok=True)

        # Активные модели
        self.active_models: Dict[str, Any] = {}

        # Конфигурации моделей
        self.model_configs = {
            "anomaly_detection": {
                "description": "Обнаружение аномалий в WebSocket трафике",
                "features": [
                    "messages_per_minute",
                    "avg_message_size",
                    "error_rate",
                    "rate_limit_violations",
                ],
                "target": "is_anomaly",
                "model_type": "classification",
            },
            "load_prediction": {
                "description": "Предсказание нагрузки на сервер",
                "features": [
                    "messages_per_minute",
                    "connection_duration",
                    "channels_count",
                ],
                "target": "future_load",
                "model_type": "regression",
            },
            "user_behavior": {
                "description": "Анализ поведения пользователей",
                "features": [
                    "messages_per_minute",
                    "channels_count",
                    "connection_duration",
                ],
                "target": "user_type",
                "model_type": "clustering",
            },
        }

    def create_kenning_config(self, model_name: str, training_data_path: str) -> str:
        """
        Создает YAML конфигурацию для Kenning AutoML pipeline.

        Args:
            model_name: Название модели
            training_data_path: Путь к данным для обучения

        Returns:
            Путь к созданному конфиг-файлу
        """
        if model_name not in self.model_configs:
            raise ValueError(f"Неизвестная модель: {model_name}")

        config = self.model_configs[model_name]

        kenning_config = {
            "pipeline": {
                "name": f"resonance_liminal_{model_name}",
                "description": config["description"],
            },
            "data": {
                "training_data": training_data_path,
                "features": config["features"],
                "target": config["target"],
            },
            "model": {
                "type": config["model_type"],
                "optimization": {
                    "target": (
                        "accuracy"
                        if config["model_type"] == "classification"
                        else "mse"
                    ),
                    "time_budget": 300,  # 5 минут на AutoML поиск
                    "memory_limit": "1GB",
                },
            },
            "deployment": {
                "format": "onnx",
                "quantization": True,
                "target_platform": "cpu",
            },
            "evaluation": {
                "metrics": (
                    ["accuracy", "precision", "recall"]
                    if config["model_type"] == "classification"
                    else ["mse", "mae"]
                ),
                "cross_validation": {"folds": 5},
            },
        }

        config_path = self.kenning_config_dir / f"{model_name}_config.yaml"
        with open(config_path, "w", encoding="utf-8") as f:
            yaml.dump(kenning_config, f, default_flow_style=False, allow_unicode=True)

        logger.info(f"Создан Kenning конфиг для модели {model_name}: {config_path}")
        return str(config_path)

    async def train_model_with_kenning(
        self, model_name: str, training_data: List[Dict[str, Any]]
    ) -> bool:
        """
        Обучает модель используя Kenning AutoML pipeline.

        Args:
            model_name: Название модели
            training_data: Данные для обучения

        Returns:
            True если обучение успешно
        """
        try:
            # Сохраняем данные для обучения
            data_path = self.models_dir / f"{model_name}_training_data.json"
            with open(data_path, "w", encoding="utf-8") as f:
                json.dump(training_data, f, ensure_ascii=False, indent=2)

            # Создаем Kenning конфиг
            config_path = self.create_kenning_config(model_name, str(data_path))

            # Запускаем Kenning через Docker (предполагается, что Docker доступен)
            docker_command = [
                "docker",
                "run",
                "--rm",
                "-v",
                f"{os.getcwd()}:/workspace",
                "-w",
                "/workspace",
                "kenning/kenning:latest",
                "kenning",
                "train",
                "--config",
                config_path,
                "--output-dir",
                str(self.models_dir / model_name),
            ]

            logger.info(f"Запуск обучения модели {model_name} через Kenning...")

            # Асинхронный запуск процесса
            process = await asyncio.create_subprocess_exec(
                *docker_command,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )

            stdout, stderr = await process.communicate()

            if process.returncode == 0:
                logger.info(f"Модель {model_name} успешно обучена")
                logger.debug(f"Kenning output: {stdout.decode()}")
                return True
            else:
                logger.error(f"Ошибка обучения модели {model_name}: {stderr.decode()}")
                return False

        except Exception as e:
            logger.error(f"Исключение при обучении модели {model_name}: {e}")
            return False

    def load_model(self, model_name: str) -> bool:
        """
        Загружает обученную модель для использования.

        Args:
            model_name: Название модели

        Returns:
            True если модель успешно загружена
        """
        model_path = self.models_dir / model_name / "model.onnx"

        if not model_path.exists():
            logger.warning(f"Модель {model_name} не найдена по пути {model_path}")
            return False

        try:
            # Здесь должна быть логика загрузки ONNX модели
            # Для демонстрации просто помечаем как загруженную
            self.active_models[model_name] = {
                "path": str(model_path),
                "loaded_at": asyncio.get_event_loop().time(),
                "status": "active",
            }

            logger.info(f"Модель {model_name} успешно загружена")
            return True

        except Exception as e:
            logger.error(f"Ошибка загрузки модели {model_name}: {e}")
            return False

    def predict(self, model_name: str, features: Dict[str, Any]) -> Optional[Any]:
        """
        Выполняет предсказание используя загруженную модель.

        Args:
            model_name: Название модели
            features: Фичи для предсказания

        Returns:
            Результат предсказания или None при ошибке
        """
        if model_name not in self.active_models:
            logger.warning(f"Модель {model_name} не загружена")
            return None

        try:
            # Здесь должна быть логика инференса ONNX модели
            # Для демонстрации возвращаем заглушку

            if model_name == "anomaly_detection":
                # Простая эвристика для демонстрации
                score = features.get("messages_per_minute", 0) * features.get(
                    "error_rate", 0
                )
                return {"is_anomaly": score > 10, "anomaly_score": score}

            elif model_name == "load_prediction":
                # Предсказание нагрузки
                predicted_load = features.get("messages_per_minute", 0) * 1.2
                return {"predicted_load": predicted_load}

            return {"prediction": "model_placeholder"}

        except Exception as e:
            logger.error(f"Ошибка предсказания модели {model_name}: {e}")
            return None

    def get_model_status(self) -> Dict[str, Any]:
        """
        Возвращает статус всех моделей.

        Returns:
            Словарь со статусом моделей
        """
        return {
            "active_models": list(self.active_models.keys()),
            "available_configs": list(self.model_configs.keys()),
            "models_directory": str(self.models_dir),
            "total_models": len(self.active_models),
        }


# Глобальный экземпляр для использования в приложении
model_manager = ModelManager()
