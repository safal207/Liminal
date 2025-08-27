"""
ML Scheduler для автоматического обучения и переобучения моделей.
Реализует continuous learning pipeline для Resonance Liminal.
"""

import asyncio
import json
import time
from datetime import datetime
from typing import Any, Dict

import requests
import schedule
from loguru import logger

import redis


class MLScheduler:
    """
    Планировщик для автоматического обучения ML-моделей.
    Поддерживает scheduled retraining, A/B testing, model versioning.
    """

    def __init__(
        self,
        redis_host: str = "redis",
        kenning_ml_url: str = "http://kenning-ml:5000",
        min_data_threshold: int = 100,
    ):
        self.redis_client = redis.Redis(
            host=redis_host, port=6379, decode_responses=True
        )
        self.kenning_ml_url = kenning_ml_url
        self.min_data_threshold = min_data_threshold

        # Конфигурация моделей для автоматического обучения
        self.model_configs = {
            "anomaly_detection": {
                "retrain_interval_hours": 6,  # Переобучение каждые 6 часов
                "min_accuracy_threshold": 0.8,
                "data_key_pattern": "ml:features:*",
                "priority": "high",
            },
            "load_prediction": {
                "retrain_interval_hours": 12,  # Переобучение каждые 12 часов
                "min_accuracy_threshold": 0.75,
                "data_key_pattern": "ml:features:*",
                "priority": "medium",
            },
            "user_behavior": {
                "retrain_interval_hours": 24,  # Переобучение раз в день
                "min_accuracy_threshold": 0.7,
                "data_key_pattern": "ml:features:*",
                "priority": "low",
            },
        }

        # Статистика обучения
        self.training_stats = {
            "total_trainings": 0,
            "successful_trainings": 0,
            "failed_trainings": 0,
            "last_training_time": None,
            "models_trained": {},
        }

        logger.info("ML Scheduler инициализирован")

    async def check_data_availability(self, model_name: str) -> Dict[str, Any]:
        """
        Проверяет доступность данных для обучения модели.

        Args:
            model_name: Название модели

        Returns:
            Информация о доступных данных
        """
        config = self.model_configs.get(model_name, {})
        pattern = config.get("data_key_pattern", "ml:features:*")

        try:
            keys = self.redis_client.keys(pattern)
            data_count = len(keys)

            # Проверяем качество данных (последние записи)
            recent_data_count = 0
            current_time = time.time()

            for key in keys[-50:]:  # Проверяем последние 50 записей
                data = self.redis_client.get(key)
                if data:
                    try:
                        record = json.loads(data)
                        record_time = record.get("timestamp", 0)
                        # Данные не старше 24 часов
                        if current_time - record_time <= 86400:
                            recent_data_count += 1
                    except json.JSONDecodeError:
                        continue

            return {
                "total_records": data_count,
                "recent_records": recent_data_count,
                "sufficient_data": data_count >= self.min_data_threshold,
                "data_quality": recent_data_count / max(min(len(keys), 50), 1),
            }

        except Exception as e:
            logger.error(f"Ошибка проверки данных для {model_name}: {e}")
            return {
                "total_records": 0,
                "recent_records": 0,
                "sufficient_data": False,
                "data_quality": 0.0,
                "error": str(e),
            }

    async def should_retrain_model(self, model_name: str) -> Dict[str, Any]:
        """
        Определяет, нужно ли переобучать модель.

        Args:
            model_name: Название модели

        Returns:
            Решение о переобучении и причины
        """
        config = self.model_configs.get(model_name, {})
        if not config:
            return {"should_retrain": False, "reason": "Model not configured"}

        reasons = []
        should_retrain = False

        # 1. Проверяем временной интервал
        last_training_key = f"ml:training:last:{model_name}"
        last_training_time = self.redis_client.get(last_training_key)

        if last_training_time:
            last_time = float(last_training_time)
            hours_since_training = (time.time() - last_time) / 3600
            interval_hours = config.get("retrain_interval_hours", 24)

            if hours_since_training >= interval_hours:
                should_retrain = True
                reasons.append(
                    f"Interval exceeded: {hours_since_training:.1f}h >= {interval_hours}h"
                )
        else:
            should_retrain = True
            reasons.append("No previous training found")

        # 2. Проверяем доступность данных
        data_info = await self.check_data_availability(model_name)
        if not data_info["sufficient_data"]:
            should_retrain = False
            reasons.append(
                f"Insufficient data: {data_info['total_records']} < {self.min_data_threshold}"
            )

        # 3. Проверяем качество текущей модели (если есть)
        try:
            model_metadata_key = f"model:metadata:{model_name}"
            metadata = self.redis_client.get(model_metadata_key)
            if metadata:
                model_info = json.loads(metadata)
                current_accuracy = model_info.get("accuracy", 0)
                min_accuracy = config.get("min_accuracy_threshold", 0.8)

                if current_accuracy < min_accuracy:
                    should_retrain = True
                    reasons.append(
                        f"Low accuracy: {current_accuracy:.3f} < {min_accuracy}"
                    )
        except Exception as e:
            logger.warning(f"Не удалось проверить метаданные модели {model_name}: {e}")

        # 4. Проверяем drift данных (упрощенная версия)
        if data_info["data_quality"] < 0.5:
            reasons.append(f"Data quality concern: {data_info['data_quality']:.2f}")

        return {
            "should_retrain": should_retrain,
            "reasons": reasons,
            "data_info": data_info,
            "priority": config.get("priority", "medium"),
        }

    async def trigger_model_training(self, model_name: str) -> Dict[str, Any]:
        """
        Запускает обучение модели через Kenning ML Service.

        Args:
            model_name: Название модели

        Returns:
            Результат запуска обучения
        """
        try:
            # Отправляем запрос на обучение
            training_request = {
                "model_name": model_name,
                "config": self.model_configs.get(model_name, {}),
                "data_source": "redis",
                "auto_deploy": True,
            }

            response = requests.post(
                f"{self.kenning_ml_url}/train", json=training_request, timeout=30
            )

            if response.status_code == 200:
                result = response.json()

                # Обновляем статистику
                self.training_stats["total_trainings"] += 1
                self.training_stats["successful_trainings"] += 1
                self.training_stats["last_training_time"] = datetime.now().isoformat()
                self.training_stats["models_trained"][model_name] = {
                    "last_trained": datetime.now().isoformat(),
                    "status": "success",
                }

                # Сохраняем время последнего обучения
                self.redis_client.set(f"ml:training:last:{model_name}", time.time())

                logger.info(f"Успешно запущено обучение модели {model_name}")
                return {"status": "success", "result": result}

            else:
                error_msg = f"HTTP {response.status_code}: {response.text}"
                logger.error(f"Ошибка запуска обучения {model_name}: {error_msg}")

                self.training_stats["total_trainings"] += 1
                self.training_stats["failed_trainings"] += 1

                return {"status": "error", "error": error_msg}

        except Exception as e:
            logger.error(f"Исключение при запуске обучения {model_name}: {e}")

            self.training_stats["total_trainings"] += 1
            self.training_stats["failed_trainings"] += 1

            return {"status": "error", "error": str(e)}

    async def run_scheduled_training(self) -> None:
        """
        Выполняет запланированное обучение всех моделей.
        """
        logger.info("Запуск запланированного обучения моделей")

        training_results = {}

        # Сортируем модели по приоритету
        priority_order = {"high": 0, "medium": 1, "low": 2}
        sorted_models = sorted(
            self.model_configs.items(),
            key=lambda x: priority_order.get(x[1].get("priority", "medium"), 1),
        )

        for model_name, config in sorted_models:
            try:
                # Проверяем, нужно ли переобучение
                decision = await self.should_retrain_model(model_name)

                if decision["should_retrain"]:
                    logger.info(
                        f"Начинаем обучение модели {model_name}. Причины: {', '.join(decision['reasons'])}"
                    )

                    # Запускаем обучение
                    result = await self.trigger_model_training(model_name)
                    training_results[model_name] = result

                    # Пауза между обучениями для снижения нагрузки
                    await asyncio.sleep(30)

                else:
                    logger.debug(f"Модель {model_name} не требует переобучения")
                    training_results[model_name] = {
                        "status": "skipped",
                        "reason": "No retraining needed",
                    }

            except Exception as e:
                logger.error(f"Ошибка при обработке модели {model_name}: {e}")
                training_results[model_name] = {"status": "error", "error": str(e)}

        # Сохраняем результаты в Redis
        self.redis_client.set(
            "ml:training:last_results",
            json.dumps(
                {
                    "timestamp": datetime.now().isoformat(),
                    "results": training_results,
                    "stats": self.training_stats,
                }
            ),
        )

        logger.info(
            f"Завершено запланированное обучение. Результаты: {training_results}"
        )

    async def cleanup_old_data(self) -> None:
        """
        Очищает старые данные для освобождения места.
        """
        try:
            current_time = time.time()
            max_age = 7 * 24 * 3600  # 7 дней

            # Очищаем старые фичи
            keys = self.redis_client.keys("ml:features:*")
            deleted_count = 0

            for key in keys:
                data = self.redis_client.get(key)
                if data:
                    try:
                        record = json.loads(data)
                        record_time = record.get("timestamp", 0)

                        if current_time - record_time > max_age:
                            self.redis_client.delete(key)
                            deleted_count += 1
                    except json.JSONDecodeError:
                        # Удаляем поврежденные записи
                        self.redis_client.delete(key)
                        deleted_count += 1

            logger.info(f"Очищено {deleted_count} старых записей данных")

        except Exception as e:
            logger.error(f"Ошибка очистки данных: {e}")

    def setup_schedule(self) -> None:
        """
        Настраивает расписание для автоматических задач.
        """
        # Основное обучение каждые 2 часа
        schedule.every(2).hours.do(
            lambda: asyncio.create_task(self.run_scheduled_training())
        )

        # Очистка данных раз в день
        schedule.every().day.at("02:00").do(
            lambda: asyncio.create_task(self.cleanup_old_data())
        )

        # Проверка состояния каждые 30 минут
        schedule.every(30).minutes.do(lambda: asyncio.create_task(self.health_check()))

        logger.info("Расписание ML Scheduler настроено")

    async def health_check(self) -> Dict[str, Any]:
        """
        Проверяет состояние ML системы.
        """
        try:
            # Проверяем доступность Kenning ML Service
            response = requests.get(f"{self.kenning_ml_url}/health", timeout=10)
            kenning_status = "healthy" if response.status_code == 200 else "unhealthy"
        except:
            kenning_status = "unreachable"

        # Проверяем Redis
        try:
            self.redis_client.ping()
            redis_status = "healthy"
        except:
            redis_status = "unhealthy"

        # Проверяем доступность данных
        total_data = len(self.redis_client.keys("ml:features:*"))

        health_status = {
            "timestamp": datetime.now().isoformat(),
            "kenning_ml_service": kenning_status,
            "redis": redis_status,
            "total_data_records": total_data,
            "training_stats": self.training_stats,
            "overall_status": (
                "healthy"
                if kenning_status == "healthy" and redis_status == "healthy"
                else "degraded"
            ),
        }

        # Сохраняем статус в Redis
        self.redis_client.set("ml:scheduler:health", json.dumps(health_status))

        return health_status

    async def run_forever(self) -> None:
        """
        Основной цикл планировщика.
        """
        logger.info("Запуск ML Scheduler")

        # Настраиваем расписание
        self.setup_schedule()

        # Выполняем начальную проверку здоровья
        await self.health_check()

        # Основной цикл
        while True:
            try:
                # Выполняем запланированные задачи
                schedule.run_pending()

                # Ждем 60 секунд до следующей проверки
                await asyncio.sleep(60)

            except KeyboardInterrupt:
                logger.info("Получен сигнал остановки")
                break
            except Exception as e:
                logger.error(f"Ошибка в главном цикле планировщика: {e}")
                await asyncio.sleep(60)  # Продолжаем работу после ошибки


async def main():
    """Точка входа для ML Scheduler."""
    scheduler = MLScheduler()
    await scheduler.run_forever()


if __name__ == "__main__":
    asyncio.run(main())
