#!/usr/bin/env python
"""
Расширенная мок-реализация OpenAI API для Resonance Liminal ML
Используется для разработки и тестирования без реальной библиотеки OpenAI
"""

import asyncio
import hashlib
import json
import os
import time

from loguru import logger

# Конфигурационные параметры
MOCK_RESPONSES_DIR = os.environ.get("OPENAI_MOCK_RESPONSES_DIR", "mock_data")
CONNECTION_TIMEOUT = int(os.environ.get("OPENAI_CONNECTION_TIMEOUT", "10"))


# Создаём расширенный мок-класс для AsyncOpenAI
class AsyncOpenAI:
    """Мок-реализация AsyncOpenAI клиента для локальной разработки"""

    def __init__(self, api_key=None):
        """Инициализация мок-клиента"""
        self.api_key = api_key
        self.last_request_time = 0
        self.request_count = 0
        self.cache = {}
        self._ensure_mock_dir()
        logger.info("Создан расширенный мок-объект AsyncOpenAI")

    def _ensure_mock_dir(self):
        """Создает директорию для хранения мок-данных если она не существует"""
        if not os.path.exists(MOCK_RESPONSES_DIR):
            try:
                os.makedirs(MOCK_RESPONSES_DIR)
                logger.info(f"Создана директория для мок-данных: {MOCK_RESPONSES_DIR}")
            except Exception as e:
                logger.error(f"Ошибка при создании директории для мок-данных: {e}")

    def _get_cache_key(self, model, messages):
        """Генерирует ключ кэша на основе запроса"""
        # Создаем детерминированный хеш на основе модели и сообщений
        message_str = json.dumps(messages, sort_keys=True)
        key = f"{model}:{message_str}"
        return hashlib.md5(key.encode()).hexdigest()

    def _save_mock_response(self, cache_key, response_data):
        """Сохраняет ответ в файл для переиспользования"""
        try:
            filename = f"{MOCK_RESPONSES_DIR}/{cache_key}.json"
            with open(filename, "w", encoding="utf-8") as f:
                json.dump(response_data, f, ensure_ascii=False, indent=2)
            logger.debug(f"Мок-ответ сохранен в файл: {filename}")
        except Exception as e:
            logger.error(f"Ошибка при сохранении мок-ответа: {e}")

    def _load_mock_response(self, cache_key):
        """Загружает мок-ответ из файла если он существует"""
        try:
            filename = f"{MOCK_RESPONSES_DIR}/{cache_key}.json"
            if os.path.exists(filename):
                with open(filename, encoding="utf-8") as f:
                    data = json.load(f)
                logger.debug(f"Загружен мок-ответ из файла: {filename}")
                return data
        except Exception as e:
            logger.error(f"Ошибка при загрузке мок-ответа: {e}")
        return None

    class chat:
        class completions:
            @classmethod
            async def create(cls, model=None, messages=None, max_tokens=None, temperature=None):
                """Мок-реализация OpenAI API для генерации ответов"""
                # Симулируем задержку для реалистичности
                await asyncio.sleep(0.5)

                logger.info(f"Мок-вызов OpenAI API, модель: {model}, max_tokens: {max_tokens}")

                # Получаем текст запроса для генерации соответствующего мок-ответа
                request_text = messages[-1]["content"] if messages and len(messages) > 0 else ""

                # Паттерны для различных типов предсказаний
                is_anomaly = "anomaly" in request_text.lower() or "аномал" in request_text.lower()
                is_fraud = "fraud" in request_text.lower() or "мошенни" in request_text.lower()
                is_performance = (
                    "performance" in request_text.lower()
                    or "производительн" in request_text.lower()
                )

                # Выбор шаблона ответа в зависимости от запроса
                if is_anomaly:
                    mock_response = cls._generate_anomaly_response(request_text)
                elif is_fraud:
                    mock_response = cls._generate_fraud_response(request_text)
                elif is_performance:
                    mock_response = cls._generate_performance_response(request_text)
                else:
                    mock_response = cls._generate_generic_response(request_text)

                # Преобразуем в JSON строку
                mock_json = json.dumps(mock_response, ensure_ascii=False)

                # Создаём объект с похожей на OpenAI API структурой
                class MockChoice:
                    class Message:
                        def __init__(self, content):
                            self.content = content

                    def __init__(self, content):
                        self.message = self.Message(content)

                class MockResponse:
                    def __init__(self, content):
                        self.choices = [MockChoice(content)]

                # Записываем в лог для отладки
                logger.debug(
                    f"Сгенерирован мок-ответ типа: {mock_response.get('severity', 'unknown')}"
                )

                return MockResponse(mock_json)

            @classmethod
            def _generate_anomaly_response(cls, request_text):
                """Генерирует ответ для аномалий"""
                return {
                    "analysis": "Повышенная активность пользователя была выявлена как аномальная из-за высокого количества сообщений в минуту (120) и повышенного количества неудачных попыток авторизации. Это может указывать на попытку автоматизированного доступа или брутфорс-атаку.",
                    "recommendations": [
                        "Ввести временное ограничение на частоту сообщений для этого пользователя",
                        "Проверить журналы авторизации на предмет других подозрительных действий",
                        "Использовать CAPTCHA или двухфакторную аутентификацию при подозрительной активности",
                    ],
                    "severity": "high",
                    "confidence": 0.85,
                    "action_items": [
                        "Временно заблокировать IP-адрес пользователя",
                        "Снизить лимиты сообщений до 30 в минуту",
                        "Отправить уведомление службе безопасности",
                    ],
                    "summary": "Обнаружена подозрительная активность пользователя с высоким количеством сообщений и неудачными попытками авторизации",
                    "technical_details": {
                        "anomaly_score_factors": {
                            "messages_per_minute": 0.45,
                            "error_rate": 0.35,
                            "failed_auth_attempts": 0.15,
                            "connection_duration": 0.03,
                            "unique_ips": 0.02,
                        },
                        "detection_method": "shap",
                        "threshold": 0.6,
                        "timestamp": time.time(),
                    },
                    "follow_up_questions": [
                        "Наблюдалась ли подобная активность ранее с этого IP-адреса?",
                        "Сколько пользователей показывают похожие паттерны активности?",
                    ],
                }

            @classmethod
            def _generate_fraud_response(cls, request_text):
                """Генерирует ответ для случаев мошенничества"""
                return {
                    "analysis": "Обнаружена потенциальная попытка мошенничества с использованием подмены учетных данных. Пользователь пытался авторизоваться с разных IP-адресов (7 уникальных адресов) в течение короткого промежутка времени (15 минут). Поведение не соответствует нормальному паттерну авторизации для данного аккаунта.",
                    "recommendations": [
                        "Немедленная временная блокировка учетной записи для предотвращения несанкционированного доступа",
                        "Проверка недавних операций на предмет подозрительной активности",
                        "Оповещение владельца учетной записи по дополнительному каналу связи",
                    ],
                    "severity": "critical",
                    "confidence": 0.92,
                    "action_items": [
                        "Заблокировать учетную запись до подтверждения личности",
                        "Запросить повторную аутентификацию с дополнительной проверкой",
                        "Обновить модель обнаружения мошенничества с новыми данными",
                    ],
                    "summary": "Выявлена подозрительная активность с признаками компрометации учетной записи и попытки неавторизованного доступа",
                    "technical_details": {
                        "fraud_indicators": {
                            "unique_ips_in_timeframe": 7,
                            "geographic_dispersion_score": 0.85,
                            "authentication_velocity": "abnormal",
                            "behavior_deviation": 0.78,
                        },
                        "detection_method": "anomaly_detection + rule_based",
                        "timestamp": time.time(),
                    },
                    "follow_up_questions": [
                        "Использовались ли скомпрометированные пароли из известных утечек данных?",
                        "Есть ли признаки автоматизированного доступа через API?",
                    ],
                }

            @classmethod
            def _generate_performance_response(cls, request_text):
                """Генерирует ответ для проблем с производительностью"""
                return {
                    "analysis": "Анализ производительности системы выявил потенциальное узкое место в обработке WebSocket-соединений. При превышении порога в 5000 одновременных соединений наблюдается экспоненциальный рост задержки обработки сообщений (с 45мс до 320мс) и увеличение использования памяти. Основная проблема связана с неэффективным управлением соединениями в Redis-кластере и недостаточной оптимизацией запросов к Neo4j базе данных.",
                    "recommendations": [
                        "Оптимизировать работу с Redis-кластером через шардирование и реализацию connection pooling",
                        "Внедрить кэширование для частых запросов к Neo4j",
                        "Реализовать backpressure механизмы для ограничения нагрузки",
                    ],
                    "severity": "medium",
                    "confidence": 0.88,
                    "action_items": [
                        "Увеличить размер Redis-кластера на 2 узла",
                        "Оптимизировать Cypher-запросы к Neo4j с использованием индексов",
                        "Добавить метрики отслеживания latency для критичных операций",
                    ],
                    "summary": "Выявлены проблемы масштабирования при высокой нагрузке, связанные с неоптимальным использованием Redis и Neo4j",
                    "technical_details": {
                        "performance_metrics": {
                            "max_connections_tested": 8500,
                            "latency_increase_factor": 7.1,
                            "memory_usage_growth": "superlinear",
                            "cpu_bottlenecks": ["redis_operations", "graph_traversals"],
                        },
                        "detection_method": "load_testing + profiling",
                        "timestamp": time.time(),
                    },
                    "follow_up_questions": [
                        "Какой эффект окажет шардирование Redis на текущую архитектуру?",
                        "Насколько критичны для бизнеса периоды повышенной задержки?",
                    ],
                }

            @classmethod
            def _generate_generic_response(cls, request_text):
                """Генерирует общий ответ для неопознанных типов запросов"""
                return {
                    "analysis": f"Анализ данных показывает стандартное поведение системы без выраженных аномалий или отклонений. Запрос: {request_text[:50]}...",
                    "recommendations": [
                        "Продолжить мониторинг ключевых метрик системы",
                        "Регулярно обновлять базовые показатели для точного обнаружения аномалий",
                    ],
                    "severity": "low",
                    "confidence": 0.75,
                    "action_items": [
                        "Поддерживать текущие настройки системы",
                        "Провести плановый аудит безопасности в следующем квартале",
                    ],
                    "summary": "Система функционирует в нормальном режиме, значительных отклонений не обнаружено",
                    "technical_details": {
                        "baseline_metrics": {
                            "average_load": "within normal range",
                            "response_time": "stable",
                            "error_rate": "<1%",
                            "success_rate": ">99%",
                        },
                        "detection_method": "statistical_analysis",
                        "timestamp": time.time(),
                    },
                    "follow_up_questions": [
                        "Какие метрики стоит добавить в мониторинг?",
                        "Каковы пороговые значения для включения автоматических алертов?",
                    ],
                }


# Функция для создания экземпляра мок-клиента
def create_mock_client(api_key=None):
    """Создаёт мок-клиент OpenAI API"""
    return AsyncOpenAI(api_key=api_key)
