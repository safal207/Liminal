"""
OpenAI Integration Service для Resonance Liminal.
Интеграция GPT-4 для intelligent analysis, automated responses и natural language explanations.
"""

import json
import os
import time
import traceback
from dataclasses import dataclass
from datetime import datetime
from typing import Any

from loguru import logger

# Проверяем доступность библиотеки openai
try:
    import openai

    OPENAI_AVAILABLE = True
except ImportError:
    openai = None
    OPENAI_AVAILABLE = False


# Импортируем универсальный адаптер
try:
    from .openai_wrapper import (
        AsyncOpenAI,
        initialize_llm_client,
    )

    logger.success("Универсальный OpenAI адаптер успешно импортирован")
    WRAPPER_AVAILABLE = True
except ImportError as e:
    logger.error(f"Не удалось импортировать универсальный OpenAI адаптер: {e}")
    WRAPPER_AVAILABLE = False

    # Резервная базовая мок-реализация для обратной совместимости
    class AsyncOpenAI:
        def __init__(self, api_key=None):
            self.api_key = api_key
            logger.info("Создан резервный базовый мок-объект AsyncOpenAI")

        class chat:
            class completions:
                @classmethod
                async def create(cls, model=None, messages=None, max_tokens=None, temperature=None):
                    logger.info(f"Резервный базовый мок-вызов OpenAI API, модель: {model}")

                    # Получаем текст запроса
                    request_text = messages[-1]["content"] if messages and len(messages) > 0 else ""

                    # Упрощенный мок-ответ
                    mock_response = {
                        "analysis": f"Анализ запроса: {request_text[:50]}...",
                        "recommendations": ["Рекомендация 1", "Рекомендация 2"],
                        "severity": "medium",
                        "confidence": 0.75,
                        "action_items": ["Действие 1", "Действие 2"],
                        "summary": "Краткое описание результатов анализа",
                        "technical_details": {"mock": True, "timestamp": time.time()},
                    }

                    # Создаём объект с структурой OpenAI API
                    mock_json = json.dumps(mock_response, ensure_ascii=False)

                    class MockChoice:
                        class Message:
                            def __init__(self, content):
                                self.content = content

                        def __init__(self, content):
                            self.message = self.Message(content)

                    class MockResponse:
                        def __init__(self, content):
                            self.choices = [MockChoice(content)]

                    return MockResponse(mock_json)


@dataclass
class AnalysisRequest:
    """Запрос на анализ для OpenAI."""

    type: str  # "anomaly", "log_analysis", "incident", "pattern_recognition"
    data: dict[str, Any]
    context: dict[str, Any] | None = None
    priority: str = "normal"  # "low", "normal", "high", "critical"
    timestamp: str | None = None


@dataclass
class AnalysisResponse:
    """Ответ от OpenAI анализа."""

    analysis: str
    recommendations: list[str]
    severity: str  # "low", "medium", "high", "critical"
    confidence: float
    action_items: list[str]
    summary: str
    technical_details: dict[str, Any]
    follow_up_questions: list[str]


class OpenAIService:
    """
    Сервис для интеграции с OpenAI API.
    Предоставляет intelligent analysis, automated responses и natural language explanations.
    """

    def __init__(self, api_key: str | None = None):
        self.api_key = api_key or os.getenv("OPENAI_API_KEY")
        self.client = None
        self.model = "gpt-4-turbo-preview"  # Используем GPT-4 Turbo
        self.max_tokens = 2000
        self.temperature = 0.3  # Низкая температура для более точных ответов

        # Кэш для ответов
        self.response_cache = {}
        self.cache_ttl = 3600  # 1 час

        # Контекст системы
        self.system_context = self._build_system_context()

        # Инициализируем клиент
        self._init_client()

        logger.info("OpenAI Service инициализирован")

    async def _init_client(self):
        """Инициализирует OpenAI клиент."""
        if not self.api_key:
            # Пытаемся загрузить API ключ из переменной окружения
            self.api_key = os.environ.get("OPENAI_API_KEY")

            if not self.api_key:
                logger.warning("API ключ OpenAI не настроен!")
                return False
            else:
                logger.info(
                    f"Загружен API ключ OpenAI из переменной окружения: {self.api_key[:5]}...{self.api_key[-4:] if len(self.api_key) > 9 else ''}"
                )

        try:
            if WRAPPER_AVAILABLE:
                # Инициализируем универсальный клиент
                success = await initialize_llm_client()
                if success:
                    self.client = AsyncOpenAI(api_key=self.api_key)
                    logger.success(
                        "Универсальный OpenAI клиент инициализирован в режиме реального API"
                    )
                else:
                    self.client = AsyncOpenAI(api_key=self.api_key)  # Мок-режим в адаптере
                    logger.info("Универсальный OpenAI клиент инициализирован в режиме мока")
                return True
            else:
                # Резервная мок-реализация
                self.client = AsyncOpenAI(api_key=self.api_key)
                logger.info("Инициализирован резервный мок-клиент OpenAI")
                return True

        except Exception:
            logger.error(f"Ошибка инициализации OpenAI клиента: {traceback.format_exc()}")
            self.client = None
            return False

        # Дополнительная проверка формата ключа
        if self.api_key and ("\n" in self.api_key or "\r" in self.api_key):
            logger.warning("API ключ содержит переносы строк, что может вызвать проблемы")

    def initialize(self, api_key=None):
        """Публичный метод для инициализации клиента.

        Args:
            api_key: Опциональный API ключ, если не указан - используется ключ из конструктора или из переменных окружения
        """
        if api_key:
            self.api_key = api_key
        self._init_client()
        return self

    def initialize_client(self, api_key=None):
        """Публичный метод для инициализации клиента (альтернативное название для совместимости).

        Args:
            api_key: Опциональный API ключ
        """
        return self.initialize(api_key=api_key)

    def _build_system_context(self) -> str:
        """Строит системный контекст для OpenAI."""
        return """
Вы - AI-аналитик для системы Resonance Liminal, real-time WebSocket платформы.

ВАША РОЛЬ:
- Анализировать аномалии в WebSocket трафике
- Объяснять ML-предсказания простым языком
- Предлагать решения для инцидентов
- Генерировать intelligent alerts и recommendations

КОНТЕКСТ СИСТЕМЫ:
- WebSocket сервер с JWT аутентификацией
- Redis для rate limiting и кэширования
- ML модели для anomaly detection и predictive analytics
- Prometheus/Grafana для мониторинга
- Docker-based микросервисная архитектура

ВАШИ ПРИНЦИПЫ:
1. Краткость и ясность
2. Actionable recommendations
3. Техническая точность
4. Приоритизация по критичности
5. Проактивные предложения

ФОРМАТЫ ОТВЕТОВ:
- Анализ: краткое описание проблемы
- Рекомендации: конкретные действия
- Severity: low/medium/high/critical
- Action items: пошаговые инструкции
"""

    async def analyze_anomaly(
        self,
        anomaly_data: dict[str, Any],
        ml_explanation: dict[str, Any] | None = None,
        context: dict[str, Any] | None = None,
    ) -> AnalysisResponse:
        """
        Анализирует аномалию с помощью GPT-4.

        Args:
            anomaly_data: Данные об аномалии
            ml_explanation: Объяснение от ML модели
            context: Дополнительный контекст

        Returns:
            Анализ аномалии
        """
        request = AnalysisRequest(
            type="anomaly",
            data=anomaly_data,
            context={"ml_explanation": ml_explanation, "additional": context},
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_anomaly_prompt(request)
        return await self._get_analysis(prompt, "anomaly")

    async def analyze_logs(
        self,
        log_entries: list[str],
        time_range: str,
        error_patterns: list[str] | None = None,
    ) -> AnalysisResponse:
        """
        Анализирует логи для поиска паттернов и проблем.

        Args:
            log_entries: Записи логов
            time_range: Временной диапазон
            error_patterns: Известные паттерны ошибок

        Returns:
            Анализ логов
        """
        request = AnalysisRequest(
            type="log_analysis",
            data={
                "logs": log_entries,
                "time_range": time_range,
                "error_patterns": error_patterns or [],
            },
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_log_analysis_prompt(request)
        return await self._get_analysis(prompt, "log_analysis")

    async def explain_ml_prediction(
        self,
        prediction: Any,
        features: dict[str, Any],
        model_type: str,
        xai_explanation: dict[str, Any] | None = None,
    ) -> AnalysisResponse:
        """
        Объясняет ML-предсказание естественным языком.

        Args:
            prediction: Предсказание модели
            features: Входные фичи
            model_type: Тип модели
            xai_explanation: XAI объяснение

        Returns:
            AnalysisResponse с объяснением на естественном языке
        """
        # Кэш-ключ на основе входных данных
        cache_key = f"ml_explanation:{model_type}:{hash(frozenset(str(features.items())))}"

        # Проверяем кэш
        if cache_key in self.response_cache:
            cached_response, cached_time = self.response_cache[cache_key]
            if time.time() - cached_time < self.cache_ttl:
                logger.debug("Используется кэшированное объяснение ML")
                return cached_response

        prompt = f"""
Объясните это ML-предсказание простым языком для DevOps инженера:

МОДЕЛЬ: {model_type}
ПРЕДСКАЗАНИЕ: {json.dumps(prediction, indent=2, ensure_ascii=False)}

ВХОДНЫЕ ДАННЫЕ:
{json.dumps(features, indent=2, ensure_ascii=False)}

XAI АНАЛИЗ:
{json.dumps(xai_explanation, indent=2, ensure_ascii=False) if xai_explanation else "Недоступен"}

Предоставьте следующую информацию в формате JSON:
1. analysis: полный анализ предсказания (что означает, какие факторы повлияли)
2. recommendations: список конкретных рекомендуемых действий
3. severity: оценка серьезности ситуации (low, medium, high, critical)
4. confidence: число от 0 до 1, показывающее уверенность в анализе
5. action_items: список конкретных шагов для исправления ситуации
6. summary: краткое описание предсказания в 1-2 предложения
7. technical_details: технические детали о предсказании
8. follow_up_questions: 2-3 вопроса для дальнейшего анализа

Ответ должен быть понятен технически грамотному человеку без глубоких знаний ML.
"""

        try:
            # Получаем анализ от OpenAI
            response = await self._call_openai(prompt)

            # Пытаемся распарсить JSON
            try:
                json_response = json.loads(response)
                result = AnalysisResponse(
                    analysis=json_response.get("analysis", "Анализ недоступен"),
                    recommendations=json_response.get(
                        "recommendations", ["Рекомендации недоступны"]
                    ),
                    severity=json_response.get("severity", "medium"),
                    confidence=float(json_response.get("confidence", 0.5)),
                    action_items=json_response.get("action_items", ["Действия не определены"]),
                    summary=json_response.get("summary", "Краткое описание недоступно"),
                    technical_details=json_response.get(
                        "technical_details", {"info": "Технические детали недоступны"}
                    ),
                    follow_up_questions=json_response.get(
                        "follow_up_questions",
                        ["Дополнительные вопросы не предусмотрены"],
                    ),
                )
            except json.JSONDecodeError:
                # Если не удалось распарсить JSON, создаем объект с текстовым ответом
                logger.warning(
                    "Не удалось распарсить JSON ответ от OpenAI, используем текстовый формат"
                )
                result = AnalysisResponse(
                    analysis=response.strip(),
                    recommendations=["Автоматические рекомендации недоступны"],
                    severity="medium",
                    confidence=0.5,
                    action_items=["Автоматические действия недоступны"],
                    summary=response[:100] + "..." if len(response) > 100 else response,
                    technical_details={"raw_response": response},
                    follow_up_questions=[
                        "Как улучшить мониторинг этой ситуации?",
                        "Какие пороговые значения стоит установить для алертов?",
                    ],
                )

            # Сохраняем в кэш
            self.response_cache[cache_key] = (result, time.time())
            return result

        except Exception as e:
            logger.error(f"Ошибка объяснения ML-предсказания: {e}")
            # Возвращаем объект с ошибкой
            return AnalysisResponse(
                analysis=f"Не удалось получить объяснение: {str(e)}",
                recommendations=[
                    "Проверьте подключение к OpenAI API",
                    "Убедитесь в валидности API ключа",
                ],
                severity="low",
                confidence=0.0,
                action_items=[
                    "Проверьте логи для деталей ошибки",
                    "Попробуйте запрос позже",
                ],
                summary=f"Ошибка при получении объяснения: {str(e)}",
                technical_details={
                    "error": str(e),
                    "traceback": traceback.format_exc(),
                },
                follow_up_questions=[
                    "Что могло вызвать эту ошибку?",
                    "Как проверить статус OpenAI API?",
                ],
            )

    async def generate_incident_response(
        self, incident_data: dict[str, Any], severity: str, affected_systems: list[str]
    ) -> AnalysisResponse:
        """
        Генерирует план реагирования на инцидент.

        Args:
            incident_data: Данные об инциденте
            severity: Критичность инцидента
            affected_systems: Затронутые системы

        Returns:
            План реагирования
        """
        request = AnalysisRequest(
            type="incident",
            data=incident_data,
            context={"affected_systems": affected_systems},
            priority=severity,
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_incident_response_prompt(request)
        return await self._get_analysis(prompt, "incident")

    async def analyze_performance_patterns(
        self,
        metrics: dict[str, Any],
        time_series_data: list[dict[str, Any]],
        baseline: dict[str, Any] | None = None,
    ) -> AnalysisResponse:
        """
        Анализирует паттерны производительности.

        Args:
            metrics: Текущие метрики
            time_series_data: Временные ряды
            baseline: Базовые показатели

        Returns:
            Анализ производительности
        """
        request = AnalysisRequest(
            type="pattern_recognition",
            data={
                "current_metrics": metrics,
                "time_series": time_series_data,
                "baseline": baseline,
            },
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_performance_analysis_prompt(request)
        return await self._get_analysis(prompt, "performance")

    async def generate_smart_alert(
        self,
        alert_data: dict[str, Any],
        context: dict[str, Any],
        recipient_role: str = "devops",
    ) -> str:
        """
        Генерирует умное уведомление.

        Args:
            alert_data: Данные для алерта
            context: Контекст системы
            recipient_role: Роль получателя (devops, developer, manager)

        Returns:
            Текст умного уведомления
        """
        prompt = f"""
Создайте smart alert для {recipient_role} на основе этих данных:

ДАННЫЕ АЛЕРТА:
{json.dumps(alert_data, indent=2, ensure_ascii=False)}

КОНТЕКСТ:
{json.dumps(context, indent=2, ensure_ascii=False)}

Требования к алерту:
1. Краткое описание проблемы
2. Влияние на пользователей
3. Рекомендуемые действия
4. Приоритет и временные рамки
5. Ссылки на дашборды/логи

Стиль: профессиональный, но понятный. Длина: до 200 слов.
"""

        try:
            response = await self._call_openai(prompt)
            return response.strip()
        except Exception as e:
            logger.error(f"Ошибка генерации smart alert: {e}")
            return f"Стандартный алерт: {alert_data.get('message', 'Неизвестная проблема')}"

    def _build_anomaly_prompt(self, request: AnalysisRequest) -> str:
        """Строит промпт для анализа аномалий."""
        return f"""
Проанализируйте эту аномалию в WebSocket системе:

ДАННЫЕ АНОМАЛИИ:
{json.dumps(request.data, indent=2, ensure_ascii=False)}

КОНТЕКСТ:
{json.dumps(request.context, indent=2, ensure_ascii=False)}

ВРЕМЯ: {request.timestamp}

Предоставьте:
1. Краткий анализ проблемы
2. Возможные причины
3. Рекомендации по устранению
4. Оценка критичности (low/medium/high/critical)
5. Конкретные действия для DevOps

Формат ответа: JSON с полями analysis, recommendations, severity, action_items, summary.
"""

    def _build_log_analysis_prompt(self, request: AnalysisRequest) -> str:
        """Строит промпт для анализа логов."""
        logs_sample = request.data["logs"][:10]  # Берем первые 10 записей

        return f"""
Проанализируйте эти логи WebSocket сервера:

ПЕРИОД: {request.data["time_range"]}
КОЛИЧЕСТВО ЗАПИСЕЙ: {len(request.data["logs"])}

ОБРАЗЕЦ ЛОГОВ:
{chr(10).join(logs_sample)}

ИЗВЕСТНЫЕ ПАТТЕРНЫ ОШИБОК:
{request.data.get("error_patterns", [])}

Найдите:
1. Паттерны ошибок и аномалии
2. Проблемы производительности
3. Подозрительную активность
4. Рекомендации по улучшению

Формат ответа: JSON с полями analysis, recommendations, severity, action_items, summary.
"""

    def _build_incident_response_prompt(self, request: AnalysisRequest) -> str:
        """Строит промпт для плана реагирования на инцидент."""
        return f"""
Создайте план реагирования на этот инцидент:

ДАННЫЕ ИНЦИДЕНТА:
{json.dumps(request.data, indent=2, ensure_ascii=False)}

КРИТИЧНОСТЬ: {request.priority}
ЗАТРОНУТЫЕ СИСТЕМЫ: {request.context.get("affected_systems", [])}
ВРЕМЯ: {request.timestamp}

Создайте план включающий:
1. Немедленные действия (первые 15 минут)
2. Краткосрочные меры (1-4 часа)
3. Долгосрочные улучшения
4. Коммуникационный план
5. Метрики для отслеживания восстановления

Формат ответа: JSON с полями analysis, recommendations, severity, action_items, summary.
"""

    async def _call_openai(self, prompt: str) -> str:
        """
        Вызывает OpenAI API для получения ответа на промпт.

        Args:
            prompt: Текст промпта

        Returns:
            Ответ от OpenAI API
        """
        if not OPENAI_AVAILABLE:
            logger.error("OpenAI API недоступно")
            return "Ошибка: OpenAI API недоступно. Установите библиотеку openai."

        if not self.client:
            logger.error("OpenAI клиент не инициализирован")
            return "Ошибка: OpenAI клиент не инициализирован"

        # Строим сообщения для чата
        messages = [
            {"role": "system", "content": self.system_context},
            {"role": "user", "content": prompt},
        ]

        # Параметры запроса
        try:
            logger.debug(f"Отправка запроса в OpenAI API: {self.model}")
            response = await self.client.chat.completions.create(
                model=self.model,
                messages=messages,
                max_tokens=self.max_tokens,
                temperature=self.temperature,
            )
            logger.debug("Получен ответ от OpenAI API")

            # Извлекаем текст
            content = response.choices[0].message.content
            return content

        except Exception as e:
            logger.error(f"Ошибка при вызове OpenAI API: {e}")
            return f"Ошибка при вызове OpenAI API: {str(e)}"

    async def _get_analysis(self, prompt: str, analysis_type: str) -> AnalysisResponse:
        """
        Получает анализ от OpenAI и преобразует в AnalysisResponse.

        Args:
            prompt: Промпт для анализа
            analysis_type: Тип анализа для кэширования

        Returns:
            AnalysisResponse с результатами анализа
        """
        # Создаем кэш-ключ на основе хэша промпта
        cache_key = f"{analysis_type}:{hash(prompt)}"

        # Проверяем кэш
        if cache_key in self.response_cache:
            cached_response, cached_time = self.response_cache[cache_key]
            if time.time() - cached_time < self.cache_ttl:
                logger.debug(f"Используется кэшированный анализ {analysis_type}")
                return cached_response

        try:
            # Получаем ответ
            response = await self._call_openai(prompt)

            # Пытаемся парсить JSON
            try:
                json_response = json.loads(response)
                result = AnalysisResponse(
                    analysis=json_response.get("analysis", "Анализ недоступен"),
                    recommendations=json_response.get(
                        "recommendations", ["Рекомендации недоступны"]
                    ),
                    severity=json_response.get("severity", "medium"),
                    confidence=float(json_response.get("confidence", 0.5)),
                    action_items=json_response.get("action_items", ["Действия не определены"]),
                    summary=json_response.get("summary", "Краткое описание недоступно"),
                    technical_details=json_response.get(
                        "technical_details", {"info": "Технические детали недоступны"}
                    ),
                    follow_up_questions=json_response.get(
                        "follow_up_questions",
                        ["Дополнительные вопросы не предусмотрены"],
                    ),
                )
            except json.JSONDecodeError:
                # Если не удалось парсить JSON
                logger.warning(
                    f"Не удалось парсить JSON из ответа OpenAI. Ответ: {response[:100]}..."
                )
                result = AnalysisResponse(
                    analysis=response,
                    recommendations=["Не удалось извлечь рекомендации"],
                    severity="medium",
                    confidence=0.5,
                    action_items=["Ручная проверка необходима"],
                    summary=response[:100] + "..." if len(response) > 100 else response,
                    technical_details={"raw_response": response},
                    follow_up_questions=["Как улучшить точность анализа?"],
                )

            # Сохраняем в кэш
            self.response_cache[cache_key] = (result, time.time())
            return result

        except Exception as e:
            logger.error(f"Ошибка при получении анализа: {e}")
            return AnalysisResponse(
                analysis=f"Ошибка при получении анализа: {str(e)}",
                recommendations=["Проверьте подключение к API"],
                severity="high",
                confidence=0.0,
                action_items=["Проверьте логи", "Проверьте API ключ"],
                summary=f"Ошибка: {str(e)}",
                technical_details={
                    "error": str(e),
                    "traceback": traceback.format_exc(),
                },
                follow_up_questions=[],
            )

    def _build_performance_analysis_prompt(self, request: AnalysisRequest) -> str:
        """Строит промпт для анализа производительности."""
        return f"""
Проанализируйте производительность системы:

ТЕКУЩИЕ МЕТРИКИ:
{json.dumps(request.data["current_metrics"], indent=2, ensure_ascii=False)}

ВРЕМЕННЫЕ РЯДЫ (последние точки):
{json.dumps(request.data["time_series"][-5:], indent=2, ensure_ascii=False)}

БАЗОВЫЕ ПОКАЗАТЕЛИ:
{json.dumps(request.data.get("baseline"), indent=2, ensure_ascii=False)}

Определите:
1. Тренды производительности
2. Узкие места
3. Аномальные паттерны
4. Рекомендации по оптимизации
5. Предсказания на ближайшее время

Формат ответа: JSON с полями analysis, recommendations, severity, action_items, summary.
"""

    async def _get_analysis(self, prompt: str, analysis_type: str) -> AnalysisResponse:
        """Получает анализ от OpenAI и парсит ответ."""
        try:
            # Проверяем кэш
            cache_key = f"{analysis_type}:{hash(prompt)}"
            if cache_key in self.response_cache:
                cached_response, timestamp = self.response_cache[cache_key]
                if time.time() - timestamp < self.cache_ttl:
                    return cached_response

            # Получаем ответ от OpenAI
            response_text = await self._call_openai(prompt)

            # Парсим JSON ответ
            try:
                response_data = json.loads(response_text)
            except json.JSONDecodeError:
                # Если не JSON, создаем структурированный ответ
                response_data = {
                    "analysis": response_text,
                    "recommendations": ["Требуется дополнительный анализ"],
                    "severity": "medium",
                    "action_items": ["Проверить детали вручную"],
                    "summary": "Анализ завершен",
                }

            # Создаем AnalysisResponse
            analysis_response = AnalysisResponse(
                analysis=response_data.get("analysis", ""),
                recommendations=response_data.get("recommendations", []),
                severity=response_data.get("severity", "medium"),
                confidence=response_data.get("confidence", 0.8),
                action_items=response_data.get("action_items", []),
                summary=response_data.get("summary", ""),
                technical_details=response_data.get("technical_details", {}),
                follow_up_questions=response_data.get("follow_up_questions", []),
            )

            # Кэшируем ответ
            self.response_cache[cache_key] = (analysis_response, time.time())

            return analysis_response

        except Exception as e:
            logger.error(f"Ошибка получения анализа: {e}")
            return AnalysisResponse(
                analysis=f"Ошибка анализа: {str(e)}",
                recommendations=["Проверить подключение к OpenAI"],
                severity="low",
                confidence=0.0,
                action_items=["Повторить запрос позже"],
                summary="Анализ недоступен",
                technical_details={"error": str(e)},
                follow_up_questions=[],
            )

    async def _call_openai(self, prompt: str) -> str:
        """Вызывает OpenAI API."""
        if not self.client:
            raise Exception("OpenAI клиент не инициализирован")

        try:
            response = await self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": self.system_context},
                    {"role": "user", "content": prompt},
                ],
                max_tokens=self.max_tokens,
                temperature=self.temperature,
                timeout=30.0,
            )

            return response.choices[0].message.content

        except Exception as e:
            logger.error(f"Ошибка вызова OpenAI API: {e}")
            raise

    async def health_check(self) -> dict[str, Any]:
        """Проверка здоровья OpenAI сервиса."""
        if not self.client:
            return {
                "status": "unhealthy",
                "reason": "Клиент не инициализирован",
                "api_key_configured": bool(self.api_key),
            }

        try:
            # Простой тестовый запрос
            test_response = await self.client.chat.completions.create(
                model=self.model,
                messages=[{"role": "user", "content": "Тест"}],
                max_tokens=10,
                timeout=10.0,
            )

            return {
                "status": "healthy",
                "model": self.model,
                "cache_size": len(self.response_cache),
                "test_response_length": len(test_response.choices[0].message.content),
            }

        except Exception as e:
            return {
                "status": "unhealthy",
                "reason": str(e),
                "api_key_configured": bool(self.api_key),
            }


# Глобальный экземпляр OpenAI сервиса
openai_service = OpenAIService()
