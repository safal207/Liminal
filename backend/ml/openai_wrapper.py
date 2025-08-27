#!/usr/bin/env python
"""
OpenAI API Wrapper для Resonance Liminal ML Backend

Универсальный адаптер, который может работать как с реальным API OpenAI,
так и с локальной мок-реализацией для разработки и тестирования.
Поддерживает кэширование, настройку прокси, расширенную диагностику и интеграцию с Prometheus.

Расширенные возможности:
- Гибкое управление мок-режимом (env vars + runtime)
- Детальная диагностика ошибок подключения
- Статус адаптера и проверка здоровья
- Подробная информация о конфигурации
"""
import hashlib
import json
import os
import time
import traceback
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional

import httpx

# Логирование
from loguru import logger

# Интеллектуальный логгер
from .adapter_logger import log_error, log_hypothesis, log_insight

# Конфигурация по умолчанию
DEFAULT_CACHE_TTL = 3600  # 1 час по умолчанию
DEFAULT_CONNECTION_TIMEOUT = 10  # 10 секунд
DEFAULT_MOCK_DATA_DIR = str(Path(__file__).parent / "mock_data")
DEFAULT_FALLBACK_TO_LOCAL = True
DEFAULT_MOCK_ONLY = False
DEFAULT_DEBUG_LEVEL = 0  # 0 = minimal, 1 = normal, 2 = verbose

# Загрузка конфигурации из переменных окружения
CACHE_TTL = int(os.getenv("OPENAI_CACHE_TTL", str(DEFAULT_CACHE_TTL)))
MOCK_DATA_DIR = os.getenv("OPENAI_MOCK_DIR", DEFAULT_MOCK_DATA_DIR)
CONNECTION_TIMEOUT = int(
    os.getenv("OPENAI_CONNECTION_TIMEOUT", str(DEFAULT_CONNECTION_TIMEOUT))
)
FALLBACK_TO_LOCAL = (
    os.getenv("OPENAI_FALLBACK_TO_LOCAL", str(DEFAULT_FALLBACK_TO_LOCAL).lower())
    == "true"
)
MOCK_ONLY = os.getenv("OPENAI_MOCK_ONLY", str(DEFAULT_MOCK_ONLY).lower()) == "true"
DEBUG_LEVEL = int(os.getenv("OPENAI_DEBUG_LEVEL", str(DEFAULT_DEBUG_LEVEL)))

# Создаем директорию для мок-данных, если она не существует
if not os.path.exists(MOCK_DATA_DIR):
    try:
        os.makedirs(MOCK_DATA_DIR)
        logger.info(f"Создана директория для мок-данных: {MOCK_DATA_DIR}")
    except Exception as e:
        logger.warning(f"Не удалось создать директорию для мок-данных: {e}")


# Определение структуры данных для запросов и ответов
@dataclass
class LLMRequest:
    """Запрос к языковой модели с параметрами"""

    model: str
    messages: List[Dict[str, str]]
    max_tokens: int = 800
    temperature: float = 0.3
    response_format: Dict[str, str] = field(
        default_factory=lambda: {"type": "json_object"}
    )

    def get_cache_key(self) -> str:
        """Создает хеш-ключ для кеширования запроса"""
        # Преобразуем запрос в JSON и создаем MD5 хеш
        request_str = json.dumps(
            {
                "model": self.model,
                "messages": self.messages,
                "max_tokens": self.max_tokens,
                "temperature": self.temperature,
            },
            sort_keys=True,
        )
        return hashlib.md5(request_str.encode()).hexdigest()


@dataclass
class LLMResponse:
    """Ответ языковой модели с метаданными"""

    content: str
    model: str
    usage: Dict[str, int] = None
    finish_reason: str = "stop"
    created_at: float = field(default_factory=time.time)
    is_mock: bool = False
    cached: bool = False

    def as_dict(self) -> Dict[str, Any]:
        """Преобразует ответ в словарь"""
        return {
            "content": self.content,
            "model": self.model,
            "usage": self.usage or {},
            "finish_reason": self.finish_reason,
            "created_at": self.created_at,
            "is_mock": self.is_mock,
            "cached": self.cached,
        }


class OpenAIWrapper:
    """
    Универсальный адаптер для взаимодействия с OpenAI API или локальной мок-реализацией
    с расширенными возможностями диагностики и управления режимом работы
    """

    def __init__(self, force_mock_mode=None, debug_level=None, cache_ttl=None):
        # Конфигурация
        self.mock_only = MOCK_ONLY if force_mock_mode is None else force_mock_mode
        self.debug_level = (
            int(os.getenv("OPENAI_DEBUG_LEVEL", "0"))
            if debug_level is None
            else debug_level
        )
        self.cache_ttl = CACHE_TTL if cache_ttl is None else cache_ttl
        self.fallback_to_local = FALLBACK_TO_LOCAL

        # Клиенты API
        self.openai_client = None
        self.anthropic_client = None
        self.response_cache = {}

        # API ключи
        self.api_key = None
        self.anthropic_key = None
        self.xai_key = None

        # Статусы инициализации
        self.initialization_status = {
            "openai": {"initialized": False, "error": None, "time": None},
            "anthropic": {"initialized": False, "error": None, "time": None},
            "xai": {"initialized": False, "error": None, "time": None},
            "mock": {"initialized": True, "error": None, "time": time.time()},
        }

        # Загружаем API ключи
        self._load_api_keys()

    def _load_api_keys(self) -> None:
        """Загружает API ключи из переменных окружения"""
        # OpenAI
        self.api_key = os.getenv("OPENAI_API_KEY", "").strip()
        if self.api_key:
            logger.debug(
                f"OpenAI API ключ загружен: {self.api_key[:5]}...{self.api_key[-5:] if len(self.api_key) > 10 else ''}"
            )
        else:
            logger.warning("OpenAI API ключ не найден")

        # Anthropic
        self.anthropic_key = os.getenv("ANTHROPIC_API_KEY", "").strip()
        if self.anthropic_key:
            logger.debug(
                f"Anthropic API ключ загружен: {self.anthropic_key[:5]}...{self.anthropic_key[-5:] if len(self.anthropic_key) > 10 else ''}"
            )

        # XAI API
        self.xai_key = os.getenv("XAI_API_KEY", "").strip()
        if self.xai_key:
            logger.debug(
                f"XAI API ключ загружен: {self.xai_key[:5]}...{self.xai_key[-5:] if len(self.xai_key) > 10 else ''}"
            )

    async def initialize(self) -> bool:
        """Инициализирует клиент OpenAI с расширенной диагностикой"""
        start_time = time.time()

        # Если включен режим принудительного мока, пропускаем инициализацию API
        if self.mock_only:
            if self.debug_level > 0:
                logger.info(
                    f"Режим mock_only включен, пропускаем инициализацию реального API"
                )
            self.initialization_status["openai"] = {
                "initialized": False,
                "error": "mock_only mode enabled",
                "time": time.time(),
            }
            return True

        # Пытаемся импортировать OpenAI Client
        try:
            if self.debug_level > 1:
                logger.debug(f"Попытка импорта библиотеки OpenAI...")

            from openai import AsyncOpenAI

            # Создаем клиент с указанным API ключом
            if self.api_key:
                # Добавляем расширенное логирование при высоком уровне отладки
                if self.debug_level > 1:
                    logger.debug(
                        f"Создание клиента OpenAI с API ключом {self.api_key[:5]}...{self.api_key[-5:] if len(self.api_key) > 10 else ''}"
                    )

                self.openai_client = AsyncOpenAI(
                    api_key=self.api_key, timeout=httpx.Timeout(CONNECTION_TIMEOUT)
                )

                self.initialization_status["openai"] = {
                    "initialized": True,
                    "error": None,
                    "time": time.time(),
                }

                if self.debug_level > 0:
                    logger.info(
                        f"OpenAI клиент успешно инициализирован за {time.time() - start_time:.3f} сек"
                    )
                return True
            else:
                error_msg = "OpenAI API ключ не найден или пустой"
                logger.warning(error_msg)
                self.initialization_status["openai"] = {
                    "initialized": False,
                    "error": error_msg,
                    "time": time.time(),
                }

                # Записываем в лог опыта
                context = {"initialization_time": time.time() - start_time}
                error_hash = log_error(error_msg, context)

        except ImportError as e:
            error_msg = f"Не удалось импортировать библиотеку OpenAI: {e}"
            logger.warning(error_msg)
            self.initialization_status["openai"] = {
                "initialized": False,
                "error": error_msg,
                "time": time.time(),
            }

            # Записываем в лог опыта
            context = {
                "error_type": "ImportError",
                "initialization_time": time.time() - start_time,
            }
            error_hash = log_error(error_msg, context)

            # Предлагаем решение - инсайт
            solution = "Установите библиотеку httpx для HTTP запросов вместо официального SDK: pip install httpx"
            log_insight(error_hash, solution, {"solution_type": "library_replacement"})

        except Exception as e:
            error_msg = f"Ошибка инициализации OpenAI клиента: {e}"
            if self.debug_level > 0:
                error_msg = (
                    f"Ошибка инициализации OpenAI клиента: {traceback.format_exc()}"
                )
            logger.error(error_msg)
            self.initialization_status["openai"] = {
                "initialized": False,
                "error": str(e),
                "time": time.time(),
            }

            # Записываем в лог опыта
            context = {
                "error_type": type(e).__name__,
                "initialization_time": time.time() - start_time,
            }
            error_hash = log_error(error_msg, context)

        # Если не удалось инициализировать клиент, проверяем настройки мока
        if self.fallback_to_local:
            if self.debug_level > 0:
                logger.info(
                    f"Включен режим fallback_to_local, используем мок-реализацию"
                )
            return True
        else:
            error_msg = (
                "Не удалось инициализировать OpenAI клиент и fallback_to_local выключен"
            )
            logger.error(error_msg)

            # Записываем гипотезу для решения проблемы
            hypothesis = "Возможно, проблема в сетевом подключении или блокировке API. Или API ключ неверный/истек."
            experiment = "Проверьте доступность api.openai.com, валидность ключа и настройте fallback_to_local=True"
            log_hypothesis(error_msg, hypothesis, experiment)

            return False

    def _get_cached_response(self, request: LLMRequest) -> Optional[LLMResponse]:
        """Получает ответ из кеша"""
        cache_key = request.get_cache_key()

        if cache_key in self.response_cache:
            cached_response = self.response_cache[cache_key]
            # Проверяем, не истек ли срок кеша
            if time.time() - cached_response.created_at < self.cache_ttl:
                logger.info(f"Найден кешированный ответ для запроса {cache_key[:8]}")
                cached_response.cached = True
                return cached_response
            else:
                # Удаляем устаревший ответ из кеша
                del self.response_cache[cache_key]

        return None

    def _cache_response(self, request: LLMRequest, response: LLMResponse) -> None:
        """Сохраняет ответ в кеш"""
        cache_key = request.get_cache_key()
        self.response_cache[cache_key] = response
        logger.debug(f"Ответ сохранен в кеш с ключом {cache_key[:8]}")

    def _save_mock_response(self, request: LLMRequest, response: LLMResponse) -> None:
        """Сохраняет мок-ответ на диск для последующего использования"""
        try:
            cache_key = request.get_cache_key()
            file_path = os.path.join(MOCK_DATA_DIR, f"{cache_key}.json")

            # Сохраняем данные в JSON файл
            with open(file_path, "w", encoding="utf-8") as f:
                json.dump(
                    {
                        "request": {
                            "model": request.model,
                            "messages": request.messages,
                            "max_tokens": request.max_tokens,
                            "temperature": request.temperature,
                        },
                        "response": response.as_dict(),
                    },
                    f,
                    ensure_ascii=False,
                    indent=2,
                )

            logger.debug(f"Мок-ответ сохранен в файл: {file_path}")
        except Exception as e:
            logger.error(f"Ошибка при сохранении мок-ответа: {e}")

    def _load_mock_response(self, request: LLMRequest) -> Optional[LLMResponse]:
        """Загружает мок-ответ с диска"""
        try:
            cache_key = request.get_cache_key()
            file_path = os.path.join(MOCK_DATA_DIR, f"{cache_key}.json")

            if os.path.exists(file_path):
                with open(file_path, "r", encoding="utf-8") as f:
                    data = json.load(f)

                response_data = data.get("response", {})
                response = LLMResponse(
                    content=response_data.get("content", ""),
                    model=response_data.get("model", request.model),
                    usage=response_data.get("usage"),
                    finish_reason=response_data.get("finish_reason", "stop"),
                    created_at=response_data.get("created_at", time.time()),
                    is_mock=True,
                    cached=True,
                )

                logger.info(f"Загружен сохраненный мок-ответ из файла: {file_path}")
                return response

        except Exception as e:
            logger.error(f"Ошибка при загрузке мок-ответа: {e}")

        return None

    def _generate_mock_response(self, request: LLMRequest) -> LLMResponse:
        """Генерирует мок-ответ на основе типа запроса"""
        # Получаем текст последнего сообщения
        last_message = request.messages[-1]["content"] if request.messages else ""

        # Определяем тип ответа на основе запроса
        is_anomaly = (
            "anomaly" in last_message.lower() or "аномалия" in last_message.lower()
        )
        is_fraud = (
            "fraud" in last_message.lower() or "мошенничество" in last_message.lower()
        )
        is_performance = (
            "performance" in last_message.lower()
            or "производительность" in last_message.lower()
        )

        # Генерируем ответ в зависимости от типа
        if is_anomaly:
            mock_response = self._generate_anomaly_explanation()
        elif is_fraud:
            mock_response = self._generate_fraud_explanation()
        elif is_performance:
            mock_response = self._generate_performance_explanation()
        else:
            mock_response = self._generate_generic_explanation(last_message)

        # Создаем объект ответа
        response = LLMResponse(
            content=json.dumps(mock_response, ensure_ascii=False),
            model=f"mock-{request.model}",
            usage={
                "prompt_tokens": sum(
                    len(m.get("content", "")) for m in request.messages
                )
                // 4,
                "completion_tokens": len(json.dumps(mock_response)) // 4,
                "total_tokens": (
                    sum(len(m.get("content", "")) for m in request.messages) // 4
                )
                + (len(json.dumps(mock_response)) // 4),
            },
            finish_reason="stop",
            created_at=time.time(),
            is_mock=True,
        )

        # Сохраняем мок-ответ для будущего использования
        self._save_mock_response(request, response)

        return response

    def _generate_anomaly_explanation(self) -> Dict[str, Any]:
        """Генерирует объяснение для аномалий"""
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
        }

    def _generate_fraud_explanation(self) -> Dict[str, Any]:
        """Генерирует объяснение для случаев мошенничества"""
        return {
            "analysis": "Анализ транзакций выявил подозрительные паттерны, характерные для мошенничества: множественные небольшие транзакции с постепенным увеличением сумм, транзакции в нетипичное время и с нетипичных устройств.",
            "recommendations": [
                "Временно ограничить доступ к финансовым операциям",
                "Запросить дополнительную верификацию для подтверждения личности",
                "Пересмотреть правила авторизации для данного типа транзакций",
            ],
            "severity": "critical",
            "confidence": 0.92,
            "action_items": [
                "Заморозить аккаунт до выяснения обстоятельств",
                "Отправить уведомление службе безопасности",
                "Инициировать процедуру возврата средств",
            ],
            "summary": "Обнаружены признаки мошенничества в серии транзакций с нетипичными паттернами использования",
            "technical_details": {
                "fraud_score_factors": {
                    "transaction_frequency": 0.40,
                    "unusual_time": 0.25,
                    "device_mismatch": 0.20,
                    "amount_pattern": 0.15,
                },
                "detection_method": "xgboost",
                "threshold": 0.75,
                "timestamp": time.time(),
            },
        }

    def _generate_performance_explanation(self) -> Dict[str, Any]:
        """Генерирует объяснение для проблем производительности"""
        return {
            "analysis": "Наблюдается снижение производительности системы, вызванное высокой нагрузкой на базу данных и недостаточным кэшированием частых запросов. Количество соединений к базе данных превысило оптимальный порог на 40%.",
            "recommendations": [
                "Увеличить уровень кэширования для часто запрашиваемых данных",
                "Оптимизировать индексы базы данных для ускорения запросов",
                "Внедрить connection pooling для эффективного управления соединениями",
            ],
            "severity": "medium",
            "confidence": 0.78,
            "action_items": [
                "Увеличить размер кэша Redis на 50%",
                "Перестроить индексы базы данных",
                "Настроить автоматическое масштабирование для периодов пиковой нагрузки",
            ],
            "summary": "Снижение производительности системы вызвано высокой нагрузкой на базу данных и неэффективным кэшированием",
            "technical_details": {
                "performance_factors": {
                    "database_connections": 0.45,
                    "cache_hit_rate": 0.30,
                    "query_execution_time": 0.15,
                    "network_latency": 0.10,
                },
                "detection_method": "time_series_analysis",
                "threshold": 0.65,
                "timestamp": time.time(),
            },
        }

    def _generate_generic_explanation(self, query: str) -> Dict[str, Any]:
        """Генерирует общее объяснение на основе запроса"""
        return {
            "analysis": f"Анализ запроса: {query[:100]}... Обнаружены стандартные паттерны поведения пользователя без значительных отклонений от нормы.",
            "recommendations": [
                "Продолжить мониторинг активности",
                "Обновить базовые показатели для данного типа поведения",
                "Собрать дополнительные данные для улучшения точности модели",
            ],
            "severity": "low",
            "confidence": 0.75,
            "action_items": [
                "Обновить профиль пользователя",
                "Пересмотреть пороговые значения для триггеров оповещений",
                "Добавить дополнительные метрики мониторинга",
            ],
            "summary": "Активность пользователя находится в пределах нормы, существенных отклонений не выявлено",
            "technical_details": {
                "model_confidence": 0.75,
                "baseline_deviation": 0.15,
                "detection_method": "statistical_analysis",
                "features_analyzed": [
                    "user_activity",
                    "session_length",
                    "error_rate",
                    "request_pattern",
                ],
                "timestamp": time.time(),
            },
        }

    async def _call_real_openai(self, request: LLMRequest) -> LLMResponse:
        """Выполняет вызов реального OpenAI API с расширенной диагностикой"""
        start_time = time.time()

        if not self.openai_client:
            error_msg = "OpenAI клиент не инициализирован"
            if self.debug_level > 0:
                logger.error(f"Ошибка вызова API: {error_msg}")

            # Записываем в лог опыта
            error_hash = log_error(error_msg, {"request_model": request.model})

            # Предлагаем решение - инсайт
            solution = "Проверьте, что метод initialize() был вызван и вернул True перед вызовом API"
            log_insight(error_hash, solution, {"solution_type": "initialization_check"})

            raise ValueError(error_msg)

        try:
            if self.debug_level > 1:
                logger.debug(
                    f"Вызов OpenAI API: модель={request.model}, токенов={request.max_tokens}, temp={request.temperature}"
                )

            # Преобразуем наш запрос в формат для API
            response = await self.openai_client.chat.completions.create(
                model=request.model,
                messages=request.messages,
                max_tokens=request.max_tokens,
                temperature=request.temperature,
                response_format=request.response_format,
            )

            # Извлекаем данные из ответа
            result = response.choices[0].message.content

            usage = {
                "prompt_tokens": response.usage.prompt_tokens,
                "completion_tokens": response.usage.completion_tokens,
                "total_tokens": response.usage.total_tokens,
            }

            elapsed_time = time.time() - start_time
            if self.debug_level > 0:
                logger.info(
                    f"OpenAI API ответил за {elapsed_time:.3f} сек, токенов: {usage['total_tokens']}"
                )

            return LLMResponse(
                content=result,
                model=request.model,
                usage=usage,
                finish_reason=response.choices[0].finish_reason,
                is_mock=False,
                cached=False,
            )

        except Exception as e:
            elapsed_time = time.time() - start_time
            error_msg = f"Ошибка вызова OpenAI API за {elapsed_time:.3f} сек: {e}"
            if self.debug_level > 0:
                logger.error(error_msg)
                if self.debug_level > 1:
                    logger.error(f"Подробности: {traceback.format_exc()}")

            # Записываем в лог опыта
            context = {
                "request_model": request.model,
                "response_time": elapsed_time,
                "error_type": type(e).__name__,
            }
            error_hash = log_error(error_msg, context)

            # Предлагаем решение в зависимости от типа ошибки
            if "Rate limit" in str(e) or "rate_limit" in str(e).lower():
                solution = "Уменьшите частоту запросов или увеличьте задержку между ними. Используйте кэширование."
                log_insight(error_hash, solution, {"solution_type": "rate_limiting"})
            elif "Invalid authentication" in str(e) or "key" in str(e).lower():
                solution = "Проверьте правильность API ключа и его активацию в аккаунте OpenAI."
                log_insight(error_hash, solution, {"solution_type": "authentication"})
            elif "timeout" in str(e).lower():
                solution = "Увеличьте параметр CONNECTION_TIMEOUT или проверьте сетевое соединение."
                log_insight(error_hash, solution, {"solution_type": "network_timeout"})

            raise

    async def call(self, request: LLMRequest) -> LLMResponse:
        """
        Универсальный метод для вызова языковой модели (реальной или мок)
        с расширенной диагностикой и интеллектуальным логированием
        """
        start_time = time.time()

        # Проверяем кеш
        if self.debug_level > 1:
            logger.debug(f"Проверяем кеш для запроса {request.get_cache_key()[:8]}")

        cached_response = self._get_cached_response(request)
        if cached_response:
            if self.debug_level > 0:
                logger.info(f"Запрос {request.get_cache_key()[:8]} найден в кеше")
            return cached_response

        # Логируем информацию о запросе при высоком уровне отладки
        if self.debug_level > 1:
            messages_info = [
                f"{m['role']}:{len(m['content'])} chars" for m in request.messages
            ]
            logger.debug(f"Запрос: модель={request.model}, сообщения={messages_info}")

        # Проверяем режим мока
        if self.mock_only or not self.openai_client:
            reason = "mock_only=True" if self.mock_only else "openai_client=None"
            if self.debug_level > 0:
                logger.info(
                    f"Использование мок-реализации для модели {request.model} (причина: {reason})"
                )

            # Проверяем наличие сохраненного мок-ответа
            saved_mock = self._load_mock_response(request)
            if saved_mock:
                if self.debug_level > 1:
                    logger.debug(
                        f"Загружен существующий мок-ответ для {request.get_cache_key()[:8]}"
                    )
                self._cache_response(request, saved_mock)
                return saved_mock

            # Генерируем новый мок-ответ
            if self.debug_level > 1:
                logger.debug(
                    f"Генерация нового мок-ответа для {request.get_cache_key()[:8]}"
                )

            mock_response = self._generate_mock_response(request)
            self._cache_response(request, mock_response)

            elapsed_time = time.time() - start_time
            if self.debug_level > 0:
                logger.info(f"Мок-ответ сгенерирован за {elapsed_time:.3f} сек")

            return mock_response

        # Пытаемся выполнить реальный вызов API
        try:
            if self.debug_level > 0:
                logger.info(
                    f"Выполняем реальный вызов OpenAI API для модели {request.model}"
                )

            response = await self._call_real_openai(request)
            self._cache_response(request, response)

            elapsed_time = time.time() - start_time
            if self.debug_level > 0:
                logger.info(f"Запрос к OpenAI API выполнен за {elapsed_time:.3f} сек")

            return response

        except Exception as e:
            elapsed_time = time.time() - start_time
            error_msg = f"Ошибка при вызове API за {elapsed_time:.3f} сек: {e}"

            logger.error(error_msg)
            if self.debug_level > 1:
                logger.error(f"Подробности: {traceback.format_exc()}")

            # Записываем в лог опыта
            context = {
                "request_model": request.model,
                "request_hash": request.get_cache_key(),
                "response_time": elapsed_time,
            }
            error_hash = log_error(error_msg, context)

            if self.fallback_to_local:
                logger.warning("Переключение на мок-реализацию из-за исключения")
                mock_response = self._generate_mock_response(request)
                self._cache_response(request, mock_response)

                # Записываем инсайт о решении проблемы через fallback
                solution = "Автоматически выполнен fallback на мок-реализацию для обеспечения работоспособности"
                log_insight(error_hash, solution, {"solution_type": "auto_fallback"})

                return mock_response
            else:
                if self.debug_level > 0:
                    logger.error(f"fallback_to_local отключен, пробрасываем исключение")

                # Записываем гипотезу для решения проблемы
                hypothesis = "Возможно, стоит включить fallback_to_local для автоматического перехода на мок-режим при ошибках"
                experiment = "Установите fallback_to_local=True для обеспечения отказоустойчивости"
                log_hypothesis(error_msg, hypothesis, experiment)

                raise e


# Создаем глобальный экземпляр клиента
llm_client = OpenAIWrapper()


async def initialize_llm_client(force_mock_mode=None, debug_level=None) -> bool:
    """Инициализирует глобальный LLM клиент с опциональными параметрами"""
    # Обновляем параметры если они указаны
    if force_mock_mode is not None:
        llm_client.mock_only = force_mock_mode

    if debug_level is not None:
        llm_client.debug_level = debug_level

    result = await llm_client.initialize()

    if result:
        # Записываем успешную инициализацию в инсайты
        if force_mock_mode:
            log_insight(
                "mock_mode_enabled",
                "Адаптер успешно инициализирован в мок-режиме",
                {"mock_only": force_mock_mode, "debug_level": llm_client.debug_level},
            )
        else:
            log_insight(
                "adapter_initialized",
                "Адаптер успешно инициализирован",
                {
                    "mock_only": llm_client.mock_only,
                    "debug_level": llm_client.debug_level,
                },
            )

    return result


async def get_adapter_status() -> dict:
    """Возвращает текущий статус адаптера и его конфигурацию"""
    return {
        "initialization": llm_client.initialization_status,
        "configuration": {
            "mock_only": llm_client.mock_only,
            "fallback_to_local": llm_client.fallback_to_local,
            "debug_level": llm_client.debug_level,
            "cache_ttl": llm_client.cache_ttl,
            "cache_size": len(llm_client.response_cache),
            "api_keys": {
                "openai": bool(llm_client.api_key),
                "anthropic": bool(llm_client.anthropic_key),
                "xai": bool(llm_client.xai_key),
            },
        },
    }


# Интерфейс совместимости с OpenAI AsyncClient для облегчения миграции кода
class AsyncOpenAI:
    """
    Мок-класс для совместимости с интерфейсом OpenAI AsyncClient
    """

    def __init__(self, api_key=None):
        self.api_key = api_key
        self.chat = self._ChatCompletions()

    class _ChatCompletions:
        class completions:
            @staticmethod
            async def create(
                model=None,
                messages=None,
                max_tokens=None,
                temperature=None,
                response_format=None,
            ):
                request = LLMRequest(
                    model=model,
                    messages=messages,
                    max_tokens=max_tokens or 800,
                    temperature=temperature or 0.3,
                    response_format=response_format or {"type": "json_object"},
                )

                response = await llm_client.call(request)

                # Создаем объект, совместимый с форматом возврата OpenAI
                class MockChoice:
                    class Message:
                        def __init__(self, content):
                            self.content = content

                    def __init__(self, content, finish_reason):
                        self.message = self.Message(content)
                        self.finish_reason = finish_reason

                class MockUsage:
                    def __init__(self, usage_dict):
                        self.prompt_tokens = usage_dict.get("prompt_tokens", 0)
                        self.completion_tokens = usage_dict.get("completion_tokens", 0)
                        self.total_tokens = usage_dict.get("total_tokens", 0)

                class MockResponse:
                    def __init__(self, content, finish_reason, usage):
                        self.choices = [MockChoice(content, finish_reason)]
                        self.usage = MockUsage(usage or {})

                return MockResponse(
                    response.content, response.finish_reason, response.usage
                )
