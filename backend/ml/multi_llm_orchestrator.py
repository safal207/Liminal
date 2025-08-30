"""
Multi-LLM Orchestrator для Resonance Liminal.
Управляет OpenAI GPT-4 и Anthropic Claude для оптимального AI анализа.
"""

import asyncio
import time
from dataclasses import dataclass
from enum import Enum
from typing import Any

from loguru import logger

from .claude_service import ClaudeAnalysisResponse, claude_service
from .openai_service import AnalysisResponse as OpenAIResponse
from .openai_service import openai_service


class LLMProvider(Enum):
    OPENAI = "openai"
    CLAUDE = "claude"
    BOTH = "both"
    AUTO = "auto"


class TaskType(Enum):
    GENERAL_ANALYSIS = "general_analysis"
    SAFETY_ANALYSIS = "safety_analysis"
    ETHICS_REVIEW = "ethics_review"
    SECURITY_ANALYSIS = "security_analysis"
    LOG_ANALYSIS = "log_analysis"
    SMART_ALERTS = "smart_alerts"
    DETAILED_REASONING = "detailed_reasoning"
    CONSENSUS_DECISION = "consensus_decision"


@dataclass
class MultiLLMRequest:
    """Запрос к Multi-LLM системе."""

    task_type: TaskType
    data: dict[str, Any]
    context: dict[str, Any] | None = None
    preferred_provider: LLMProvider = LLMProvider.AUTO
    require_consensus: bool = False
    priority: str = "normal"
    max_cost: float | None = None
    timeout_seconds: int = 30


@dataclass
class MultiLLMResponse:
    """Ответ от Multi-LLM системы."""

    primary_analysis: str
    provider_used: str
    confidence: float
    recommendations: list[str]

    # Consensus data (если использовались оба провайдера)
    consensus_analysis: str | None = None
    openai_response: OpenAIResponse | None = None
    claude_response: ClaudeAnalysisResponse | None = None
    agreement_score: float | None = None

    # Metadata
    cost_estimate: float = 0.0
    processing_time: float = 0.0
    fallback_used: bool = False
    errors: list[str] = None


class MultiLLMOrchestrator:
    """
    Оркестратор для управления несколькими LLM провайдерами.
    Автоматически выбирает оптимального провайдера и обеспечивает fallback.
    """

    def __init__(self):
        self.openai_service = openai_service
        self.claude_service = claude_service

        # Конфигурация провайдеров
        self.provider_config = {
            LLMProvider.OPENAI: {
                "available": True,
                "cost_per_1k_tokens": 0.01,
                "avg_response_time": 2.0,
                "specializations": [
                    TaskType.GENERAL_ANALYSIS,
                    TaskType.LOG_ANALYSIS,
                    TaskType.SMART_ALERTS,
                ],
            },
            LLMProvider.CLAUDE: {
                "available": True,
                "cost_per_1k_tokens": 0.008,
                "avg_response_time": 3.0,
                "specializations": [
                    TaskType.SAFETY_ANALYSIS,
                    TaskType.ETHICS_REVIEW,
                    TaskType.DETAILED_REASONING,
                    TaskType.SECURITY_ANALYSIS,
                ],
            },
        }

        # Статистика использования
        self.usage_stats = {
            "openai": {"requests": 0, "errors": 0, "avg_time": 0.0},
            "claude": {"requests": 0, "errors": 0, "avg_time": 0.0},
            "consensus": {"requests": 0, "agreements": 0, "disagreements": 0},
        }

        # Fallback конфигурация
        self.fallback_enabled = True
        self.max_retries = 3

        logger.info("Multi-LLM Orchestrator инициализирован")

    async def analyze(self, request: MultiLLMRequest) -> MultiLLMResponse:
        """
        Главный метод для анализа с автоматическим выбором провайдера.

        Args:
            request: Запрос на анализ

        Returns:
            Результат анализа от оптимального провайдера
        """
        start_time = time.time()
        errors = []

        try:
            # Выбираем оптимального провайдера
            if request.preferred_provider == LLMProvider.AUTO:
                provider = self._select_optimal_provider(request)
            else:
                provider = request.preferred_provider

            logger.info(f"Выбран провайдер {provider.value} для задачи {request.task_type.value}")

            # Выполняем анализ
            if request.require_consensus or provider == LLMProvider.BOTH:
                response = await self._consensus_analysis(request)
            else:
                response = await self._single_provider_analysis(request, provider)

            # Обновляем статистику
            response.processing_time = time.time() - start_time
            self._update_usage_stats(provider.value, response.processing_time, len(errors) > 0)

            return response

        except Exception as e:
            logger.error(f"Ошибка в Multi-LLM анализе: {e}")
            errors.append(str(e))

            # Пытаемся fallback
            if self.fallback_enabled and request.preferred_provider != LLMProvider.AUTO:
                try:
                    fallback_provider = self._get_fallback_provider(request.preferred_provider)
                    logger.info(f"Используем fallback провайдер: {fallback_provider.value}")

                    response = await self._single_provider_analysis(request, fallback_provider)
                    response.fallback_used = True
                    response.errors = errors
                    response.processing_time = time.time() - start_time

                    return response

                except Exception as fallback_error:
                    errors.append(f"Fallback error: {str(fallback_error)}")

            # Если все провайдеры недоступны, возвращаем ошибку
            return MultiLLMResponse(
                primary_analysis="Ошибка: все AI провайдеры недоступны",
                provider_used="none",
                confidence=0.0,
                recommendations=[
                    "Проверить подключение к AI сервисам",
                    "Повторить запрос позже",
                ],
                processing_time=time.time() - start_time,
                errors=errors,
            )

    def _select_optimal_provider(self, request: MultiLLMRequest) -> LLMProvider:
        """Выбирает оптимального провайдера для задачи."""

        # Проверяем специализации
        for provider, config in self.provider_config.items():
            if request.task_type in config["specializations"] and config["available"]:
                return provider

        # Проверяем стоимость
        if request.max_cost:
            cheapest_provider = min(
                self.provider_config.keys(),
                key=lambda p: self.provider_config[p]["cost_per_1k_tokens"],
            )
            if self.provider_config[cheapest_provider]["available"]:
                return cheapest_provider

        # Проверяем скорость отклика
        if request.priority == "high":
            fastest_provider = min(
                self.provider_config.keys(),
                key=lambda p: self.provider_config[p]["avg_response_time"],
            )
            if self.provider_config[fastest_provider]["available"]:
                return fastest_provider

        # По умолчанию OpenAI для общих задач
        return (
            LLMProvider.OPENAI
            if self.provider_config[LLMProvider.OPENAI]["available"]
            else LLMProvider.CLAUDE
        )

    def _get_fallback_provider(self, primary_provider: LLMProvider) -> LLMProvider:
        """Возвращает fallback провайдера."""
        if primary_provider == LLMProvider.OPENAI:
            return LLMProvider.CLAUDE
        else:
            return LLMProvider.OPENAI

    async def _single_provider_analysis(
        self, request: MultiLLMRequest, provider: LLMProvider
    ) -> MultiLLMResponse:
        """Выполняет анализ с одним провайдером."""

        if provider == LLMProvider.OPENAI:
            return await self._openai_analysis(request)
        elif provider == LLMProvider.CLAUDE:
            return await self._claude_analysis(request)
        else:
            raise ValueError(f"Неподдерживаемый провайдер: {provider}")

    async def _openai_analysis(self, request: MultiLLMRequest) -> MultiLLMResponse:
        """Выполняет анализ через OpenAI."""

        if request.task_type == TaskType.LOG_ANALYSIS:
            response = await self.openai_service.analyze_logs(
                log_entries=request.data.get("log_entries", []),
                time_range=request.data.get("time_range", "unknown"),
                error_patterns=request.data.get("error_patterns"),
            )
        elif request.task_type == TaskType.SMART_ALERTS:
            alert_text = await self.openai_service.generate_smart_alert(
                alert_data=request.data.get("alert_data", {}),
                context=request.context or {},
                recipient_role=request.data.get("recipient_role", "devops"),
            )
            response = OpenAIResponse(
                analysis=alert_text,
                recommendations=["Проверить детали алерта"],
                severity="medium",
                confidence=0.8,
                action_items=["Отправить уведомление"],
                summary="Smart alert сгенерирован",
                technical_details={},
                follow_up_questions=[],
            )
        else:
            # Общий анализ
            response = await self.openai_service.analyze_anomaly(
                anomaly_data=request.data, context=request.context
            )

        return MultiLLMResponse(
            primary_analysis=response.analysis,
            provider_used="openai",
            confidence=response.confidence,
            recommendations=response.recommendations,
            openai_response=response,
            cost_estimate=self._estimate_cost("openai", response.analysis),
        )

    async def _claude_analysis(self, request: MultiLLMRequest) -> MultiLLMResponse:
        """Выполняет анализ через Claude."""

        if request.task_type == TaskType.SAFETY_ANALYSIS:
            response = await self.claude_service.analyze_ml_safety(
                ml_decision=request.data, context=request.context
            )
        elif request.task_type == TaskType.ETHICS_REVIEW:
            response = await self.claude_service.analyze_ethical_implications(
                automated_action=request.data.get("action", {}),
                affected_users=request.data.get("affected_users", []),
                context=request.context,
            )
        elif request.task_type == TaskType.SECURITY_ANALYSIS:
            response = await self.claude_service.security_threat_analysis(
                threat_indicators=request.data.get("threats", {}),
                system_state=request.data.get("system", {}),
                historical_data=request.data.get("history", []),
            )
        elif request.task_type == TaskType.DETAILED_REASONING:
            response = await self.claude_service.detailed_reasoning_analysis(
                complex_problem=request.data.get("problem", {}),
                available_data=request.data.get("data", {}),
                constraints=request.data.get("constraints", []),
            )
        else:
            # Общий анализ безопасности
            response = await self.claude_service.analyze_ml_safety(
                ml_decision=request.data, context=request.context
            )

        return MultiLLMResponse(
            primary_analysis=response.analysis,
            provider_used="claude",
            confidence=response.confidence,
            recommendations=response.recommendations,
            claude_response=response,
            cost_estimate=self._estimate_cost("claude", response.analysis),
        )

    async def _consensus_analysis(self, request: MultiLLMRequest) -> MultiLLMResponse:
        """Выполняет консенсус-анализ с обоими провайдерами."""

        try:
            # Параллельно запрашиваем оба провайдера
            openai_task = self._openai_analysis(request)
            claude_task = self._claude_analysis(request)

            openai_response, claude_response = await asyncio.gather(
                openai_task, claude_task, return_exceptions=True
            )

            # Обрабатываем исключения
            if isinstance(openai_response, Exception):
                logger.error(f"OpenAI ошибка в консенсус-анализе: {openai_response}")
                openai_response = None

            if isinstance(claude_response, Exception):
                logger.error(f"Claude ошибка в консенсус-анализе: {claude_response}")
                claude_response = None

            # Если оба провайдера недоступны
            if not openai_response and not claude_response:
                raise Exception("Оба AI провайдера недоступны")

            # Если доступен только один провайдер
            if not openai_response:
                return claude_response
            if not claude_response:
                return openai_response

            # Выполняем консенсус-анализ
            consensus_analysis = await self._generate_consensus(
                openai_response, claude_response, request
            )

            # Вычисляем agreement score
            agreement_score = self._calculate_agreement_score(openai_response, claude_response)

            # Обновляем статистику консенсуса
            self.usage_stats["consensus"]["requests"] += 1
            if agreement_score > 0.7:
                self.usage_stats["consensus"]["agreements"] += 1
            else:
                self.usage_stats["consensus"]["disagreements"] += 1

            return MultiLLMResponse(
                primary_analysis=consensus_analysis,
                provider_used="consensus",
                confidence=(openai_response.confidence + claude_response.confidence) / 2,
                recommendations=self._merge_recommendations(
                    openai_response.recommendations, claude_response.recommendations
                ),
                consensus_analysis=consensus_analysis,
                openai_response=openai_response.openai_response,
                claude_response=claude_response.claude_response,
                agreement_score=agreement_score,
                cost_estimate=openai_response.cost_estimate + claude_response.cost_estimate,
            )

        except Exception as e:
            logger.error(f"Ошибка консенсус-анализа: {e}")
            raise

    async def _generate_consensus(
        self,
        openai_response: MultiLLMResponse,
        claude_response: MultiLLMResponse,
        request: MultiLLMRequest,
    ) -> str:
        """Генерирует консенсус-анализ между OpenAI и Claude."""

        # Используем Claude для консенсус-анализа (его специализация)
        consensus_response = await self.claude_service.consensus_analysis_with_openai(
            openai_analysis=openai_response.primary_analysis,
            problem_data=request.data,
            context=request.context or {},
        )

        return consensus_response.analysis

    def _calculate_agreement_score(
        self, openai_response: MultiLLMResponse, claude_response: MultiLLMResponse
    ) -> float:
        """Вычисляет степень согласия между провайдерами."""

        # Простая эвристика на основе ключевых слов
        openai_words = set(openai_response.primary_analysis.lower().split())
        claude_words = set(claude_response.primary_analysis.lower().split())

        if not openai_words or not claude_words:
            return 0.0

        intersection = openai_words.intersection(claude_words)
        union = openai_words.union(claude_words)

        jaccard_similarity = len(intersection) / len(union) if union else 0.0

        # Учитываем similarity в рекомендациях
        openai_rec_words = set(" ".join(openai_response.recommendations).lower().split())
        claude_rec_words = set(" ".join(claude_response.recommendations).lower().split())

        rec_intersection = openai_rec_words.intersection(claude_rec_words)
        rec_union = openai_rec_words.union(claude_rec_words)

        rec_similarity = len(rec_intersection) / len(rec_union) if rec_union else 0.0

        # Взвешенная оценка
        return jaccard_similarity * 0.7 + rec_similarity * 0.3

    def _merge_recommendations(self, openai_recs: list[str], claude_recs: list[str]) -> list[str]:
        """Объединяет рекомендации от разных провайдеров."""

        merged = []

        # Добавляем уникальные рекомендации
        all_recs = openai_recs + claude_recs
        seen = set()

        for rec in all_recs:
            rec_lower = rec.lower().strip()
            if rec_lower not in seen:
                merged.append(rec)
                seen.add(rec_lower)

        return merged[:10]  # Ограничиваем количество

    def _estimate_cost(self, provider: str, text: str) -> float:
        """Оценивает стоимость запроса."""

        # Простая оценка на основе длины текста
        estimated_tokens = len(text.split()) * 1.3  # Примерное соотношение слова/токены

        if provider == "openai":
            return (estimated_tokens / 1000) * 0.01
        elif provider == "claude":
            return (estimated_tokens / 1000) * 0.008

        return 0.0

    def _update_usage_stats(self, provider: str, processing_time: float, had_error: bool):
        """Обновляет статистику использования."""

        if provider in self.usage_stats:
            stats = self.usage_stats[provider]
            stats["requests"] += 1

            if had_error:
                stats["errors"] += 1

            # Обновляем среднее время
            current_avg = stats["avg_time"]
            new_avg = (current_avg * (stats["requests"] - 1) + processing_time) / stats["requests"]
            stats["avg_time"] = new_avg

    async def health_check(self) -> dict[str, Any]:
        """Проверка здоровья Multi-LLM системы."""

        openai_health = await self.openai_service.health_check()
        claude_health = await self.claude_service.health_check()

        return {
            "status": (
                "healthy"
                if (
                    openai_health.get("status") == "healthy"
                    or claude_health.get("status") == "healthy"
                )
                else "unhealthy"
            ),
            "providers": {"openai": openai_health, "claude": claude_health},
            "usage_stats": self.usage_stats,
            "provider_config": {
                provider.value: config for provider, config in self.provider_config.items()
            },
            "features": {
                "consensus_analysis": True,
                "automatic_fallback": self.fallback_enabled,
                "cost_optimization": True,
                "provider_selection": True,
            },
        }

    def get_usage_report(self) -> dict[str, Any]:
        """Возвращает отчет об использовании провайдеров."""

        total_requests = sum(stats["requests"] for stats in self.usage_stats.values())

        return {
            "total_requests": total_requests,
            "provider_breakdown": {
                provider: {
                    "requests": stats["requests"],
                    "error_rate": stats["errors"] / max(stats["requests"], 1),
                    "avg_response_time": stats["avg_time"],
                    "usage_percentage": (stats["requests"] / max(total_requests, 1)) * 100,
                }
                for provider, stats in self.usage_stats.items()
            },
            "consensus_stats": {
                "total_consensus_requests": self.usage_stats["consensus"]["requests"],
                "agreement_rate": (
                    self.usage_stats["consensus"]["agreements"]
                    / max(self.usage_stats["consensus"]["requests"], 1)
                )
                * 100,
            },
        }


# Глобальный экземпляр Multi-LLM Orchestrator
multi_llm_orchestrator = MultiLLMOrchestrator()
