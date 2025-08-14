"""
Anthropic Claude Integration Service для Resonance Liminal.
Интеграция Claude для безопасного AI анализа, этических решений и детального reasoning.
"""

import asyncio
import json
import os
import time
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Union

from loguru import logger

# Anthropic Claude API
try:
    import anthropic
    from anthropic import AsyncAnthropic

    ANTHROPIC_AVAILABLE = True
except ImportError:
    logger.warning("Anthropic library не установлена. Используются заглушки.")
    ANTHROPIC_AVAILABLE = False


@dataclass
class ClaudeAnalysisRequest:
    """Запрос на анализ для Claude."""

    type: str  # "safety", "ethics", "detailed_reasoning", "security", "consensus"
    data: Dict[str, Any]
    context: Optional[Dict[str, Any]] = None
    priority: str = "normal"  # "low", "normal", "high", "critical"
    model: str = (
        "claude-3-sonnet-20240229"  # claude-3-opus, claude-3-sonnet, claude-3-haiku
    )
    timestamp: Optional[str] = None


@dataclass
class ClaudeAnalysisResponse:
    """Ответ от Claude анализа."""

    analysis: str
    reasoning: List[str]  # Пошаговое рассуждение
    safety_assessment: str  # "safe", "caution", "unsafe"
    ethical_considerations: List[str]
    recommendations: List[str]
    confidence: float
    model_used: str
    constitutional_ai_notes: List[str]  # Constitutional AI insights
    harm_assessment: Dict[str, Any]
    follow_up_questions: List[str]


class ClaudeService:
    """
    Сервис для интеграции с Anthropic Claude API.
    Специализируется на безопасном AI анализе, этических решениях и детальном reasoning.
    """

    def __init__(self, api_key: Optional[str] = None):
        self.api_key = api_key or os.getenv("ANTHROPIC_API_KEY")
        self.client = None

        # Модели Claude с их специализациями
        self.models = {
            "claude-3-opus-20240229": {
                "name": "Claude 3 Opus",
                "specialization": "Complex reasoning, detailed analysis",
                "max_tokens": 4096,
                "cost_per_1k": 0.015,  # Примерная стоимость
            },
            "claude-3-sonnet-20240229": {
                "name": "Claude 3 Sonnet",
                "specialization": "Balanced performance, general tasks",
                "max_tokens": 4096,
                "cost_per_1k": 0.003,
            },
            "claude-3-haiku-20240307": {
                "name": "Claude 3 Haiku",
                "specialization": "Fast responses, simple tasks",
                "max_tokens": 4096,
                "cost_per_1k": 0.00025,
            },
        }

        self.default_model = "claude-3-sonnet-20240229"
        self.max_tokens = 4000
        self.temperature = 0.1  # Низкая температура для более точных ответов

        # Кэш для ответов
        self.response_cache = {}
        self.cache_ttl = 3600  # 1 час

        # Constitutional AI принципы
        self.constitutional_principles = self._build_constitutional_principles()

        # Контекст системы
        self.system_context = self._build_system_context()

        # Инициализируем клиент
        self._init_client()

        logger.info("Claude Service инициализирован")

    def _init_client(self):
        """Инициализирует Anthropic Claude клиент."""
        if not ANTHROPIC_AVAILABLE:
            logger.warning("Anthropic Claude недоступен")
            return

        if not self.api_key:
            logger.warning("Anthropic API key не найден. Установите ANTHROPIC_API_KEY")
            return

        try:
            self.client = AsyncAnthropic(api_key=self.api_key)
            logger.info("Claude клиент успешно инициализирован")
        except Exception as e:
            logger.error(f"Ошибка инициализации Claude клиента: {e}")

    def _build_constitutional_principles(self) -> List[str]:
        """Строит Constitutional AI принципы для Claude."""
        return [
            "Анализируй безопасность и потенциальные риски каждого ML-решения",
            "Учитывай этические аспекты автоматизированных решений",
            "Предоставляй честную оценку ограничений и неопределенностей",
            "Избегай предвзятости в анализе данных и паттернов",
            "Приоритизируй безопасность пользователей и системы",
            "Будь прозрачным относительно источников неопределенности",
            "Рекомендуй человеческий надзор для критических решений",
            "Учитывай долгосрочные последствия автоматизированных действий",
        ]

    def _build_system_context(self) -> str:
        """Строит системный контекст для Claude."""
        return f"""
Вы - AI-аналитик безопасности для системы Resonance Liminal, real-time WebSocket платформы.

ВАША СПЕЦИАЛИЗАЦИЯ:
- Анализ безопасности ML-решений и их последствий
- Этическая оценка автоматизированных действий
- Детальное пошаговое рассуждение для сложных проблем
- Constitutional AI подход к принятию решений
- Оценка рисков и потенциального вреда

КОНТЕКСТ СИСТЕМЫ:
- WebSocket сервер с JWT аутентификацией
- ML модели для anomaly detection и predictive analytics
- Автоматизированные системы rate limiting и блокировки
- Real-time обработка больших объемов пользовательских данных
- Критическая инфраструктура с требованиями безопасности

CONSTITUTIONAL AI ПРИНЦИПЫ:
{chr(10).join(f"- {principle}" for principle in self.constitutional_principles)}

ВАШИ ПРИНЦИПЫ АНАЛИЗА:
1. Безопасность превыше всего
2. Прозрачность в рассуждениях
3. Этическая ответственность
4. Учет долгосрочных последствий
5. Рекомендации человеческого надзора
6. Честная оценка ограничений

ФОРМАТЫ ОТВЕТОВ:
- Analysis: детальный анализ с пошаговым рассуждением
- Safety Assessment: safe/caution/unsafe с обоснованием
- Ethical Considerations: этические аспекты решения
- Recommendations: конкретные действия с учетом рисков
- Constitutional AI Notes: insights из Constitutional AI подхода
"""

    def _select_optimal_model(self, task_type: str, complexity: str = "medium") -> str:
        """Выбирает оптимальную модель Claude для задачи."""
        if (
            task_type in ["safety", "ethics", "detailed_reasoning"]
            and complexity == "high"
        ):
            return "claude-3-opus-20240229"  # Самая мощная для сложных задач
        elif task_type in ["security", "consensus"] or complexity == "medium":
            return "claude-3-sonnet-20240229"  # Сбалансированная
        else:
            return "claude-3-haiku-20240307"  # Быстрая для простых задач

    async def analyze_ml_safety(
        self, ml_decision: Dict[str, Any], context: Optional[Dict[str, Any]] = None
    ) -> ClaudeAnalysisResponse:
        """
        Анализирует безопасность ML-решения с Constitutional AI подходом.

        Args:
            ml_decision: Данные о ML-решении
            context: Дополнительный контекст

        Returns:
            Анализ безопасности
        """
        request = ClaudeAnalysisRequest(
            type="safety",
            data=ml_decision,
            context=context,
            model=self._select_optimal_model("safety", "high"),
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_safety_analysis_prompt(request)
        return await self._get_claude_analysis(prompt, "safety", request.model)

    async def analyze_ethical_implications(
        self,
        automated_action: Dict[str, Any],
        affected_users: List[str],
        context: Optional[Dict[str, Any]] = None,
    ) -> ClaudeAnalysisResponse:
        """
        Анализирует этические аспекты автоматизированного действия.

        Args:
            automated_action: Данные об автоматизированном действии
            affected_users: Список затронутых пользователей
            context: Дополнительный контекст

        Returns:
            Этический анализ
        """
        request = ClaudeAnalysisRequest(
            type="ethics",
            data={
                "action": automated_action,
                "affected_users": affected_users,
                "user_count": len(affected_users),
            },
            context=context,
            model=self._select_optimal_model("ethics", "high"),
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_ethics_analysis_prompt(request)
        return await self._get_claude_analysis(prompt, "ethics", request.model)

    async def detailed_reasoning_analysis(
        self,
        complex_problem: Dict[str, Any],
        available_data: Dict[str, Any],
        constraints: List[str],
    ) -> ClaudeAnalysisResponse:
        """
        Выполняет детальный пошаговый анализ сложной проблемы.

        Args:
            complex_problem: Описание сложной проблемы
            available_data: Доступные данные
            constraints: Ограничения и требования

        Returns:
            Детальный анализ с рассуждениями
        """
        request = ClaudeAnalysisRequest(
            type="detailed_reasoning",
            data={
                "problem": complex_problem,
                "data": available_data,
                "constraints": constraints,
            },
            model=self._select_optimal_model("detailed_reasoning", "high"),
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_detailed_reasoning_prompt(request)
        return await self._get_claude_analysis(
            prompt, "detailed_reasoning", request.model
        )

    async def security_threat_analysis(
        self,
        threat_indicators: Dict[str, Any],
        system_state: Dict[str, Any],
        historical_data: Optional[List[Dict]] = None,
    ) -> ClaudeAnalysisResponse:
        """
        Анализирует угрозы безопасности и рекомендует меры защиты.

        Args:
            threat_indicators: Индикаторы угроз
            system_state: Текущее состояние системы
            historical_data: Исторические данные об инцидентах

        Returns:
            Анализ угроз безопасности
        """
        request = ClaudeAnalysisRequest(
            type="security",
            data={
                "threats": threat_indicators,
                "system": system_state,
                "history": historical_data or [],
            },
            model=self._select_optimal_model("security", "medium"),
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_security_analysis_prompt(request)
        return await self._get_claude_analysis(prompt, "security", request.model)

    async def consensus_analysis_with_openai(
        self,
        openai_analysis: str,
        problem_data: Dict[str, Any],
        context: Dict[str, Any],
    ) -> ClaudeAnalysisResponse:
        """
        Выполняет консенсус-анализ с результатами OpenAI для критических решений.

        Args:
            openai_analysis: Анализ от OpenAI
            problem_data: Данные проблемы
            context: Контекст

        Returns:
            Консенсус-анализ
        """
        request = ClaudeAnalysisRequest(
            type="consensus",
            data={
                "openai_analysis": openai_analysis,
                "problem": problem_data,
                "context": context,
            },
            model=self._select_optimal_model("consensus", "medium"),
            timestamp=datetime.now().isoformat(),
        )

        prompt = self._build_consensus_analysis_prompt(request)
        return await self._get_claude_analysis(prompt, "consensus", request.model)

    def _build_safety_analysis_prompt(self, request: ClaudeAnalysisRequest) -> str:
        """Строит промпт для анализа безопасности."""
        return f"""
Проанализируйте безопасность этого ML-решения с Constitutional AI подходом:

ML РЕШЕНИЕ:
{json.dumps(request.data, indent=2, ensure_ascii=False)}

КОНТЕКСТ:
{json.dumps(request.context, indent=2, ensure_ascii=False)}

ВРЕМЯ: {request.timestamp}

Выполните анализ безопасности по следующим аспектам:

1. ОЦЕНКА РИСКОВ:
   - Потенциальный вред для пользователей
   - Риски ложных срабатываний/пропусков
   - Влияние на доступность сервиса
   - Долгосрочные последствия

2. CONSTITUTIONAL AI АНАЛИЗ:
   - Соответствие принципам безопасности
   - Этические аспекты автоматизированного решения
   - Необходимость человеческого надзора
   - Прозрачность и объяснимость

3. РЕКОМЕНДАЦИИ ПО БЕЗОПАСНОСТИ:
   - Меры по снижению рисков
   - Мониторинг и алерты
   - Процедуры отката
   - Улучшения системы

Формат ответа: JSON с полями analysis, reasoning, safety_assessment, ethical_considerations, recommendations, constitutional_ai_notes, harm_assessment.
"""

    def _build_ethics_analysis_prompt(self, request: ClaudeAnalysisRequest) -> str:
        """Строит промпт для этического анализа."""
        return f"""
Проанализируйте этические аспекты этого автоматизированного действия:

АВТОМАТИЗИРОВАННОЕ ДЕЙСТВИЕ:
{json.dumps(request.data, indent=2, ensure_ascii=False)}

КОНТЕКСТ:
{json.dumps(request.context, indent=2, ensure_ascii=False)}

Выполните этический анализ по Constitutional AI принципам:

1. ЭТИЧЕСКИЕ ПРИНЦИПЫ:
   - Справедливость и недискриминация
   - Прозрачность и подотчетность
   - Автономия пользователей
   - Пропорциональность мер

2. АНАЛИЗ ВОЗДЕЙСТВИЯ:
   - Влияние на затронутых пользователей
   - Потенциальная предвзятость
   - Долгосрочные социальные эффекты
   - Альтернативные подходы

3. РЕКОМЕНДАЦИИ:
   - Этические улучшения
   - Процедуры обжалования
   - Человеческий надзор
   - Мониторинг справедливости

Формат ответа: JSON с полями analysis, reasoning, safety_assessment, ethical_considerations, recommendations, constitutional_ai_notes.
"""

    def _build_detailed_reasoning_prompt(self, request: ClaudeAnalysisRequest) -> str:
        """Строит промпт для детального анализа."""
        return f"""
Выполните детальный пошаговый анализ этой сложной проблемы:

ПРОБЛЕМА:
{json.dumps(request.data["problem"], indent=2, ensure_ascii=False)}

ДОСТУПНЫЕ ДАННЫЕ:
{json.dumps(request.data["data"], indent=2, ensure_ascii=False)}

ОГРАНИЧЕНИЯ:
{json.dumps(request.data["constraints"], indent=2, ensure_ascii=False)}

Проведите систематический анализ:

1. ДЕКОМПОЗИЦИЯ ПРОБЛЕМЫ:
   - Разбейте на подзадачи
   - Определите зависимости
   - Выявите критические точки

2. ПОШАГОВОЕ РАССУЖДЕНИЕ:
   - Логическая цепочка анализа
   - Рассмотрение альтернатив
   - Оценка каждого шага

3. СИНТЕЗ РЕШЕНИЯ:
   - Интеграция выводов
   - Учет ограничений
   - Рекомендации по реализации

4. ПРОВЕРКА КАЧЕСТВА:
   - Валидация логики
   - Оценка рисков
   - Планы на случай непредвиденных обстоятельств

Формат ответа: JSON с детальным reasoning в поле reasoning (массив шагов).
"""

    def _build_security_analysis_prompt(self, request: ClaudeAnalysisRequest) -> str:
        """Строит промпт для анализа безопасности."""
        return f"""
Проанализируйте угрозы безопасности и рекомендуйте защитные меры:

ИНДИКАТОРЫ УГРОЗ:
{json.dumps(request.data["threats"], indent=2, ensure_ascii=False)}

СОСТОЯНИЕ СИСТЕМЫ:
{json.dumps(request.data["system"], indent=2, ensure_ascii=False)}

ИСТОРИЧЕСКИЕ ДАННЫЕ:
{json.dumps(request.data["history"][-5:], indent=2, ensure_ascii=False)}

Выполните анализ безопасности:

1. КЛАССИФИКАЦИЯ УГРОЗ:
   - Тип и серьезность угроз
   - Вероятность реализации
   - Потенциальный ущерб

2. АНАЛИЗ УЯЗВИМОСТЕЙ:
   - Слабые места системы
   - Векторы атак
   - Существующие защитные меры

3. РЕКОМЕНДАЦИИ ПО ЗАЩИТЕ:
   - Немедленные меры
   - Долгосрочные улучшения
   - Мониторинг и обнаружение

4. ПЛАН РЕАГИРОВАНИЯ:
   - Процедуры при инциденте
   - Эскалация и уведомления
   - Восстановление после атаки

Формат ответа: JSON с анализом безопасности и конкретными рекомендациями.
"""

    def _build_consensus_analysis_prompt(self, request: ClaudeAnalysisRequest) -> str:
        """Строит промпт для консенсус-анализа с OpenAI."""
        return f"""
Выполните независимый анализ и сравните с результатами OpenAI:

АНАЛИЗ OPENAI:
{request.data["openai_analysis"]}

ДАННЫЕ ПРОБЛЕМЫ:
{json.dumps(request.data["problem"], indent=2, ensure_ascii=False)}

КОНТЕКСТ:
{json.dumps(request.data["context"], indent=2, ensure_ascii=False)}

Проведите консенсус-анализ:

1. НЕЗАВИСИМЫЙ АНАЛИЗ:
   - Ваша оценка проблемы
   - Альтернативные интерпретации
   - Constitutional AI перспектива

2. СРАВНЕНИЕ С OPENAI:
   - Точки согласия
   - Различия в подходах
   - Дополнительные соображения

3. КОНСЕНСУС И РАЗНОГЛАСИЯ:
   - Области консенсуса
   - Критические разногласия
   - Необходимость дополнительного анализа

4. ИТОГОВЫЕ РЕКОМЕНДАЦИИ:
   - Объединенные выводы
   - Рекомендации по действиям
   - Области для человеческого решения

Формат ответа: JSON с консенсус-анализом и сравнением подходов.
"""

    async def _get_claude_analysis(
        self, prompt: str, analysis_type: str, model: str
    ) -> ClaudeAnalysisResponse:
        """Получает анализ от Claude и парсит ответ."""
        try:
            # Проверяем кэш
            cache_key = f"{analysis_type}:{model}:{hash(prompt)}"
            if cache_key in self.response_cache:
                cached_response, timestamp = self.response_cache[cache_key]
                if time.time() - timestamp < self.cache_ttl:
                    return cached_response

            # Получаем ответ от Claude
            response_text = await self._call_claude(prompt, model)

            # Парсим JSON ответ
            try:
                response_data = json.loads(response_text)
            except json.JSONDecodeError:
                # Если не JSON, создаем структурированный ответ
                response_data = {
                    "analysis": response_text,
                    "reasoning": ["Детальный анализ предоставлен в текстовом формате"],
                    "safety_assessment": "caution",
                    "ethical_considerations": ["Требуется дополнительный анализ"],
                    "recommendations": ["Проверить детали вручную"],
                    "constitutional_ai_notes": [
                        "Анализ выполнен с учетом Constitutional AI принципов"
                    ],
                    "harm_assessment": {
                        "level": "unknown",
                        "details": "Требует дополнительной оценки",
                    },
                }

            # Создаем ClaudeAnalysisResponse
            analysis_response = ClaudeAnalysisResponse(
                analysis=response_data.get("analysis", ""),
                reasoning=response_data.get("reasoning", []),
                safety_assessment=response_data.get("safety_assessment", "caution"),
                ethical_considerations=response_data.get("ethical_considerations", []),
                recommendations=response_data.get("recommendations", []),
                confidence=response_data.get("confidence", 0.8),
                model_used=model,
                constitutional_ai_notes=response_data.get(
                    "constitutional_ai_notes", []
                ),
                harm_assessment=response_data.get("harm_assessment", {}),
                follow_up_questions=response_data.get("follow_up_questions", []),
            )

            # Кэшируем ответ
            self.response_cache[cache_key] = (analysis_response, time.time())

            return analysis_response

        except Exception as e:
            logger.error(f"Ошибка получения анализа от Claude: {e}")
            return ClaudeAnalysisResponse(
                analysis=f"Ошибка анализа: {str(e)}",
                reasoning=["Произошла ошибка при обращении к Claude"],
                safety_assessment="unknown",
                ethical_considerations=["Не удалось выполнить этический анализ"],
                recommendations=[
                    "Повторить запрос позже",
                    "Проверить подключение к Anthropic",
                ],
                confidence=0.0,
                model_used=model,
                constitutional_ai_notes=["Анализ недоступен из-за технической ошибки"],
                harm_assessment={"level": "unknown", "error": str(e)},
                follow_up_questions=[],
            )

    async def _call_claude(self, prompt: str, model: str) -> str:
        """Вызывает Anthropic Claude API."""
        if not self.client:
            raise Exception("Claude клиент не инициализирован")

        try:
            response = await self.client.messages.create(
                model=model,
                max_tokens=self.max_tokens,
                temperature=self.temperature,
                system=self.system_context,
                messages=[{"role": "user", "content": prompt}],
            )

            return response.content[0].text

        except Exception as e:
            logger.error(f"Ошибка вызова Claude API: {e}")
            raise

    async def health_check(self) -> Dict[str, Any]:
        """Проверка здоровья Claude сервиса."""
        if not self.client:
            return {
                "status": "unhealthy",
                "reason": "Клиент не инициализирован",
                "api_key_configured": bool(self.api_key),
            }

        try:
            # Простой тестовый запрос
            test_response = await self.client.messages.create(
                model=self.default_model,
                max_tokens=50,
                temperature=0.1,
                system="Вы - тестовый помощник.",
                messages=[{"role": "user", "content": "Тест"}],
            )

            return {
                "status": "healthy",
                "default_model": self.default_model,
                "available_models": list(self.models.keys()),
                "cache_size": len(self.response_cache),
                "constitutional_principles": len(self.constitutional_principles),
                "test_response_length": len(test_response.content[0].text),
            }

        except Exception as e:
            return {
                "status": "unhealthy",
                "reason": str(e),
                "api_key_configured": bool(self.api_key),
            }

    def get_model_info(self) -> Dict[str, Any]:
        """Возвращает информацию о доступных моделях Claude."""
        return {
            "models": self.models,
            "default_model": self.default_model,
            "constitutional_principles": self.constitutional_principles,
            "specializations": {
                "safety_analysis": "claude-3-opus-20240229",
                "ethics_review": "claude-3-opus-20240229",
                "detailed_reasoning": "claude-3-opus-20240229",
                "security_analysis": "claude-3-sonnet-20240229",
                "consensus_analysis": "claude-3-sonnet-20240229",
                "quick_analysis": "claude-3-haiku-20240307",
            },
        }


# Глобальный экземпляр Claude сервиса
claude_service = ClaudeService()
