"""
API эндпоинты для ML-функций Resonance Liminal.
Предоставляет доступ к аномалиям, моделям и аналитике.
"""
from datetime import datetime
from typing import Any

from fastapi import APIRouter, BackgroundTasks, HTTPException, status
from loguru import logger
from pydantic import BaseModel

from .anomaly_detector import anomaly_detector
from .claude_service import (
    claude_service,
)
from .feature_extractor import feature_extractor
from .model_manager import model_manager
from .multi_llm_orchestrator import (
    LLMProvider,
    MultiLLMRequest,
    TaskType,
    multi_llm_orchestrator,
)
from .openai_service import openai_service
from .xai_service import xai_service

router = APIRouter(prefix="/ml", tags=["machine-learning"])


# Pydantic модели для API
class TrainingRequest(BaseModel):
    model_name: str
    data_source: str = "recent_features"  # или путь к файлу
    auto_deploy: bool = True


class PredictionRequest(BaseModel):
    model_name: str
    features: dict[str, Any]


class AnomalyAnalysisRequest(BaseModel):
    user_id: str


class ExplainPredictionRequest(BaseModel):
    model_name: str
    features: dict[str, Any]
    prediction: Any
    confidence: float = 0.0


class AnalyzeLogsRequest(BaseModel):
    log_entries: list[str]
    time_range: str
    error_patterns: list[str] | None = None


class SmartAlertRequest(BaseModel):
    alert_data: dict[str, Any]
    context: dict[str, Any]
    recipient_role: str = "devops"


class ClaudeSafetyAnalysisRequest(BaseModel):
    ml_decision: dict[str, Any]
    context: dict[str, Any] | None = None


class ClaudeEthicsAnalysisRequest(BaseModel):
    automated_action: dict[str, Any]
    affected_users: list[str]
    context: dict[str, Any] | None = None


class ClaudeSecurityAnalysisRequest(BaseModel):
    threat_indicators: dict[str, Any]
    system_state: dict[str, Any]
    historical_data: list[dict] | None = None


class MultiLLMAnalysisRequest(BaseModel):
    task_type: str  # "general", "safety", "ethics", "security", "logs", "alerts"
    data: dict[str, Any]
    context: dict[str, Any] | None = None
    preferred_provider: str = "auto"  # "openai", "claude", "both", "auto"
    require_consensus: bool = False
    priority: str = "normal"
    max_cost: float | None = None


@router.get("/status")
async def get_ml_status() -> dict[str, Any]:
    """
    Возвращает общий статус ML-системы.
    """
    return {
        "feature_extractor": {
            "active_sessions": len(feature_extractor.user_sessions),
            "features_buffer_size": len(feature_extractor.feature_buffer),
        },
        "model_manager": model_manager.get_model_status(),
        "anomaly_detector": {
            "alerts_buffer_size": len(anomaly_detector.alerts_buffer),
            "user_baselines": len(anomaly_detector.user_baselines),
        },
    }


@router.get("/features/recent")
async def get_recent_features(limit: int = 100) -> dict[str, Any]:
    """
    Возвращает последние извлеченные фичи.

    Args:
        limit: Максимальное количество записей
    """
    features = feature_extractor.get_recent_features(limit)
    return {"features": features, "count": len(features), "limit": limit}


@router.get("/anomalies/recent")
async def get_recent_anomalies(limit: int = 50, min_severity: str = "low") -> dict[str, Any]:
    """
    Возвращает последние обнаруженные аномалии.

    Args:
        limit: Максимальное количество алертов
        min_severity: Минимальная серьезность (low, medium, high, critical)
    """
    alerts = anomaly_detector.get_recent_alerts(limit, min_severity)
    return {"alerts": alerts, "count": len(alerts), "min_severity": min_severity}


@router.post("/anomalies/analyze")
async def analyze_user_anomalies(request: AnomalyAnalysisRequest) -> dict[str, Any]:
    """
    Анализирует активность конкретного пользователя на предмет аномалий.

    Args:
        request: Запрос с user_id для анализа
    """
    try:
        alerts = anomaly_detector.analyze_user_activity(request.user_id)

        return {
            "user_id": request.user_id,
            "anomalies_detected": len(alerts),
            "alerts": [
                {
                    "type": alert.anomaly_type,
                    "severity": alert.severity,
                    "confidence": alert.confidence,
                    "recommended_action": alert.recommended_action,
                }
                for alert in alerts
            ],
        }
    except Exception as e:
        logger.error(f"Ошибка анализа аномалий для пользователя {request.user_id}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка анализа: {str(e)}",
        ) from e


@router.post("/models/train")
async def train_model(
    request: TrainingRequest, background_tasks: BackgroundTasks
) -> dict[str, Any]:
    """
    Запускает обучение ML-модели в фоновом режиме.

    Args:
        request: Параметры обучения модели
        background_tasks: FastAPI background tasks
    """
    try:
        # Получаем данные для обучения
        if request.data_source == "recent_features":
            training_data = feature_extractor.get_recent_features(1000)
        else:
            # Здесь можно добавить загрузку из файла
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Поддерживается только data_source='recent_features'",
            )

        if len(training_data) < 10:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="Недостаточно данных для обучения (минимум 10 записей)",
            )

        # Запускаем обучение в фоне
        background_tasks.add_task(
            model_manager.train_model_with_kenning, request.model_name, training_data
        )

        return {
            "message": f"Обучение модели {request.model_name} запущено в фоновом режиме",
            "model_name": request.model_name,
            "training_data_size": len(training_data),
            "auto_deploy": request.auto_deploy,
        }

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Ошибка запуска обучения модели {request.model_name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка запуска обучения: {str(e)}",
        ) from e


@router.post("/models/{model_name}/load")
async def load_model(model_name: str) -> dict[str, Any]:
    """
    Загружает обученную модель для использования.

    Args:
        model_name: Название модели для загрузки
    """
    try:
        success = model_manager.load_model(model_name)

        if success:
            return {
                "message": f"Модель {model_name} успешно загружена",
                "model_name": model_name,
                "status": "loaded",
            }
        else:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Не удалось загрузить модель {model_name}",
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Ошибка загрузки модели {model_name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка загрузки модели: {str(e)}",
        ) from e


@router.post("/models/predict")
async def make_prediction(request: PredictionRequest) -> dict[str, Any]:
    """
    Выполняет предсказание используя загруженную модель.

    Args:
        request: Запрос с названием модели и фичами
    """
    try:
        prediction = model_manager.predict(request.model_name, request.features)

        if prediction is not None:
            return {
                "model_name": request.model_name,
                "prediction": prediction,
                "features": request.features,
            }
        else:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=(
                    f"Модель {request.model_name} не загружена или произошла ошибка предсказания"
                ),
            )

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Ошибка предсказания модели {request.model_name}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка предсказания: {str(e)}",
        ) from e


@router.get("/models")
async def list_models() -> dict[str, Any]:
    """
    Возвращает список доступных моделей и их статус.
    """
    return model_manager.get_model_status()


@router.delete("/features/cleanup")
async def cleanup_old_data(max_age_hours: int = 24) -> dict[str, Any]:
    """
    Очищает старые данные из буферов.

    Args:
        max_age_hours: Максимальный возраст данных в часах
    """
    try:
        # Очищаем старые сессии пользователей
        old_sessions_count = len(feature_extractor.user_sessions)
        feature_extractor.cleanup_old_sessions(max_age_hours)
        new_sessions_count = len(feature_extractor.user_sessions)

        cleaned_sessions = old_sessions_count - new_sessions_count

        return {
            "message": "Очистка завершена",
            "cleaned_sessions": cleaned_sessions,
            "remaining_sessions": new_sessions_count,
            "max_age_hours": max_age_hours,
        }

    except Exception as e:
        logger.error(f"Ошибка очистки данных: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка очистки: {str(e)}",
        ) from e


# =============================================================================
# XAI (Explainable AI) Endpoints
# =============================================================================


@router.post("/explain/prediction")
async def explain_prediction(request: ExplainPredictionRequest) -> dict[str, Any]:
    """
    Объясняет ML-предсказание с помощью XAI методов.

    Args:
        request: Запрос с данными для объяснения
    """
    try:
        # Получаем XAI объяснение
        explanation = await xai_service.explain_prediction(
            model_name=request.model_name,
            features=request.features,
            prediction=request.prediction,
            confidence=request.confidence,
        )

        # Получаем OpenAI объяснение
        natural_explanation = await openai_service.explain_ml_prediction(
            prediction=request.prediction,
            features=request.features,
            model_type=request.model_name,
            xai_explanation={
                "feature_importance": explanation.feature_importance,
                "shap_values": explanation.shap_values,
                "decision_path": explanation.decision_path,
            },
        )

        return {
            "model_name": request.model_name,
            "prediction": explanation.prediction,
            "confidence": explanation.confidence,
            "feature_importance": explanation.feature_importance,
            "technical_explanation": explanation.explanation_text,
            "natural_explanation": natural_explanation,
            "shap_values": explanation.shap_values,
            "lime_explanation": explanation.lime_explanation,
            "counterfactual": explanation.counterfactual,
            "decision_path": explanation.decision_path,
        }

    except Exception as e:
        logger.error(f"Ошибка объяснения предсказания: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка объяснения: {str(e)}",
        ) from e


@router.post("/explain/anomaly")
async def explain_anomaly(request: AnomalyAnalysisRequest) -> dict[str, Any]:
    """
    Объясняет обнаруженную аномалию с помощью XAI и OpenAI.

    Args:
        request: Запрос с user_id для анализа аномалии
    """
    try:
        # Анализируем аномалии пользователя
        alerts = anomaly_detector.analyze_user_activity(request.user_id)

        if not alerts:
            return {
                "user_id": request.user_id,
                "message": "Аномалии не обнаружены",
                "explanation": "Активность пользователя в пределах нормы",
            }

        # Берем самую критичную аномалию
        critical_alert = max(alerts, key=lambda x: x.confidence)

        # Получаем фичи пользователя
        user_features = feature_extractor.get_user_features(request.user_id)

        # OpenAI анализ аномалии
        analysis = await openai_service.analyze_anomaly(
            anomaly_data={
                "user_id": request.user_id,
                "anomaly_type": critical_alert.anomaly_type,
                "severity": critical_alert.severity,
                "confidence": critical_alert.confidence,
                "features": user_features,
                "recommended_action": critical_alert.recommended_action,
            },
            context={"total_alerts": len(alerts)},
        )

        return {
            "user_id": request.user_id,
            "anomaly_detected": True,
            "critical_alert": {
                "type": critical_alert.anomaly_type,
                "severity": critical_alert.severity,
                "confidence": critical_alert.confidence,
            },
            "analysis": analysis.analysis,
            "recommendations": analysis.recommendations,
            "action_items": analysis.action_items,
            "severity_assessment": analysis.severity,
            "ai_confidence": analysis.confidence,
            "summary": analysis.summary,
            "total_alerts": len(alerts),
        }

    except Exception as e:
        logger.error(f"Ошибка объяснения аномалии для {request.user_id}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка анализа аномалии: {str(e)}",
        ) from e


# =============================================================================
# OpenAI Intelligence Endpoints
# =============================================================================


@router.post("/analyze/logs")
async def analyze_logs_with_ai(request: AnalyzeLogsRequest) -> dict[str, Any]:
    """
    Анализирует логи с помощью OpenAI для поиска паттернов и проблем.

    Args:
        request: Запрос с логами для анализа
    """
    try:
        analysis = await openai_service.analyze_logs(
            log_entries=request.log_entries,
            time_range=request.time_range,
            error_patterns=request.error_patterns,
        )

        return {
            "logs_analyzed": len(request.log_entries),
            "time_range": request.time_range,
            "analysis": analysis.analysis,
            "recommendations": analysis.recommendations,
            "severity": analysis.severity,
            "action_items": analysis.action_items,
            "summary": analysis.summary,
            "confidence": analysis.confidence,
            "technical_details": analysis.technical_details,
            "follow_up_questions": analysis.follow_up_questions,
        }

    except Exception as e:
        logger.error(f"Ошибка анализа логов: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка анализа логов: {str(e)}",
        ) from e


@router.post("/alerts/smart")
async def generate_smart_alert(request: SmartAlertRequest) -> dict[str, Any]:
    """
    Генерирует умное уведомление с помощью OpenAI.

    Args:
        request: Данные для генерации smart alert
    """
    try:
        smart_alert = await openai_service.generate_smart_alert(
            alert_data=request.alert_data,
            context=request.context,
            recipient_role=request.recipient_role,
        )

        return {
            "recipient_role": request.recipient_role,
            "smart_alert": smart_alert,
            "generated_at": datetime.now().isoformat(),
            "alert_data": request.alert_data,
        }

    except Exception as e:
        logger.error(f"Ошибка генерации smart alert: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка генерации alert: {str(e)}",
        ) from e


@router.get("/health/xai")
async def xai_health_check() -> dict[str, Any]:
    """
    Проверка здоровья XAI сервиса.
    """
    try:
        # Простой тест XAI
        test_features = {
            "messages_per_minute": 10.0,
            "error_rate": 0.05,
            "connection_duration": 300.0,
        }

        test_explanation = await xai_service.explain_prediction(
            model_name="test",
            features=test_features,
            prediction="normal",
            confidence=0.8,
        )

        return {
            "status": "healthy",
            "xai_available": True,
            "cache_size": len(xai_service.explanation_cache),
            "test_explanation_generated": bool(test_explanation.explanation_text),
        }

    except Exception as e:
        return {"status": "unhealthy", "xai_available": False, "error": str(e)}


@router.get("/health/openai")
async def openai_health_check() -> dict[str, Any]:
    """
    Проверка здоровья OpenAI сервиса.
    """
    return await openai_service.health_check()


@router.get("/intelligence/status")
async def get_intelligence_status() -> dict[str, Any]:
    """
    Возвращает статус всех AI/ML сервисов.
    """
    try:
        xai_health = await xai_health_check()
        openai_health = await openai_health_check()

        return {
            "timestamp": datetime.now().isoformat(),
            "xai_service": xai_health,
            "openai_service": openai_health,
            "overall_status": (
                "healthy"
                if (
                    xai_health.get("status") == "healthy"
                    and openai_health.get("status") == "healthy"
                )
                else "degraded"
            ),
            "features": {
                "explainable_ai": xai_health.get("xai_available", False),
                "natural_language_analysis": openai_health.get("status") == "healthy",
                "smart_alerts": True,
                "log_analysis": True,
                "anomaly_explanation": True,
            },
        }

    except Exception as e:
        logger.error(f"Ошибка получения статуса intelligence: {e}")
        return {
            "timestamp": datetime.now().isoformat(),
            "overall_status": "error",
            "error": str(e),
        }


# =============================================================================
# Anthropic Claude Endpoints
# =============================================================================


@router.post("/claude/safety-analysis")
async def claude_safety_analysis(
    request: ClaudeSafetyAnalysisRequest,
) -> dict[str, Any]:
    """
    Анализ безопасности ML-решения с помощью Claude Constitutional AI.

    Args:
        request: Запрос с данными ML-решения для анализа безопасности
    """
    try:
        analysis = await claude_service.analyze_ml_safety(
            ml_decision=request.ml_decision, context=request.context
        )

        return {
            "analysis": analysis.analysis,
            "reasoning": analysis.reasoning,
            "safety_assessment": analysis.safety_assessment,
            "ethical_considerations": analysis.ethical_considerations,
            "recommendations": analysis.recommendations,
            "confidence": analysis.confidence,
            "model_used": analysis.model_used,
            "constitutional_ai_notes": analysis.constitutional_ai_notes,
            "harm_assessment": analysis.harm_assessment,
            "follow_up_questions": analysis.follow_up_questions,
        }

    except Exception as e:
        logger.error(f"Ошибка Claude анализа безопасности: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка анализа безопасности: {str(e)}",
        ) from e


@router.post("/claude/ethics-analysis")
async def claude_ethics_analysis(
    request: ClaudeEthicsAnalysisRequest,
) -> dict[str, Any]:
    """
    Этический анализ автоматизированного действия с помощью Claude.

    Args:
        request: Запрос с данными для этического анализа
    """
    try:
        analysis = await claude_service.analyze_ethical_implications(
            automated_action=request.automated_action,
            affected_users=request.affected_users,
            context=request.context,
        )

        return {
            "analysis": analysis.analysis,
            "reasoning": analysis.reasoning,
            "safety_assessment": analysis.safety_assessment,
            "ethical_considerations": analysis.ethical_considerations,
            "recommendations": analysis.recommendations,
            "confidence": analysis.confidence,
            "constitutional_ai_notes": analysis.constitutional_ai_notes,
            "harm_assessment": analysis.harm_assessment,
            "affected_users_count": len(request.affected_users),
        }

    except Exception as e:
        logger.error(f"Ошибка Claude этического анализа: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка этического анализа: {str(e)}",
        ) from e


@router.post("/claude/security-analysis")
async def claude_security_analysis(
    request: ClaudeSecurityAnalysisRequest,
) -> dict[str, Any]:
    """
    Анализ угроз безопасности с помощью Claude.

    Args:
        request: Запрос с данными для анализа безопасности
    """
    try:
        analysis = await claude_service.security_threat_analysis(
            threat_indicators=request.threat_indicators,
            system_state=request.system_state,
            historical_data=request.historical_data,
        )

        return {
            "analysis": analysis.analysis,
            "reasoning": analysis.reasoning,
            "safety_assessment": analysis.safety_assessment,
            "recommendations": analysis.recommendations,
            "confidence": analysis.confidence,
            "constitutional_ai_notes": analysis.constitutional_ai_notes,
            "harm_assessment": analysis.harm_assessment,
            "threat_indicators_analyzed": len(request.threat_indicators),
        }

    except Exception as e:
        logger.error(f"Ошибка Claude анализа безопасности: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка анализа безопасности: {str(e)}",
        ) from e


@router.get("/claude/health")
async def claude_health_check() -> dict[str, Any]:
    """
    Проверка здоровья Claude сервиса.
    """
    return await claude_service.health_check()


@router.get("/claude/models")
async def claude_model_info() -> dict[str, Any]:
    """
    Информация о доступных моделях Claude.
    """
    return claude_service.get_model_info()


# =============================================================================
# Multi-LLM Orchestrator Endpoints
# =============================================================================


@router.post("/multi-llm/analyze")
async def multi_llm_analyze(request: MultiLLMAnalysisRequest) -> dict[str, Any]:
    """
    Универсальный анализ с автоматическим выбором оптимального AI провайдера.

    Args:
        request: Запрос с данными для Multi-LLM анализа
    """
    try:
        # Преобразуем строковые значения в enum
        task_type_map = {
            "general": TaskType.GENERAL_ANALYSIS,
            "safety": TaskType.SAFETY_ANALYSIS,
            "ethics": TaskType.ETHICS_REVIEW,
            "security": TaskType.SECURITY_ANALYSIS,
            "logs": TaskType.LOG_ANALYSIS,
            "alerts": TaskType.SMART_ALERTS,
            "reasoning": TaskType.DETAILED_REASONING,
            "consensus": TaskType.CONSENSUS_DECISION,
        }

        provider_map = {
            "openai": LLMProvider.OPENAI,
            "claude": LLMProvider.CLAUDE,
            "both": LLMProvider.BOTH,
            "auto": LLMProvider.AUTO,
        }

        task_type = task_type_map.get(request.task_type, TaskType.GENERAL_ANALYSIS)
        preferred_provider = provider_map.get(request.preferred_provider, LLMProvider.AUTO)

        # Создаем Multi-LLM запрос
        multi_llm_request = MultiLLMRequest(
            task_type=task_type,
            data=request.data,
            context=request.context,
            preferred_provider=preferred_provider,
            require_consensus=request.require_consensus,
            priority=request.priority,
            max_cost=request.max_cost,
        )

        # Выполняем анализ
        response = await multi_llm_orchestrator.analyze(multi_llm_request)

        return {
            "primary_analysis": response.primary_analysis,
            "provider_used": response.provider_used,
            "confidence": response.confidence,
            "recommendations": response.recommendations,
            "consensus_analysis": response.consensus_analysis,
            "agreement_score": response.agreement_score,
            "cost_estimate": response.cost_estimate,
            "processing_time": response.processing_time,
            "fallback_used": response.fallback_used,
            "errors": response.errors or [],
            "openai_available": response.openai_response is not None,
            "claude_available": response.claude_response is not None,
        }

    except Exception as e:
        logger.error(f"Ошибка Multi-LLM анализа: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка Multi-LLM анализа: {str(e)}",
        ) from e


@router.post("/multi-llm/consensus")
async def multi_llm_consensus_analysis(
    request: MultiLLMAnalysisRequest,
) -> dict[str, Any]:
    """
    Консенсус-анализ критических решений с использованием OpenAI и Claude.

    Args:
        request: Запрос для консенсус-анализа
    """
    try:
        # Принудительно требуем консенсус
        request.require_consensus = True
        request.preferred_provider = "both"

        # Используем обычный multi-llm анализ с консенсусом
        return await multi_llm_analyze(request)

    except Exception as e:
        logger.error(f"Ошибка консенсус-анализа: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка консенсус-анализа: {str(e)}",
        ) from e


@router.get("/multi-llm/health")
async def multi_llm_health_check() -> dict[str, Any]:
    """
    Проверка здоровья Multi-LLM системы.
    """
    return await multi_llm_orchestrator.health_check()


@router.get("/multi-llm/usage-report")
async def multi_llm_usage_report() -> dict[str, Any]:
    """
    Отчет об использовании AI провайдеров.
    """
    return multi_llm_orchestrator.get_usage_report()


@router.get("/ai/comprehensive-status")
async def comprehensive_ai_status() -> dict[str, Any]:
    """
    Комплексный статус всех AI сервисов (XAI, OpenAI, Claude, Multi-LLM).
    """
    try:
        xai_health = await xai_health_check()
        openai_health = await openai_health_check()
        claude_health = await claude_health_check()
        multi_llm_health = await multi_llm_health_check()

        # Определяем общий статус
        all_healthy = all(
            [
                xai_health.get("status") == "healthy",
                openai_health.get("status") == "healthy",
                claude_health.get("status") == "healthy",
                multi_llm_health.get("status") == "healthy",
            ]
        )

        any_healthy = any(
            [
                xai_health.get("status") == "healthy",
                openai_health.get("status") == "healthy",
                claude_health.get("status") == "healthy",
            ]
        )

        overall_status = "healthy" if all_healthy else ("degraded" if any_healthy else "unhealthy")

        return {
            "timestamp": datetime.now().isoformat(),
            "overall_status": overall_status,
            "services": {
                "xai_service": xai_health,
                "openai_service": openai_health,
                "claude_service": claude_health,
                "multi_llm_orchestrator": multi_llm_health,
            },
            "capabilities": {
                "explainable_ai": xai_health.get("xai_available", False),
                "openai_analysis": openai_health.get("status") == "healthy",
                "claude_constitutional_ai": claude_health.get("status") == "healthy",
                "multi_llm_orchestration": multi_llm_health.get("status") == "healthy",
                "consensus_analysis": (
                    openai_health.get("status") == "healthy"
                    and claude_health.get("status") == "healthy"
                ),
                "automated_provider_selection": True,
                "cost_optimization": True,
                "fallback_resilience": True,
            },
            "usage_statistics": multi_llm_health.get("usage_stats", {}),
            "recommendations": [
                (
                    "Система готова к production использованию"
                    if all_healthy
                    else (
                        "Некоторые AI сервисы недоступны, но система функциональна"
                        if any_healthy
                        else "Критическая ошибка: все AI сервисы недоступны"
                    )
                )
            ],
        }

    except Exception as e:
        logger.error(f"Ошибка получения комплексного AI статуса: {e}")
        return {
            "timestamp": datetime.now().isoformat(),
            "overall_status": "error",
            "error": str(e),
        }
