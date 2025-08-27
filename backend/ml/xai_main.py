"""
XAI Intelligence Service Main Application.
Standalone FastAPI сервис для XAI и OpenAI интеграции.
"""

import os
import sys
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from loguru import logger

# Добавляем путь к backend модулям
sys.path.append("/app")
sys.path.append("/app/..")

from openai_service import openai_service
from xai_service import xai_service


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Управление жизненным циклом приложения."""
    logger.info("🚀 Запуск XAI Intelligence Service...")

    # Инициализация сервисов
    try:
        # Проверяем OpenAI API key
        openai_key = os.getenv("OPENAI_API_KEY")
        if openai_key:
            logger.info("✅ OpenAI API key найден")
        else:
            logger.warning(
                "⚠️ OpenAI API key не найден. Некоторые функции будут недоступны."
            )

        # Инициализируем XAI сервис
        logger.info("🧠 Инициализация XAI Service...")

        # Инициализируем OpenAI сервис
        logger.info("🤖 Инициализация OpenAI Service...")

        logger.info("✅ XAI Intelligence Service готов к работе!")

    except Exception as e:
        logger.error(f"❌ Ошибка инициализации: {e}")
        raise

    yield

    # Cleanup при завершении
    logger.info("🛑 Завершение XAI Intelligence Service...")


# Создаем FastAPI приложение
app = FastAPI(
    title="XAI Intelligence Service",
    description="Explainable AI и OpenAI интеграция для Resonance Liminal",
    version="1.0.0",
    lifespan=lifespan,
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # В продакшене ограничить
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# =============================================================================
# Health Check Endpoints
# =============================================================================


@app.get("/health")
async def health_check():
    """Основная проверка здоровья сервиса."""
    return {
        "status": "healthy",
        "service": "XAI Intelligence Service",
        "version": "1.0.0",
        "features": {
            "xai_available": True,
            "openai_available": bool(os.getenv("OPENAI_API_KEY")),
            "shap_enabled": os.getenv("XAI_ENABLE_SHAP", "true").lower() == "true",
            "lime_enabled": os.getenv("XAI_ENABLE_LIME", "true").lower() == "true",
        },
    }


@app.get("/health/detailed")
async def detailed_health_check():
    """Детальная проверка здоровья всех компонентов."""
    try:
        # Проверяем XAI сервис
        xai_health = {
            "status": "healthy",
            "cache_size": len(xai_service.explanation_cache),
            "models_available": len(xai_service.model_cache),
            "explainers_cached": len(xai_service.explainers),
        }

        # Проверяем OpenAI сервис
        openai_health = await openai_service.health_check()

        return {
            "overall_status": "healthy",
            "timestamp": "2024-07-30T17:01:10+03:00",
            "components": {"xai_service": xai_health, "openai_service": openai_health},
            "environment": {
                "cache_size_limit": int(os.getenv("XAI_CACHE_SIZE", "1000")),
                "openai_model": os.getenv("OPENAI_MODEL", "gpt-4-turbo-preview"),
                "max_tokens": int(os.getenv("OPENAI_MAX_TOKENS", "2000")),
                "temperature": float(os.getenv("OPENAI_TEMPERATURE", "0.3")),
            },
        }

    except Exception as e:
        logger.error(f"Ошибка detailed health check: {e}")
        return {"overall_status": "unhealthy", "error": str(e)}


# =============================================================================
# XAI Endpoints
# =============================================================================


@app.post("/explain/prediction")
async def explain_prediction(request: dict):
    """
    Объясняет ML-предсказание с помощью XAI методов.

    Body:
    {
        "model_name": "anomaly_detection",
        "features": {"messages_per_minute": 25.0, "error_rate": 0.1},
        "prediction": "anomaly",
        "confidence": 0.85
    }
    """
    try:
        explanation = await xai_service.explain_prediction(
            model_name=request.get("model_name", "default"),
            features=request.get("features", {}),
            prediction=request.get("prediction"),
            confidence=request.get("confidence", 0.0),
        )

        # Получаем OpenAI объяснение
        natural_explanation = await openai_service.explain_ml_prediction(
            prediction=explanation.prediction,
            features=request.get("features", {}),
            model_type=request.get("model_name", "default"),
            xai_explanation={
                "feature_importance": explanation.feature_importance,
                "shap_values": explanation.shap_values,
                "decision_path": explanation.decision_path,
            },
        )

        return {
            "model_name": request.get("model_name"),
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
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/explain/anomaly")
async def explain_anomaly(request: dict):
    """
    Объясняет аномалию с помощью XAI и OpenAI.

    Body:
    {
        "user_id": "user123",
        "anomaly_data": {
            "type": "rate_limit_violation",
            "severity": "high",
            "confidence": 0.9,
            "features": {...}
        }
    }
    """
    try:
        anomaly_data = request.get("anomaly_data", {})

        # OpenAI анализ аномалии
        analysis = await openai_service.analyze_anomaly(
            anomaly_data=anomaly_data, context=request.get("context", {})
        )

        return {
            "user_id": request.get("user_id"),
            "anomaly_detected": True,
            "analysis": analysis.analysis,
            "recommendations": analysis.recommendations,
            "action_items": analysis.action_items,
            "severity_assessment": analysis.severity,
            "ai_confidence": analysis.confidence,
            "summary": analysis.summary,
        }

    except Exception as e:
        logger.error(f"Ошибка объяснения аномалии: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# OpenAI Intelligence Endpoints
# =============================================================================


@app.post("/analyze/logs")
async def analyze_logs(request: dict):
    """
    Анализирует логи с помощью OpenAI.

    Body:
    {
        "log_entries": ["ERROR: Connection failed", "WARNING: High latency"],
        "time_range": "last_hour",
        "error_patterns": ["Connection", "Timeout"]
    }
    """
    try:
        analysis = await openai_service.analyze_logs(
            log_entries=request.get("log_entries", []),
            time_range=request.get("time_range", "unknown"),
            error_patterns=request.get("error_patterns"),
        )

        return {
            "logs_analyzed": len(request.get("log_entries", [])),
            "time_range": request.get("time_range"),
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
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/alerts/smart")
async def generate_smart_alert(request: dict):
    """
    Генерирует умное уведомление.

    Body:
    {
        "alert_data": {"message": "High error rate detected", "metric": "error_rate", "value": 0.15},
        "context": {"service": "websocket", "environment": "production"},
        "recipient_role": "devops"
    }
    """
    try:
        smart_alert = await openai_service.generate_smart_alert(
            alert_data=request.get("alert_data", {}),
            context=request.get("context", {}),
            recipient_role=request.get("recipient_role", "devops"),
        )

        return {
            "recipient_role": request.get("recipient_role"),
            "smart_alert": smart_alert,
            "generated_at": "2024-07-30T17:01:10+03:00",
            "alert_data": request.get("alert_data"),
        }

    except Exception as e:
        logger.error(f"Ошибка генерации smart alert: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/analyze/performance")
async def analyze_performance(request: dict):
    """
    Анализирует производительность системы.

    Body:
    {
        "metrics": {"cpu_usage": 0.8, "memory_usage": 0.6, "response_time": 150},
        "time_series_data": [...],
        "baseline": {"cpu_usage": 0.4, "memory_usage": 0.3, "response_time": 50}
    }
    """
    try:
        analysis = await openai_service.analyze_performance_patterns(
            metrics=request.get("metrics", {}),
            time_series_data=request.get("time_series_data", []),
            baseline=request.get("baseline"),
        )

        return {
            "performance_analysis": analysis.analysis,
            "recommendations": analysis.recommendations,
            "severity": analysis.severity,
            "action_items": analysis.action_items,
            "summary": analysis.summary,
            "confidence": analysis.confidence,
            "technical_details": analysis.technical_details,
        }

    except Exception as e:
        logger.error(f"Ошибка анализа производительности: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# Utility Endpoints
# =============================================================================


@app.get("/cache/stats")
async def get_cache_stats():
    """Статистика кэша XAI сервиса."""
    return {
        "explanation_cache_size": len(xai_service.explanation_cache),
        "model_cache_size": len(xai_service.model_cache),
        "explainers_cached": len(xai_service.explainers),
        "openai_cache_size": len(openai_service.response_cache),
        "cache_limit": int(os.getenv("XAI_CACHE_SIZE", "1000")),
    }


@app.delete("/cache/clear")
async def clear_cache():
    """Очищает кэш XAI сервиса."""
    try:
        old_xai_size = len(xai_service.explanation_cache)
        old_openai_size = len(openai_service.response_cache)

        xai_service.explanation_cache.clear()
        openai_service.response_cache.clear()

        return {
            "message": "Кэш очищен",
            "cleared_xai_entries": old_xai_size,
            "cleared_openai_entries": old_openai_size,
        }

    except Exception as e:
        logger.error(f"Ошибка очистки кэша: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/")
async def root():
    """Корневой эндпоинт."""
    return {
        "service": "XAI Intelligence Service",
        "version": "1.0.0",
        "description": "Explainable AI и OpenAI интеграция для Resonance Liminal",
        "endpoints": {
            "health": "/health",
            "explain_prediction": "/explain/prediction",
            "explain_anomaly": "/explain/anomaly",
            "analyze_logs": "/analyze/logs",
            "smart_alerts": "/alerts/smart",
            "performance_analysis": "/analyze/performance",
        },
        "docs": "/docs",
    }


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(
        "xai_main:app", host="0.0.0.0", port=8000, reload=True, log_level="info"
    )
