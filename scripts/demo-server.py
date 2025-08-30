#!/usr/bin/env python3
"""
Демо-сервер для тестирования Multi-LLM endpoints
Имитирует работу реальной системы без внешних API
"""

from datetime import datetime
from typing import Any

import uvicorn
from fastapi import FastAPI
from pydantic import BaseModel

app = FastAPI(
    title="Resonance Liminal Multi-LLM Demo",
    description="Демонстрация Multi-LLM архитектуры",
    version="1.0.0",
)


# Модели данных
class HealthResponse(BaseModel):
    status: str
    timestamp: str
    services: dict[str, str]


class XAIRequest(BaseModel):
    model_name: str
    features: dict[str, Any]


class OpenAIRequest(BaseModel):
    logs: list[str]
    context: dict[str, Any]


class ClaudeRequest(BaseModel):
    ml_decision: dict[str, Any]
    context: dict[str, Any]


class MultiLLMRequest(BaseModel):
    task_type: str
    data: dict[str, Any]
    preferred_provider: str = "auto"
    require_consensus: bool = False
    priority: str = "normal"


# Базовые endpoints
@app.get("/health")
async def health_check():
    return HealthResponse(
        status="healthy",
        timestamp=datetime.now().isoformat(),
        services={
            "api": "healthy",
            "xai": "healthy",
            "openai": "demo_mode",
            "claude": "demo_mode",
            "multi_llm": "healthy",
        },
    )


@app.get("/ml/status")
async def ml_status():
    return {
        "status": "operational",
        "models_loaded": 3,
        "cache_size": 1000,
        "active_connections": 5,
        "uptime": "2h 15m",
        "features": {
            "xai_enabled": True,
            "openai_enabled": True,
            "claude_enabled": True,
            "multi_llm_enabled": True,
        },
    }


# XAI endpoints
@app.post("/ml/xai/explain-prediction")
async def explain_prediction(request: XAIRequest):
    return {
        "model_name": request.model_name,
        "prediction": ("anomaly" if request.features.get("error_rate", 0) > 0.1 else "normal"),
        "confidence": 0.87,
        "explanation": {
            "shap_values": {
                "messages_per_minute": 0.45,
                "error_rate": 0.32,
                "user_count": 0.23,
            },
            "feature_importance": [
                {"feature": "error_rate", "importance": 0.32, "direction": "positive"},
                {
                    "feature": "messages_per_minute",
                    "importance": 0.45,
                    "direction": "positive",
                },
                {"feature": "user_count", "importance": 0.23, "direction": "negative"},
            ],
            "natural_language": "Высокая частота ошибок (32% влияния) и большое количество сообщений в минуту (45% влияния) указывают на аномалию.",
        },
        "counterfactual": {
            "to_make_normal": {"error_rate": "< 0.05", "messages_per_minute": "< 80"}
        },
    }


# OpenAI endpoints (демо режим)
@app.post("/ml/openai/analyze-logs")
async def analyze_logs(request: OpenAIRequest):
    error_count = sum(1 for log in request.logs if "ERROR" in log)
    warning_count = sum(1 for log in request.logs if "WARNING" in log)

    return {
        "analysis": f"🔍 АНАЛИЗ ЛОГОВ (DEMO MODE):\n\nОбнаружено {error_count} ошибок и {warning_count} предупреждений.\n\n📊 КЛЮЧЕВЫЕ ПРОБЛЕМЫ:\n• Database connection timeout - критическая проблема\n• High memory usage - требует внимания\n• System load: {request.context.get('system_load', 'unknown')}\n\n💡 РЕКОМЕНДАЦИИ:\n1. Проверить подключение к базе данных\n2. Оптимизировать использование памяти\n3. Мониторить системную нагрузку",
        "severity": "medium" if error_count > 0 else "low",
        "recommendations": [
            "Restart database connection pool",
            "Check memory leaks in application",
            "Scale horizontally if load > 0.8",
        ],
        "confidence": 0.85,
        "provider": "openai_demo",
        "tokens_used": 150,
    }


@app.get("/ml/openai/health")
async def openai_health():
    return {
        "status": "demo_mode",
        "model": "gpt-4-turbo-preview",
        "requests_today": 42,
        "avg_response_time": "1.2s",
        "note": "Running in demo mode - no real OpenAI API calls",
    }


# Claude endpoints (демо режим)
@app.post("/ml/claude/safety-analysis")
async def claude_safety_analysis(request: ClaudeRequest):
    confidence = request.ml_decision.get("confidence", 0.5)
    prediction = request.ml_decision.get("prediction", "normal")

    return {
        "safety_assessment": (
            "APPROVED_WITH_CONDITIONS" if confidence > 0.7 else "REQUIRES_REVIEW"
        ),
        "constitutional_ai_notes": [
            "✅ Решение основано на объективных данных",
            "⚠️ Рекомендуется человеческий контроль для критических случаев",
            "✅ Минимизация вреда для пользователей учтена",
            "📋 Прозрачность решения обеспечена",
        ],
        "ethical_considerations": [
            "Принцип пропорциональности: меры соответствуют угрозе",
            "Минимизация вреда: защита большинства пользователей",
            "Справедливость: равное отношение ко всем пользователям",
        ],
        "harm_assessment": {
            "level": "minimal" if prediction == "normal" else "low",
            "reasoning": "Автоматические действия имеют низкий потенциал вреда",
            "mitigation_required": prediction != "normal",
        },
        "recommendations": [
            "Реализовать whitelist для известных хороших пользователей",
            "Добавить возможность обжалования автоматических решений",
            "Логировать все действия для аудита",
        ],
        "provider": "claude_demo",
        "note": "Running in demo mode - no real Anthropic API calls",
    }


@app.get("/ml/claude/health")
async def claude_health():
    return {
        "status": "demo_mode",
        "model": "claude-3-sonnet-20240229",
        "requests_today": 28,
        "avg_response_time": "1.8s",
        "constitutional_ai": "enabled",
        "note": "Running in demo mode - no real Anthropic API calls",
    }


# Multi-LLM endpoints
@app.post("/ml/multi-llm/analyze")
async def multi_llm_analyze(request: MultiLLMRequest):
    # Симулируем выбор провайдера
    if request.preferred_provider == "auto":
        provider = "openai" if request.task_type in ["logs", "general"] else "claude"
    else:
        provider = request.preferred_provider

    return {
        "provider_used": provider,
        "primary_analysis": f"Анализ задачи типа '{request.task_type}' выполнен провайдером {provider}. Обнаружена проблема с error_rate: {request.data.get('metrics', {}).get('error_rate', 'unknown')}",
        "confidence": 0.88,
        "reasoning": f"Провайдер {provider} выбран как оптимальный для задач типа '{request.task_type}'",
        "cost_estimate": 0.003 if provider == "openai" else 0.002,
        "response_time_ms": 1200,
        "fallback_used": False,
        "openai_available": True,
        "claude_available": True,
        "demo_mode": True,
    }


@app.post("/ml/multi-llm/consensus")
async def multi_llm_consensus(request: MultiLLMRequest):
    return {
        "consensus_analysis": {
            "decision": "EXECUTE_WITH_SAFEGUARDS",
            "agreement_score": 0.85,
            "reasoning": "Оба AI провайдера согласны: ситуация требует действий, но с мерами предосторожности",
        },
        "provider_consensus": {
            "openai_recommendation": "proceed_with_monitoring",
            "claude_recommendation": "proceed_with_safeguards",
            "final_decision": "hybrid_approach",
        },
        "agreement_score": 0.85,
        "openai_available": True,
        "claude_available": True,
        "consensus_reached": True,
        "demo_mode": True,
    }


@app.get("/ml/multi-llm/health")
async def multi_llm_health():
    return {
        "status": "healthy",
        "providers_available": ["openai", "claude"],
        "consensus_requests_today": 15,
        "fallback_usage": 2,
        "cost_optimization_active": True,
        "demo_mode": True,
    }


@app.get("/ml/ai-status")
async def ai_status():
    return {
        "overall_status": "healthy",
        "demo_mode": True,
        "services": {
            "xai": {"status": "healthy", "cache_size": 1000, "models_loaded": 3},
            "openai": {
                "status": "demo_mode",
                "model": "gpt-4-turbo-preview",
                "requests_today": 42,
            },
            "claude": {
                "status": "demo_mode",
                "model": "claude-3-sonnet-20240229",
                "requests_today": 28,
            },
            "multi_llm": {
                "status": "healthy",
                "providers_available": ["openai", "claude"],
                "consensus_requests": 15,
            },
        },
        "note": "System running in demo mode for testing purposes",
    }


if __name__ == "__main__":
    print("🚀 Starting Resonance Liminal Multi-LLM Demo Server...")
    print("📊 API Documentation: http://localhost:8000/docs")
    print("🧪 Test endpoints with: python scripts/test-api-endpoints.py")

    uvicorn.run(app, host="0.0.0.0", port=8000)
