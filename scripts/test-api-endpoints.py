#!/usr/bin/env python3
"""
Resonance Liminal Multi-LLM API Testing Script
Тестирует все основные endpoints нашей Multi-LLM системы
"""

import json
from datetime import datetime

import requests

# Базовый URL API
BASE_URL = "http://localhost:8000"


def test_endpoint(name, url, method="GET", data=None, expected_status=200):
    """Тестирует один endpoint"""
    print(f"\n🧪 Testing {name}...")
    print(f"   URL: {method} {url}")

    try:
        if method == "GET":
            response = requests.get(url, timeout=10)
        elif method == "POST":
            response = requests.post(url, json=data, timeout=15)

        print(f"   Status: {response.status_code}")

        if response.status_code == expected_status:
            print(f"   ✅ SUCCESS")
            try:
                result = response.json()
                print(f"   Response: {json.dumps(result, indent=2)[:200]}...")
                return True, result
            except:
                print(f"   Response: {response.text[:200]}...")
                return True, response.text
        else:
            print(
                f"   ❌ FAILED - Expected {expected_status}, got {response.status_code}"
            )
            print(f"   Error: {response.text[:200]}...")
            return False, None

    except requests.exceptions.RequestException as e:
        print(f"   ❌ CONNECTION ERROR: {e}")
        return False, None


def main():
    print("🚀 Resonance Liminal Multi-LLM API Testing")
    print("=" * 50)

    # Базовые endpoints
    print("\n📋 BASIC ENDPOINTS")
    test_endpoint("Health Check", f"{BASE_URL}/health")
    test_endpoint("ML Status", f"{BASE_URL}/ml/status")

    # XAI endpoints
    print("\n🧠 XAI ENDPOINTS")
    xai_data = {
        "model_name": "test_model",
        "features": {"messages_per_minute": 100, "error_rate": 0.1, "user_count": 50},
    }
    test_endpoint(
        "XAI Prediction Explanation",
        f"{BASE_URL}/ml/xai/explain-prediction",
        "POST",
        xai_data,
    )

    # OpenAI endpoints (будут работать только с реальным API ключом)
    print("\n🤖 OPENAI ENDPOINTS")
    openai_data = {
        "logs": [
            "INFO: System startup completed",
            "ERROR: Database connection timeout",
            "WARNING: High memory usage detected",
        ],
        "context": {"system_load": 0.7, "error_rate": 0.05, "time_of_day": "evening"},
    }
    test_endpoint(
        "OpenAI Log Analysis", f"{BASE_URL}/ml/openai/analyze-logs", "POST", openai_data
    )

    # Claude endpoints (будут работать только с реальным API ключом)
    print("\n👼 CLAUDE ENDPOINTS")
    claude_data = {
        "ml_decision": {
            "model_name": "anomaly_detection",
            "prediction": "anomaly",
            "confidence": 0.85,
            "features": {"messages_per_minute": 150, "error_rate": 0.25},
        },
        "context": {"system_load": 0.8, "time_of_day": "night"},
    }
    test_endpoint(
        "Claude Safety Analysis",
        f"{BASE_URL}/ml/claude/safety-analysis",
        "POST",
        claude_data,
    )

    # Multi-LLM endpoints
    print("\n🎛️ MULTI-LLM ENDPOINTS")
    multi_llm_data = {
        "task_type": "general",
        "data": {
            "problem_description": "High error rate detected in production",
            "metrics": {
                "error_rate": 0.15,
                "response_time": 1500,
                "concurrent_users": 800,
            },
            "context": {"environment": "production", "severity": "medium"},
        },
        "preferred_provider": "auto",
        "require_consensus": False,
        "priority": "normal",
    }
    test_endpoint(
        "Multi-LLM Auto Analysis",
        f"{BASE_URL}/ml/multi-llm/analyze",
        "POST",
        multi_llm_data,
    )

    # Health checks
    print("\n🏥 HEALTH CHECKS")
    test_endpoint("OpenAI Health", f"{BASE_URL}/ml/openai/health")
    test_endpoint("Claude Health", f"{BASE_URL}/ml/claude/health")
    test_endpoint("Multi-LLM Health", f"{BASE_URL}/ml/multi-llm/health")
    test_endpoint("Combined AI Status", f"{BASE_URL}/ml/ai-status")

    print("\n" + "=" * 50)
    print("🎉 Testing completed!")
    print(f"⏰ Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")


if __name__ == "__main__":
    main()
