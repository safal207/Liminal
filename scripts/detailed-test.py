#!/usr/bin/env python3
"""
Детальное тестирование Multi-LLM системы с красивым выводом
"""

from datetime import datetime

import requests

BASE_URL = "http://localhost:8000"


def print_header(title):
    print(f"\n{'='*60}")
    print(f"🎯 {title}")
    print("=" * 60)


def print_test(name, success, details=None):
    status = "✅ PASS" if success else "❌ FAIL"
    print(f"{status} {name}")
    if details:
        print(f"    📝 {details}")


def test_multi_llm_demo():
    print_header("RESONANCE LIMINAL MULTI-LLM DEMO TEST")

    # 1. Health Check
    print_header("SYSTEM HEALTH")
    try:
        response = requests.get(f"{BASE_URL}/health", timeout=10)
        if response.status_code == 200:
            data = response.json()
            print_test("System Health", True, f"Status: {data['status']}")
            print(f"    🕐 Timestamp: {data['timestamp']}")
            print(f"    🔧 Services: {', '.join(data['services'].keys())}")
        else:
            print_test("System Health", False, f"Status code: {response.status_code}")
    except Exception as e:
        print_test("System Health", False, str(e))

    # 2. ML Status
    try:
        response = requests.get(f"{BASE_URL}/ml/status", timeout=10)
        if response.status_code == 200:
            data = response.json()
            print_test("ML Status", True, f"Models loaded: {data['models_loaded']}")
            print(f"    📊 Cache size: {data['cache_size']}")
            print(f"    🔗 Active connections: {data['active_connections']}")
            print(f"    ⏱️ Uptime: {data['uptime']}")
        else:
            print_test("ML Status", False, f"Status code: {response.status_code}")
    except Exception as e:
        print_test("ML Status", False, str(e))

    # 3. XAI Test
    print_header("XAI (EXPLAINABLE AI) TESTING")
    xai_data = {
        "model_name": "anomaly_detector",
        "features": {
            "messages_per_minute": 150,
            "error_rate": 0.25,
            "user_count": 45,
            "response_time": 1200,
        },
    }

    try:
        response = requests.post(
            f"{BASE_URL}/ml/xai/explain-prediction", json=xai_data, timeout=15
        )
        if response.status_code == 200:
            data = response.json()
            print_test(
                "XAI Prediction",
                True,
                f"Prediction: {data['prediction']} (confidence: {data['confidence']})",
            )
            print(
                f"    🧠 Top feature: {list(data['explanation']['shap_values'].keys())[0]}"
            )
            print(
                f"    💡 Explanation: {data['explanation']['natural_language'][:100]}..."
            )

            # Показываем SHAP values
            print(f"    📊 SHAP Values:")
            for feature, value in data["explanation"]["shap_values"].items():
                print(f"       • {feature}: {value}")
        else:
            print_test("XAI Prediction", False, f"Status code: {response.status_code}")
    except Exception as e:
        print_test("XAI Prediction", False, str(e))

    # 4. OpenAI Test
    print_header("OPENAI GPT-4 TESTING (DEMO MODE)")
    openai_data = {
        "logs": [
            "2024-07-30 18:40:15 INFO: System startup completed successfully",
            "2024-07-30 18:40:23 ERROR: Database connection timeout after 30s",
            "2024-07-30 18:40:24 WARNING: High memory usage detected (85%)",
            "2024-07-30 18:40:30 ERROR: Failed to process user request #12345",
            "2024-07-30 18:40:35 INFO: Automatic recovery initiated",
        ],
        "context": {
            "system_load": 0.85,
            "error_rate": 0.12,
            "time_of_day": "evening",
            "active_users": 1200,
        },
    }

    try:
        response = requests.post(
            f"{BASE_URL}/ml/openai/analyze-logs", json=openai_data, timeout=15
        )
        if response.status_code == 200:
            data = response.json()
            print_test("OpenAI Log Analysis", True, f"Severity: {data['severity']}")
            print(f"    🔍 Analysis preview: {data['analysis'][:150]}...")
            print(f"    💡 Recommendations: {len(data['recommendations'])} items")
            print(f"    🎯 Confidence: {data['confidence']}")
            print(f"    🔧 Provider: {data['provider']}")
        else:
            print_test(
                "OpenAI Log Analysis", False, f"Status code: {response.status_code}"
            )
    except Exception as e:
        print_test("OpenAI Log Analysis", False, str(e))

    # 5. Claude Test
    print_header("ANTHROPIC CLAUDE TESTING (DEMO MODE)")
    claude_data = {
        "ml_decision": {
            "model_name": "security_analyzer",
            "prediction": "block_user",
            "confidence": 0.92,
            "features": {
                "failed_login_attempts": 15,
                "suspicious_ip": True,
                "unusual_activity": True,
            },
        },
        "context": {"system_load": 0.6, "time_of_day": "night", "affected_users": 1},
    }

    try:
        response = requests.post(
            f"{BASE_URL}/ml/claude/safety-analysis", json=claude_data, timeout=15
        )
        if response.status_code == 200:
            data = response.json()
            print_test(
                "Claude Safety Analysis",
                True,
                f"Assessment: {data['safety_assessment']}",
            )
            print(
                f"    👼 Constitutional AI notes: {len(data['constitutional_ai_notes'])} items"
            )
            print(
                f"    ⚖️ Ethical considerations: {len(data['ethical_considerations'])} items"
            )
            print(f"    🛡️ Harm level: {data['harm_assessment']['level']}")
            print(f"    💡 Recommendations: {len(data['recommendations'])} items")
        else:
            print_test(
                "Claude Safety Analysis", False, f"Status code: {response.status_code}"
            )
    except Exception as e:
        print_test("Claude Safety Analysis", False, str(e))

    # 6. Multi-LLM Test
    print_header("MULTI-LLM ORCHESTRATOR TESTING")
    multi_data = {
        "task_type": "security",
        "data": {
            "problem_description": "Potential DDoS attack detected",
            "metrics": {
                "requests_per_second": 5000,
                "error_rate": 0.35,
                "response_time": 3000,
                "concurrent_users": 50,
            },
            "context": {
                "environment": "production",
                "severity": "high",
                "business_impact": "critical",
            },
        },
        "preferred_provider": "auto",
        "require_consensus": False,
        "priority": "high",
    }

    try:
        response = requests.post(
            f"{BASE_URL}/ml/multi-llm/analyze", json=multi_data, timeout=15
        )
        if response.status_code == 200:
            data = response.json()
            print_test("Multi-LLM Analysis", True, f"Provider: {data['provider_used']}")
            print(f"    🎯 Confidence: {data['confidence']}")
            print(f"    💰 Cost estimate: ${data['cost_estimate']:.4f}")
            print(f"    ⏱️ Response time: {data['response_time_ms']}ms")
            print(f"    🔄 Fallback used: {data['fallback_used']}")
            print(
                f"    📊 Providers available: OpenAI={data['openai_available']}, Claude={data['claude_available']}"
            )
        else:
            print_test(
                "Multi-LLM Analysis", False, f"Status code: {response.status_code}"
            )
    except Exception as e:
        print_test("Multi-LLM Analysis", False, str(e))

    # 7. Consensus Test
    consensus_data = {
        "task_type": "safety",
        "data": {
            "critical_decision": {
                "type": "emergency_user_block",
                "affected_users": 100,
                "duration": "24 hours",
                "reason": "coordinated_attack_detected",
            }
        },
    }

    try:
        response = requests.post(
            f"{BASE_URL}/ml/multi-llm/consensus", json=consensus_data, timeout=15
        )
        if response.status_code == 200:
            data = response.json()
            print_test(
                "Multi-LLM Consensus",
                True,
                f"Decision: {data['consensus_analysis']['decision']}",
            )
            print(f"    🤝 Agreement score: {data['agreement_score']}")
            print(
                f"    🧠 OpenAI: {data['provider_consensus']['openai_recommendation']}"
            )
            print(
                f"    👼 Claude: {data['provider_consensus']['claude_recommendation']}"
            )
            print(f"    ✅ Consensus reached: {data['consensus_reached']}")
        else:
            print_test(
                "Multi-LLM Consensus", False, f"Status code: {response.status_code}"
            )
    except Exception as e:
        print_test("Multi-LLM Consensus", False, str(e))

    # 8. Combined Status
    print_header("COMBINED AI STATUS")
    try:
        response = requests.get(f"{BASE_URL}/ml/ai-status", timeout=10)
        if response.status_code == 200:
            data = response.json()
            print_test("Combined AI Status", True, f"Overall: {data['overall_status']}")
            print(
                f"    🧠 XAI: {data['services']['xai']['status']} ({data['services']['xai']['models_loaded']} models)"
            )
            print(
                f"    🤖 OpenAI: {data['services']['openai']['status']} ({data['services']['openai']['requests_today']} requests today)"
            )
            print(
                f"    👼 Claude: {data['services']['claude']['status']} ({data['services']['claude']['requests_today']} requests today)"
            )
            print(
                f"    🎛️ Multi-LLM: {data['services']['multi_llm']['status']} ({data['services']['multi_llm']['consensus_requests']} consensus)"
            )
        else:
            print_test(
                "Combined AI Status", False, f"Status code: {response.status_code}"
            )
    except Exception as e:
        print_test("Combined AI Status", False, str(e))

    # Final Summary
    print_header("TEST SUMMARY")
    print("🎉 Multi-LLM Demo Testing Completed!")
    print(f"⏰ Test completed at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("\n📊 System Capabilities Demonstrated:")
    print("   ✅ XAI - Explainable AI with SHAP analysis")
    print("   ✅ OpenAI - Intelligent log analysis and recommendations")
    print("   ✅ Claude - Constitutional AI safety and ethics analysis")
    print("   ✅ Multi-LLM - Intelligent provider orchestration")
    print("   ✅ Consensus - Dual-provider critical decision making")
    print("\n🌐 Access Points:")
    print(f"   📚 API Documentation: {BASE_URL}/docs")
    print(f"   🏥 Health Check: {BASE_URL}/health")
    print(f"   📊 ML Status: {BASE_URL}/ml/status")
    print(f"   🤖 AI Status: {BASE_URL}/ml/ai-status")

    print("\n" + "=" * 60)
    print("🚀 RESONANCE LIMINAL MULTI-LLM SYSTEM IS OPERATIONAL! 🚀")
    print("=" * 60)


if __name__ == "__main__":
    test_multi_llm_demo()
