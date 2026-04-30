#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Simple API functionality test for LIMINAL.
Tests the core API endpoints without requiring external services.
"""

import asyncio
import json
import os
import sys
from pathlib import Path

# Add backend to path
backend_path = Path(__file__).parent / "backend"
sys.path.insert(0, str(backend_path))


async def test_api_basic_functionality():
    """Test basic API functionality."""
    print("🚀 Testing LIMINAL API Basic Functionality...")

    try:
        # Import the FastAPI app
        from api import app

        print("✅ FastAPI app imported successfully")

        # Check routes
        routes = [route.path for route in app.routes]
        print(f"✅ API configured with {len(routes)} routes")

        # Check some key routes
        key_routes = ["/health", "/metrics", "/consciousness", "/api/v1"]
        found_routes = []
        for route in key_routes:
            if any(route in r for r in routes):
                found_routes.append(route)

        print(f"✅ Key routes found: {found_routes}")

        # Test health endpoint (if available)
        try:
            from fastapi.testclient import TestClient

            client = TestClient(app)

            # Test health endpoint
            response = client.get("/health")
            print(f"✅ Health endpoint response: {response.status_code}")

            # Test root endpoint
            response = client.get("/")
            print(f"✅ Root endpoint response: {response.status_code}")

        except ImportError:
            print("⚠️  TestClient not available, skipping endpoint tests")

        return True

    except Exception as e:
        print(f"❌ API test failed: {e}")
        return False


async def test_configuration_loading():
    """Test configuration loading."""
    print("\n🔧 Testing Configuration Loading...")

    try:
        from config import get_settings

        settings = get_settings()

        print(f"✅ Environment: {settings.environment}")
        print(f"✅ Debug mode: {settings.debug}")
        print(f"✅ Neo4j URI: {settings.neo4j_uri}")
        print(f"✅ ML enabled: {settings.ml_enabled}")

        return True

    except Exception as e:
        print(f"❌ Configuration test failed: {e}")
        return False


async def test_monitoring_system():
    """Test monitoring system."""
    print("\n📊 Testing Monitoring System...")

    try:
        from monitoring import monitoring_service, record_business_metric

        # Test business metric recording
        record_business_metric("test_metric", 42, "count", "Test metric")
        print("✅ Business metric recorded")

        # Test health check
        health = monitoring_service.health_monitor.get_overall_health()
        print(f"✅ Health status: {health['status']}")

        return True

    except Exception as e:
        print(f"❌ Monitoring test failed: {e}")
        return False


async def test_resilience_patterns():
    """Test resilience patterns."""
    print("\n🛡️ Testing Resilience Patterns...")

    try:
        from resilience import circuit_breaker, resilience_manager

        # Test resilience manager
        health = resilience_manager.get_health_status()
        print(f"✅ Resilience health: {health['status']}")

        # Test circuit breaker decorator
        @circuit_breaker(name="test_breaker")
        async def test_function():
            return "success"

        result = await test_function()
        print(f"✅ Circuit breaker test: {result}")

        return True

    except Exception as e:
        print(f"❌ Resilience test failed: {e}")
        return False


def main():
    """Run all functionality tests."""
    print("🔬 LIMINAL API Functionality Tests")
    print("=" * 50)

    async def run_tests():
        results = []

        # Run test suites
        test_suites = [
            ("Configuration Loading", test_configuration_loading),
            ("API Basic Functionality", test_api_basic_functionality),
            ("Monitoring System", test_monitoring_system),
            ("Resilience Patterns", test_resilience_patterns),
        ]

        for test_name, test_func in test_suites:
            print(f"\n{'='*20} {test_name} {'='*20}")
            try:
                result = await test_func()
                results.append((test_name, result))
            except Exception as e:
                print(f"❌ {test_name} failed with exception: {e}")
                results.append((test_name, False))

        # Summary
        print(f"\n{'='*50}")
        print("🎯 Test Results Summary")
        print("=" * 50)

        passed = sum(1 for _, result in results if result)
        total = len(results)

        for test_name, result in results:
            status = "✅ PASS" if result else "❌ FAIL"
            print(f"  {status} {test_name}")

        print(f"\nTotal: {total}, Passed: {passed}, Failed: {total - passed}")
        print(f"Success Rate: {(passed/total)*100:.1f}%")

        if passed == total:
            print("\n🎉 All functionality tests passed! LIMINAL API is ready.")
            return 0
        else:
            print(f"\n⚠️  {total - passed} tests failed. Check errors above.")
            return 1

    return asyncio.run(run_tests())


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)
