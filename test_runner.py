#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
LIMINAL Local Testing Runner
Comprehensive testing script for all production-ready components
"""

import json
import os
import sys
import traceback
from pathlib import Path

# Add backend to path
backend_path = Path(__file__).parent / "backend"
sys.path.insert(0, str(backend_path))


def test_imports():
    """Test all core module imports."""
    print("🧪 Testing Core Module Imports...")

    try:
        # Test configuration
        from config import get_settings

        settings = get_settings()
        print(f"✅ Configuration loaded - Environment: {settings.environment}")

        # Test FastAPI
        import fastapi

        print("✅ FastAPI available")

        # Test Neo4j driver
        import neo4j

        print("✅ Neo4j driver available")

        # Test Pydantic
        import pydantic

        print(f"✅ Pydantic available - Version: {pydantic.VERSION}")

        # Test other core modules
        try:
            import uvicorn

            print("✅ Uvicorn available")
        except ImportError:
            print("⚠️  Uvicorn not available")

        try:
            import redis

            print("✅ Redis client available")
        except ImportError:
            print("⚠️  Redis client not available")

        return True

    except Exception as e:
        print(f"❌ Import test failed: {e}")
        traceback.print_exc()
        return False


def test_api_module():
    """Test API module loading."""
    print("\n🌐 Testing API Module...")

    try:
        # Import main API module
        import api

        print("✅ API module imported successfully")

        # Check if FastAPI app is created
        if hasattr(api, "app"):
            print("✅ FastAPI app instance found")
        else:
            print("⚠️  FastAPI app instance not found")

        return True

    except Exception as e:
        print(f"❌ API module test failed: {e}")
        traceback.print_exc()
        return False


def test_configuration():
    """Test configuration system."""
    print("\n🔧 Testing Configuration System...")

    try:
        from config import (
            get_database_settings,
            get_ml_settings,
            get_monitoring_settings,
            get_security_settings,
            get_settings,
            get_websocket_settings,
        )

        # Test main settings
        settings = get_settings()
        print(f"✅ Main settings: Environment={settings.environment}")

        # Test database settings
        db_settings = get_database_settings()
        print(f"✅ Database settings: Neo4j URI={db_settings.neo4j_uri}")

        # Test security settings
        sec_settings = get_security_settings()
        print(f"✅ Security settings: JWT Algorithm={sec_settings.jwt_algorithm}")

        # Test ML settings
        ml_settings = get_ml_settings()
        print(f"✅ ML settings: Enabled={ml_settings.ml_enabled}")

        # Test WebSocket settings
        ws_settings = get_websocket_settings()
        print(f"✅ WebSocket settings: Max connections={ws_settings.max_connections}")

        # Test monitoring settings
        mon_settings = get_monitoring_settings()
        print(f"✅ Monitoring settings: Metrics enabled={mon_settings.metrics_enabled}")

        return True

    except Exception as e:
        print(f"❌ Configuration test failed: {e}")
        traceback.print_exc()
        return False


def test_production_modules():
    """Test production-ready modules."""
    print("\n🏭 Testing Production Modules...")

    modules_to_test = [
        ("Resilience", "resilience"),
        ("Monitoring", "monitoring"),
        ("ML Pipeline", "ml_pipeline_enhanced"),
        ("Security", "security_enhanced"),
        ("Database Optimization", "database_optimization"),
        ("WebSocket Manager", "websocket_manager"),
    ]

    results = {}

    for name, module_name in modules_to_test:
        try:
            __import__(module_name)
            print(f"✅ {name} module imported successfully")
            results[name] = True
        except Exception as e:
            print(f"❌ {name} module failed: {e}")
            results[name] = False

    return all(results.values())


def test_environment():
    """Test environment setup."""
    print("\n🌍 Testing Environment Setup...")

    # Check environment file
    env_file = Path(__file__).parent / ".env.local"
    if env_file.exists():
        print("✅ .env.local file found")
    else:
        print("⚠️  .env.local file not found")

    # Check required directories
    required_dirs = ["backend", "scripts", "config", "k8s"]
    for dir_name in required_dirs:
        dir_path = Path(__file__).parent / dir_name
        if dir_path.exists():
            print(f"✅ Directory {dir_name} exists")
        else:
            print(f"❌ Directory {dir_name} missing")

    # Check Python version
    python_version = sys.version_info
    if python_version >= (3, 8):
        print(
            f"✅ Python version: {python_version.major}.{python_version.minor}.{python_version.micro}"
        )
    else:
        print(
            f"❌ Python version too old: {python_version.major}.{python_version.minor}.{python_version.micro}"
        )
        return False

    return True


def test_api_startup():
    """Test API startup without actually starting the server."""
    print("\n🚀 Testing API Startup...")

    try:
        # Import and create app
        from api import app

        # Check if app is properly configured
        if hasattr(app, "routes"):
            route_count = len(app.routes)
            print(f"✅ FastAPI app configured with {route_count} routes")

        # Check middleware
        if hasattr(app, "middleware_stack"):
            print("✅ Middleware stack configured")

        return True

    except Exception as e:
        print(f"❌ API startup test failed: {e}")
        traceback.print_exc()
        return False


def test_database_clients():
    """Test database client initialization."""
    print("\n💾 Testing Database Clients...")

    try:
        # Test Datomic client
        try:
            from datomic_client import DatomicClient

            print("✅ Datomic client available")
        except ImportError:
            print("⚠️  Datomic client not available")

        # Test Neo4j connection capability (without actual connection)
        from neo4j import GraphDatabase

        print("✅ Neo4j driver available for connections")

        return True

    except Exception as e:
        print(f"❌ Database client test failed: {e}")
        return False


def generate_test_report(results):
    """Generate a comprehensive test report."""
    print("\n📊 Test Report Summary")
    print("=" * 50)

    total_tests = len(results)
    passed_tests = sum(1 for result in results.values() if result)
    failed_tests = total_tests - passed_tests

    print(f"Total Tests: {total_tests}")
    print(f"Passed: {passed_tests}")
    print(f"Failed: {failed_tests}")
    print(f"Success Rate: {(passed_tests/total_tests)*100:.1f}%")

    print("\nDetailed Results:")
    for test_name, result in results.items():
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"  {status} {test_name}")

    # Save results to file
    report_file = Path(__file__).parent / "test_report.json"
    with open(report_file, "w") as f:
        json.dump(
            {
                "total_tests": total_tests,
                "passed_tests": passed_tests,
                "failed_tests": failed_tests,
                "success_rate": (passed_tests / total_tests) * 100,
                "results": results,
                "timestamp": str(Path(__file__).stat().st_mtime),
            },
            f,
            indent=2,
        )

    print(f"\n📝 Detailed report saved to: {report_file}")

    return passed_tests == total_tests


def main():
    """Run all tests."""
    print("🔬 LIMINAL Local Testing Suite")
    print("=" * 50)

    # Test cases
    test_cases = [
        ("Environment Setup", test_environment),
        ("Core Imports", test_imports),
        ("Configuration System", test_configuration),
        ("Production Modules", test_production_modules),
        ("Database Clients", test_database_clients),
        ("API Module", test_api_module),
        ("API Startup", test_api_startup),
    ]

    results = {}

    for test_name, test_func in test_cases:
        print(f"\n{'='*20} {test_name} {'='*20}")
        try:
            results[test_name] = test_func()
        except Exception as e:
            print(f"❌ {test_name} failed with exception: {e}")
            results[test_name] = False

    # Generate report
    success = generate_test_report(results)

    if success:
        print("\n🎉 All tests passed! LIMINAL is ready for local development.")
        return 0
    else:
        print("\n⚠️  Some tests failed. Please check the errors above.")
        return 1


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)
