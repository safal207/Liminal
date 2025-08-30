#!/usr/bin/env python
"""
Test Universal OpenAI Adapter for Resonance Liminal

This script tests the universal adapter for OpenAI API integration,
which provides both real API and mock implementations.
"""

import argparse
import asyncio
import json
import os
import sys
import time
from datetime import datetime
from pathlib import Path

# Add project path
sys.path.append(str(Path(__file__).parent.parent))

# Color output
GREEN = "\033[92m"
YELLOW = "\033[93m"
RED = "\033[91m"
CYAN = "\033[96m"
MAGENTA = "\033[95m"
RESET = "\033[0m"


def print_header(message):
    print(f"\n{MAGENTA}{'=' * 60}{RESET}")
    print(f"{MAGENTA}  {message}{RESET}")
    print(f"{MAGENTA}{'=' * 60}{RESET}\n")


def print_section(message):
    print(f"\n{CYAN}--- {message} ---{RESET}")


def print_success(message):
    print(f"{GREEN}[+] {message}{RESET}")


def print_warning(message):
    print(f"{YELLOW}[!] {message}{RESET}")


def print_error(message):
    print(f"{RED}[-] {message}{RESET}")


def print_info(message):
    print(f"{CYAN}[*] {message}{RESET}")


def mask_key(key):
    if not key or len(key) < 8:
        return "not found"
    return f"{key[:5]}...{key[-4:]}"


async def test_universal_adapter(show_response=False, mock_only=False):
    """Test the universal OpenAI adapter"""
    print_header("UNIVERSAL OPENAI ADAPTER TEST")

    # Check environment variables
    print_section("Environment Variables")

    openai_key = os.environ.get("OPENAI_API_KEY", "")
    anthropic_key = os.environ.get("ANTHROPIC_API_KEY", "")
    xai_key = os.environ.get("XAI_API_KEY", "")

    print_info(f"OpenAI API Key: {mask_key(openai_key)}")
    print_info(f"Anthropic API Key: {mask_key(anthropic_key)}")
    print_info(f"XAI API Key: {mask_key(xai_key)}")

    # Set mock mode if needed
    if mock_only:
        print_warning("Setting MOCK_ONLY=True for testing")
        os.environ["OPENAI_MOCK_ONLY"] = "true"

    # Test universal adapter
    print_section("Universal Adapter Module")

    try:
        from backend.ml.openai_wrapper import (
            LLMRequest,
            initialize_llm_client,
            llm_client,
        )

        print_success("Module imported successfully")

        # Initialize client
        print_section("Client Initialization")
        start_time = time.time()
        success = await initialize_llm_client()
        elapsed = time.time() - start_time

        if success:
            print_success(f"Client initialized with real API in {elapsed:.2f}s")
        else:
            print_warning(f"Client initialized in mock mode in {elapsed:.2f}s")

        # Test client parameters
        print_info(f"API key: {mask_key(llm_client.api_key)}")
        print_info(f"Mock mode: {llm_client.mock_only}")
        print_info(f"Caching enabled: {llm_client.cache_enabled}")
        print_info(f"Cache TTL: {llm_client.cache_ttl} seconds")

        # Run test request for anomaly detection
        print_section("Anomaly Detection Test")

        request = LLMRequest(
            model="gpt-4",
            messages=[
                {
                    "role": "system",
                    "content": "You are an analytical AI for anomaly detection in systems.",
                },
                {
                    "role": "user",
                    "content": "Explain anomaly: high number of requests (120 per minute) from a single IP address.",
                },
            ],
            max_tokens=500,
            temperature=0.2,
        )

        # Execute request
        start_time = time.time()
        response = await llm_client.call(request)
        elapsed = time.time() - start_time

        # Show response info
        print_success(f"Request completed in {elapsed:.2f}s")
        print_info(f"Model: {response.model}")
        print_info(f"Mock mode: {response.is_mock}")
        print_info(f"Cached: {response.cached}")

        if response.usage:
            print_info(f"Token usage: {response.usage}")

        # Check if response is valid JSON
        try:
            content = json.loads(response.content)
            print_success("Received structured JSON response")

            # Check for key fields
            if all(k in content for k in ["analysis", "recommendations", "severity"]):
                print_success("Response contains all required fields")
            else:
                print_warning("Response is missing some required fields")

            if show_response:
                print("\n===== FULL RESPONSE =====")
                print(json.dumps(content, indent=2))
                print("========================\n")

        except json.JSONDecodeError:
            print_warning("Response is not valid JSON")
            if show_response:
                print("\n===== RESPONSE =====")
                print(
                    response.content[:300] + "..."
                    if len(response.content) > 300
                    else response.content
                )
                print("===================\n")

        # Test caching
        print_section("Cache Test")

        start_time = time.time()
        cache_response = await llm_client.call(request)
        elapsed = time.time() - start_time

        if cache_response.cached:
            print_success(f"Caching works correctly (response time: {elapsed:.2f}s)")
        else:
            print_warning(f"Caching did not work (response time: {elapsed:.2f}s)")

        # Test OpenAI Service integration
        print_section("OpenAI Service Test")

        try:
            from backend.ml.openai_service import OpenAIService

            print_info("Testing OpenAIService integration")
            openai_service = OpenAIService()
            await openai_service.initialize()

            print_success("OpenAIService initialized")

            # Test health check
            health = await openai_service.health_check()
            print_info(f"Health check: {health}")

            # Test analyze_anomaly method
            print_info("Testing analyze_anomaly method...")
            anomaly_data = {
                "event_type": "connection_spike",
                "timestamp": datetime.now().isoformat(),
                "details": {
                    "ip_address": "192.168.1.42",
                    "request_count": 120,
                    "timeframe_seconds": 60,
                    "user_id": "test_user",
                    "normal_baseline": 20,
                },
            }

            try:
                anomaly_result = await openai_service.analyze_anomaly(anomaly_data)
                print_success("analyze_anomaly method works")

                if show_response:
                    print("\n===== ANOMALY ANALYSIS =====")
                    if isinstance(anomaly_result, dict):
                        print(json.dumps(anomaly_result, indent=2))
                    else:
                        print(str(anomaly_result))
                    print("===========================\n")
            except Exception as e:
                print_error(f"Error in analyze_anomaly: {e}")

        except ImportError as e:
            print_error(f"Could not import OpenAIService: {e}")
        except Exception as e:
            print_error(f"Error testing OpenAIService: {e}")

        return True

    except ImportError as e:
        print_error(f"Import error: {e}")
    except Exception as e:
        print_error(f"Unexpected error: {e}")
        import traceback

        print(traceback.format_exc())

    return False


async def test_backend_endpoints():
    """Test Backend API endpoints related to ML/OpenAI"""
    print_header("BACKEND API ENDPOINTS TEST")

    import requests

    endpoints = {
        "Health": "http://localhost:8000/health",
        "ML Health": "http://localhost:8000/ml/health",
        "OpenAI Health": "http://localhost:8000/ml/openai/health",
        "ML Metrics": "http://localhost:8000/ml/metrics",
    }

    results = []

    for name, url in endpoints.items():
        print_section(f"Testing {name}")

        try:
            response = requests.get(url, timeout=5)
            if response.status_code == 200:
                print_success(f"{name}: {response.status_code} OK")

                if "application/json" in response.headers.get("content-type", ""):
                    data = response.json()

                    # Different handling for different endpoints
                    if "health" in url.lower():
                        if data.get("status") in ["ok", "healthy"]:
                            print_success(f"Status: {data.get('status')}")
                        else:
                            print_warning(f"Status: {data.get('status')}")

                    # For OpenAI specific endpoint
                    if "openai" in url.lower():
                        if data.get("mock_mode") is True:
                            print_warning("OpenAI is working in mock mode")
                        else:
                            print_success("OpenAI is using real API")

                results.append({"endpoint": name, "status": "SUCCESS"})
            else:
                print_warning(f"{name}: {response.status_code}")
                results.append({"endpoint": name, "status": "WARNING"})

        except requests.RequestException as e:
            print_error(f"{name}: Unavailable - {e}")
            results.append({"endpoint": name, "status": "FAILED"})

    # Results summary
    print_header("TEST RESULTS")

    success_count = sum(1 for r in results if r["status"] == "SUCCESS")
    total_count = len(results)

    for result in results:
        if result["status"] == "SUCCESS":
            status_color = GREEN
        elif result["status"] == "WARNING":
            status_color = YELLOW
        else:
            status_color = RED

        print(f"{status_color}  • {result['endpoint']:<15}: {result['status']}{RESET}")

    print()
    if success_count == total_count:
        print_success(f"All tests passed successfully! ({success_count}/{total_count})")
    elif success_count > 0:
        print_warning(f"Some tests passed ({success_count}/{total_count})")
    else:
        print_error(f"All tests failed (0/{total_count})")

    return success_count, total_count


async def main():
    parser = argparse.ArgumentParser(description="Test Universal OpenAI Adapter")
    parser.add_argument("--show-response", action="store_true", help="Show full API responses")
    parser.add_argument("--mock-only", action="store_true", help="Force mock mode")
    parser.add_argument("--skip-backend", action="store_true", help="Skip backend endpoint tests")

    args = parser.parse_args()

    # Load environment variables from .env if exists
    env_path = Path(__file__).parent.parent / ".env"
    if env_path.exists():
        print_info("Loading environment variables from .env")
        with open(env_path) as f:
            for line in f:
                if line.strip() and not line.startswith("#"):
                    try:
                        key, value = line.strip().split("=", 1)
                        os.environ[key] = value
                    except ValueError:
                        pass

    # Run tests
    adapter_ok = await test_universal_adapter(
        show_response=args.show_response, mock_only=args.mock_only
    )

    # Test backend endpoints if not skipped
    if not args.skip_backend:
        await test_backend_endpoints()

    # Final summary
    print_header("FINAL SUMMARY")
    if adapter_ok:
        print_success("Universal OpenAI adapter is working correctly")
    else:
        print_error("Universal OpenAI adapter has issues")

    print_info("Recommendations:")
    if adapter_ok:
        print_success("✓ The universal adapter is ready for production use")
        print_info("  • Use the adapter in your ML pipelines for OpenAI integration")
        print_info("  • Ensure environment variables are properly set in production")
    else:
        print_warning("✗ The universal adapter needs fixes before production use")
        print_info("  • Check environment variables for API keys")
        print_info("  • Verify the mock implementation works as expected")


if __name__ == "__main__":
    asyncio.run(main())
