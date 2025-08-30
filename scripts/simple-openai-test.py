#!/usr/bin/env python
"""
Simplified Universal OpenAI Adapter Test
Outputs results to both console and a log file for reliable diagnostics
"""

import asyncio
import json
import os
import sys
import time
from datetime import datetime
from pathlib import Path

# Add project directory to path
sys.path.append(str(Path(__file__).parent.parent))

# Setup logging to file
log_dir = Path(__file__).parent.parent / "logs"
log_dir.mkdir(exist_ok=True)
log_file = log_dir / f"openai_test_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"


def log(message):
    """Log message to both console and file"""
    print(message)
    with open(log_file, "a", encoding="utf-8") as f:
        f.write(f"{message}\n")


async def test_adapter():
    """Run basic tests on the universal adapter"""
    log("\n=== UNIVERSAL OPENAI ADAPTER TEST ===\n")
    log(f"Test time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    log(f"Log file: {log_file}")

    # Check environment
    log("\n--- Environment ---")
    openai_key = os.environ.get("OPENAI_API_KEY", "")
    anthropic_key = os.environ.get("ANTHROPIC_API_KEY", "")
    xai_key = os.environ.get("XAI_API_KEY", "")

    # Mask keys for display
    def mask(key):
        if not key or len(key) < 8:
            return "not found"
        return f"{key[:5]}...{key[-4:]}"

    log(f"OpenAI API Key: {mask(openai_key)}")
    log(f"Anthropic API Key: {mask(anthropic_key)}")
    log(f"XAI API Key: {mask(xai_key)}")

    # Load universal adapter
    try:
        log("\n--- Loading Adapter ---")
        from backend.ml.openai_wrapper import (
            LLMRequest,
            initialize_llm_client,
            llm_client,
        )

        log("✓ Universal adapter module loaded")

        # Initialize client
        log("\n--- Initializing Client ---")
        start = time.time()
        success = await initialize_llm_client()
        elapsed = time.time() - start

        if success:
            log(f"✓ Client initialized with real API ({elapsed:.2f}s)")
        else:
            log(f"⚠ Client initialized in mock mode ({elapsed:.2f}s)")

        log(f"API key: {mask(llm_client.api_key)}")
        log(f"Mock only: {llm_client.mock_only}")
        log(f"Cache enabled: {llm_client.cache_enabled}")

        # Test request
        log("\n--- Test Request ---")
        request = LLMRequest(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are an analytical AI."},
                {
                    "role": "user",
                    "content": "Explain anomaly: high CPU usage spikes at regular intervals.",
                },
            ],
            max_tokens=300,
            temperature=0.2,
        )

        log("Sending request...")
        start = time.time()
        response = await llm_client.call(request)
        elapsed = time.time() - start

        log(f"✓ Request completed in {elapsed:.2f}s")
        log(f"Model: {response.model}")
        log(f"Mock mode: {response.is_mock}")
        log(f"Cached: {response.cached}")

        if response.usage:
            log(f"Token usage: {response.usage}")

        # Parse response
        try:
            content = json.loads(response.content)
            log("\n--- Response Content ---")
            log(json.dumps(content, indent=2, ensure_ascii=False))
        except json.JSONDecodeError:
            log("\n--- Raw Response ---")
            log(response.content[:500] + "..." if len(response.content) > 500 else response.content)

        # Test caching
        log("\n--- Cache Test ---")
        start = time.time()
        cache_response = await llm_client.call(request)
        elapsed = time.time() - start

        if cache_response.cached:
            log(f"✓ Cache working ({elapsed:.2f}s)")
        else:
            log(f"⚠ Cache not working ({elapsed:.2f}s)")

        # Test OpenAI Service
        log("\n--- OpenAI Service Integration ---")
        try:
            from backend.ml.openai_service import OpenAIService

            service = OpenAIService()
            await service.initialize()
            log("✓ OpenAI Service initialized")

            health = await service.health_check()
            log(f"Health check: {health}")

            # Test analyze_anomaly
            anomaly_data = {
                "event_type": "high_cpu",
                "timestamp": datetime.now().isoformat(),
                "details": {
                    "server": "app-server-01",
                    "cpu_percent": 95,
                    "duration_seconds": 120,
                    "pattern": "periodic",
                },
            }

            log("Testing analyze_anomaly...")
            try:
                result = await service.analyze_anomaly(anomaly_data)
                log("✓ analyze_anomaly succeeded")
                log("\n--- Anomaly Analysis Result ---")
                if isinstance(result, dict):
                    log(json.dumps(result, indent=2, ensure_ascii=False))
                else:
                    log(str(result))
            except Exception as e:
                log(f"⚠ analyze_anomaly failed: {e}")

        except Exception as e:
            log(f"⚠ OpenAI Service test failed: {e}")

        log("\n=== TEST COMPLETE ===")
        log("Results saved to log file")
        return True

    except ImportError as e:
        log(f"✗ Import error: {e}")
    except Exception as e:
        log(f"✗ Unexpected error: {e}")
        import traceback

        log(traceback.format_exc())

    return False


if __name__ == "__main__":
    log("Starting universal OpenAI adapter test...")

    # Load .env if exists
    env_path = Path(__file__).parent.parent / ".env"
    if env_path.exists():
        log("Loading environment from .env file")
        with open(env_path) as f:
            for line in f:
                if line.strip() and not line.startswith("#"):
                    try:
                        key, value = line.strip().split("=", 1)
                        os.environ[key] = value
                    except ValueError:
                        pass

    # Force mock mode if needed
    if "--mock" in sys.argv:
        log("Forcing mock mode")
        os.environ["OPENAI_MOCK_ONLY"] = "true"

    asyncio.run(test_adapter())
