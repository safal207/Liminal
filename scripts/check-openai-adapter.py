#!/usr/bin/env python
"""
Simple diagnostic script for Universal OpenAI Adapter
"""
import asyncio
import json
import os
import sys
import traceback
from datetime import datetime
from pathlib import Path

# Add project root to path
sys.path.append(str(Path(__file__).parent.parent))

print("\n======== UNIVERSAL OPENAI ADAPTER CHECK ========\n")

# Create log file
log_dir = Path(__file__).parent.parent / "logs"
log_dir.mkdir(exist_ok=True)
log_file = (
    log_dir / f"openai_adapter_check_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
)


def log_msg(msg):
    """Print to console and log file"""
    print(msg)
    with open(log_file, "a", encoding="utf-8") as f:
        f.write(f"{msg}\n")


log_msg(f"Log file: {log_file}")

# Load .env if available
try:
    from dotenv import load_dotenv

    env_path = Path(__file__).parent.parent / ".env"
    if env_path.exists():
        load_dotenv(env_path)
        log_msg("[OK] Loaded .env file")
    else:
        log_msg("[INFO] .env file not found")
except ImportError:
    log_msg("[INFO] dotenv not installed")

# Check environment variables
log_msg("\n--- API Keys ---")
openai_key = os.environ.get("OPENAI_API_KEY", "")
anthropic_key = os.environ.get("ANTHROPIC_API_KEY", "")
xai_key = os.environ.get("XAI_API_KEY", "")


def mask_api_key(key):
    if not key or len(key) < 8:
        return "not found"
    return f"{key[:5]}...{key[-4:]}"


log_msg(f"OpenAI API Key: {mask_api_key(openai_key)}")
log_msg(f"Anthropic API Key: {mask_api_key(anthropic_key)}")
log_msg(f"XAI API Key: {mask_api_key(xai_key)}")

# Check adapter settings
log_msg("\n--- Adapter Configuration ---")
mock_only = os.environ.get("OPENAI_MOCK_ONLY", "").lower() in ["true", "1", "yes"]
cache_ttl = os.environ.get("OPENAI_CACHE_TTL", "3600")

log_msg(f"OPENAI_MOCK_ONLY: {mock_only}")
log_msg(f"OPENAI_CACHE_TTL: {cache_ttl}")

# Import and test the adapter
log_msg("\n--- Testing Adapter ---")


async def run_tests():
    # Try to import the wrapper module
    try:
        log_msg("Importing openai_wrapper...")
        from backend.ml.openai_wrapper import (LLMRequest,
                                               initialize_llm_client,
                                               llm_client)

        log_msg("[OK] Module imported successfully")

        # Initialize the client
        log_msg("\nInitializing LLM client...")
        start_time = datetime.now()
        api_success = await initialize_llm_client()
        elapsed = (datetime.now() - start_time).total_seconds()

        if api_success:
            log_msg(f"[OK] Client initialized with real API in {elapsed:.2f}s")
        else:
            log_msg(f"[INFO] Client initialized in mock mode in {elapsed:.2f}s")

        # Check client attributes safely
        log_msg("\nClient configuration:")

        # Check attributes with proper error handling
        try:
            api_key = getattr(llm_client, "api_key", None)
            log_msg(f"API key: {mask_api_key(api_key) if api_key else 'None'}")
        except Exception as e:
            log_msg(f"[ERROR] Could not access api_key: {e}")

        try:
            mock_mode = getattr(llm_client, "mock_only", None)
            log_msg(f"Mock mode: {mock_mode}")
        except Exception as e:
            log_msg(f"[ERROR] Could not access mock_only: {e}")

        try:
            cache_enabled = getattr(llm_client, "cache_enabled", None)
            log_msg(f"Cache enabled: {cache_enabled}")
        except Exception as e:
            log_msg(f"[ERROR] Could not access cache_enabled: {e}")

        # Test an API call
        log_msg("\nTesting API call...")
        request = LLMRequest(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are an AI assistant."},
                {
                    "role": "user",
                    "content": "Return a simple JSON with keys: status, message",
                },
            ],
            max_tokens=100,
        )

        start_time = datetime.now()
        response = await llm_client.call(request)
        elapsed = (datetime.now() - start_time).total_seconds()

        log_msg(f"[OK] Request completed in {elapsed:.2f}s")
        log_msg(f"Model: {getattr(response, 'model', 'unknown')}")
        log_msg(f"Mock: {getattr(response, 'is_mock', 'unknown')}")
        log_msg(f"Cached: {getattr(response, 'cached', 'unknown')}")

        if hasattr(response, "usage"):
            log_msg(f"Usage: {response.usage}")

        # Try to parse the response content
        try:
            content = json.loads(response.content)
            log_msg("\n[OK] Response is valid JSON:")
            log_msg(json.dumps(content, indent=2))
        except:
            log_msg("\n[INFO] Raw response content:")
            content_preview = response.content[:300]
            if len(response.content) > 300:
                content_preview += "..."
            log_msg(content_preview)

        # Test caching
        log_msg("\nTesting caching...")
        start_time = datetime.now()
        cached_response = await llm_client.call(request)
        elapsed = (datetime.now() - start_time).total_seconds()

        if getattr(cached_response, "cached", False):
            log_msg(f"[OK] Cache working (response time: {elapsed:.2f}s)")
        else:
            log_msg(f"[INFO] Cache not used (response time: {elapsed:.2f}s)")

        # Try to test integration with OpenAIService
        log_msg("\nTesting OpenAI Service integration...")
        try:
            from backend.ml.openai_service import OpenAIService

            service = OpenAIService()
            await service.initialize()
            log_msg("[OK] OpenAI Service initialized")

            health = await service.health_check()
            log_msg(f"Health check: {health}")

            # Test analyze_anomaly method if available
            try:
                anomaly_data = {
                    "event_type": "connection_spike",
                    "timestamp": datetime.now().isoformat(),
                    "details": {
                        "ip_address": "192.168.1.100",
                        "request_count": 120,
                        "interval_seconds": 60,
                    },
                }

                log_msg("Testing analyze_anomaly method...")
                result = await service.analyze_anomaly(anomaly_data)

                log_msg("[OK] analyze_anomaly method successful")
                log_msg("\nAnalysis result:")
                if isinstance(result, dict):
                    log_msg(json.dumps(result, indent=2))
                else:
                    log_msg(str(result))

            except Exception as e:
                log_msg(f"[ERROR] analyze_anomaly test failed: {e}")

        except ImportError as e:
            log_msg(f"[ERROR] Could not import OpenAI Service: {e}")
        except Exception as e:
            log_msg(f"[ERROR] OpenAI Service test failed: {e}")

        # Final summary
        log_msg("\n======== TEST RESULTS ========")
        if api_success:
            log_msg("[OK] Universal adapter is working with real API")
        elif mock_only:
            log_msg("[OK] Universal adapter is working in mock mode (as configured)")
        else:
            log_msg("[WARNING] Universal adapter is in mock mode (unintended)")

    except ImportError as e:
        log_msg(f"[ERROR] Could not import adapter module: {e}")
    except Exception as e:
        log_msg(f"[ERROR] Unexpected error: {e}")
        log_msg(traceback.format_exc())

    log_msg("\nCheck completed, see log file for details")


if __name__ == "__main__":
    # For Windows console compatibility
    os.system("")  # Enable VT100 escape sequences

    # Force mock mode if requested
    if len(sys.argv) > 1 and sys.argv[1] == "--mock":
        os.environ["OPENAI_MOCK_ONLY"] = "true"
        log_msg("[INFO] Forced mock mode enabled")

    # Run the tests
    log_msg("Running adapter tests...")
    asyncio.run(run_tests())

    log_msg(f"\nLog file saved to: {log_file}")
