#!/usr/bin/env python
"""
Minimal diagnostic script for OpenAI Wrapper
Tests only essential functionality with detailed error handling
"""
import asyncio
import os
import sys
from pathlib import Path

# Add project directory to path
sys.path.append(str(Path(__file__).parent.parent))

# Setup logging
log_file = Path(__file__).parent.parent / "logs" / "openai_minimal_test.log"
log_file.parent.mkdir(exist_ok=True)


def log(msg):
    """Write to both console and log file"""
    print(msg)
    with open(log_file, "a") as f:
        f.write(f"{msg}\n")


async def diagnose_openai_wrapper():
    """Run minimal diagnostics on OpenAI wrapper"""
    log("\n=== OPENAI WRAPPER MINIMAL DIAGNOSTIC ===\n")

    try:
        log("1. Importing modules...")

        # Attempt importing the wrapper module
        try:
            from backend.ml.openai_wrapper import (
                LLMRequest,
                initialize_llm_client,
                llm_client,
            )

            log("✓ Successfully imported openai_wrapper module")
        except ImportError as e:
            log(f"✗ Failed to import openai_wrapper: {e}")
            log(f"  - Python path: {sys.path}")
            return

        # Check module attributes
        log("\n2. Checking module attributes...")

        try:
            log(f"  - initialize_llm_client: {initialize_llm_client}")
            log(f"  - llm_client: {llm_client}")
            log(f"  - LLMRequest: {LLMRequest}")
        except Exception as e:
            log(f"✗ Error checking module attributes: {e}")
            return

        # Examine initial llm_client state
        log("\n3. Examining initial llm_client state...")

        try:
            log(f"  - Type: {type(llm_client)}")
            log(f"  - Dir: {dir(llm_client)}")

            # Check if key attributes exist before accessing them
            if hasattr(llm_client, "api_key"):
                masked_key = (
                    llm_client.api_key[:5] + "..." + llm_client.api_key[-4:]
                    if llm_client.api_key
                    else "None"
                )
                log(f"  - api_key: {masked_key}")
            else:
                log("  - api_key attribute doesn't exist")

            if hasattr(llm_client, "mock_only"):
                log(f"  - mock_only: {llm_client.mock_only}")
            else:
                log("  - mock_only attribute doesn't exist")

            if hasattr(llm_client, "cache_enabled"):
                log(f"  - cache_enabled: {llm_client.cache_enabled}")
            else:
                log("  - cache_enabled attribute doesn't exist")

        except Exception as e:
            log(f"✗ Error examining llm_client: {e}")

        # Initialize client
        log("\n4. Initializing client...")

        try:
            success = await initialize_llm_client()
            log(f"✓ initialize_llm_client() returned: {success}")
        except Exception as e:
            log(f"✗ Error initializing client: {e}")
            import traceback

            log(traceback.format_exc())
            return

        # Re-examine client state after initialization
        log("\n5. Examining client after initialization...")

        try:
            log(f"  - Type: {type(llm_client)}")
            log(f"  - Dir: {dir(llm_client)}")

            if hasattr(llm_client, "api_key"):
                masked_key = (
                    llm_client.api_key[:5] + "..." + llm_client.api_key[-4:]
                    if llm_client.api_key
                    else "None"
                )
                log(f"  - api_key: {masked_key}")
            else:
                log("  - api_key attribute doesn't exist")

            if hasattr(llm_client, "mock_only"):
                log(f"  - mock_only: {llm_client.mock_only}")
            else:
                log("  - mock_only attribute doesn't exist")

            if hasattr(llm_client, "cache_enabled"):
                log(f"  - cache_enabled: {llm_client.cache_enabled}")
            else:
                log("  - cache_enabled attribute doesn't exist")

        except Exception as e:
            log(f"✗ Error examining llm_client after initialization: {e}")

        # Try a simple call if client is initialized
        log("\n6. Testing simple API call...")

        try:
            request = LLMRequest(
                model="gpt-3.5-turbo",
                messages=[{"role": "user", "content": "Hello, this is a test."}],
                max_tokens=50,
            )

            log("  - Created LLMRequest")
            log("  - Calling API...")

            response = await llm_client.call(request)

            log("✓ API call successful")
            log(
                f"  - Model: {response.model if hasattr(response, 'model') else 'unknown'}"
            )
            log(
                f"  - Mock: {response.is_mock if hasattr(response, 'is_mock') else 'unknown'}"
            )
            log(f"  - Content preview: {response.content[:50]}...")

        except Exception as e:
            log(f"✗ Error making API call: {e}")
            import traceback

            log(traceback.format_exc())

        log("\n=== DIAGNOSTIC COMPLETE ===")
        log(f"Full log written to: {log_file}")

    except Exception as e:
        log(f"✗ Unexpected error: {e}")
        import traceback

        log(traceback.format_exc())


# Load environment variables from .env
def load_env():
    env_path = Path(__file__).parent.parent / ".env"
    if env_path.exists():
        log(f"Loading environment from {env_path}")
        with open(env_path, "r") as f:
            for line in f:
                if line.strip() and not line.startswith("#"):
                    try:
                        key, value = line.strip().split("=", 1)
                        os.environ[key] = value
                        if key.endswith("API_KEY"):
                            masked = (
                                value[:5] + "..." + value[-4:]
                                if len(value) > 9
                                else "[masked]"
                            )
                            log(f"  - Set {key}={masked}")
                        else:
                            log(f"  - Set {key}={value}")
                    except ValueError:
                        pass
    else:
        log(f"No .env file found at {env_path}")


if __name__ == "__main__":
    log(f"Starting minimal OpenAI wrapper diagnostic at {Path(__file__).resolve()}")
    load_env()

    # Check if forcing mock mode
    if "--mock" in sys.argv:
        os.environ["OPENAI_MOCK_ONLY"] = "true"
        log("Forced OPENAI_MOCK_ONLY=true")

    asyncio.run(diagnose_openai_wrapper())
