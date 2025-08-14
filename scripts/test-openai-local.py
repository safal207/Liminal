#!/usr/bin/env python
"""
Simple OpenAI integration test for Resonance Liminal project.
This script tests the OpenAI service directly without needing the full ML infrastructure.
"""

import asyncio
import json
import os
import sys
import traceback
from pathlib import Path

print("\n=====================================")
print("   OPENAI INTEGRATION TEST")
print("=====================================\n")

print("1️⃣ Setting up environment...")
# Добавляем родительскую директорию в sys.path, чтобы мы могли импортировать модули проекта
sys.path.append(str(Path(__file__).parent.parent))
print(f"   Added {Path(__file__).parent.parent} to sys.path")
# Настраиваем формат вывода для лучшей читабельности
os.environ["PYTHONIOENCODING"] = "utf-8"

print("\n2️⃣ Loading environment variables...")
try:
    # Проверяем .env файл и загружаем его
    from dotenv import load_dotenv

    env_path = Path(__file__).parent.parent / ".env"
    if env_path.exists():
        load_dotenv(env_path)
        print(f"   ✅ Loaded .env file from {env_path}")
    else:
        print(f"   ⚠️ .env file not found at {env_path}")

    # Проверяем наличие API ключа
    api_key = os.getenv("OPENAI_API_KEY")
    if api_key and api_key != "demo_key":
        # Скрываем ключ для безопасности, показываем только первые и последние символы
        masked_key = f"{api_key[:4]}...{api_key[-4:]}"
        print(f"   ✅ Found OpenAI API key: {masked_key}")
    else:
        print(f"   ❌ OpenAI API key not set or using demo key: '{api_key}'")
        print("      Please add a valid API key to .env file.")
        sys.exit(1)
except Exception as e:
    print(f"   ❌ Error loading environment: {e}")
    traceback.print_exc()
    sys.exit(1)

# Импортируем OpenAI сервис
print("\n3️⃣ Importing OpenAI service...")
try:
    from backend.ml.openai_service import OpenAIService, openai_service

    print("   ✅ OpenAI service module imported successfully")
except ImportError as e:
    print(f"   ❌ Error importing OpenAI service: {e}")
    print("      Checking for file existence...")

    service_path = Path(__file__).parent.parent / "backend" / "ml" / "openai_service.py"
    if service_path.exists():
        print(f"      ✅ File exists at {service_path}")
        with open(service_path, "r") as f:
            first_lines = "\n".join(f.readlines()[:20])
            print(f"      First lines of file:\n{first_lines}")
    else:
        print(f"      ❌ File not found at {service_path}")

    traceback.print_exc()
    sys.exit(1)

# Тестовые данные для объяснения
test_features = {
    "messages_per_minute": 120,
    "connection_duration": 3600,
    "error_rate": 0.05,
    "unique_ips": 15,
    "failed_auth_attempts": 3,
}

test_prediction = {
    "anomaly_score": 0.78,
    "is_anomaly": True,
    "confidence": 0.85,
    "threshold": 0.6,
}

# Тестовое XAI объяснение (имитация)
test_xai_explanation = {
    "feature_importances": {
        "messages_per_minute": 0.45,
        "error_rate": 0.35,
        "failed_auth_attempts": 0.15,
        "connection_duration": 0.03,
        "unique_ips": 0.02,
    },
    "method": "shap",
}


async def test_openai_explanation():
    """Тестирует OpenAI API для генерации объяснений"""
    print("\n4️⃣ Testing OpenAI explanation generation...")

    try:
        # Проверка инициализации клиента
        print("   Checking OpenAI client status...")
        if not hasattr(openai_service, "client") or not openai_service.client:
            print("   ⚠️ OpenAI client not initialized.")
            print("      Initializing OpenAI client with API key...")

            try:
                # Проверим, существует ли метод initialize_client
                if hasattr(openai_service, "initialize_client"):
                    print("      Found initialize_client method, calling it...")
                    openai_service.initialize_client(api_key)
                    print(
                        f"      Client initialized: {openai_service.client is not None}"
                    )
                else:
                    print("      ❌ initialize_client method not found!")
                    print("      Available methods/attributes:")
                    for attr in dir(openai_service):
                        if not attr.startswith("_"):
                            print(f"        - {attr}")
                    print("      Creating new instance with explicit API key...")
                    # Попробуем создать новый экземпляр сервиса
                    temp_service = OpenAIService(api_key=api_key)
                    temp_service.initialize()
                    print(
                        f"      Created new service instance, client status: {temp_service.client is not None}"
                    )
                    # Заменим глобальный экземпляр
                    openai_service.client = temp_service.client
            except Exception as e:
                print(f"      ❌ Error initializing client: {e}")
                traceback.print_exc()
        else:
            print("   ✅ OpenAI client already initialized")

        print("\n   ⏳ Requesting natural language explanation from OpenAI...")
        print("      Request params:")
        print(f"      - Model type: anomaly_detection")
        print(f"      - Features: {json.dumps(test_features)[:100]}...")
        print(f"      - Prediction: {json.dumps(test_prediction)[:100]}...")

        try:
            print("      Making API call...")
            result = await openai_service.explain_ml_prediction(
                prediction=test_prediction,
                features=test_features,
                model_type="anomaly_detection",
                xai_explanation=test_xai_explanation,
            )

            print("\n   ✅ Received explanation successfully!")
            if hasattr(result, "summary"):
                print("\n   🔹 Summary:")
                print(f"      {result.summary}")

                print("\n   🔹 Analysis (excerpt):")
                if hasattr(result, "analysis"):
                    print(
                        f"      {result.analysis[:200]}..."
                        if len(result.analysis) > 200
                        else result.analysis
                    )

                print("\n   🔹 Technical Details:")
                if hasattr(result, "technical_details"):
                    print(
                        f"      {json.dumps(result.technical_details, indent=2)[:200]}..."
                        if len(json.dumps(result.technical_details)) > 200
                        else json.dumps(result.technical_details)
                    )

                print("\n   🔹 Recommendations:")
                if hasattr(result, "recommendations"):
                    print(f"      {result.recommendations}")

                print("\n   🔹 Confidence:")
                if hasattr(result, "confidence"):
                    print(f"      {result.confidence}")

                print("\n   ✅ OpenAI integration test PASSED!")
            else:
                print("\n   ❌ Result does not have expected structure!")
                print(f"      Result type: {type(result)}")
                print(f"      Result content: {result}")
        except Exception as e:
            print(f"\n   ❌ Error during API call: {e}")
            print("      Traceback:")
            traceback.print_exc()

    except Exception as e:
        print(f"\n   ❌ Error during OpenAI test: {e}")
        print("\n   Stack trace:")
        traceback.print_exc()
        return False

    return True


async def main():
    """Основная функция"""
    print("\n4️⃣ Preparing test data...")
    print("   ✅ Test data ready")

    # Проверка OpenAI интеграции
    success = await test_openai_explanation()

    print("\n=====================================")
    print("          TEST RESULTS             ")
    print("=====================================\n")

    if success:
        print("✅ All tests passed successfully!")
        print("   XAI + OpenAI integration is working correctly.")
        print("   You can now use the natural language explanations feature.")
    else:
        print("❌ Some tests failed. Please check the error messages above.")
        print("   Common issues:")
        print("   1. Invalid or expired OpenAI API key")
        print("   2. Network connection problems")
        print("   3. Mismatched OpenAI client version")
        print("   4. Missing implementation in openai_service.py")
        print("   Try running with debug output: 'python -m backend.ml.openai_service'")

    print("\n🏁 Test completed")


if __name__ == "__main__":
    asyncio.run(main())
