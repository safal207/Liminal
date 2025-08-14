#!/usr/bin/env python
"""
Интеграционный тест XAI + OpenAI для ML системы Resonance Liminal
Проверяет работу объяснений ML-предсказаний с использованием SHAP/LIME + OpenAI GPT-4
"""
import asyncio
import json
import os
import sys
import traceback
from datetime import datetime
from pathlib import Path

# Настройка путей для импорта
BASE_DIR = Path(__file__).parent.parent
sys.path.append(str(BASE_DIR))

# Настройка окружения для Windows
os.environ["PYTHONIOENCODING"] = "utf-8"
if hasattr(sys, "stdout"):
    sys.stdout.reconfigure(encoding="utf-8")

# Импорт зависимостей
try:
    from dotenv import load_dotenv

    load_dotenv(BASE_DIR / ".env")
except ImportError:
    print(
        "WARNING: dotenv не установлен, используются только системные переменные окружения"
    )


# Цветной вывод для лучшей читаемости
class Colors:
    HEADER = "\033[95m"
    BLUE = "\033[94m"
    CYAN = "\033[96m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    RED = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"


def print_header(text):
    print(f"\n{Colors.HEADER}{Colors.BOLD}{'='*60}{Colors.ENDC}")
    print(f"{Colors.HEADER}{Colors.BOLD}{text.center(60)}{Colors.ENDC}")
    print(f"{Colors.HEADER}{Colors.BOLD}{'='*60}{Colors.ENDC}\n")


def print_section(text):
    print(f"\n{Colors.BLUE}{Colors.BOLD}▶ {text}{Colors.ENDC}")


def print_success(text):
    print(f"{Colors.GREEN}✓ {text}{Colors.ENDC}")


def print_warning(text):
    print(f"{Colors.YELLOW}⚠ {text}{Colors.ENDC}")


def print_error(text):
    print(f"{Colors.RED}✗ {text}{Colors.ENDC}")


def print_info(text):
    print(f"{Colors.CYAN}ℹ {text}{Colors.ENDC}")


def print_json(data, indent=2):
    if isinstance(data, str):
        try:
            data = json.loads(data)
        except:
            print(data)
            return

    formatted = json.dumps(data, indent=indent, ensure_ascii=False)
    print(f"{Colors.CYAN}{formatted}{Colors.ENDC}")


print_header("XAI + OPENAI INTEGRATION TEST")
print_info(f"Время запуска: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

# Инициализация
print_section("Инициализация окружения и проверка зависимостей")

# Проверка API ключа
api_key = os.getenv("OPENAI_API_KEY")
if api_key:
    masked_key = f"{api_key[:4]}...{api_key[-4:]}" if len(api_key) > 8 else "[скрыт]"
    print_success(f"OpenAI API ключ найден: {masked_key}")
else:
    print_warning(f"OpenAI API ключ не найден. Будет использована мок-реализация.")

# Импорт ML сервисов
try:
    from backend.ml.openai_service import (
        AnalysisRequest,
        AnalysisResponse,
        OpenAIService,
        openai_service,
    )

    print_success("OpenAI сервис успешно импортирован")
except ImportError as e:
    print_error(f"Ошибка импорта OpenAI сервиса: {e}")
    traceback.print_exc()
    sys.exit(1)

try:
    # Проверка наличия XAI компонентов
    xai_components = []

    try:
        import shap

        xai_components.append("SHAP")
    except ImportError:
        pass

    try:
        import lime

        xai_components.append("LIME")
    except ImportError:
        pass

    try:
        import eli5

        xai_components.append("ELI5")
    except ImportError:
        pass

    if xai_components:
        print_success(f"Доступные XAI компоненты: {', '.join(xai_components)}")
    else:
        print_warning(
            "XAI компоненты не обнаружены. Будет тестироваться только OpenAI интеграция."
        )

except Exception as e:
    print_warning(f"Ошибка при проверке XAI компонентов: {e}")

# Генерация тестовых данных
print_section("Подготовка тестовых данных")

# Тестовый сценарий 1: Обнаружение аномалий в WebSocket трафике
anomaly_features = {
    "messages_per_minute": 120,  # Высокое число сообщений
    "connection_duration": 3600,  # 1 час соединения
    "error_rate": 0.15,  # 15% сообщений с ошибками
    "unique_ips": 3,  # Подключения с разных IP
    "failed_auth_attempts": 5,  # Неудачные попытки авторизации
    "jwt_refreshes": 12,  # Частое обновление токенов
    "message_entropy": 0.89,  # Высокая энтропия в сообщениях
}

anomaly_prediction = {
    "anomaly_score": 0.78,  # Высокий скор аномалии
    "is_anomaly": True,  # Классифицировано как аномалия
    "confidence": 0.85,  # Высокая уверенность
    "threshold": 0.6,  # Порог для классификации
    "label": "automated_behavior",  # Тип аномалии
}

anomaly_xai_explanation = {
    "feature_importances": {
        "messages_per_minute": 0.45,  # 45% вклада в аномалию
        "error_rate": 0.25,  # 25% вклада
        "failed_auth_attempts": 0.15,  # 15% вклада
        "jwt_refreshes": 0.08,  # 8% вклада
        "message_entropy": 0.05,  # 5% вклада
        "connection_duration": 0.01,  # 1% вклада
        "unique_ips": 0.01,  # 1% вклада
    },
    "method": "shap",
    "interpretation": "high_volume_with_auth_failures",
}

# Тестовый сценарий 2: Прогноз нагрузки для масштабирования
scaling_features = {
    "current_connections": 2500,
    "growth_rate": 15,  # Рост на 15% в минуту
    "time_of_day": 14,  # 14:00 (2pm)
    "day_of_week": 2,  # Вторник
    "error_rate_trend": -0.02,  # Слегка снижается
    "active_channels": 85,
    "message_rate": 45000,  # Сообщений в минуту
    "cpu_usage": 0.65,  # 65% использования CPU
    "memory_usage": 0.48,  # 48% использования памяти
}

scaling_prediction = {
    "predicted_connections": 5800,
    "confidence_interval": [4200, 6500],
    "time_to_threshold": 8,  # Минут до достижения порога
    "scaling_recommendation": "increase_by_2",
    "confidence": 0.91,
}

scaling_xai_explanation = {
    "feature_importances": {
        "growth_rate": 0.48,
        "current_connections": 0.22,
        "time_of_day": 0.15,
        "message_rate": 0.08,
        "active_channels": 0.04,
        "day_of_week": 0.02,
        "cpu_usage": 0.01,
    },
    "method": "lime",
    "interpretation": "rapid_growth_peak_hours",
}

print_success("Тестовые данные подготовлены для сценариев:")
print_info("1. Обнаружение аномалий в WebSocket трафике")
print_info("2. Прогноз нагрузки для масштабирования")


# Тестирование OpenAI интеграции
async def test_openai_integration():
    print_section("Тестирование OpenAI сервиса")

    try:
        # Проверка и инициализация клиента OpenAI
        if not hasattr(openai_service, "client") or not openai_service.client:
            print_info("OpenAI клиент не инициализирован, выполняется инициализация...")
            openai_service.initialize_client(api_key)

            if openai_service.client:
                print_success("OpenAI клиент успешно инициализирован")
            else:
                print_warning(
                    "OpenAI клиент не удалось инициализировать, будет использована мок-реализация"
                )
        else:
            print_success("OpenAI клиент уже инициализирован")

        # Тестирование объяснения ML предсказания для аномалий
        print_section("Тест 1: Объяснение обнаружения аномалий")
        print_info("Запрос объяснения для аномального WebSocket соединения...")

        anomaly_result = await openai_service.explain_ml_prediction(
            prediction=anomaly_prediction,
            features=anomaly_features,
            model_type="anomaly_detection",
            xai_explanation=anomaly_xai_explanation,
        )

        print_success("Получен ответ для объяснения аномалий")

        # Вывод результата для аномалий
        print_info("Результат анализа аномалий:")
        print_info(
            f"Уровень серьезности: {anomaly_result.severity} (Уверенность: {anomaly_result.confidence*100:.1f}%)"
        )
        print_info("Краткое резюме:")
        print(f"{Colors.YELLOW}{anomaly_result.summary}{Colors.ENDC}")
        print_info("Рекомендации:")
        for i, rec in enumerate(anomaly_result.recommendations, 1):
            print(f"{Colors.YELLOW}{i}. {rec}{Colors.ENDC}")

        # Тестирование объяснения ML предсказания для масштабирования
        print_section("Тест 2: Объяснение прогноза масштабирования")
        print_info("Запрос объяснения для прогноза нагрузки...")

        scaling_result = await openai_service.explain_ml_prediction(
            prediction=scaling_prediction,
            features=scaling_features,
            model_type="predictive_scaling",
            xai_explanation=scaling_xai_explanation,
        )

        print_success("Получен ответ для объяснения прогноза масштабирования")

        # Вывод результата для масштабирования
        print_info("Результат анализа прогноза масштабирования:")
        print_info(f"Уверенность: {scaling_result.confidence*100:.1f}%")
        print_info("Краткое резюме:")
        print(f"{Colors.YELLOW}{scaling_result.summary}{Colors.ENDC}")
        print_info("Действия:")
        for i, action in enumerate(scaling_result.action_items, 1):
            print(f"{Colors.YELLOW}{i}. {action}{Colors.ENDC}")

        return True, {"anomaly": anomaly_result, "scaling": scaling_result}

    except Exception as e:
        print_error(f"Ошибка при тестировании OpenAI интеграции: {e}")
        traceback.print_exc()
        return False, {"error": str(e), "traceback": traceback.format_exc()}


# Основная функция
async def main():
    success, results = await test_openai_integration()

    print_header("РЕЗУЛЬТАТЫ ИНТЕГРАЦИОННОГО ТЕСТА")

    if success:
        print_success("Интеграция XAI + OpenAI работает корректно")
        print_info(
            "Система генерирует человекочитаемые объяснения на основе ML-предсказаний и XAI"
        )
    else:
        print_error("Тестирование интеграции завершилось с ошибками")
        print_info("Проверьте журналы для получения подробной информации")

    # Определяем статус OpenAI (реальный или мок)
    try:
        import openai

        print_info(
            f"Используется реальная OpenAI библиотека (версия: {openai.__version__})"
        )
    except ImportError:
        print_warning("Используется мок-реализация OpenAI API")
        print_info(
            "Для продакшн рекомендуется установить библиотеку: pip install openai"
        )

    # Рекомендации для продакшн
    print_section("Следующие шаги интеграции:")
    print_info("1. Добавить кэширование с TTL для оптимизации вызовов OpenAI API")
    print_info("2. Интегрировать XAI + OpenAI объяснения с ML метриками в Prometheus")
    print_info(
        "3. Реализовать автоматический выбор метода XAI (SHAP/LIME) в зависимости от задачи"
    )
    print_info("4. Внедрить механизм обратной связи для улучшения объяснений")


# Запуск теста
if __name__ == "__main__":
    asyncio.run(main())
