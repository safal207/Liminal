#!/usr/bin/env python
"""
Расширенный тест интеграции XAI + OpenAI для системы Resonance Liminal
"""
import asyncio
import json
import os
import sys
import traceback
from pathlib import Path
from pprint import pprint

# Настройка кодировки для Windows
os.environ["PYTHONIOENCODING"] = "utf-8"
sys.stdout.reconfigure(encoding="utf-8")

# Добавление проектных путей
BASE_DIR = Path(__file__).parent.parent
sys.path.append(str(BASE_DIR))


# Инициализация цветных выводов для лучшей читаемости
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
    print(f"\n{Colors.HEADER}{Colors.BOLD}{'='*50}{Colors.ENDC}")
    print(f"{Colors.HEADER}{Colors.BOLD}{text.center(50)}{Colors.ENDC}")
    print(f"{Colors.HEADER}{Colors.BOLD}{'='*50}{Colors.ENDC}\n")


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


def print_json(data):
    formatted = json.dumps(data, indent=2, ensure_ascii=False)
    print(f"{Colors.CYAN}{formatted}{Colors.ENDC}")


print_header("XAI + OPENAI INTEGRATION TEST")

print_section("Инициализация окружения")

# Загрузка переменных окружения
try:
    from dotenv import load_dotenv

    load_dotenv(BASE_DIR / ".env")
    print_success("Переменные окружения загружены")
except ImportError:
    print_warning(
        "dotenv не установлен, используются только системные переменные окружения"
    )

# Проверка API ключа
api_key = os.getenv("OPENAI_API_KEY")
if api_key:
    masked_key = f"{api_key[:4]}...{api_key[-4:]}" if len(api_key) > 8 else "[скрыт]"
    print_success(f"OpenAI API ключ найден: {masked_key}")
else:
    print_error("OpenAI API ключ отсутствует")

# Импорт сервисов
print_section("Импорт ML и OpenAI сервисов")

try:
    from backend.ml.openai_service import (
        AnalysisResponse,
        OpenAIService,
        openai_service,
    )

    print_success("OpenAI сервис импортирован")
except ImportError as e:
    print_error(f"Ошибка импорта OpenAI сервиса: {e}")
    traceback.print_exc()
    sys.exit(1)

# Подготовка тестовых данных
print_section("Подготовка тестовых данных")

# Тестовые метрики для аномального соединения
test_features = {
    "messages_per_minute": 120,  # Аномально высокое число сообщений
    "connection_duration": 3600,  # 1 час соединения
    "error_rate": 0.05,  # 5% ошибок
    "unique_ips": 3,  # Доступ с 3 разных IP
    "failed_auth_attempts": 5,  # 5 неудачных попыток авторизации
}

# Результат ML-предсказания (аномалия обнаружена)
test_prediction = {
    "anomaly_score": 0.78,  # Высокий скор аномалии
    "is_anomaly": True,
    "confidence": 0.85,
    "threshold": 0.6,  # Порог для классификации аномалий
}

# XAI объяснение от SHAP/LIME
test_xai_explanation = {
    "feature_importances": {
        "messages_per_minute": 0.45,  # 45% вклада в аномалию
        "error_rate": 0.35,  # 35% вклада
        "failed_auth_attempts": 0.15,  # 15% вклада
        "connection_duration": 0.03,  # 3% вклада
        "unique_ips": 0.02,  # 2% вклада
    },
    "method": "shap",  # Используется SHAP метод объяснения
}

print_success("Тестовые данные подготовлены")
print_info("Метрики аномального соединения:")
print_json(test_features)
print_info("ML-предсказание:")
print_json(test_prediction)


# Проверка OpenAI сервиса
async def test_openai_integration():
    print_section("Тестирование OpenAI интеграции")

    try:
        # Инициализация клиента OpenAI
        print_info("Проверка статуса клиента OpenAI")

        if not hasattr(openai_service, "client") or not openai_service.client:
            print_warning("OpenAI клиент не инициализирован")
            print_info("Инициализация клиента с API ключом...")

            if hasattr(openai_service, "initialize_client"):
                openai_service.initialize_client(api_key)
                print_info(
                    f"Статус клиента после инициализации: {'активен' if openai_service.client else 'не активен'}"
                )
            else:
                print_error("Метод initialize_client не найден")
                # Создаем новый экземпляр сервиса
                temp_service = OpenAIService(api_key=api_key)
                temp_service.initialize()
                print_info(
                    f"Создан новый экземпляр сервиса, статус клиента: {'активен' if temp_service.client else 'не активен'}"
                )
                # Заменяем глобальный экземпляр
                openai_service.client = temp_service.client
        else:
            print_success("OpenAI клиент уже инициализирован")

        # Тестирование генерации объяснения
        print_section("Запрос объяснения для аномального соединения")
        print_info("Отправка запроса в OpenAI...")

        # Вызов метода объяснения предсказания
        result = await openai_service.explain_ml_prediction(
            prediction=test_prediction,
            features=test_features,
            model_type="anomaly_detection",
            xai_explanation=test_xai_explanation,
        )

        print_success("Получен ответ от OpenAI")

        print_section("Результат анализа")

        # Проверяем структуру ответа
        if hasattr(result, "summary") and hasattr(result, "analysis"):
            print_info("Краткое резюме:")
            print(f"{Colors.YELLOW}{result.summary}{Colors.ENDC}")

            print_info("Детальный анализ:")
            print(
                f"{Colors.YELLOW}{result.analysis[:300]}{'...' if len(result.analysis) > 300 else ''}{Colors.ENDC}"
            )

            print_info("Рекомендации:")
            for i, rec in enumerate(result.recommendations, 1):
                print(f"{Colors.YELLOW}{i}. {rec}{Colors.ENDC}")

            print_info("Уровень серьезности:")
            print(
                f"{Colors.YELLOW}{result.severity} (Уверенность: {result.confidence * 100:.1f}%){Colors.ENDC}"
            )

            print_info("Технические детали:")
            if hasattr(result, "technical_details"):
                print_json(result.technical_details)
        else:
            print_error("Структура ответа не соответствует ожидаемой")
            print_info(f"Тип результата: {type(result)}")
            print_info(f"Содержимое: {result}")

        return True

    except Exception as e:
        print_error(f"Ошибка при тестировании OpenAI интеграции: {e}")
        traceback.print_exc()
        return False


# Основная функция
async def main():
    # Запускаем тест OpenAI интеграции
    success = await test_openai_integration()

    print_header("РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ")

    if success:
        print_success("Интеграция XAI + OpenAI работает корректно")
        print_info(
            "Система может генерировать человекочитаемые объяснения для ML-предсказаний"
        )
        print_info(
            "OpenAI используется для объяснения результатов XAI методов (SHAP/LIME)"
        )
    else:
        print_error("Тестирование интеграции завершилось с ошибками")
        print_info("Проверьте журналы для получения подробной информации")

    print_section("Заметки по реализации")

    # Определяем, используем ли мы мок или реальный OpenAI API
    try:
        import openai

        print_info(
            f"Используется реальная OpenAI библиотека, версия: {openai.__version__}"
        )
    except ImportError:
        print_warning("Используется мок-реализация OpenAI API")
        print_info(
            "Для продакшн используйте реальную библиотеку openai: pip install openai"
        )


# Запуск
if __name__ == "__main__":
    asyncio.run(main())
