#!/usr/bin/env python
"""
Тестирование интеллектуальной системы логирования для OpenAI адаптера

Этот скрипт проверяет работу системы интеллектуального логирования,
созданной для анализа работы универсального OpenAI адаптера:
1. Записи опыта (ошибки) в experience.log
2. Записи инсайтов (решения) в insights.log
3. Записи кармы (повторяющиеся ошибки) в karma.log
4. Записи гипотез (тупиковые ситуации) в hypotheses.log

Запускает разные сценарии использования адаптера для генерации
данных в логах и демонстрирует их анализ.
"""

import asyncio
import json
import os
import random
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

# Добавляем путь к бэкенду для импорта модулей
sys.path.append(str(Path(__file__).parent.parent))

# Настраиваем логирование
from loguru import logger

logger.remove()
logger.add(sys.stderr, level="INFO")
logger.add("logs/intelligent_test_{time}.log", level="DEBUG")

# Загружаем переменные окружения, если доступны
try:
    from dotenv import load_dotenv

    dotenv_path = Path(__file__).parent.parent / ".env"
    if dotenv_path.exists():
        load_dotenv(dotenv_path)
        logger.info(f"Загружены переменные окружения из {dotenv_path}")
    else:
        logger.warning(f"Файл .env не найден: {dotenv_path}")
except ImportError:
    logger.warning("Библиотека dotenv не установлена, пропускаем загрузку .env")

# Импорт адаптера и логгера
try:
    from backend.ml.adapter_logger import adapter_logger
    from backend.ml.openai_wrapper import (
        LLMRequest,
        LLMResponse,
        get_adapter_status,
        initialize_llm_client,
        llm_client,
    )

    logger.info("Успешно импортированы модули адаптера и логгера")
except ImportError as e:
    logger.error(f"Ошибка импорта модулей: {e}")
    sys.exit(1)

# Цвета для консоли (используем только на Unix-системах)
is_windows = sys.platform.startswith("win")
if not is_windows:
    GREEN = "\033[92m"
    RED = "\033[91m"
    YELLOW = "\033[93m"
    BLUE = "\033[94m"
    MAGENTA = "\033[95m"
    CYAN = "\033[96m"
    RESET = "\033[0m"
else:
    # На Windows не используем ANSI-цвета, чтобы избежать проблем с отображением
    GREEN = ""
    RED = ""
    YELLOW = ""
    BLUE = ""
    MAGENTA = ""
    CYAN = ""
    RESET = ""


class IntelligentLogTester:
    """Класс для тестирования системы интеллектуального логирования"""

    def __init__(self):
        self.log_files = {
            "experience": Path(adapter_logger.experience_path),
            "insights": Path(adapter_logger.insights_path),
            "karma": Path(adapter_logger.karma_path),
            "hypotheses": Path(adapter_logger.hypotheses_path),
        }
        self.test_cases_run = 0
        self.test_cases_passed = 0

    async def initialize_adapter(
        self, mock_mode: bool = None, debug_level: int = None
    ) -> bool:
        """Инициализация адаптера с заданными параметрами"""
        logger.info(
            f"Инициализация адаптера: mock_mode={mock_mode}, debug_level={debug_level}"
        )
        result = await initialize_llm_client(
            force_mock_mode=mock_mode, debug_level=debug_level
        )

        if result:
            logger.success("Адаптер успешно инициализирован")
        else:
            logger.error("Ошибка инициализации адаптера")
        return result

    async def get_status(self) -> Dict[str, Any]:
        """Получение статуса адаптера"""
        status = await get_adapter_status()
        logger.info(
            f"Статус адаптера: mock_only={status['configuration']['mock_only']}, debug_level={status['configuration']['debug_level']}"
        )
        return status

    async def make_api_call(
        self, model: str = "gpt-4", prompt: str = "Привет, мир!"
    ) -> Optional[LLMResponse]:
        """Выполнение вызова API (или мок-вызова)"""
        request = LLMRequest(
            model=model,
            messages=[{"role": "user", "content": prompt}],
            max_tokens=100,
            temperature=0.7,
        )

        try:
            logger.info(f"Отправка запроса к модели {model}: '{prompt[:50]}...'")
            response = await llm_client.call(request)
            logger.success(f"Получен ответ: '{response.content[:50]}...'")
            return response
        except Exception as e:
            logger.error(f"Ошибка вызова: {e}")
            return None

    async def make_failing_call(self, error_type: str = "timeout") -> None:
        """Выполнение заведомо проблемного вызова для генерации ошибок"""
        if error_type == "timeout":
            # Имитируем таймаут изменяя CONNECTION_TIMEOUT временно
            original_timeout = os.environ.get("OPENAI_CONNECTION_TIMEOUT", "10")
            os.environ["OPENAI_CONNECTION_TIMEOUT"] = "0.001"
            logger.info(
                "Запуск тестового сценария с таймаутом (CONNECTION_TIMEOUT=0.001)"
            )
            try:
                await self.make_api_call(prompt="Этот запрос должен вызвать таймаут")
            except Exception:
                logger.warning("Ожидаемая ошибка таймаута произошла")
            finally:
                os.environ["OPENAI_CONNECTION_TIMEOUT"] = original_timeout

        elif error_type == "invalid_key":
            # Имитируем неверный API ключ
            original_key = llm_client.api_key
            llm_client.api_key = "sk-invalid-key-12345"
            logger.info("Запуск тестового сценария с неверным API ключом")
            try:
                await self.make_api_call(
                    prompt="Этот запрос должен вызвать ошибку аутентификации"
                )
            except Exception:
                logger.warning("Ожидаемая ошибка аутентификации произошла")
            finally:
                llm_client.api_key = original_key

        elif error_type == "no_client":
            # Имитируем отсутствие клиента
            original_client = llm_client.openai_client
            llm_client.openai_client = None
            llm_client.mock_only = False  # Отключаем мок, чтобы был сбой
            logger.info("Запуск тестового сценария без инициализированного клиента")
            try:
                await self.make_api_call(
                    prompt="Этот запрос должен вызвать ошибку клиента"
                )
            except Exception:
                logger.warning("Ожидаемая ошибка отсутствия клиента произошла")
            finally:
                llm_client.openai_client = original_client
                llm_client.mock_only = True  # Возвращаем мок-режим

    async def run_test_scenario(self, scenario_name: str, actions: List) -> bool:
        """Запуск тестового сценария с несколькими действиями"""
        logger.info(f"\n===== СЦЕНАРИЙ: {scenario_name} =====")
        self.test_cases_run += 1
        success = True

        try:
            for action in actions:
                action_type = action.get("type")
                action_params = action.get("params", {})

                # Делаем небольшую паузу между действиями, чтобы избежать смешивания вывода
                time.sleep(0.5)

                if action_type == "initialize":
                    await self.initialize_adapter(**action_params)
                elif action_type == "status":
                    await self.get_status()
                elif action_type == "call":
                    await self.make_api_call(**action_params)
                elif action_type == "fail":
                    await self.make_failing_call(**action_params)
                elif action_type == "repeat_error":
                    # Повторяем ошибку несколько раз для создания записи в karma.log
                    for i in range(action_params.get("count", 3)):
                        await self.make_failing_call(
                            action_params.get("error_type", "timeout")
                        )
                        # Пауза между повторениями
                        time.sleep(0.3)
                else:
                    logger.error(f"Неизвестный тип действия: {action_type}")
                    success = False

            # Делаем паузу перед проверкой лог-файлов, чтобы убедиться, что все записи сохранены
            time.sleep(0.5)

            # Проверяем состояние лог-файлов после сценария
            log_sizes = self.check_log_files()
            logger.info(f"Размеры лог-файлов: {log_sizes}")

            if success:
                self.test_cases_passed += 1
                logger.success(f"Сценарий {scenario_name} успешно выполнен")
            else:
                logger.error(f"Сценарий {scenario_name} выполнен с ошибками")

            return success

        except Exception as e:
            logger.error(f"Ошибка выполнения сценария {scenario_name}: {e}")
            return False

    def check_log_files(self) -> Dict[str, int]:
        """Проверяет наличие и размер лог-файлов"""
        result = {}

        for name, path in self.log_files.items():
            if path.exists():
                size = path.stat().st_size
                result[name] = size
            else:
                result[name] = 0
                logger.warning(f"Лог-файл {name} не существует: {path}")

        return result

    def show_log_files_content(self) -> None:
        """Показывает содержимое всех лог-файлов для анализа"""
        for name, path in self.log_files.items():
            if path.exists() and path.stat().st_size > 0:
                print(f"\n{CYAN}===== Содержимое {name}.log ====={RESET}")
                try:
                    with open(path, "r", encoding="utf-8") as f:
                        # Читаем файл построчно и выводим каждую строку отдельно
                        # чтобы избежать проблем с перекрытием вывода
                        lines = f.readlines()
                        for line in lines:
                            print(line.strip())
                            # Небольшая пауза между строками для стабильности вывода
                            time.sleep(0.01)
                except Exception as e:
                    print(f"Ошибка при чтении {name}.log: {e}")
            else:
                print(f"\n{YELLOW}Файл {name}.log пуст или не существует{RESET}")

    def print_test_summary(self) -> None:
        """Выводит сводку по тестам"""
        print("\n===== РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ =====")
        print(f"Запущено сценариев: {self.test_cases_run}")
        print(f"Успешно выполнено: {self.test_cases_passed}")
        print(
            f"Процент успешных: {(self.test_cases_passed / max(1, self.test_cases_run) * 100):.1f}%"
        )

        # Проверяем наполнение лог-файлов
        log_sizes = self.check_log_files()
        for name, size in log_sizes.items():
            if size > 0:
                print(f"✓ {name}.log: {size} байт")
            else:
                print(f"✗ {name}.log: не создан или пуст")

        # Дополнительная информация о расположении лог-файлов
        print(f"\nПуть к лог-файлам: {adapter_logger.log_dir}")


async def main():
    """Основная функция для запуска тестов"""
    print("===== ТЕСТИРОВАНИЕ ИНТЕЛЛЕКТУАЛЬНОЙ СИСТЕМЫ ЛОГИРОВАНИЯ =====")
    print(f"Время запуска: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    # Удаляем старые лог-файлы, чтобы видеть только результаты текущего запуска
    log_dir = Path(adapter_logger.log_dir)
    if log_dir.exists():
        for log_file in [
            "experience.log",
            "insights.log",
            "karma.log",
            "hypotheses.log",
        ]:
            log_path = log_dir / log_file
            if log_path.exists():
                try:
                    log_path.unlink()
                    logger.info(f"Удален старый лог-файл {log_file}")
                except Exception as e:
                    logger.warning(f"Не удалось удалить {log_file}: {e}")

    tester = IntelligentLogTester()

    # Определяем тестовые сценарии
    test_scenarios = [
        {
            "name": "Инициализация в мок-режиме",
            "actions": [
                {"type": "initialize", "params": {"mock_mode": True, "debug_level": 2}},
                {"type": "status"},
                {
                    "type": "call",
                    "params": {"prompt": "Что такое интеллектуальное логирование?"},
                },
            ],
        },
        {
            "name": "Ошибка отсутствия клиента",
            "actions": [
                {
                    "type": "initialize",
                    "params": {"mock_mode": False, "debug_level": 2},
                },
                {"type": "fail", "params": {"error_type": "no_client"}},
            ],
        },
        {
            "name": "Ошибка таймаута",
            "actions": [
                {
                    "type": "initialize",
                    "params": {"mock_mode": False, "debug_level": 2},
                },
                {"type": "fail", "params": {"error_type": "timeout"}},
            ],
        },
        {
            "name": "Повторяющиеся ошибки для кармы",
            "actions": [
                {
                    "type": "initialize",
                    "params": {"mock_mode": False, "debug_level": 2},
                },
                {
                    "type": "repeat_error",
                    "params": {"error_type": "invalid_key", "count": 3},
                },
            ],
        },
        {
            "name": "Успешные запросы в мок-режиме",
            "actions": [
                {"type": "initialize", "params": {"mock_mode": True, "debug_level": 1}},
                {"type": "call", "params": {"prompt": "Первый тестовый запрос"}},
                {"type": "call", "params": {"prompt": "Второй тестовый запрос"}},
                {"type": "call", "params": {"prompt": "Третий тестовый запрос"}},
            ],
        },
    ]

    # Запускаем все сценарии
    for scenario in test_scenarios:
        await tester.run_test_scenario(scenario["name"], scenario["actions"])
        print()  # Пустая строка между сценариями

    # Выводим сводку и содержимое лог-файлов
    tester.print_test_summary()
    print("\nПоказываем содержимое лог-файлов:")
    tester.show_log_files_content()


if __name__ == "__main__":
    # Устанавливаем максимальную ширину вывода для предотвращения переносов строк
    if hasattr(os, "get_terminal_size"):
        try:
            width = os.get_terminal_size().columns
            os.environ["COLUMNS"] = str(width)
        except OSError:
            pass

    print("\n" + "=" * 80)
    print("Запуск тестирования интеллектуальной системы логирования OpenAI адаптера")
    print("=" * 80 + "\n")

    # Запускаем асинхронный код
    try:
        asyncio.run(main())
        print("\n" + "=" * 80)
        print("Тестирование завершено успешно!")
        print("=" * 80)
    except Exception as e:
        print(f"\nОшибка при выполнении тестов: {e}")
        import traceback

        traceback.print_exc()
