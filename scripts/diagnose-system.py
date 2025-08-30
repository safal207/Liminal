#!/usr/bin/env python
"""
Комплексная диагностика системы Resonance Liminal

Этот скрипт проверяет:
1. Docker сервисы (Neo4j, Redis, Prometheus, Grafana)
2. Backend API endpoints
3. OpenAI/Anthropic/XAI интеграцию
4. ML сервисы и метрики
5. WebSocket соединения
6. Базы данных и кэширование
"""

import asyncio
import json
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path

import requests

# Добавляем путь к бэкенду для импорта модулей
sys.path.append(str(Path(__file__).parent.parent))

# Загрузка переменных окружения
try:
    from dotenv import load_dotenv

    load_dotenv(Path(__file__).parent.parent / ".env")
    print("✓ Переменные окружения загружены")
except ImportError:
    print("⚠ dotenv не установлен")

# Цветной вывод
GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
BLUE = "\033[94m"
MAGENTA = "\033[95m"
CYAN = "\033[96m"
RESET = "\033[0m"


def print_header(title: str) -> None:
    print(f"\n{MAGENTA}{'=' * 60}{RESET}")
    print(f"{MAGENTA}  {title}{RESET}")
    print(f"{MAGENTA}{'=' * 60}{RESET}\n")


def print_section(title: str) -> None:
    print(f"\n{CYAN}--- {title} ---{RESET}")


def print_success(message: str) -> None:
    print(f"{GREEN}✓ {message}{RESET}")


def print_error(message: str) -> None:
    print(f"{RED}✗ {message}{RESET}")


def print_warning(message: str) -> None:
    print(f"{YELLOW}⚠ {message}{RESET}")


def print_info(message: str) -> None:
    print(f"{BLUE}ℹ {message}{RESET}")


def run_command(cmd: str, timeout: int = 30) -> tuple[bool, str]:
    """Выполняет команду и возвращает результат"""
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=timeout)
        return result.returncode == 0, result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        return False, f"Команда превысила таймаут {timeout} сек"
    except Exception as e:
        return False, str(e)


def check_url(url: str, timeout: int = 10) -> tuple[bool, str, dict | None]:
    """Проверяет доступность URL"""
    try:
        response = requests.get(url, timeout=timeout)
        return (
            True,
            f"HTTP {response.status_code}",
            (response.json() if "json" in response.headers.get("content-type", "") else None),
        )
    except requests.exceptions.ConnectionError:
        return False, "Соединение отклонено", None
    except requests.exceptions.Timeout:
        return False, f"Таймаут {timeout} сек", None
    except Exception as e:
        return False, str(e), None


def check_docker_services():
    """Проверяет состояние Docker сервисов"""
    print_header("Диагностика Docker сервисов")

    # Проверяем Docker
    print_section("Docker Engine")
    success, output = run_command("docker --version")
    if success:
        print_success(f"Docker установлен: {output.strip()}")
    else:
        print_error("Docker не установлен или недоступен")
        return False

    # Проверяем Docker Compose
    success, output = run_command("docker compose version")
    if success:
        print_success(f"Docker Compose: {output.strip()}")
    else:
        print_error("Docker Compose недоступен")
        return False

    # Проверяем запущенные контейнеры
    print_section("Запущенные контейнеры")
    success, output = run_command("docker compose ps --format json")
    if success and output.strip():
        try:
            containers = [json.loads(line) for line in output.strip().split("\n") if line.strip()]
            for container in containers:
                name = container.get("Name", "unknown")
                state = container.get("State", "unknown")
                health = container.get("Health", "unknown")

                if state == "running":
                    if health == "healthy" or health == "unknown":
                        print_success(f"{name}: {state} ({health})")
                    else:
                        print_warning(f"{name}: {state} ({health})")
                else:
                    print_error(f"{name}: {state}")
        except json.JSONDecodeError:
            print_info("Контейнеры запущены, но формат вывода не JSON")
            print_info(output)
    else:
        print_warning("Нет запущенных контейнеров")

    return True


def check_backend_services():
    """Проверяет backend сервисы"""
    print_header("Диагностика Backend сервисов")

    # Основные endpoints
    endpoints = {
        "Health Check": "http://localhost:8000/health",
        "Metrics": "http://localhost:8000/metrics",
        "WebSocket Info": "http://localhost:8000/ws/info",
        "ML Health": "http://localhost:8000/ml/health",
        "ML Metrics": "http://localhost:8000/ml/metrics",
    }

    for name, url in endpoints.items():
        print_section(f"Проверка {name}")
        success, status, data = check_url(url)

        if success:
            print_success(f"{name}: {status}")
            if data and isinstance(data, dict):
                # Выводим ключевую информацию
                if "status" in data:
                    print_info(f"Статус: {data['status']}")
                if "version" in data:
                    print_info(f"Версия: {data['version']}")
                if "uptime" in data:
                    print_info(f"Время работы: {data['uptime']}")
        else:
            print_error(f"{name}: {status}")


def check_database_services():
    """Проверяет базы данных"""
    print_header("Диагностика баз данных")

    # Neo4j
    print_section("Neo4j")
    success, status, data = check_url("http://localhost:7474/db/data/")
    if success:
        print_success(f"Neo4j доступен: {status}")
    else:
        print_error(f"Neo4j недоступен: {status}")

    # Redis через backend API
    print_section("Redis")
    success, status, data = check_url("http://localhost:8000/health")
    if success and data:
        redis_status = data.get("redis", {}).get("status", "unknown")
        if redis_status == "connected":
            print_success("Redis подключен")
        else:
            print_error(f"Redis: {redis_status}")
    else:
        print_warning("Не удалось проверить Redis через API")


def check_monitoring_services():
    """Проверяет сервисы мониторинга"""
    print_header("Диагностика мониторинга")

    # Prometheus
    print_section("Prometheus")
    success, status, data = check_url("http://localhost:9090/-/healthy")
    if success:
        print_success(f"Prometheus: {status}")

        # Проверяем targets
        success, status, targets = check_url("http://localhost:9090/api/v1/targets")
        if success and targets:
            active_targets = [
                t
                for t in targets.get("data", {}).get("activeTargets", [])
                if t.get("health") == "up"
            ]
            print_info(f"Активных targets: {len(active_targets)}")
    else:
        print_error(f"Prometheus недоступен: {status}")

    # Grafana
    print_section("Grafana")
    success, status, data = check_url("http://localhost:3000/api/health")
    if success:
        print_success(f"Grafana: {status}")
    else:
        print_error(f"Grafana недоступен: {status}")


async def check_openai_integration():
    """Проверяет интеграцию с OpenAI"""
    print_header("Диагностика OpenAI/Anthropic/XAI интеграции")

    try:
        # Импортируем наш универсальный адаптер
        from backend.ml.openai_wrapper import (
            LLMRequest,
            initialize_llm_client,
            llm_client,
        )

        print_success("Универсальный адаптер импортирован")

        # Инициализация
        print_section("Инициализация")
        success = await initialize_llm_client()
        if success:
            print_success("Клиент инициализирован с реальным API")
        else:
            print_warning("Клиент инициализирован в мок-режиме")

        # Тестовый запрос
        print_section("Тестовый запрос")
        request = LLMRequest(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "Ты - диагностический AI."},
                {
                    "role": "user",
                    "content": "Проверь работоспособность системы ML анализа.",
                },
            ],
            max_tokens=200,
        )

        start_time = time.time()
        response = await llm_client.call(request)
        elapsed = time.time() - start_time

        print_success(f"Запрос выполнен за {elapsed:.2f} сек")
        print_info(f"Режим мока: {response.is_mock}")
        print_info(f"Модель: {response.model}")

        # Проверяем через backend API
        print_section("OpenAI через Backend API")
        success, status, data = check_url("http://localhost:8000/ml/openai/health")
        if success:
            print_success(f"OpenAI API через backend: {status}")
            if data:
                print_info(f"Статус: {data.get('status', 'unknown')}")
        else:
            print_warning(f"OpenAI API через backend недоступен: {status}")

    except ImportError as e:
        print_error(f"Ошибка импорта: {e}")
    except Exception as e:
        print_error(f"Ошибка проверки OpenAI: {e}")


def check_ml_services():
    """Проверяет ML сервисы"""
    print_header("Диагностика ML сервисов")

    # ML Health
    print_section("ML Health Check")
    success, status, data = check_url("http://localhost:8000/ml/health")
    if success:
        print_success(f"ML сервисы: {status}")
        if data:
            for service, info in data.items():
                if isinstance(info, dict) and "status" in info:
                    if info["status"] == "healthy":
                        print_success(f"  {service}: {info['status']}")
                    else:
                        print_warning(f"  {service}: {info['status']}")
    else:
        print_error(f"ML сервисы недоступны: {status}")

    # ML Metrics
    print_section("ML Metrics")
    success, status, data = check_url("http://localhost:8000/ml/metrics")
    if success:
        print_success(f"ML метрики: {status}")
        if data:
            print_info(f"Доступно метрик: {len(data)}")
    else:
        print_warning(f"ML метрики недоступны: {status}")


def check_system_resources():
    """Проверяет системные ресурсы"""
    print_header("Диагностика системных ресурсов")

    # Docker stats
    print_section("Использование ресурсов Docker")
    success, output = run_command(
        "docker stats --no-stream --format 'table {{.Name}}\\t{{.CPUPerc}}\\t{{.MemUsage}}'"
    )
    if success:
        print_success("Статистика контейнеров:")
        print(output)
    else:
        print_warning("Не удалось получить статистику Docker")

    # Disk space
    print_section("Дисковое пространство")
    success, output = run_command("docker system df")
    if success:
        print_success("Использование дискового пространства Docker:")
        print(output)
    else:
        print_warning("Не удалось получить информацию о дисковом пространстве")


async def run_full_diagnostics():
    """Запускает полную диагностику системы"""
    print_header(
        f"Комплексная диагностика Resonance Liminal - {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}"
    )

    # Проверяем Docker сервисы
    if not check_docker_services():
        print_error("Docker недоступен, прерываем диагностику")
        return

    # Ждем немного для стабилизации сервисов
    print_info("Ожидание стабилизации сервисов...")
    time.sleep(5)

    # Проверяем все компоненты
    check_backend_services()
    check_database_services()
    check_monitoring_services()
    await check_openai_integration()
    check_ml_services()
    check_system_resources()

    print_header("Диагностика завершена")
    print_info("Для детального анализа проверьте логи: docker compose logs")


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--help":
        print("Использование: python diagnose-system.py")
        print("Опции:")
        print("  --help    Показать эту справку")
        sys.exit(0)

    asyncio.run(run_full_diagnostics())
