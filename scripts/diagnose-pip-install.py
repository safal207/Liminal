#!/usr/bin/env python
"""
Расширенная диагностика установки библиотек Python и сетевого доступа
Проверяет возможные проблемы с установкой библиотек, настройками прокси и доступом к PyPI
"""

import json
import os
import platform
import socket
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path

# Цветной вывод для Windows
if platform.system() == "Windows":
    import ctypes

    kernel32 = ctypes.windll.kernel32
    kernel32.SetConsoleMode(kernel32.GetStdHandle(-11), 7)


# ANSI цвета для лучшей читаемости
class Colors:
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    RED = "\033[91m"
    BLUE = "\033[94m"
    ENDC = "\033[0m"


def print_header(text):
    print(f"\n{Colors.BLUE}{'=' * 70}{Colors.ENDC}")
    print(f"{Colors.BLUE}{text.center(70)}{Colors.ENDC}")
    print(f"{Colors.BLUE}{'=' * 70}{Colors.ENDC}\n")


def print_success(text):
    print(f"{Colors.GREEN}✓ {text}{Colors.ENDC}")


def print_warning(text):
    print(f"{Colors.YELLOW}⚠ {text}{Colors.ENDC}")


def print_error(text):
    print(f"{Colors.RED}✗ {text}{Colors.ENDC}")


def print_info(text):
    print(f"{Colors.BLUE}ℹ {text}{Colors.ENDC}")


def run_command(cmd, timeout=60):
    """Выполняет команду в shell и возвращает результат"""
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=timeout)
        return {
            "returncode": result.returncode,
            "stdout": result.stdout.strip(),
            "stderr": result.stderr.strip(),
            "success": result.returncode == 0,
        }
    except subprocess.TimeoutExpired:
        return {
            "returncode": -1,
            "stdout": "",
            "stderr": f"Timeout after {timeout} seconds",
            "success": False,
        }
    except Exception as e:
        return {"returncode": -1, "stdout": "", "stderr": str(e), "success": False}


def check_network_connectivity():
    """Проверяет соединение с основными PyPI серверами и CDN"""
    print_header("ПРОВЕРКА СЕТЕВОГО ДОСТУПА")

    # Проверка DNS и основных PyPI хостов
    hosts = [
        ("pypi.org", 443),
        ("files.pythonhosted.org", 443),
        ("pypi.python.org", 443),
        ("upload.pypi.org", 443),
        ("test.pypi.org", 443),
    ]

    all_successful = True

    for host, port in hosts:
        try:
            socket.create_connection((host, port), timeout=5)
            print_success(f"Соединение с {host}:{port} успешно установлено")
        except Exception as e:
            all_successful = False
            print_error(f"Не удалось соединиться с {host}:{port} - {str(e)}")

    # Проверка скачивания маленького файла с PyPI
    try:
        urllib.request.urlretrieve(
            "https://pypi.org/static/images/logo-small.2a411bc6.svg",
            tempfile.gettempdir() + "/pypi_logo.svg",
        )
        print_success("Загрузка тестового файла с PyPI успешно выполнена")
    except Exception as e:
        all_successful = False
        print_error(f"Не удалось загрузить тестовый файл с PyPI: {str(e)}")

    # Проверка скачивания JSON данных о пакете
    try:
        with urllib.request.urlopen("https://pypi.org/pypi/openai/json") as response:
            data = json.loads(response.read().decode("utf-8"))
            latest_version = data["info"]["version"]
            print_success(
                f"Получена информация о пакете OpenAI (последняя версия: {latest_version})"
            )
    except Exception as e:
        all_successful = False
        print_error(f"Не удалось получить информацию о пакете OpenAI: {str(e)}")

    return all_successful


def check_pip_configuration():
    """Проверяет конфигурацию pip и возможные проблемы"""
    print_header("ПРОВЕРКА КОНФИГУРАЦИИ PIP")

    # Информация о версии pip
    pip_version = run_command(f"{sys.executable} -m pip --version")
    if pip_version["success"]:
        print_success(f"Версия pip: {pip_version['stdout']}")
    else:
        print_error(f"Не удалось определить версию pip: {pip_version['stderr']}")

    # Конфигурация pip
    pip_config = run_command(f"{sys.executable} -m pip config list")
    if pip_config["success"]:
        if pip_config["stdout"]:
            print_info("Конфигурация pip:")
            for line in pip_config["stdout"].split("\n"):
                if "proxy" in line.lower():
                    print_warning(f"  {line} (обнаружена настройка прокси)")
                else:
                    print_info(f"  {line}")
        else:
            print_info("Нет пользовательской конфигурации pip")
    else:
        print_error(f"Ошибка получения конфигурации pip: {pip_config['stderr']}")

    # Проверка переменных окружения
    env_vars = [
        "PIP_INDEX_URL",
        "PIP_TRUSTED_HOST",
        "HTTP_PROXY",
        "HTTPS_PROXY",
        "NO_PROXY",
    ]
    found_env = False

    for var in env_vars:
        if var in os.environ:
            found_env = True
            value = os.environ[var]
            print_warning(f"Переменная окружения {var}={value}")

    if not found_env:
        print_success("Не обнаружено переменных окружения, влияющих на pip")

    return True


def check_alternative_install_methods():
    """Проверяет альтернативные методы установки OpenAI"""
    print_header("ТЕСТИРОВАНИЕ АЛЬТЕРНАТИВНЫХ МЕТОДОВ УСТАНОВКИ")

    # 1. Тестирование загрузки wheel-файла
    print_info("1. Загрузка wheel файла напрямую")
    wheel_url = "https://files.pythonhosted.org/packages/32/a1/a2790e953c9e4ea91f0ef8c799ce40c555f4f193b65f93a4b931d9d7ecc7/openai-1.20.0-py3-none-any.whl"
    wheel_path = os.path.join(tempfile.gettempdir(), "openai.whl")

    try:
        urllib.request.urlretrieve(wheel_url, wheel_path)
        print_success(f"Загрузка wheel файла успешна: {wheel_path}")

        # Проверка размера файла
        size_kb = os.path.getsize(wheel_path) / 1024
        if size_kb < 10:  # Если файл меньше 10 КБ, скорее всего это не реальный wheel
            print_warning(f"Подозрительно маленький размер файла: {size_kb:.1f} KB")
            with open(wheel_path, "rb") as f:
                content = f.read(100)  # Читаем первые 100 байт
                print_warning(f"Начало содержимого файла: {content}")
        else:
            print_success(f"Размер файла: {size_kb:.1f} KB - это нормальный размер")

            # Попытка установки из загруженного wheel
            install_result = run_command(
                f"{sys.executable} -m pip install --force-reinstall {wheel_path}"
            )
            if install_result["success"]:
                print_success("Установка из wheel файла успешна!")
            else:
                print_error(f"Ошибка установки из wheel файла: {install_result['stderr']}")
    except Exception as e:
        print_error(f"Ошибка загрузки wheel файла: {str(e)}")

    # 2. Тестирование установки с альтернативного индекса
    print_info("\n2. Использование альтернативного индекса PyPI")
    alt_install = run_command(
        f"{sys.executable} -m pip install openai --index-url https://pypi.org/simple"
    )
    if alt_install["success"]:
        print_success("Установка с альтернативного индекса успешна!")
    else:
        print_error(f"Ошибка установки с альтернативного индекса: {alt_install['stderr']}")

    # 3. Тестирование с отключением кэша pip
    print_info("\n3. Установка с отключением кэша pip")
    no_cache_install = run_command(f"{sys.executable} -m pip install openai --no-cache-dir")
    if no_cache_install["success"]:
        print_success("Установка с отключением кэша успешна!")
    else:
        print_error(f"Ошибка установки с отключением кэша: {no_cache_install['stderr']}")

    # 4. Тестирование зависимостей OpenAI
    print_info("\n4. Проверка установки зависимостей OpenAI")
    dependencies = [
        "anyio",
        "httpx",
        "pydantic",
        "typing-extensions",
        "tqdm",
        "sniffio",
    ]

    for dep in dependencies:
        dep_install = run_command(f"{sys.executable} -m pip install {dep}")
        if dep_install["success"]:
            print_success(f"Установка {dep} успешна")
        else:
            print_error(f"Ошибка установки {dep}: {dep_install['stderr']}")


def check_system_info():
    """Собирает системную информацию для диагностики"""
    print_header("СИСТЕМНАЯ ИНФОРМАЦИЯ")

    print_info(f"ОС: {platform.platform()}")
    print_info(f"Python: {sys.version}")
    print_info(f"Путь Python: {sys.executable}")
    print_info(
        f"Виртуальное окружение: {'активно' if hasattr(sys, 'real_prefix') or getattr(sys, 'base_prefix', sys.prefix) != sys.prefix else 'неактивно'}"
    )

    # Проверка сетевых настроек Windows
    if platform.system() == "Windows":
        print_info("\nНастройки Windows:")

        # Проверка настроек прокси Windows
        proxy_result = run_command("netsh winhttp show proxy")
        if proxy_result["success"]:
            if "Direct access" in proxy_result["stdout"]:
                print_success("Windows HTTP: Прямое соединение без прокси")
            else:
                print_warning(
                    f"Windows HTTP: Обнаружены настройки прокси: {proxy_result['stdout']}"
                )

    # Проверка установленных библиотек для OpenAI и зависимостей
    print_info("\nУстановленные зависимости OpenAI:")
    dependencies = ["openai", "anyio", "httpx", "pydantic", "typing-extensions", "tqdm"]
    for dep in dependencies:
        try:
            module = __import__(dep)
            if hasattr(module, "__version__"):
                print_success(f"{dep}: версия {module.__version__}")
            else:
                print_success(f"{dep}: установлен")
        except ImportError:
            print_error(f"{dep}: не установлен")

    print_header("РЕКОМЕНДАЦИИ")

    # Генерируем рекомендации для установки OpenAI
    print_info("1. Создать чистое виртуальное окружение:")
    print(
        """
    python -m venv venv
    venv\\Scripts\\activate  # для Windows
    """
    )

    print_info("2. Попробовать установку OpenAI с опциями --no-cache-dir и --verbose:")
    print(
        """
    pip install --no-cache-dir --verbose openai
    """
    )

    print_info("3. Установить библиотеку из GitHub репозитория:")
    print(
        """
    pip install git+https://github.com/openai/openai-python.git
    """
    )

    print_info("4. Загрузить wheel файл и установить локально:")
    print(
        """
    # Скачать с URL: https://files.pythonhosted.org/packages/32/a1/a2790e953c9e4ea91f0ef8c799ce40c555f4f193b65f93a4b931d9d7ecc7/openai-1.20.0-py3-none-any.whl
    pip install openai-1.20.0-py3-none-any.whl
    """
    )

    print_info("5. Проверить временное отключение антивируса/файервола")

    print_info("6. Использовать мок-реализацию OpenAI для разработки:")
    print(
        """
    # Мок-реализация OpenAI доступна в: backend/ml/openai_mock.py
    # Она позволяет продолжать разработку без реального API клиента
    """
    )


def main():
    print_header("ДИАГНОСТИКА УСТАНОВКИ OPENAI")
    print_info(f"Запуск: {Path(__file__).name}")
    print_info(f"Время: {os.path.basename(sys.argv[0])}")

    network_ok = check_network_connectivity()
    pip_ok = check_pip_configuration()
    check_system_info()

    if network_ok and pip_ok:
        print_info("\nПопытка установки OpenAI с verbose output:")
        install_result = run_command(f"{sys.executable} -m pip install --verbose openai")

        if install_result["success"]:
            print_success("Установка OpenAI успешно выполнена!")
        else:
            print_error("Установка OpenAI не удалась")
            print_info("Полный вывод ошибки:")
            print(install_result["stderr"])

            check_alternative_install_methods()
    else:
        check_alternative_install_methods()


if __name__ == "__main__":
    main()
