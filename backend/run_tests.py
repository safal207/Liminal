"""
Скрипт для запуска тестов.
"""

import subprocess
import sys


def run_tests():
    """Запускает тесты с помощью pytest."""
    print("Running tests...")

    # Команда для запуска pytest
    cmd = [sys.executable, "-m", "pytest", "backend/tests/test_sanity_check.py", "-v"]

    # Запускаем тесты
    result = subprocess.run(cmd, capture_output=True, text=True)

    # Выводим результат
    print(result.stdout)
    if result.stderr:
        print("Errors:", file=sys.stderr)
        print(result.stderr, file=sys.stderr)

    return result.returncode == 0


if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)