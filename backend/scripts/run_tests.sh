#!/bin/bash

# Установка зависимостей, если их еще нет
pip install -r requirements-dev.txt

# Запуск тестов с покрытием
pytest tests/ -v --cov=. --cov-report=term-missing

# Проверка стиля кода
echo "\nПроверка стиля кода..."
black --check .
flake8 .
