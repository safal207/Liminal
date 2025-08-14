# Docker Build Optimization Guide

## Выявленная проблема

При сборке Docker-образа для Resonance Liminal backend были обнаружены следующие проблемы:
- Длительное время сборки (~10 минут)
- "Зависание" на шаге установки Python-зависимостей
- Отсутствие отображения прогресса
- Повторная установка всех зависимостей при минимальных изменениях

## Проведенные оптимизации

### 1. Диагностика проблем

Создан скрипт `scripts/docker-debug-build.ps1` для выявления узких мест:
- Подробное логирование с `--progress=plain`
- Анализ размера контекста сборки
- Выявление тяжелых ML-зависимостей

### 2. Оптимизированные Dockerfile

#### Базовая оптимизация (`Dockerfile.basic`)
```dockerfile
FROM python:3.11-slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y build-essential gcc

# Create virtual environment
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Install pip tools with optimizations
RUN pip install --no-cache-dir --upgrade pip wheel setuptools

# Copy and install requirements with progress indication
WORKDIR /app
COPY requirements.txt .
RUN echo "Installing Python packages - this may take several minutes..."
RUN pip install --no-cache-dir --prefer-binary -r requirements.txt

# Final stage for smaller image
FROM python:3.11-slim
COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
WORKDIR /app
COPY . .
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000", "--workers", "4"]
```

#### Продвинутая оптимизация (`Dockerfile.cached`)
```dockerfile
# Разделение зависимостей на слои для лучшего кеширования
# Отдельные слои для:
# 1. Базовых зависимостей (fastapi, uvicorn)
# 2. Numpy и pandas
# 3. Тяжелых ML-библиотек
```

### 3. Ускорение сборки с помощью BuildKit

Созданы скрипты для запуска с оптимизациями:
- `scripts/turbo-build-en.ps1` - быстрая сборка с BuildKit
- `scripts/ml-start.ps1` - запуск ML-инфраструктуры

### 4. Уменьшение размера контекста

Создан оптимизированный `.dockerignore`:
- Игнорирование кеша Python
- Игнорирование временных файлов
- Игнорирование Git-директорий

## Результаты оптимизации

- ✅ **Видимый прогресс установки**: Подробное логирование сборки
- ✅ **Ускорение повторных сборок**: Эффективное кеширование Docker-слоев
- ✅ **Разделение зависимостей**: Легкие и тяжелые ML-библиотеки в разных слоях
- ✅ **Уменьшение размера образа**: Multi-stage сборка с минимальным финальным образом
- ✅ **Оптимизация ML-библиотек**: Переменные среды для контроля многопоточности

## Рекомендации для будущих обновлений

1. **Разделение requirements.txt**:
   ```
   requirements-base.txt  # Базовые зависимости
   requirements-ml.txt    # ML и Data Science
   ```

2. **Локальный PyPI-кеш**:
   ```dockerfile
   VOLUME pip_cache:/root/.cache/pip
   ```

3. **Использование пре-скомпилированных образов**:
   ```dockerfile
   FROM pytorch/pytorch:2.0.0-cuda11.7-cudnn8-runtime
   ```

4. **Автоматизация сборки и тестирования**:
   ```powershell
   .\scripts\turbo-build-en.ps1 -StartAfterBuild -TestAfterStart
   ```

## Troubleshooting

При "зависании" сборки рекомендуется:
1. Использовать флаг `--progress=plain`
2. Запускать с помощью скрипта диагностики `docker-debug-build.ps1`
3. Анализировать тяжелые зависимости в `requirements.txt`
4. Проверять подключение к сети и репозиториям PyPI
