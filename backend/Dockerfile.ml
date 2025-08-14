FROM python:3.11-slim

# Установка необходимых системных зависимостей
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gcc \
    curl \
    libopenblas-base \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Установка базовых Python-пакетов
RUN pip install --no-cache-dir --upgrade pip wheel

# Копирование и установка зависимостей
WORKDIR /app
COPY requirements.txt .

# Установка критических ML-зависимостей
RUN pip install --no-cache-dir pyyaml prometheus-client redis

# Установка остальных зависимостей
RUN pip install --no-cache-dir -r requirements.txt

# ML-переменные окружения
ENV OPENBLAS_NUM_THREADS=1 \
    OMP_NUM_THREADS=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    ML_ENABLED=true

# Копирование кода приложения
COPY . .

# Healthcheck
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8000/health || exit 1

# Открытие порта
EXPOSE 8000

# Запуск приложения
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000", "--workers", "4"]
