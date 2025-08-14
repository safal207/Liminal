FROM python:3.11-slim

# Установка необходимых системных зависимостей
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Создание рабочей директории
WORKDIR /app

# Копирование requirements.txt и установка зависимостей
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Копирование кода предиктора
COPY anomaly_predictor.py .

# Установка переменных среды для оптимизации
ENV PYTHONUNBUFFERED=1 \
    OPENBLAS_NUM_THREADS=1 \
    OMP_NUM_THREADS=1 \
    FLASK_APP=anomaly_predictor.py

# Добавление проверки здоровья
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8002/health || exit 1

# Открытие порта
EXPOSE 8002

# Запуск предиктора
CMD ["python", "anomaly_predictor.py"]
