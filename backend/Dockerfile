FROM python:3.11-slim

# Явное указание версии Python
ENV PYTHON_VERSION=3.11
ENV PYTHONUNBUFFERED=1

WORKDIR /app

# Копируем файл зависимостей
COPY requirements.txt .

# Устанавливаем зависимости
RUN pip install --no-cache-dir -r requirements.txt

# Копируем код приложения
COPY . .

# Открываем порт для FastAPI
EXPOSE 8000

# Запускаем приложение через uvicorn с настройками логирования
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000", "--log-config", "logging.yaml"]
