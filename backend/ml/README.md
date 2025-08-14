# ML Module - Resonance Liminal

Модуль машинного обучения для интеллектуальной оптимизации WebSocket-сервера с использованием AutoML for Embedded (Kenning framework).

## 🚀 Возможности

### Real-time Analytics
- Автоматическое извлечение фичей из WebSocket трафика
- Обнаружение аномалий в поведении пользователей
- Baseline learning для каждого пользователя
- Интеграция с Prometheus метриками

### AutoML Integration
- Автоматическое обучение моделей через Kenning
- Поддержка ONNX для production inference
- Docker-based training pipeline
- Continuous learning capabilities

### Anomaly Detection
- Rule-based детекция (флуд, ошибки, rate limit abuse)
- ML-based детекция аномальных паттернов
- Baseline deviation detection
- Multi-level severity alerts

## 📁 Структура модуля

```
ml/
├── __init__.py              # Основные импорты
├── feature_extractor.py     # Извлечение фичей из WebSocket данных
├── model_manager.py         # Управление ML-моделями (Kenning)
├── anomaly_detector.py      # Детекция аномалий
├── api.py                   # REST API для ML-функций
├── docker-compose.kenning.yml  # Docker для Kenning + Jupyter
├── models/                  # Обученные модели
├── configs/                 # Конфигурации Kenning
└── README.md               # Этот файл
```

## 🔧 API Endpoints

### Статус системы
```http
GET /ml/status
```

### Фичи и данные
```http
GET /ml/features/recent?limit=100
DELETE /ml/features/cleanup?max_age_hours=24
```

### Аномалии
```http
GET /ml/anomalies/recent?limit=50&min_severity=medium
POST /ml/anomalies/analyze
```

### Модели
```http
GET /ml/models
POST /ml/models/train
POST /ml/models/{model_name}/load
POST /ml/models/predict
```

## 🐳 Запуск с Docker

### Kenning + Jupyter
```bash
cd backend/ml
docker-compose -f docker-compose.kenning.yml up -d
```

### Доступ к Jupyter
- URL: http://localhost:8888
- Работа с ML-экспериментами в браузере

## 📊 Доступные модели

### 1. Anomaly Detection
- **Цель**: Обнаружение аномальных паттернов в трафике
- **Фичи**: messages_per_minute, avg_message_size, error_rate, rate_limit_violations
- **Тип**: Classification

### 2. Load Prediction
- **Цель**: Предсказание нагрузки на сервер
- **Фичи**: messages_per_minute, connection_duration, channels_count
- **Тип**: Regression

### 3. User Behavior
- **Цель**: Кластеризация пользователей по поведению
- **Фичи**: messages_per_minute, channels_count, connection_duration
- **Тип**: Clustering

## 🔄 Workflow

### 1. Сбор данных
```python
from ml.feature_extractor import feature_extractor

# Автоматически вызывается в main.py при каждом сообщении
feature_extractor.track_user_activity(
    user_id="user123",
    message_size=256,
    channels=["general", "tech"],
    ip_address="192.168.1.1"
)
```

### 2. Обучение модели
```python
# Через API
curl -X POST "http://localhost:8000/ml/models/train" \
  -H "Content-Type: application/json" \
  -d '{
    "model_name": "anomaly_detection",
    "data_source": "recent_features",
    "auto_deploy": true
  }'
```

### 3. Детекция аномалий
```python
from ml.anomaly_detector import anomaly_detector

# Автоматически вызывается каждые 10 сообщений
alerts = anomaly_detector.analyze_user_activity("user123")
```

## 📈 Мониторинг

### Prometheus метрики
- Все ML-операции интегрированы с существующими метриками
- Аномалии отслеживаются в `websocket_rate_limit_total`
- Производительность ML в отдельных метриках

### Логирование
- Критические аномалии логируются как WARNING
- ML-ошибки как ERROR
- Debug информация для разработки

## 🔧 Конфигурация

### Feature Extractor
```python
# Размер буфера фичей
feature_extractor.buffer_size = 1000

# Автоочистка старых сессий
feature_extractor.cleanup_old_sessions(max_age_hours=24)
```

### Anomaly Detector
```python
# Пороги для rule-based детекции
anomaly_detector.detection_rules = {
    "message_flood": {"threshold": 100, "severity": "high"},
    "error_spike": {"threshold": 0.5, "severity": "medium"},
    # ...
}
```

## 🚀 Следующие шаги

1. **Обучение на реальных данных**: Накопить достаточно данных для первого обучения
2. **A/B тестирование**: Сравнение эффективности разных моделей
3. **Автоматический retraining**: Scheduled обучение на новых данных
4. **Advanced features**: Интеграция с Neo4j для graph-based анализа

## 🔗 Интеграция

### С Rate Limiting
- ML-модели могут динамически корректировать лимиты
- Персонализированные ограничения на основе поведения

### С Health Checks
- ML-модели участвуют в readiness проверках
- Мониторинг состояния моделей

### С Prometheus
- Все ML-метрики экспортируются в Prometheus
- Grafana дашборды для ML-аналитики

## 📚 Ресурсы

- [Kenning Framework](https://github.com/antmicro/kenning)
- [AutoML for Embedded](https://github.com/antmicro/automl-for-embedded)
- [ONNX Runtime](https://onnxruntime.ai/)
- [FastAPI Background Tasks](https://fastapi.tiangolo.com/tutorial/background-tasks/)
