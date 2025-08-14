# Resonance Liminal - Документация

## Обзор проекта

Resonance Liminal - это распределенная WebSocket платформа, предназначенная для высоконагруженных приложений реального времени с интеграцией графовой базы данных Neo4j и Redis для масштабирования.

## Основные компоненты

### Backend
- **FastAPI** - Основа API и WebSocket сервера
- **Redis** - Для синхронизации между экземплярами и масштабирования
- **Neo4j** - Графовая база данных для хранения связанных данных
- **JWT Authentication** - Аутентификация для WebSocket и REST API

### Frontend
- **Flutter (liminal_pulse)** - Мобильное приложение для iOS и Android
- **Web Interface** - Веб-интерфейс для управления и мониторинга

### Инфраструктура
- **Docker** - Контейнеризация всех компонентов
- **Prometheus** - Сбор метрик и мониторинг
- **Grafana** - Визуализация метрик

## Ключевые функции

- WebSocket коммуникация в реальном времени
- Распределенная архитектура с поддержкой масштабирования
- Защита от DoS атак
- Аутентификация и авторизация
- Метрики и мониторинг

## Документация

- [API Reference](api.md) - Документация по API endpoints
- [Deployment Guide](deployment.md) - Руководство по развертыванию
- [Redis Channels](REDIS_CHANNELS.md) - Описание каналов Redis и протокола сообщений

## Требования

- Python 3.11+
- Redis 7.0+
- Neo4j 5.11+
- Docker и docker-compose
- Flutter SDK (для сборки мобильного приложения)

## Разработка

Для начала разработки:

```bash
# Клонирование репозитория
git clone https://github.com/yourusername/resonance-liminal.git
cd resonance-liminal

# Запуск среды разработки
docker-compose -f docker-compose.dev.yml up -d

# Запуск backend в режиме разработки
cd backend
pip install -r requirements-dev.txt
uvicorn main:app --reload --log-config logging.yaml
```

## Лицензия

© 2025 Resonance Liminal. Все права защищены.
