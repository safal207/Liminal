# Resonance-Liminal Test Structure (2025-01-01)
# Организованная структура тестов для эффективного тестирования

tests/
├── __init__.py
├── conftest.py                    # Общие фикстуры и конфигурация
├── pytest.ini                     # Настройки pytest
│
├── unit/                          # Unit тесты
│   ├── __init__.py
│   ├── test_api.py               # API функции
│   ├── test_auth.py              # Аутентификация
│   ├── test_database.py          # База данных
│   ├── test_ml_models.py         # ML модели
│   ├── test_websocket.py         # WebSocket функции
│   └── test_utils.py             # Утилиты
│
├── integration/                   # Integration тесты
│   ├── __init__.py
│   ├── test_api_integration.py   # API интеграция
│   ├── test_database_integration.py # БД интеграция
│   ├── test_websocket_integration.py # WebSocket интеграция
│   └── test_full_flow.py         # Полный пользовательский flow
│
├── e2e/                          # End-to-end тесты
│   ├── __init__.py
│   ├── test_user_journey.py      # Пользовательские сценарии
│   ├── test_frontend.py          # Frontend тесты
│   └── test_mobile.py            # Мобильные тесты
│
├── load/                         # Load тесты
│   ├── __init__.py
│   ├── artillery/                # Artillery конфиги
│   └── locust/                   # Locust сценарии
│
├── data/                         # Тестовые данные
│   ├── fixtures/                 # Фикстуры данных
│   ├── mocks/                    # Моки
│   └── samples/                  # Примеры данных
│
└── reports/                      # Результаты тестов
    ├── coverage/                 # Покрытие кода
    ├── performance/              # Производительность
    └── screenshots/              # Скриншоты
