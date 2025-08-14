# �� Белый список критичных проблем для исправления

**Дата:** Январь 2025  
**Статус:** Критично для production readiness  
**Приоритет:** Выполнить в течение 1-2 дней

---

## �� **КРИТИЧНЫЕ ПРОБЛЕМЫ (Исправить СЕГОДНЯ)**

### **1. 🐍 Python Version Conflicts**

#### **Проблема:** Конфликт версий FastAPI
**Файлы:** `requirements.txt` vs `backend/requirements.txt`
**Статус:** 🔴 Блокирует запуск приложения

**Что исправить:**
```bash
# В backend/requirements.txt заменить:
fastapi>=0.68.0
# На:
fastapi>=0.95.0
```

**Команда для исправления:**
```bash
cd backend
pip install fastapi>=0.95.0
pip freeze > requirements.txt
```

#### **Проблема:** Конфликт версий Pydantic
**Файлы:** `requirements.txt` vs `backend/requirements.txt`
**Статус:** 🔴 Блокирует валидацию данных

**Что исправить:**
```bash
# В backend/requirements.txt заменить:
pydantic>=1.8.0
# На:
pydantic>=2.0.0
```

**Команда для исправления:**
```bash
cd backend
pip install pydantic>=2.0.0
pip freeze > requirements.txt
```

### **2. 🔐 Security Issues**

#### **Проблема:** Хардкод паролей в docker-compose.yml
**Файл:** `docker-compose.yml`
**Статус:** 🔴 Уязвимость безопасности

**Что исправить:**
```yaml
# В docker-compose.yml заменить:
environment:
  - NEO4J_AUTH=neo4j/NewStrongPass123!
  - REDIS_PASSWORD=your_redis_password

# На:
environment:
  - NEO4J_AUTH=neo4j/${NEO4J_PASSWORD}
  - REDIS_PASSWORD=${REDIS_PASSWORD}
```

**Создать файл `.env`:**
```bash
# Создать .env файл
echo "NEO4J_PASSWORD=NewStrongPass123!" > .env
echo "REDIS_PASSWORD=your_redis_password" >> .env
```

### **3. 🏗️ Haskell Dependencies**

#### **Проблема:** Дублирование regex-tdfa
**Файл:** `rinse/stack.yaml`
**Статус:** �� Предупреждения при сборке

**Что исправить:**
```yaml
# В rinse/stack.yaml удалить дублирование:
regex-tdfa: "1.3.2"  # Удалить эту строку
# Оставить только:
regex-tdfa: "1.3.3"
```

**Команда для проверки:**
```bash
cd rinse
stack build --dry-run
```

---

## 🟡 **ВАЖНЫЕ ПРОБЛЕМЫ (Исправить НА ЭТОЙ НЕДЕЛЕ)**

### **4. 📱 Flutter Permissions**

#### **Проблема:** Отсутствуют разрешения для камеры
**Файлы:** `frontend/liminal_pulse/android/app/src/main/AndroidManifest.xml`
**Статус:** 🟡 Приложение упадет при использовании камеры

**Что исправить:**
```xml
<!-- Добавить в AndroidManifest.xml -->
<uses-permission android:name="android.permission.CAMERA" />
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
```

**Для iOS добавить в `Info.plist`:**
```xml
<key>NSCameraUsageDescription</key>
<string>This app needs camera access for biometric analysis</string>
```

### **5. 🐳 Docker Configuration**

#### **Проблема:** Не указана версия Python в Dockerfile
**Файл:** `backend/Dockerfile`
**Статус:** 🟡 Потенциальные проблемы совместимости

**Что исправить:**
```dockerfile
# В backend/Dockerfile добавить:
FROM python:3.11-slim
ENV PYTHON_VERSION=3.11
ENV PYTHONUNBUFFERED=1
```

### **6. 📊 Monitoring Setup**

#### **Проблема:** Отсутствуют health checks
**Файл:** `docker-compose.yml`
**Статус:** 🟡 Нет мониторинга состояния сервисов

**Что исправить:**
```yaml
# Добавить в docker-compose.yml для liminal-backend:
healthcheck:
  test: ["CMD", "curl", "-f", "http://localhost:8000/health"]
  interval: 30s
  timeout: 10s
  retries: 3
```

---

## 🟢 **УЛУЧШЕНИЯ (Исправить В БЛИЖАЙШЕЕ ВРЕМЯ)**

### **7. 🧪 Testing Setup**

#### **Проблема:** Неполное покрытие тестами
**Файлы:** `backend/tests/`
**Статус:** �� Низкое качество тестирования

**Что исправить:**
```bash
# Добавить в backend/requirements-dev.txt:
pytest-cov>=4.0.0
pytest-mock>=3.10.0
pytest-benchmark>=4.0.0
```

**Создать тесты:**
```python
# backend/tests/test_websocket.py
def test_websocket_connection():
    # Тест WebSocket соединения
    pass

def test_rinse_integration():
    # Тест интеграции с RINSE
    pass
```

### **8. 📝 Documentation**

#### **Проблема:** Отсутствует API документация
**Файлы:** `docs/`
**Статус:** 🟢 Нет документации для разработчиков

**Что исправить:**
```bash
# Добавить в backend/requirements-dev.txt:
mkdocs>=1.3.0
mkdocs-material>=8.0.0
```

**Создать документацию:**
```bash
mkdir -p docs
touch docs/index.md
touch docs/api.md
touch docs/deployment.md
```

---

## ✅ **ЧЕКЛИСТ ИСПРАВЛЕНИЙ**

### **День 1 (Критично):**
- [ ] Исправить FastAPI version conflict
- [ ] Исправить Pydantic version conflict  
- [ ] Убрать дублирование regex-tdfa
- [ ] Исправить пароли в docker-compose.yml
- [ ] Создать .env файл

### **День 2 (Важно):**
- [ ] Добавить разрешения камеры в Flutter
- [ ] Обновить Dockerfile
- [ ] Добавить health checks
- [ ] Запустить полное тестирование

### **День 3 (Улучшения):**
- [ ] Добавить comprehensive тесты
- [ ] Создать API документацию
- [ ] Настроить CI/CD pipeline
- [ ] Провести security audit

---

## �� **КОМАНДЫ ДЛЯ ТЕСТИРОВАНИЯ**

### **После исправлений запустить:**

```bash
# 1. Тест Python dependencies
cd backend
pip install -r requirements.txt
python -c "import fastapi; print('FastAPI OK')"
python -c "import pydantic; print('Pydantic OK')"

# 2. Тест Haskell dependencies
cd rinse
stack build

# 3. Тест Docker
docker-compose up --build

# 4. Тест Flutter
cd frontend/liminal_pulse
flutter pub get
flutter test

# 5. Тест WebSocket
curl -X GET "http://localhost:8000/health"
```

---

## 🎯 **КРИТЕРИИ УСПЕХА**

### **После исправлений должно работать:**
- [ ] Приложение запускается без ошибок
- [ ] WebSocket соединения работают
- [ ] Базы данных подключаются
- [ ] Flutter приложение компилируется
- [ ] Docker контейнеры запускаются
- [ ] Health checks проходят
- [ ] Нет security warnings

### **Метрики успеха:**
- ✅ 0 critical errors
- ✅ 0 security vulnerabilities  
- ✅ 100% совместимость компонентов
- ✅ Готовность к production deployment

---

## 🚀 **РЕЗУЛЬТАТ**

**После выполнения этого чеклиста:**
- Проект будет готов к production deployment
- Все компоненты будут совместимы
- Безопасность будет на уровне
- Можно будет привлекать инвесторов

**Время выполнения:** 2-3 дня
**Приоритет:** Критично для продолжения разработки 