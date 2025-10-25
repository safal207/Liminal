# CONTRIBUTING to Liminal

Спасибо, что помогаете делать Liminal лучше. Здесь — короткий, практичный гайд.

## 1) Быстрый старт (Dev)
- Python 3.10+
- Рекомендуем venv:
  - Windows PowerShell: `python -m venv .venv && . .venv/Scripts/Activate.ps1`
  - Git Bash: `python -m venv .venv && source .venv/Scripts/activate`
- Установить зависимости: `pip install -r requirements.txt`

## 2) Pre-commit хуки
Хуки помогают держать код чистым и предсказуемым.
- Установка: `pip install pre-commit`
- Активировать в репо: `pre-commit install`
- Запуск всех проверок локально: `pre-commit run --all-files`
- Временный обход (не злоупотреблять): `git commit -m "..." --no-verify`

Если коммит падает — смотри лог: `C:\Users\<you>\.cache\pre-commit\pre-commit.log`

## 3) Ветвление и PR
- Ветки по задаче: `feat/<scope>-<short>`, `fix/<scope>-<short>`, `docs/<short>`
- Открываем PR в `main`. Один PR — одна "грядка" (одно изолированное изменение)
- Перед PR: `pre-commit run --all-files`, локальные тесты

## 4) Коммиты (стиль)
- Формат: `<type>: <summary>` где `type` ∈ {feat, fix, docs, chore, refactor, test}
- Примеры:
  - `docs: restore legacy README as main; add health & TOC`
  - `feat(api): add readiness endpoint`
  - `chore(ci): bump pre-commit hooks`

## 5) Документация
- Основной файл: `README.md` (обзор, архитектура, health/readiness)
- Клон-обзор: `Liminal_readme.md` (с датой обновления)
- Архив предыдущих README: `docs/README_HISTORY/`
- Mermaid-блоки: используем ```mermaid без лишних отступов
- Добавляйте дату обновления в начало ключевых доков: `Последнее обновление: YYYY-MM-DD`

Инсайт проекта: «Случайности не случайны». Любой сбой — сигнал улучшения. Фиксируем знание в доках.

## 6) Временные/локальные файлы
- Временные каталоги (например, `temp_packages/`) — держим локально, игнорируем в git.
- Добавляйте правила в `.gitignore` и используйте `git rm --cached` для очистки индекса.

## 7) Health/Readiness (локальный чек)
- Запуск: `python -m uvicorn backend.api:app --reload --port 8000`
- Проверки:
  - PowerShell: `Invoke-RestMethod http://127.0.0.1:8000/health` и `/ready`
  - curl: `curl http://127.0.0.1:8000/health` и `/ready`
- Скрипты: `./scripts/check-health.ps1` и `bash ./scripts/check-health.sh`

## 8) CI/CD
- PR — автопроверки. Падающие проверки — не мержим.
- Просим ревьюера указывать «один главный фокус» на PR, чтобы двигались грядками.

## 9) DatabaseAdapter: работа с данными

Универсальный адаптер для работы с Datomic и Neo4j. Каждый тип данных автоматически маршрутизируется в оптимальную БД.

### Инициализация для разработки

```python
from backend.database_adapter import DatabaseAdapter, DataType, get_database_adapter

# Вариант 1: Используем глобальный экземпляр (рекомендуется)
adapter = get_database_adapter()

# Вариант 2: Создаем свой экземпляр
adapter = DatabaseAdapter(
    # Datomic настройки
    datomic_uri="http://localhost:8080",
    datomic_db_name="liminal",
    
    # Neo4j настройки
    neo4j_uri="bolt://localhost:7687",
    neo4j_user="neo4j",
    neo4j_password="password",
    
    # Включаем автоматический fallback
    fallback_enabled=True
)
```

### Работа с типами данных

```python
# Для Datomic подходят:
DataType.TEMPORAL       # Временные данные
DataType.EVENT          # События
DataType.AUDIT          # Аудит действий
DataType.EMOTION_HISTORY # История эмоций
DataType.SESSION_DATA   # Данные сессий

# Для Neo4j подходят:
DataType.RELATIONSHIP   # Связи
DataType.GRAPH          # Графовые структуры
DataType.PHILOSOPHY     # Философские состояния
DataType.CONCEPT_MAP    # Карты концептов
DataType.USER_NETWORK   # Сети пользователей
```

### Сохранение и запрос данных

```python
import asyncio

async def example():
    # Сохранение данных с автоматическим выбором БД
    emotion_id = await adapter.store_data(
        data={"emotion": "радость", "intensity": 0.8},
        data_type=DataType.EMOTION_HISTORY,
        user_id="user-123"
    )
    
    # Запрос данных с фильтрацией
    emotions = await adapter.query_data(
        data_type=DataType.EMOTION_HISTORY,
        filters={"user_id": "user-123"},
        limit=10
    )

# Запуск асинхронной функции
asyncio.run(example())
```

### Best practices

1. **Всегда закрывайте подключения**
   ```python
   try:
       # Ваш код
   finally:
       adapter.close()
   ```

2. **Проверяйте доступность БД**
   ```python
   health = adapter.get_health_status()
   if health["status"] == "healthy":
       # Работа с адаптером
   ```

3. **Используйте правильные типы данных**
   - Правило: Datomic для временных данных, Neo4j для графовых структур
   - Избегайте хранения больших массивов данных или бинарных файлов

4. **Тестирование с моками**
   ```python
   # backend/tests/test_adapter.py
   from unittest.mock import patch, MagicMock
   
   @patch('backend.database_adapter.DatomicClient')
   def test_datomic_fallback(mock_datomic):
       mock_datomic.return_value.connect.return_value = False
       adapter = DatabaseAdapter(fallback_enabled=True)
       # Тест автоматического переключения на Neo4j
   ```

### Решение частых проблем

- **Ошибка «ImportError: No module named 'backend.database_adapter'»**
  - Решение: Запускайте скрипты из корня проекта или добавьте корень в PYTHONPATH

- **Обе БД недоступны**
  - Решение: Проверьте настройки подключения и доступность БД
  - Fallback работает только при наличии хотя бы одной доступной БД

- **Исключение при сохранении данных**
  - Проверьте соответствие структуры данных типу (Datomic требует временные метки)

Применяем принцип «Случайности не случайны»: если ваш код взаимодействия с БД стабильно падает в определенном месте, возможно это сигнал к улучшению архитектуры данных или запросов.

## 10) Контакты
`safal0645@gmail.com`

## 10) WSL‑плейбук: запуск и тесты

Рекомендуемый способ разработки и запуска — через WSL. Это снижает риски сбоев pip/OneDrive/прокси в Windows.

1) Активация окружения

```bash
# внутри WSL
source ~/.venvs/liminal/bin/activate
cd /mnt/c/Users/safal/OneDrive/Documente/GitHub/resonance-liminal
```

1.1) Отключить прокси для локальных адресов (если замечены подвисания)

```bash
export NO_PROXY=127.0.0.1,localhost
export no_proxy=127.0.0.1,localhost
```

2) Устойчивая установка зависимостей

```bash
python -m pip install --upgrade pip
pip install -r requirements.txt \
  -i https://pypi.org/simple --default-timeout 25 --no-cache-dir -vvv
```

Примечание: при проблемах сети используйте wheel из локального кэша/каталога. Избегайте путей в OneDrive для кешей.

3) Запуск приложения

```bash
uvicorn backend.main:app --host 127.0.0.1 --port 8000 --reload
```

Важно: в WSL режим NAT не поддерживает прокси на localhost. Используйте явный адрес `127.0.0.1`.

4) Проверка health/docs/ready (в другом терминале WSL)

```bash
curl --noproxy '*' http://127.0.0.1:8000/health
curl --noproxy '*' http://127.0.0.1:8000/docs
curl --noproxy '*' http://127.0.0.1:8000/ready
```

Примечание: `/ready` всегда возвращает 200 (статус ready/degraded), `/health/ready` может вернуть 503.

5) JWT — быстрый тест

```bash
TOKEN=$(curl -s --noproxy '*' -X POST \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "username=testuser&password=testpass" \
  http://127.0.0.1:8000/auth/token | jq -r .access_token)

curl --noproxy '*' -H "Authorization: Bearer $TOKEN" http://127.0.0.1:8000/auth/me
```

Подробнее см. раздел "🔐 JWT: быстрый старт" в README.md.

6) Тесты

✨ Быстрый старт

- 🧪 Только мультиязычный tie-break (expected_lang):
  - Windows PowerShell: `./scripts/test-ml-expected.ps1`
  - Linux/WSL: `bash ./scripts/test-ml.sh`
- 🚀 Весь набор (кроме integration):
  - Windows PowerShell: `./scripts/test-ml-expected.ps1 -All`
  - Linux/WSL: `bash ./scripts/test-ml.sh --all`

📌 Целевые тесты JWT

```bash
pytest -q backend/tests/test_jwt_auth.py backend/tests/test_jwt_edge_cases.py
```

🔍 Подробный вывод (удобно для диагностики)

```bash
python -m pytest -vv --color=yes -rA -m "not integration"
python -m pytest -vv --color=yes -rA backend/tests/test_multilingual_expected_lang.py
```

📝 Логи в файл (если консоль «режет» вывод)

```bash
python -m pytest -vv -rA -m "not integration" > .pytest-log.txt
```

🧰 Троблшутинг

- ⏳ «Подвисает/тишина»: добавьте `-vv -rA` или уберите `-q`.
- 🐍 Импорт `backend.*`: запускайте из корня репо.
- 🌐 Сеть/прокси: для curl используйте `--noproxy '*'`.
- 📦 Отсутствуют dev-пакеты: установите `requirements-dev.txt` (устойчивой схемой pip).

🏁 Запуск через Makefile

- Только мультиязычный tie‑break: `make test-ml`
- Весь набор (без integration): `make test`

7) Логи

- Используется структурное логирование (structlog). Логи в stdout в JSON-формате.
- При ошибках импорта метрик/ML сервер должен продолжать старт с мягким отключением соответствующих модулей.

## 11) Мультиязычный анализ: expected_lang (tie-break)

Когда текст содержит только ASCII-символы, детекция языка может давать ничью между несколькими латинскими языками (en/de/fr/es/it). Для корректного разрешения используется параметр `expected_lang`:

- Вызов низкого уровня:
  ```python
  from backend.personality.multilingual_support import analyze_multilingual_text
  res = await analyze_multilingual_text(text, adapter.analyze_text, target_lang="de", expected_lang="de")
  ```
- Вызов высокого уровня через ML-адаптер:
  ```python
  from backend.personality.ml_adapter import EmotionMLAdapter
  adapter = EmotionMLAdapter()
  res = await adapter.analyze_multilingual(text, target_lang="de", expected_lang="de")
  ```

Поведение:
- Если `expected_lang` присутствует и его «счёт» по LANGUAGE_MARKERS не хуже, чем у детектированного, выбирается `expected_lang`.
- Если `detected_lang == target_lang`, перевод не выполняется, поле `original_language` отсутствует.
- Если языки различаются, результат содержит `original_language`, `original_emotion`, а `emotion_type` переводится на `target_lang`.

Тесты:
```bash
pytest -q backend/tests/test_multilingual_expected_lang.py
```

## 12) Работа с Git (WSL/PowerShell)

### 1. Push через WSL:
```bash
# Отключить proxy если есть проблемы
unset http_proxy https_proxy

# Push с использованием PAT (будет запрошен как пароль)
git push https://github.com/safal207/Liminal.git main
```

### 2. Push через PowerShell:
```powershell
# Перейти в директорию проекта
cd "C:\Users\safal\OneDrive\Documente\GitHub\resonance-liminal"

# Выполнить push (PAT запросится как пароль)
git push origin main
```

### 3. Если возникают ошибки:
- Проверить URL репозитория (с заглавной L)
- Убедиться что PAT имеет права repo
- Временный workaround: использовать PowerShell вместо WSL
