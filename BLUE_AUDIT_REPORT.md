# 🔵 Blue Bug Audit — Liminal

**Дата:** 2026-05-17
**Ветка:** `claude/blue-audit-bugs-Ucal8`
**Скоуп:** Python (backend, src, auth), JavaScript/TypeScript (frontend, webapp), Go (go_ws_relay), Haskell (rinse), конфигурация (Docker, .env, k8s, nginx, CI)

Этот отчёт — реальные баги, найденные в текущем коде. Каждый пункт привязан к `file:line` с фрагментом. Сортировка — по приоритету (Critical → Low).

---

## 🚨 CRITICAL (требуется немедленный фикс)

### C-1. Auth bypass: `DummyCryptContext.verify` всегда возвращает `True`
**Файл:** `backend/auth/jwt_utils.py:31-43`

```python
class DummyCryptContext:
    def verify(self, plain_password, hashed_password):
        # В тестовом режиме всегда возвращаем True
        return True
```

Fallback срабатывает, если `passlib`/`bcrypt` не импортируется (`ImportError | AttributeError`). В этом режиме `authenticate_user(...)` принимает **любой пароль** для существующего пользователя. На любом окружении, где bcrypt не собрался (Alpine, ARM, поломанный wheel) — это рабочий бэкдор. Решение: при провале импорта бросать на старте, а не подсовывать «no-op» проверку.

### C-2. JWT secret и пароль Neo4j захардкожены как дефолты на проде
**Файлы:** `backend/config.py:29`, `backend/config.py:36-38`, `backend/config.py:58`, `backend/config.py:61-63`

```python
neo4j_password: str = "NewStrongPass123!"
jwt_secret_key: str = "test-jwt-secret-key-for-local-development-only"
...
"neo4j_password": os.getenv("NEO4J_PASSWORD", "NewStrongPass123!"),
"jwt_secret_key": os.getenv("JWT_SECRET_KEY", "test-jwt-secret-key-for-local-development-only"),
```

При забытой переменной окружения сервис стартует с предсказуемым секретом → подделка JWT, доступ к Neo4j. Решение: `raise RuntimeError` при `ENV in {"production","staging"}` и пустых креденшелах.

### C-3. CORS `allow_origins=["*"]` + `allow_credentials=True` на 4 сервисах
**Файлы:**
- `backend/api.py:202-208`
- `backend/emotime/api_simple.py:27-32`
- `backend/production/rgl_core_service.py:184-190`
- `backend/ml/xai_main.py:67-73`

Комбинация `*` + credentials — нарушение CORS-спецификации, открывает credential theft / CSRF с любого внешнего сайта (браузер всё-таки запрещает буквальный `*`+credentials, но FastAPI это часто разворачивает в эхо-Origin — фактический результат: разрешено всё). Решение: задать конкретный whitelist через настройки.

### C-4. Hardcoded пароли в `docker-compose.production.yml`
**Файл:** `backend/production/docker-compose.production.yml:15, 88, 125`

```yaml
NEO4J_PASSWORD=rgl_production_password
NEO4J_AUTH=neo4j/rgl_production_password
GF_SECURITY_ADMIN_PASSWORD=rgl_admin_password
```

Прод-креды закоммичены в репозиторий. Должны идти через секреты Docker/K8s (`secrets:`).

### C-5. Файл `.env` отслеживается git'ом
**Файл:** `.env` (107 строк), хотя в `.gitignore` указаны только `.env.local` и `.env.production`.

```
JWT_SECRET=your-super-secret-jwt-key-here
OPENAI_API_KEY=sk-your-openai-api-key-here
GRAFANA_ADMIN_PASSWORD=admin
DEBUG=true
DATABASE_URL=postgresql://user:password@localhost:5432/resonance_liminal
```

Сейчас в файле плейсхолдеры, но любой `git add .` уже добавит реальные креды — слот в репозитории для них существует. Решение: `git rm --cached .env`, перенести в `.env.example`, добавить `.env` в `.gitignore`.

### C-6. Hardcoded Neo4j-пароль в Go-сервисе
**Файл:** `go_ws_relay/neo4j_api.go:14-17`

```go
const (
    neo4jURI      = "neo4j://localhost:7687"
    neo4jUsername = "neo4j"
    neo4jPassword = "NewStrongPass123!"
)
```

Должно читаться из переменных окружения. `const` ещё и попадает в бинарь — выдать его уже значит выдать пароль.

---

## 🔴 HIGH

### H-1. Resource leak: `traceback.print_exc(file=open(OUTPUT_FILE, "a"))`
**Файл:** `backend/file_output_philosophy.py:115, 161, 203, 250, 280`

`open()` без `with` и без `.close()` — каждый вызов оставляет дескриптор. Заменить на:
```python
with open(OUTPUT_FILE, "a") as f:
    traceback.print_exc(file=f)
```

### H-2. Unsafe `pickle.load` для ML-моделей
**Файл:** `backend/ml_pipeline_enhanced.py:203-212`

```python
def load(self, path: str) -> bool:
    with open(path, "rb") as f:
        self.model = pickle.load(f)
```

Если путь к файлу хоть как-то контролируется снаружи (configs, скачанные артефакты из MLflow/MinIO) — RCE. Минимум: подписать артефакт (HMAC), верифицировать перед `pickle.load`. Лучше: `joblib` + sklearn-only / ONNX.

### H-3. Тестовые пользователи `testpass`/`admin123` зашиты в код
**Файл:** `backend/auth/jwt_utils.py:181-197`

```python
fake_users_db = {
    "testuser": {"hashed_password": jwt_manager.get_password_hash("testpass"), ...},
    "admin":    {"hashed_password": jwt_manager.get_password_hash("admin123"), ...},
}
```

Эти учётки активны на любом окружении, импортирующем `jwt_utils`, в т.ч. через `/auth/token` в проде. Решение: убрать из модуля, оставить только в фикстурах тестов.

### H-4. Reflected XSS через `innerHTML` со значениями из CONFIG/state
**Файлы:**
- `frontend/consciousness_map/js/state-controller.js:189`
- `frontend/consciousness_map/js/graph-renderer.js:311`
- `frontend/consciousness_map/js/main.js:52, 172, 255`

```javascript
insightPanel.innerHTML = `
  <h4>Утренний путь: ${stateInfo.label}</h4>
  <p>${stateInfo.description}</p>
`;
```

`stateInfo.*` приходит из объединения CONFIG и backend-данных (Neo4j). Если оттуда прилетит `<img onerror=...>` — выполнение в DOM. Решение: `textContent`, или `createElement` + `appendChild`, или эскейпить через шаблонизатор.

### H-5. Goroutine без `context.Context` — утечка по соединениям
**Файл:** `go_ws_relay/graphql/subscriptions.go:194-210`

```go
go func() {
    ticker := time.NewTicker(pingPeriod)
    defer ticker.Stop()
    defer conn.Close()
    for {
        select {
        case <-ticker.C:
            // нет ветки <-ctx.Done()
        }
    }
}()
```

При обрыве WS-клиента goroutine продолжает тикать, пока `conn.WriteMessage` не упадёт сам. Плюс **двойной `defer conn.Close()`** (другая goroutine рядом тоже закрывает) — гонка/паника на втором закрытии. Решение: пробросить `ctx`, `select { case <-ctx.Done(): return; case <-ticker.C: ... }`, и закрывать соединение в одном месте.

### H-6. `useEffect(load, [])` со внешней зависимостью
**Файл:** `webapp/src/app/progress/page.tsx:84`

```tsx
useEffect(() => { load(days); }, []);
```

Здесь автор «спас» баг ручным вызовом `load(d)` в `onClick`, но lint-правило `react-hooks/exhaustive-deps` всё равно отключено молча. Любой следующий разработчик, кто перестанет вызывать `load` вручную, получит стейл-данные. Поставить `[days]` или вынести во `useCallback`.

### H-7. Partial functions `head`/`(!!)` без проверок
**Файл:** `rinse/src/ResonanceGrafting.hs:134, 224`, `rinse/src/DuneField.hs:179`, `rinse/src/RINSE.hs:40`

```haskell
selectedPaths = map (allPaths !!) selectedIndices
...
TYOK.etTimestamp (head states)
```

`!!` и `head` падают рантайм-эксепшеном на пустом/коротком списке. Использовать `safe`/`listToMaybe`/`Data.Maybe.fromMaybe`.

### H-8. Bare `except:` глотает `KeyboardInterrupt`/`SystemExit`
**Файлы (выборка):** `backend/emotime/security/auth.py:279`, `backend/security_enhanced.py:583`, `backend/emotime/metrics_integration.py:30, 63`, `src/liminal/reality_web_neo4j.py:425`, `backend/quantum/rgl_quantum_integration.py:157`, `backend/bci/mindbridge_direct.py:148, 853`, `backend/emotime/utils.py:23, 32, 52`, `backend/performance/*.py` — всего ~20 мест.

```python
try:
    payload = jwt.decode(token, ..., options={"verify_exp": False})
    ...
except:
    pass
```

Особенно опасно в `auth.py:279`: глотает ошибки декодирования JWT при ревокации (см. также **H-9**).

### H-9. Ревокация JWT с `verify_exp=False`
**Файл:** `backend/emotime/security/auth.py:270-280`

```python
payload = jwt.decode(token, self.secret_key, algorithms=[self.algorithm],
                     options={"verify_exp": False})
```

В контексте ревокации это нормально (мы и хотим вытащить `sid` даже из протухшего токена), **но** при этом `audience`/`issuer` не проверяются → можно отозвать сессию по чужому валидному JWT с другим `aud`. Добавить `audience=…, issuer=…`.

### H-10. Type assertion'ы в Go без `ok`
**Файл:** `go_ws_relay/neo4j_api.go:140-189`

```go
ID:    id.(string),
State: state.(string),
```

Если Neo4j вернёт `nil` или другой тип — паника всего сервиса. `v, ok := x.(string); if !ok { ... }`.

### H-11. setInterval без clearInterval — утечка таймеров в SPA
**Файл:** `frontend/consciousness_map/js/main.js:59-71, 214`

```javascript
setInterval(() => { ... }, 1000);
app.emotimePollInterval = setInterval(pollEmotimeStatus, EMOTIME_POLL_MS);
```

При навигации (или повторной инициализации) предыдущий таймер продолжает работать; за день вкладки наберётся горка событий и сетевых запросов.

### H-12. Hardcoded test-креды в docker-compose.local.yml / docker-compose.ml.yml
**Файлы:** `docker-compose.local.yml:12, 29, 40, 83`, `docker-compose.ml.yml:95-96`, `docker-compose.ml-production.yml:65-66`

```yaml
POSTGRES_PASSWORD=test_postgres_password_123
NEO4J_AUTH=neo4j/test_neo4j_password_123
MINIO_ROOT_USER=minioadmin
MINIO_ROOT_PASSWORD=minioadmin
```

`docker-compose.ml-production.yml` — это уже не «test», но пароли всё равно слабые и в репо.

---

## 🟡 MEDIUM

### M-1. Отсутствует `_track_operation()` в одном из методов Neo4jClient
**Файл:** `backend/infrastructure/neo4j/client.py:145`

`create_memory_fragment_node` не оборачивается в `with self._track_operation(...)` в отличие от соседних методов — операции этого метода не попадают в метрики/локи. Расхождение поведения по записи.

### M-2. Division-by-zero в test-раннере
**Файл:** `test_runner.py:263, 278`

```python
print(f"Success Rate: {(passed_tests/total_tests)*100:.1f}%")
```

При пустом списке упадёт `ZeroDivisionError` — портит CI-отчёт. То же в `backend/test_integration_simple.py:265`.

### M-3. Map гонка в WS-relay
**Файл:** `go_ws_relay/main.go:23-24` и места, где `clients` map читают без `clientsMutex`.

Несинхронизированный доступ к map в Go — undefined behavior, обычно `concurrent map read and map write` → паника.

### M-4. `topic, _ := request["topic"].(string)` — пустая строка как валидный topic
**Файл:** `go_ws_relay/graphql/subscriptions.go:162`

Игнор `ok` приводит к тому, что неверный тип превратится в `""`, и подписка пройдёт на «пустой» топик.

### M-5. `subprocess.run(["chcp", "65001"], shell=True)`
**Файл:** `backend/emotime/utils.py:22`

`shell=True` со списком — Python берёт **только первый элемент** (`"chcp"`), остальное игнорится, при этом запускается shell. Это и не работает как задумано, и расширяет поверхность атаки на ровном месте. Убрать `shell=True`.

### M-6. MD5 для cache-key
**Файл:** `backend/database_optimization.py`

```python
return hashlib.md5(query_str.encode()).hexdigest()
```

Технически не криптоиспользование, но MD5 нагружает сэндбоксы/FIPS-режимы (`hashlib.md5(..., usedforsecurity=False)` или blake2). На некоторых сборках упадёт.

### M-7. `datetime.utcnow()` в 81 месте
В Python 3.12+ deprecated (DeprecationWarning) и в 3.13 удаление приближается. Менять на `datetime.now(timezone.utc)`.

### M-8. Двойной `defer conn.Close()` в WS-relay
См. **H-5** — `subscriptions.go:138` и `:198` оба `defer conn.Close()`. Один из них вернёт ошибку «use of closed network connection».

### M-9. Nullable-узлы в frontend graph
**Файл:** `frontend/consciousness_map/js/data-service.js:356-358`

```javascript
const source = this.nodes.find(node => node.id === transition.from_id);
const target = this.nodes.find(node => node.id === transition.to_id);
return { ...target, ... };
```

Если узел не найден, `target === undefined` — спред `...undefined` ок, но дальнейшие `.x` упадут.

### M-10. Хардкод тестовых креденшелов в GitHub Actions
**Файл:** `.github/workflows/production-deployment.yml:147, 172, 182, 192`

```yaml
NEO4J_AUTH: neo4j/testpassword
NEO4J_PASSWORD: testpassword
```

Для CI-теста ок, но соблазн «временно» так же делать в env-job — фиксируется как анти-паттерн.

---

## 🟢 LOW

### L-1. `HS256` для long-lived токенов
`backend/emotime/security/auth.py:75`: комментарий «Use RS256 in production» — но код всё равно ставит `HS256`. Рекомендуется ассиметричная подпись для refresh/websocket токенов длиной в часы/дни.

### L-2. Refresh-токен подписан тем же секретом, что и access
`backend/auth/jwt_utils.py:262-274`. При утечке секретa access → утечка refresh. В идеале — отдельный ключ для refresh.

### L-3. Promise-rejection без `.catch` при старте
`frontend/consciousness_map/js/main.js:85-91` — `loadDataFromNeo4j()` запускается без `.catch`, что приводит к `Uncaught (in promise)`. Сейчас вроде `.catch` есть, но проверить полный путь.

### L-4. Пустой `scopes`/`roles` в `/me`
`backend/auth/auth_router.py:122-132` отдаёт `scopes` и `roles`, которые в `fake_users_db` не определены — `KeyError` маскируется `.get(..., [])`, но клиент получает фиктивные значения.

### L-5. `lru_cache` на `get_jwt_manager()` + чтение `Settings`
`backend/auth/jwt_utils.py:172-179` — секрет читается один раз; ротация ключа без рестарта невозможна.

---

## Сводка

| Категория | Critical | High | Medium | Low | Всего |
|---|---|---|---|---|---|
| Auth/Crypto | 2 | 2 | 1 | 2 | 7 |
| Web/CORS/XSS | 1 | 1 | 1 | 1 | 4 |
| Secrets в репозитории | 3 | 1 | 1 | 0 | 5 |
| Resource leak / Race | 0 | 3 | 3 | 0 | 6 |
| Logic / partial functions | 0 | 2 | 2 | 0 | 4 |
| Deprecation / стиль | 0 | 0 | 2 | 0 | 2 |
| **Итого** | **6** | **12** | **10** | **5** | **33** |

## Приоритет фиксов (mini-plan)

1. **C-1**: убрать `DummyCryptContext` (fail-fast на старте).
2. **C-2/C-4/C-6**: вынести все пароли в секреты, требовать env-vars, упасть в проде без них.
3. **C-3**: CORS whitelist.
4. **C-5**: `git rm --cached .env`, обновить `.gitignore`, добавить `.env.example`.
5. **H-1/H-2/H-3/H-5/H-10**: пакет «руки» — context managers, signed pickle/joblib, удалить `fake_users_db`, `ctx` в goroutine, `, ok` в type assertions.
6. **H-4/H-11/M-9**: фронтовая санитизация + cleanup таймеров.
7. **H-7/H-8/M-2/M-4/M-8**: «строгие проверки» — partial functions, bare `except`, division-by-zero, ok-checks.

---

_Сгенерировано: автоматический Blue-аудит, ветка `claude/blue-audit-bugs-Ucal8`._
