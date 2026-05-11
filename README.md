# Liminal

**Continuity layer for human and AI transitions.**

Liminal helps humans preserve inner continuity through difficult transitions, and helps AI agents preserve technical continuity through traces, causal memory, and auditable execution.

---

## Why it matters

Modern systems break continuity:

- humans lose clarity under stress
- AI agents lose context across steps
- actions happen without durable reasons
- logs show events but not responsibility
- agent behavior is hard to replay or audit

Liminal is an ecosystem of tools for making transitions visible, traceable, and safer.

---

## Choose your path

### I am a developer
Start with LTP, CML, CaPU, LiminalDB, DAO_lim, and the local health/readiness checks below.

### I am a researcher
Read the continuity, causal memory, trace replay, and agent audit materials.

### I am a founder or operator
Start with [Liminal Agent Efficiency Audit](docs/commercial/AGENT_EFFICIENCY_AUDIT.md), then read the [Agent Audit Demo Story](docs/demo/AGENT_AUDIT_DEMO_STORY.md).

### I am interested in human growth
Start with Liminal Pulse, Inner Council, emotional memory, and transition support concepts.

---

## Ecosystem entry points

- [Ecosystem map](ECOSYSTEM.md)
- [Project status](STATUS.md)
- [Community roadmap](COMMUNITY_ROADMAP.md)
- [Documentation index](docs/README.md)
- [Agent Efficiency Audit](docs/commercial/AGENT_EFFICIENCY_AUDIT.md)
- [Agent Audit Demo Story](docs/demo/AGENT_AUDIT_DEMO_STORY.md)

---

## Project maturity

| Component | Status | Notes |
|---|---|---|
| Liminal Core | Early-access | Product/research hub |
| WebSocket backend | Prototype/working | FastAPI, JWT, Prometheus, tests |
| Liminal Pulse | Prototype direction | Flutter app direction |
| LTP | Active technical project | Trace continuity and deterministic replay |
| CML | Active technical project | Causal audit layer |
| CaPU | Spec-first runtime | Permission-first safe action lifecycle |
| LiminalDB | Active Rust project | Reactive state database |
| DAO_lim | Active infra project | AI backend routing and fallback |
| LiminalQAengineer | Product/prototype direction | QA observability and risk scoring |

---

# 🌌 LIMINAL — путь к внутренней мудрости

**LIMINAL** — это гибридная AI-платформа для осознанных переходов, внутренней зрелости и развития автономии.

## 📚 Оглавление
- [📘 Документация проекта](docs/README.md)
- [🌍 Ecosystem map](ECOSYSTEM.md)
- [🔍 Project status](STATUS.md)
- [🧭 Community roadmap](COMMUNITY_ROADMAP.md)
- [💼 Agent Efficiency Audit](docs/commercial/AGENT_EFFICIENCY_AUDIT.md)
- [🎬 Agent Audit Demo Story](docs/demo/AGENT_AUDIT_DEMO_STORY.md)
- [✨ Что делает LIMINAL](#-что-делает-liminal)
- [🧠 Архитектура](#-архитектура)
- [🔍 Статус](#-статус)
- [🩺 Проверка здоровья (health/readiness)](#-проверка-здоровья-healthreadiness)
  - [Скрипт быстрой проверки (Windows)](#скрипт-быстрой-проверки-windows)
  - [Скрипт быстрой проверки (Linux/macOS)](#скрипт-быстрой-проверки-linuxmacos)
  - [Мини‑FAQ: health/readiness](#мини‑faq-healthreadiness)
- [📜 Лицензия](#-лицензия)
- [📫 Связь](#-связь)

## ✨ Что делает LIMINAL

- Отслеживает временные и биометрические паттерны
- Поддерживает пользователя в переходах: от тревоги к ясности
- Развивает внутреннюю мудрость через «Внутренний Совет»
- Помогает выстраивать диалог с собой

## 🧠 Архитектура

Сердце LIMINAL — это модульная архитектура:
- `Pythia Core (Haskell)`
- `Realtime Engine (Elixir)`
- `ML Pipeline (Python)`
- `Meta-Liminal Engine` + `Maturity Engine`
- Интерфейс на `Elm/Flutter`

## 🔍 Статус

Проект находится на стадии early-access.
Открыта форма сбора интереса и обратной связи: [https://safal207.github.io/Liminal](https://safal207.github.io/Liminal)

См. также: [STATUS.md](STATUS.md).

## 🩺 Проверка здоровья (health/readiness)

Быстрый локальный чек готовности backend:

1) Запуск сервера
- python -m uvicorn backend.app.main:app --reload --port 8000

2) Проверка эндпоинтов (PowerShell)
- Invoke-RestMethod http://127.0.0.1:8000/health
- Invoke-RestMethod http://127.0.0.1:8000/ready

Альтернатива (curl):
- curl http://127.0.0.1:8000/health
- curl http://127.0.0.1:8000/ready

#### Скрипт быстрой проверки (Windows)

- Запуск:
  - `./scripts/check-health.ps1 [-BaseUrl http://127.0.0.1:8000] [-Retries 10] [-DelaySec 2] [-FailOnNotReady]`
- Примеры:
  - `./scripts/check-health.ps1`
  - `./scripts/check-health.ps1 -BaseUrl http://127.0.0.1:8080 -Retries 20 -DelaySec 3 -FailOnNotReady`

#### Скрипт быстрой проверки (Linux/macOS)

- Запуск:
  - `bash ./scripts/check-health.sh [--url http://127.0.0.1:8000] [--retries 30] [--delay 2] [--fail-on-not-ready]`
- Примеры:
  - `bash ./scripts/check-health.sh`
  - `bash ./scripts/check-health.sh --url http://127.0.0.1:8080 --retries 60 --delay 1 --fail-on-not-ready`

Ожидаемые ответы (пример):

- /health

```json
{
  "status": "ok",
  "timestamp": "2025-01-01T12:00:00Z",
  "ml_enabled": true,
  "redis_connected": false
}
```

- /ready

```json
{
  "ready": true,
  "checks": {
    "app_loaded": true,
    "event_loop": true,
    "redis_configured": false,
    "redis_connected": true,
    "ml_enabled": true
  },
  "timestamp": "2025-01-01T12:00:00Z"
}
```

Примечания:
- Переменная среды `USE_REDIS=false` (по умолчанию) — Redis не требуется; `redis_configured=false`, `redis_connected` может быть `false` и это не мешает готовности.
- При `USE_REDIS=true` сервер попытается подключиться к Redis. Если Redis недоступен, `/ready` останется `true` (готовность определяется базовыми условиями), а детали будут отражены в `checks`.
- Метрики Prometheus на `/metrics`. Если модуль метрик недоступен, сервер стартует, а метрики мягко отключаются.

### Мини‑FAQ: health/readiness

- Вопрос: `/ready` возвращает `false`.
  Ответ: убедись, что сервер действительно запущен и запрос идёт на правильный порт/хост. Внутри обработчика должен быть доступен event‑loop. Проверь, не обращаешься ли к другому процессу или контейнеру.
- Вопрос: `redis_connected=false` — это ошибка?
  Ответ: нет, если `USE_REDIS=false`. Готовность не требует внешних сервисов. Это информационное поле.
- Вопрос: Сервер не поднимается из‑за `metrics` импорта.
  Ответ: установи dev‑зависимости из `requirements-dev.txt` или временно отключи метрики. По умолчанию реализован мягкий импорт; если возникла ошибка — проверь версию Python и рабочую директорию запуска.
- Вопрос: Порт занят.
  Ответ: измени порт запуска: `python -m uvicorn backend.app.main:app --port 8080` и обратись к `http://127.0.0.1:8080`.
- Вопрос: Ответ нестабилен в CI.
  Ответ: дождись готовности (в CI ожидание увеличено до 60s), проверь логи старта и совпадение адресов.

## 📜 Лицензия

**Все права защищены.** Этот проект защищен проприетарной лицензией LIMINAL.

**ВАЖНО:** Копирование, модификация, распространение или коммерческое использование этого программного обеспечения без письменного разрешения правообладателей **ЗАПРЕЩЕНО**.

Подробности см. в файле [LICENSE](LICENSE).

Для получения разрешения на использование свяжитесь с правообладателями.

## 📫 Связь

Если тебе откликается — пиши: `safal0645@gmail.com`
