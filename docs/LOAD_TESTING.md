# WebSocket Burst Load Testing (Artillery)

<a id="ws-burst-artillery-ru"></a>

## 🇷🇺 Быстрый старт
- Требуется: Node.js 18+, Python 3.11+, запущенный backend (FastAPI)
- Конфиг: `tests/load/ws-burst.yml`
- Скрипт: `scripts/ws-burst.ps1`

### 1) Локальный запуск
1. Запустите сервер:
   - `uvicorn backend.app.main:app --reload --host 127.0.0.1 --port 8000`
2. В другом терминале выполните:
   - `powershell -ExecutionPolicy Bypass -File scripts/ws-burst.ps1`

### 2) Что делает скрипт
- Проверяет доступность `http://127.0.0.1:8000/health/ready`
- Устанавливает Artillery при отсутствии (`npm i -g artillery`)
- Запускает `artillery run tests/load/ws-burst.yml`
- Возвращает код ошибки, если сбой SLO/ожиданий (HTTP 200, WS подключение)

### 3) Пороговые значения (SLO)
- В CI используется короткий smoke‑профиль (10 сек)
- Цели: `p95 < 100ms`, `errors < 1%`

---

<a id="ws-burst-artillery-en"></a>

## 🇬🇧 Quick Start
- Requirements: Node.js 18+, Python 3.11+, running FastAPI backend
- Config: `tests/load/ws-burst.yml`
- Script: `scripts/ws-burst.ps1`

### 1) Local run
1. Start server:
   - `uvicorn backend.app.main:app --reload --host 127.0.0.1 --port 8000`
2. In another terminal:
   - `powershell -ExecutionPolicy Bypass -File scripts/ws-burst.ps1`

### 2) What the script does
- Checks `http://127.0.0.1:8000/health/ready`
- Installs Artillery if missing (`npm i -g artillery`)
- Runs `artillery run tests/load/ws-burst.yml`
- Fails on unmet expectations (HTTP 200, WS connect)

### 3) SLO targets
- CI smoke profile (10s)
- Targets: `p95 < 100ms`, `errors < 1%`

---

## YAML Template Notes
- `GET /health/ready` expected 200
- `POST /token` should return `{ "token": "..." }`
- WS URL: `ws://127.0.0.1:8000/ws?token={{ token }}`

Adjust arrivalRate/duration for stronger load.
