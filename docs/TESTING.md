# Testing Guide

This project separates fast, deterministic unit tests from slower and environment‑dependent integration smoke tests.

## Quick Start

- Unit tests (default, no external services):
  - `python -m pytest -q`
- Integration tests (require a running API/WS server):
  - `python -m pytest -q -m integration`

## Pytest Configuration

- `pytest.ini` sets `addopts = -v -m "not integration"`, so integration tests are skipped by default.
- Use `-m integration` to run them explicitly.

## WebSocket Integration Smoke Tests

- Located at `tests/test_websocket_integration.py`
- Will gracefully `skip` themselves if the server is not available.
- Endpoints can be configured with env vars:
  - `WS_API_URL` (default `http://localhost:8080`)
  - `WS_URL` (default `ws://localhost:8080/ws`)

## CI

- `.github/workflows/python-ci.yml` runs unit tests only (integration excluded by default).
- `.github/workflows/python-integration.yml` runs integration tests on demand (workflow_dispatch) and nightly schedule. Tests auto‑skip if the server is down.

### Quality pulse in CI (lint + types)

- Before tests, CI now checks the "posture and breathing" of the code:
  - `black --check .` — форма тела кода (единый стиль)
  - `isort --check-only --profile black .` — ровный кровоток импортов
  - `flake8` — чистота и гигиена
  - `mypy src tests` — здравая мысль типов (профилактика логических травм)

## Local PowerShell Helpers (Windows)

- `scripts/run-tests.ps1` — run fast unit tests.
- `scripts/run-tests-integration.ps1` — run integration tests (expects server at localhost:8080).

## Philosophy First Invariants

- Philosophical conclusions consistently include the key words: «философ» и «дом».
- Deterministic time injection (`now_fn`) and in‑memory storage (`InMemoryHistoryStorage`) ensure reproducible tests.
- Learning history is unified via `learning_history` with backward‑compatible aliasing where necessary.

## The system as a living organism (plain language)

Think of tests as daily diagnostics of a living system:

- Unit tests — это сердце и дыхание. Быстрые, спокойные, детерминированные: пульс ровный, дыхание глубинное. Никаких лишних стрессов (сети/IO), всё воспроизводимо.
- Integration smoke — это краткий осмотр органов. Мы мягко проверяем связь нервной системы (WebSocket) и кровеносной (HTTP API). Если организм спит (сервер не поднят) — мы не тревожим его (tests skip).
- Линтинг и типизация — гигиена и осанка. Black/isort выравнивают тело; flake8 следит за чистотой; mypy тренирует мышечную память типов, предотвращая травмы.
- История обучения — это память организма. Мы фиксируем события сознания и переосмысливаем опыт детерминированно, чтобы рост был устойчивым.

Язык наблюдения (как смотреть):

- "Пульс" — время прогона и стабильность прохождения тестов.
- "Давление" — количество предупреждений/ошибок линтеров и mypy (стремимся к нулю).
- "Иммунитет" — отсутствие флейков и зависимостей от внешней среды в юнитах.
- "Энергия" — низкая стоимость запуска (быстро, локально, без интернета).

## Tips

- Use `-vv -rA` for more verbose output while debugging.
- Use `--maxfail=1 --lf` to iterate quickly on failures.

## API Health/Readiness Quick Checks

- Start server:
  - `python -m uvicorn backend.api:app --reload --port 8000`
- PowerShell:
  - `Invoke-RestMethod http://127.0.0.1:8000/health | ConvertTo-Json -Depth 5`
  - `Invoke-RestMethod http://127.0.0.1:8000/ready  | ConvertTo-Json -Depth 5`
  - Windows helper script:
    - `./scripts/check-health.ps1 [-BaseUrl http://127.0.0.1:8000] [-Retries 10] [-DelaySec 2] [-FailOnNotReady]`
- curl:
  - `curl http://127.0.0.1:8000/health`
  - `curl http://127.0.0.1:8000/ready`
- Linux/macOS helper script:
  - `bash ./scripts/check-health.sh [--url http://127.0.0.1:8000] [--retries 30] [--delay 2] [--fail-on-not-ready]`

Expected:

```json
{
  "status": "ok",
  "timestamp": "2025-01-01T12:00:00Z",
  "ml_enabled": true,
  "redis_connected": false
}
```

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

Notes:
- `USE_REDIS=false` (default): Redis not required; readiness is true if app and event‑loop are ok.
- `USE_REDIS=true`: connects to Redis; if down, readiness stays true (base conditions only) and details are shown in `checks`.
- Prometheus metrics at `/metrics` (soft import).

### Mini‑FAQ (health/readiness)

- Q: `/ready` is `false`.
  A: Ensure server is running and the host/port are correct; event‑loop must be accessible inside the request.
- Q: `redis_connected=false` — is it an error?
  A: Not if `USE_REDIS=false`. External services are optional for readiness.
- Q: Startup fails due to `metrics` import.
  A: Install dev deps from `requirements-dev.txt` or disable metrics; check Python version and working directory.
- Q: Port busy.
  A: Change port: `python -m uvicorn backend.api:app --port 8080`.

## Уверенный коммит на Windows

Иногда pre-commit на Windows падает при создании служебных окружений (virtualenv seed/setuptools). Это лечится очисткой кэша и обновлением инструментов. Случайности не случайны — как учит мастер Угвей: каждая «неожиданность» показывает слабое место процесса и ведёт к устойчивости.

Протокол (корень репозитория, PowerShell):

1) `pre-commit clean`
2) `Remove-Item -Recurse -Force "$env:LOCALAPPDATA\pypa\virtualenv" -ErrorAction SilentlyContinue`
3) `.\.venv\Scripts\python -m pip install -U pip setuptools virtualenv`
4) `.\.venv\Scripts\pre-commit install`
5) `.\.venv\Scripts\pre-commit run --all-files`
6) `git add -A; git commit -m "chore: <кратко>"; git push`

План Б, если всё ещё падает:

- Установить переменную на сессию: `$env:VIRTUALENV_DOWNLOAD = "1"`
- Повторить шаг 5: `.\.venv\Scripts\pre-commit run --all-files`

Локальные хуки упрощены: тяжёлые проверки (black, isort, flake8, mypy) переведены в `stages: [manual]` — запускайте при необходимости:

- `pre-commit run black -a`
- `pre-commit run isort -a`
- `pre-commit run flake8 -a`
- `pre-commit run mypy -a`

А в CI эти проверки выполняются автоматически (как «гигиена» перед тестами).

Нормализация окончаний строк (EOL): после добавления `.gitattributes` возможна миграция EOL. При странных диффах выполните:

```bash
git add --renormalize .
git commit -m "chore: renormalize line endings"
```

Аффирмация: «Я действую спокойно и ровно. Случайности не случайны — они ведут к чистоте процесса».
