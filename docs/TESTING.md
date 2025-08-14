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
