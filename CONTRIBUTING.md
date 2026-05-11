# Contributing to Liminal

Thank you for your interest in contributing to Liminal.

Liminal is an early-access ecosystem for continuity, causal memory, trace replay, safe execution, and human/AI transition support.

This guide explains how to contribute without needing private founder context.

---

## Start here

Before opening a pull request, read:

- [`README.md`](README.md)
- [`docs/DEVELOPER_QUICKSTART.md`](docs/DEVELOPER_QUICKSTART.md)
- [`docs/GLOSSARY.md`](docs/GLOSSARY.md)
- [`ECOSYSTEM.md`](ECOSYSTEM.md)
- [`STATUS.md`](STATUS.md)
- [`COMMUNITY_ROADMAP.md`](COMMUNITY_ROADMAP.md)

For the runnable demo, start with:

```bash
python examples/agent_audit_demo/run_demo.py
```

---

## Project structure

Important entry points:

```text
README.md                         Main project entry
ECOSYSTEM.md                      Ecosystem map
STATUS.md                         Current maturity and readiness
COMMUNITY_ROADMAP.md              Community-facing roadmap
CONTRIBUTING.md                   Contributor guide

docs/
  DEVELOPER_QUICKSTART.md         Local setup and first run
  GLOSSARY.md                     Shared vocabulary
  commercial/
    AGENT_EFFICIENCY_AUDIT.md     Commercial audit offer
  demo/
    AGENT_AUDIT_DEMO_STORY.md     Demo narrative

examples/
  agent_audit_demo/               Minimal runnable audit demo
```

The repository currently contains both human-facing Liminal concepts and AI-agent infrastructure concepts. Keep that dual nature explicit.

---

## How to choose an issue

Good first places to contribute:

- documentation improvements
- demo clarity
- glossary improvements
- example files
- quickstart corrections
- small test additions
- issue cleanup

Prefer issues labeled:

- `good first issue`
- `documentation`
- `demo`
- `community`
- `integration`

For larger architectural changes, open an issue first and describe the proposal before implementing it.

---

## Contribution types

### Documentation

Useful documentation contributions include:

- making setup steps clearer
- fixing broken links
- explaining terms from the glossary
- improving demo instructions
- adding before/after examples
- separating stable facts from experimental ideas

Documentation should be clear enough for someone who has never spoken with the founder.

### Demo work

Useful demo contributions include:

- improving `examples/agent_audit_demo/run_demo.py`
- adding expected output examples
- making artifacts easier to inspect
- adding static validation checks
- connecting the demo to real LTP, CML, or CaPU tooling

The static demo must keep working without external dependencies.

### Code

Useful code contributions include:

- small bug fixes
- tests
- health/readiness improvements
- CLI improvements
- validation utilities
- safe integration points

Avoid large rewrites unless there is a clear issue or discussion.

---

## Documentation style

Use direct, concrete language.

Prefer:

```text
The refund action has no approval anchor, so execution is held.
```

Avoid vague language like:

```text
The system enables better transformation of intelligence through continuity.
```

When explaining a concept, include:

1. a short definition
2. a concrete example
3. why it matters
4. where to inspect it in the repo

When a component is experimental, say so clearly.

---

## Code style

For Python code:

- use Python 3.11-compatible syntax
- keep scripts runnable from the repository root
- prefer small functions
- add clear error messages
- avoid hidden network calls in examples
- keep demo scripts deterministic

Formatting expectations:

```bash
black --check <files>
isort --check-only <files>
flake8 <files>
```

If you add Python files, run Black before opening a PR.

---

## Testing expectations

Before opening a PR, run the smallest relevant check.

For docs-only changes:

- check links manually
- ensure paths are correct
- make sure examples are copy-pasteable

For Python changes:

```bash
pytest
python examples/agent_audit_demo/run_demo.py
```

For backend changes:

```bash
python -m uvicorn backend.app.main:app --reload --port 8000
curl http://127.0.0.1:8000/health
curl http://127.0.0.1:8000/ready
```

Windows PowerShell:

```powershell
Invoke-RestMethod http://127.0.0.1:8000/health
Invoke-RestMethod http://127.0.0.1:8000/ready
```

---

## Safety boundaries

Liminal deals with auditability, memory, decisions, and potentially sensitive workflows.

Do not add code that:

- secretly sends data to external services
- collects personal data without explicit documentation
- weakens permission checks
- makes unsafe external execution easier
- hides failed checks
- converts HOLD/BLOCK decisions into silent success
- treats experimental human-facing concepts as medical or psychological diagnosis

Human-facing concepts in this repository are experimental and should not be framed as therapy, diagnosis, or clinical advice.

AI-agent infrastructure concepts should preserve clear boundaries between recommendation, approval, commit, and execution.

---

## What not to change without discussion

Open an issue before changing:

- project positioning
- license terms
- safety boundaries
- core terminology
- public commercial offer framing
- repository structure
- CI/CD deployment behavior
- health/readiness semantics
- demo scenario meaning
- CaPU lifecycle semantics
- CML causal validity semantics
- LTP trace/anchor semantics

Small wording improvements are welcome. Semantic changes need discussion.

---

## Related project boundaries

The Liminal ecosystem may reference related directions such as:

- LTP
- CML
- CaPU
- LiminalDB
- DAO_lim
- LiminalQAengineer
- DIF / DeepIntent Funnel

Keep boundaries clear.

DIF is a separate project and should not be renamed into Liminal unless explicitly discussed.

When adding cross-project references, explain whether the relation is:

- conceptual
- experimental
- planned integration
- implemented integration
- commercial positioning

Do not imply production readiness where it does not exist.

---

## Pull request checklist

Before opening a PR, check:

- [ ] The change has a clear purpose.
- [ ] Documentation links are valid.
- [ ] New terminology is either explained or linked to the glossary.
- [ ] Experimental claims are marked as experimental.
- [ ] Code examples are copy-pasteable.
- [ ] Python files are formatted with Black.
- [ ] The agent audit demo still runs if touched.
- [ ] The PR description explains why the change matters.

---

## Pull request description template

Use this structure when helpful:

```md
## Summary

What changed?

## Why

Why does this matter?

## Validation

What did you run or check?

## Notes

Anything reviewers should know?
```

---

## Issue writing template

Good issues include:

```md
## Goal

What should be improved?

## Current state

What exists now?

## Target state

What should exist after this issue is done?

## Definition of Done

How do we know this is complete?
```

---

## Contributor mindset

The best contributions make Liminal more:

- understandable
- reproducible
- auditable
- safe
- honest about maturity
- useful to developers, researchers, and operators

When in doubt, improve the path from idea to runnable proof.

---

# Legacy local playbooks

The sections below preserve older local development notes that may still be useful for Windows, WSL, database adapter, JWT, and multilingual testing workflows.

## Быстрый старт / legacy dev notes

- Python 3.10+
- Рекомендуем venv:
  - Windows PowerShell: `python -m venv .venv && . .venv/Scripts/Activate.ps1`
  - Git Bash: `python -m venv .venv && source .venv/Scripts/activate`
- Установить зависимости: `pip install -r requirements.txt`

## Pre-commit хуки

Хуки помогают держать код чистым и предсказуемым.

- Установка: `pip install pre-commit`
- Активировать в репо: `pre-commit install`
- Запуск всех проверок локально: `pre-commit run --all-files`
- Временный обход: `git commit -m "..." --no-verify`

Если коммит падает — смотри лог: `C:\Users\<you>\.cache\pre-commit\pre-commit.log`

## Ветвление и PR

- Ветки по задаче: `feat/<scope>-<short>`, `fix/<scope>-<short>`, `docs/<short>`
- Открываем PR в `main`.
- Один PR — одно изолированное изменение.
- Перед PR: `pre-commit run --all-files`, локальные тесты.

## Коммиты

Формат:

```text
<type>: <summary>
```

где `type` ∈ `{feat, fix, docs, chore, refactor, test}`.

Примеры:

- `docs: restore legacy README as main; add health & TOC`
- `feat(api): add readiness endpoint`
- `chore(ci): bump pre-commit hooks`

## Health/Readiness legacy notes

- Запуск: `python -m uvicorn backend.api:app --reload --port 8000`
- Проверки:
  - PowerShell: `Invoke-RestMethod http://127.0.0.1:8000/health` и `/ready`
  - curl: `curl http://127.0.0.1:8000/health` и `/ready`
- Скрипты: `./scripts/check-health.ps1` и `bash ./scripts/check-health.sh`

## DatabaseAdapter: работа с данными

Универсальный адаптер для работы с Datomic и Neo4j. Каждый тип данных автоматически маршрутизируется в оптимальную БД.

### Инициализация для разработки

```python
from backend.database_adapter import DatabaseAdapter, DataType, get_database_adapter

adapter = get_database_adapter()

adapter = DatabaseAdapter(
    datomic_uri="http://localhost:8080",
    datomic_db_name="liminal",
    neo4j_uri="bolt://localhost:7687",
    neo4j_user="neo4j",
    neo4j_password="password",
    fallback_enabled=True,
)
```

### Работа с типами данных

```python
DataType.TEMPORAL
DataType.EVENT
DataType.AUDIT
DataType.EMOTION_HISTORY
DataType.SESSION_DATA

DataType.RELATIONSHIP
DataType.GRAPH
DataType.PHILOSOPHY
DataType.CONCEPT_MAP
DataType.USER_NETWORK
```

### Сохранение и запрос данных

```python
import asyncio

async def example():
    emotion_id = await adapter.store_data(
        data={"emotion": "радость", "intensity": 0.8},
        data_type=DataType.EMOTION_HISTORY,
        user_id="user-123",
    )

    emotions = await adapter.query_data(
        data_type=DataType.EMOTION_HISTORY,
        filters={"user_id": "user-123"},
        limit=10,
    )

asyncio.run(example())
```

### Best practices

1. Всегда закрывайте подключения.
2. Проверяйте доступность БД.
3. Используйте правильные типы данных.
4. Тестируйте с моками.

## WSL-плейбук: запуск и тесты

Рекомендуемый способ разработки и запуска — через WSL.

```bash
source ~/.venvs/liminal/bin/activate
cd /mnt/c/Users/safal/OneDrive/Documente/GitHub/resonance-liminal
```

Отключить proxy для локальных адресов:

```bash
export NO_PROXY=127.0.0.1,localhost
export no_proxy=127.0.0.1,localhost
```

Устойчивая установка зависимостей:

```bash
python -m pip install --upgrade pip
pip install -r requirements.txt \
  -i https://pypi.org/simple --default-timeout 25 --no-cache-dir -vvv
```

Запуск приложения:

```bash
uvicorn backend.main:app --host 127.0.0.1 --port 8000 --reload
```

Проверка health/docs/ready:

```bash
curl --noproxy '*' http://127.0.0.1:8000/health
curl --noproxy '*' http://127.0.0.1:8000/docs
curl --noproxy '*' http://127.0.0.1:8000/ready
```

## JWT quick test

```bash
TOKEN=$(curl -s --noproxy '*' -X POST \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "username=testuser&password=testpass" \
  http://127.0.0.1:8000/auth/token | jq -r .access_token)

curl --noproxy '*' -H "Authorization: Bearer $TOKEN" http://127.0.0.1:8000/auth/me
```

## Tests legacy notes

```bash
pytest -q backend/tests/test_jwt_auth.py backend/tests/test_jwt_edge_cases.py
python -m pytest -vv --color=yes -rA -m "not integration"
python -m pytest -vv --color=yes -rA backend/tests/test_multilingual_expected_lang.py
```

Makefile:

```bash
make test-ml
make test
```

## Мультиязычный анализ: expected_lang

Когда текст содержит только ASCII-символы, детекция языка может давать ничью между несколькими латинскими языками. Для корректного разрешения используется параметр `expected_lang`.

```python
from backend.personality.multilingual_support import analyze_multilingual_text

res = await analyze_multilingual_text(
    text,
    adapter.analyze_text,
    target_lang="de",
    expected_lang="de",
)
```

```python
from backend.personality.ml_adapter import EmotionMLAdapter

adapter = EmotionMLAdapter()
res = await adapter.analyze_multilingual(text, target_lang="de", expected_lang="de")
```

Тесты:

```bash
pytest -q backend/tests/test_multilingual_expected_lang.py
```

## Работа с Git: WSL/PowerShell

Push через WSL:

```bash
unset http_proxy https_proxy
git push https://github.com/safal207/Liminal.git main
```

Push через PowerShell:

```powershell
cd "C:\Users\safal\OneDrive\Documente\GitHub\resonance-liminal"
git push origin main
```

Если возникают ошибки:

- Проверить URL репозитория.
- Убедиться, что PAT имеет права repo.
- Временный workaround: использовать PowerShell вместо WSL.

## Контакты

`safal0645@gmail.com`
