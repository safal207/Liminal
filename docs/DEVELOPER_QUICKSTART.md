# Developer Quickstart

This guide gives a new developer a practical first path through the root Liminal repository.

It focuses on three things:

1. cloning the repository
2. running local health/readiness checks
3. running the agent audit demo

For the full ecosystem map, see [`ECOSYSTEM.md`](../ECOSYSTEM.md).

---

## 1. Clone the repository

```bash
git clone https://github.com/safal207/Liminal.git
cd Liminal
```

---

## 2. Create a Python environment

Recommended Python version: **3.11**.

### Linux / macOS

```bash
python3.11 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
```

### Windows PowerShell

```powershell
py -3.11 -m venv .venv
.\.venv\Scripts\Activate.ps1
python -m pip install --upgrade pip
```

---

## 3. Install dependencies

```bash
pip install -r requirements.txt
```

For development checks:

```bash
pip install -r requirements-dev.txt
```

If `requirements-dev.txt` is not needed for your task, you can skip it.

---

## 4. Run the backend locally

```bash
python -m uvicorn backend.app.main:app --reload --port 8000
```

Then open another terminal for checks.

---

## 5. Check health and readiness

### Linux / macOS

```bash
curl http://127.0.0.1:8000/health
curl http://127.0.0.1:8000/ready
```

### Windows PowerShell

```powershell
Invoke-RestMethod http://127.0.0.1:8000/health
Invoke-RestMethod http://127.0.0.1:8000/ready
```

Expected result:

- `/health` should return service status.
- `/ready` should return readiness checks.

Redis is not required by default when `USE_REDIS=false`.

---

## 6. Run helper health scripts

### Windows

```powershell
./scripts/check-health.ps1
```

### Linux / macOS

```bash
bash ./scripts/check-health.sh
```

---

## 7. Run tests

Start with the normal Python test suite:

```bash
pytest
```

If you only want a quick smoke check, run the health/readiness checks first.

---

## 8. Run the Agent Audit Demo

The agent audit demo is the easiest way to understand the AI-agent side of Liminal.

From the repository root:

```bash
python examples/agent_audit_demo/run_demo.py
```

Expected final verdict:

```text
HOLD: the refund action must not execute until approval and permission records are committed.
```

This demo shows:

- trace continuity gap
- missing causal permission
- CaPU-style HOLD decision
- human-readable audit report

Demo files:

```text
examples/agent_audit_demo/
  README.md
  trace.jsonl
  cml_records.json
  capu_decision.json
  audit_report.md
  run_demo.py
```

---

## 9. Where to go next

### If you want to understand the ecosystem

Read:

- [`../ECOSYSTEM.md`](../ECOSYSTEM.md)
- [`../STATUS.md`](../STATUS.md)
- [`../COMMUNITY_ROADMAP.md`](../COMMUNITY_ROADMAP.md)

### If you want to work on the demo

Start with:

- [`examples/agent_audit_demo/README.md`](../examples/agent_audit_demo/README.md)
- [`docs/demo/AGENT_AUDIT_DEMO_STORY.md`](demo/AGENT_AUDIT_DEMO_STORY.md)
- issue `#76`: connect the demo runner to real LTP, CML, and CaPU libraries

### If you want to improve docs

Start with:

- issue `#69`: glossary of core Liminal terms
- issue `#72`: contributing guide

### If you want to understand the commercial path

Read:

- [`docs/commercial/AGENT_EFFICIENCY_AUDIT.md`](commercial/AGENT_EFFICIENCY_AUDIT.md)

---

## 10. Current project status

The root project is early-access.

Some infrastructure pieces are working prototypes, while the full ecosystem is still being integrated.

Read [`STATUS.md`](../STATUS.md) before assuming production readiness.

---

## Quick command summary

```bash
git clone https://github.com/safal207/Liminal.git
cd Liminal
python3.11 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
pip install -r requirements.txt
python -m uvicorn backend.app.main:app --reload --port 8000
curl http://127.0.0.1:8000/health
curl http://127.0.0.1:8000/ready
python examples/agent_audit_demo/run_demo.py
```

Windows PowerShell variant:

```powershell
git clone https://github.com/safal207/Liminal.git
cd Liminal
py -3.11 -m venv .venv
.\.venv\Scripts\Activate.ps1
python -m pip install --upgrade pip
pip install -r requirements.txt
python -m uvicorn backend.app.main:app --reload --port 8000
Invoke-RestMethod http://127.0.0.1:8000/health
Invoke-RestMethod http://127.0.0.1:8000/ready
python examples/agent_audit_demo/run_demo.py
```
