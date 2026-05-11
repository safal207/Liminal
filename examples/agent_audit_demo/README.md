# Agent Audit Demo

This example turns the [Agent Audit Demo Story](../../docs/demo/AGENT_AUDIT_DEMO_STORY.md) into a minimal reproducible artifact.

The demo shows an AI support agent that produces a helpful-looking output but attempts an external refund action without a valid approval chain.

---

## Files

| File | Purpose |
|---|---|
| `trace.jsonl` | Minimal agent trace with five transitions |
| `cml_records.json` | Causal memory records for each transition |
| `capu_decision.json` | CaPU HOLD decision for unsafe execution |
| `audit_report.md` | Human-readable audit report |
| `run_demo.py` | Small CLI runner that prints the final audit verdict |

---

## Run the demo

From the repository root:

```bash
python examples/agent_audit_demo/run_demo.py
```

Expected final verdict:

```text
HOLD: the refund action must not execute until approval and permission records are committed.
```

---

## What to inspect

### 1. Trace continuity

Open `trace.jsonl` and inspect transition `t5`:

```json
{"transition_id":"t5","actor":"support_agent","action":"execute_refund","amount":"50 EUR","anchors":[]}
```

The external refund action has no anchors.

### 2. Causal validity

Open `cml_records.json` and inspect `cml_005`:

```json
{
  "transition_id": "t5",
  "action": "execute_refund",
  "causal_status": "invalid",
  "violations": [
    "MISSING_PARENT",
    "SCOPE_DENIED",
    "PERMISSION_CHAIN_MISSING"
  ]
}
```

The previous policy check only supports recommendation and drafting, not execution.

### 3. Safe execution

Open `capu_decision.json`:

```json
{
  "decision": "HOLD",
  "reason_code": "PRECONDITIONS_UNMET"
}
```

The refund should be held until human approval and a committed permission record exist.

### 4. Audit output

Open `audit_report.md` to see the human-readable explanation.

---

## Demo narrative

Before Liminal:

```text
Agent output looks good.
Refund executed.
Logs exist.
No one knows whether the action was actually authorized.
```

After Liminal:

```text
Agent output is checked.
Trace gap is detected.
Missing permission is detected.
Execution is held.
Audit report explains why.
Human can approve or reject safely.
```

---

## Next step

The runner is intentionally simple. A future version can connect this example to real LTP, CML, and CaPU libraries.
