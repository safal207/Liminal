# Agent Audit Demo

This example turns the [Agent Audit Demo Story](../../docs/demo/AGENT_AUDIT_DEMO_STORY.md) into a minimal reproducible artifact.

For a step-by-step explanation of the scenario, trace gap, CML finding, CaPU HOLD decision, and business/safety meaning, read the [Agent Audit Demo Walkthrough](../../docs/demo/AGENT_AUDIT_DEMO_WALKTHROUGH.md).

The demo shows an AI support workflow where the final action must be held until trace anchors, causal permission, and approval records are present.

---

## Files

| File | Purpose |
|---|---|
| `trace.jsonl` | Minimal agent trace with five transitions |
| `cml_records.json` | Causal memory records for each transition |
| `capu_decision.json` | CaPU HOLD decision for unsafe execution |
| `audit_report.md` | Human-readable audit report |
| `run_demo.py` | CLI runner with static and optional integrated modes |

---

## Run the demo

From the repository root:

```bash
python examples/agent_audit_demo/run_demo.py
```

Equivalent explicit static mode:

```bash
python examples/agent_audit_demo/run_demo.py --mode static
```

Expected final verdict:

```text
HOLD: the refund action must not execute until approval and permission records are committed.
```

---

## Integrated mode

Integrated mode is optional. It is designed for environments where real LTP, CML, or CaPU tools are available.

```bash
python examples/agent_audit_demo/run_demo.py --mode integrated
```

By default, integrated mode still works without external dependencies. If no tools are configured, it prints that the external checks were skipped and continues with deterministic local artifact checks.

Optional command templates can be provided through environment variables:

```bash
export LTP_INSPECT_CMD="ltp inspect trace --input {trace}"
export CML_VALIDATE_CMD="cml validate --input {cml}"
export CAPU_EVALUATE_CMD="capu evaluate --input {capu}"
python examples/agent_audit_demo/run_demo.py --mode integrated
```

Supported placeholders:

| Placeholder | Expands to |
|---|---|
| `{trace}` | `examples/agent_audit_demo/trace.jsonl` |
| `{cml}` | `examples/agent_audit_demo/cml_records.json` |
| `{capu}` | `examples/agent_audit_demo/capu_decision.json` |
| `{report}` | `examples/agent_audit_demo/audit_report.md` |

If a configured command is unavailable, fails, or times out, the runner reports that status and still prints the local artifact verdict.

---

## What to inspect

### 1. Trace continuity

Open `trace.jsonl` and inspect transition `t5`:

```json
{"transition_id":"t5","actor":"support_agent","action":"execute_refund","amount":"50 EUR","anchors":[]}
```

The final action has no anchors.

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

The final action should be held until approval and a committed permission record exist.

### 4. Audit output

Open `audit_report.md` to see the human-readable explanation.

---

## Demo narrative

Before Liminal:

```text
Agent output looks good.
Final action is attempted.
Logs exist.
Authorization is unclear.
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

The static mode is intentionally dependency-free. Integrated mode provides a safe path for connecting real LTP, CML, and CaPU libraries or CLI tools when they are available.
