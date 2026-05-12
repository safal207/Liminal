# Agent Audit Demo Walkthrough

This walkthrough explains the Agent Audit Demo as a complete story for external readers.

The goal is simple: show how an AI-agent workflow can look acceptable at the output level while still being unsafe to execute because trace continuity, causal permission, and approval evidence are incomplete.

---

## 1. Run the demo

From the repository root:

```bash
python examples/agent_audit_demo/run_demo.py
```

Equivalent explicit static mode:

```bash
python examples/agent_audit_demo/run_demo.py --mode static
```

Optional integrated mode:

```bash
python examples/agent_audit_demo/run_demo.py --mode integrated
```

Integrated mode does not require external tools by default. If real LTP, CML, or CaPU command templates are not configured, the runner reports skipped external checks and continues with deterministic local artifact checks.

---

## 2. What the scenario represents

The demo models a support-style AI-agent workflow.

The agent moves through several steps:

1. receive a user request
2. inspect policy context
3. draft a response
4. prepare a recommendation
5. attempt a final operation

The final operation is the critical point. It is not enough for the agent output to look reasonable. The system must also prove that the action is traceable, causally valid, and permitted.

---

## 3. What LTP-style trace inspection finds

The trace is stored in:

```text
examples/agent_audit_demo/trace.jsonl
```

The critical transition is `t5`.

It has no declared anchors:

```json
{"transition_id":"t5","actor":"support_agent","action":"execute_refund","amount":"50 EUR","anchors":[]}
```

This means the final operation is not anchored to a committed approval or permission record.

The important lesson:

```text
A trace can contain an event without proving that the event was allowed.
```

Logs tell us that something happened. Trace continuity asks whether the action is grounded in the current thread state.

---

## 4. What CML finds

CML records are stored in:

```text
examples/agent_audit_demo/cml_records.json
```

The critical CML record marks the final transition as causally invalid.

It reports violations such as:

```text
MISSING_PARENT
SCOPE_DENIED
PERMISSION_CHAIN_MISSING
```

The previous policy context can support recommendation and drafting, but it does not grant execution permission.

The important lesson:

```text
A system can be functionally plausible while causally invalid.
```

This is the core value of CML: it records reasons, permissions, and responsibility, not only events.

---

## 5. Why CaPU returns HOLD

The CaPU decision is stored in:

```text
examples/agent_audit_demo/capu_decision.json
```

The decision is:

```text
HOLD
```

Reason code:

```text
PRECONDITIONS_UNMET
```

This means the final operation should not execute yet. The system needs committed evidence first, such as approval and permission records.

The important lesson:

```text
HOLD is not failure. HOLD is safe pause before irreversible action.
```

CaPU makes the safe action lifecycle explicit:

```text
Gate -> Incubate -> Commit -> Execute
```

Execution is allowed only after the required state has been committed.

---

## 6. What the final verdict means

The runner prints:

```text
HOLD: the refund action must not execute until approval and permission records are committed.
```

This verdict means:

- the agent output may look acceptable
- the trace exists but lacks the required anchor
- the causal chain is incomplete
- the permission scope is insufficient
- the final operation must wait for committed approval evidence

For a human operator, this gives a clear decision:

```text
Do not execute yet. Ask for approval, commit the permission record, then retry.
```

---

## 7. Why this matters

### For AI safety

The demo shows a concrete way to detect unsafe action attempts before execution.

It is not based on a vague prompt instruction. It is based on trace evidence, causal records, and an explicit runtime decision.

### For QA and platform teams

The demo gives a reproducible test fixture for agent behavior.

Instead of checking only whether the output text looks good, the system checks whether the action is replayable, anchored, and causally valid.

### For compliance and auditability

The demo produces a human-readable audit report.

A reviewer can inspect why the action was held and what evidence is missing.

### For founders and operators

The demo turns an abstract agent-risk problem into a concrete operational story:

```text
The agent looked ready to act, but the system found missing permission evidence and safely held the operation.
```

---

## 8. Static mode versus integrated mode

### Static mode

Static mode is dependency-free.

It reads local artifacts and prints the same deterministic verdict every time. This makes the demo easy to run in a fresh clone.

### Integrated mode

Integrated mode is a bridge toward real tooling.

It can call external commands through environment variables:

```bash
export LTP_INSPECT_CMD="ltp inspect trace --input {trace}"
export CML_VALIDATE_CMD="cml validate --input {cml}"
export CAPU_EVALUATE_CMD="capu evaluate --input {capu}"
python examples/agent_audit_demo/run_demo.py --mode integrated
```

If those tools are not installed, the runner reports the status and still falls back to local artifact checks.

This gives the repository a safe migration path:

```text
static artifact -> optional integration -> real LTP/CML/CaPU toolchain
```

---

## 9. Before and after Liminal

Before Liminal:

```text
Agent output looks good.
Logs exist.
The final action is attempted.
Authorization is unclear.
Audit evidence is incomplete.
```

After Liminal:

```text
Agent output is checked.
Trace continuity is inspected.
Causal permission is validated.
Unsafe execution is held.
The audit report explains why.
A human can approve, reject, or request more evidence.
```

---

## 10. What to build next

The next useful demo extension is a positive path:

```text
approval committed -> permission record present -> causal chain valid -> ACCEPT / EXECUTE allowed
```

That would show that Liminal is not only a blocking layer. It is a decision layer that explains when an action should be held and when it can safely proceed.

---

## Summary

The Agent Audit Demo proves one core idea:

```text
An AI-agent action should not be judged only by its output.
It should also be judged by trace continuity, causal validity, and committed permission evidence.
```

That is the practical bridge between LTP, CML, and CaPU.

It turns agent observability into agent accountability.
