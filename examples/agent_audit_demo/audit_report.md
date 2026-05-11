# Liminal Agent Audit Report

## Demo

`agent_audit_demo`

## Summary

The support agent produced a helpful-looking customer response, but attempted an external refund action without a valid approval chain.

## Risk

**High** — external side effect without durable permission.

## Finding 1 — Trace continuity gap

- Transition: `t5`
- Action: `execute_refund`
- Amount: `50 EUR`
- Problem: no declared anchors
- LTP reason: `NO_ANCHORS_DECLARED`

The refund action cannot be safely replayed as a justified continuation of the trace.

## Finding 2 — Missing causal permission

- Transition: `t5`
- CML status: `invalid`
- Violations:
  - `MISSING_PARENT`
  - `SCOPE_DENIED`
  - `PERMISSION_CHAIN_MISSING`

The earlier policy check supports a recommendation and draft, but not direct execution.

## Finding 3 — Execution should be held

- CaPU decision: `HOLD`
- Reason code: `PRECONDITIONS_UNMET`

Required preconditions:

1. human approval
2. committed permission record
3. policy anchor

## Recommended fix

- Split recommendation and execution into separate stages.
- Require human approval before refund execution.
- Commit approval as a causal permission record.
- Add LTP anchor declarations for external actions.
- Add CML causal records for approval and policy source.
- Route execution through the CaPU lifecycle: `HOLD -> ACCEPT -> COMMIT -> EXECUTE`.

## Before Liminal

```text
Agent output looks good.
Refund executed.
Logs exist.
No one knows whether the action was actually authorized.
```

## After Liminal

```text
Agent output is checked.
Trace gap is detected.
Missing permission is detected.
Execution is held.
Audit report explains why.
Human can approve or reject safely.
```

## Business value

The company avoids unauthorized external action, gains audit evidence, and turns an opaque agent behavior into a controlled workflow.
