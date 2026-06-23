# Codex Operational Continuity Profile v0.1

**Status:** Draft public interoperability note  
**Project:** LIMINAL  
**Date:** 2026-06-23

## 1. Purpose

This profile defines a bounded operational checkpoint for preserving task continuity across Codex context compaction and thread handoff.

It separates three mechanisms:

```text
historical summary
≠ recent operational tail
≠ resumable checkpoint
```

A historical summary compresses older context. A recent operational tail preserves the latest concrete steps. A resumable checkpoint binds that tail to workspace state, verification state, and checkpoint lineage.

Machine-readable artifacts:

- [`schemas/codex-operational-checkpoint-v0.1.schema.json`](../../schemas/codex-operational-checkpoint-v0.1.schema.json)
- [`fixtures/codex-operational-continuity-conformance-v0.1.json`](../../fixtures/codex-operational-continuity-conformance-v0.1.json)

---

## 2. Bounded operational tail

The checkpoint preserves at most five structured operational steps.

Supported step types:

```text
user_instruction
assistant_intent
tool_call
tool_result
decision
verification
```

Each step carries:

- stable `step_id`;
- exact content reference;
- content digest;
- optional short display summary.

The display summary is not the source of truth. The referenced content and digest preserve the operational record.

### Invariant

> The operational tail is bounded, but not vague.

---

## 3. Workspace binding

A checkpoint binds continuity to:

- repository identity;
- current working directory;
- Git HEAD;
- optional dirty-worktree digest.

A changed Git HEAD or incompatible dirty state must not silently continue as though the workspace were unchanged.

Recommended outcomes:

```text
CONTINUE
REVALIDATE_WORKSPACE
RESTART_REQUIRED
```

### Invariant

> A valid memory of the task does not prove that the workspace is still the same.

---

## 4. Task state

The checkpoint records:

- current goal;
- acceptance criteria;
- current task phase;
- active `must` constraints;
- active `must_not` constraints;
- rejected approaches and reasons;
- touched resources;
- required, completed, and pending verification;
- exact next action.

Rejected approaches are first-class state. Losing them during compaction can cause the agent to repeat a known-bad path while appearing confident.

---

## 5. Verification boundary

Completion claims should be tied to evidence.

For example:

```text
verification required
verification completed
verification pending
```

A checkpoint must not transform a planned verification step into a completed one without a corresponding result reference.

Recommended outcome:

```text
REJECT_UNVERIFIED_COMPLETION
```

### Invariant

> A remembered claim of success is not evidence of success.

---

## 6. Lineage and idempotency

Every checkpoint may include:

- `trajectory_id` across related sessions;
- `parent_checkpoint_id` for immediate lineage;
- stable `checkpoint_id`.

Resuming the same checkpoint more than once must be idempotent. It must not repeat tool calls or duplicate side effects merely because the client retried the resume operation.

Recommended outcomes:

```text
CONTINUE
IDEMPOTENT_REPLAY
REJECT_LINEAGE_MISMATCH
```

---

## 7. Authorization boundary

The checkpoint carries:

```text
authority = context_only
```

It preserves context and continuity evidence only. It does not carry prior tool approvals, shell permissions, resource grants, or policy decisions into the next turn or thread.

### Invariant

> Context may continue. Authority must be evaluated again.

---

## 8. Minimum conformance cases

The fixture includes:

1. same-workspace continuation;
2. duplicate resume;
3. changed Git HEAD;
4. changed dirty-worktree state;
5. missing parent checkpoint;
6. recent tail collapsed into a vague summary;
7. rejected approach lost;
8. unverified completion claim;
9. checkpoint attempting to carry tool authorization.

Every case asserts:

```text
expected authorization changes = 0
```

---

## 9. Relationship to compaction

This profile complements, rather than replaces, a lossless recent-tail compaction strategy.

A robust implementation can use:

```text
compressed historical summary
+
lossless recent operational tail
+
workspace-bound checkpoint metadata
```

The first reduces context size. The second preserves immediate task state. The third determines whether continuation is still valid.

---

## 10. Canonical principle

> A summary remembers the story. An operational checkpoint preserves the exact place where work can safely continue.
