# Claude Code Lifecycle Hook Interoperability Profile v0.1

**Status:** Draft public interoperability note  
**Project:** LIMINAL  
**Date:** 2026-06-23

## 1. Purpose

This profile defines a minimal, framework-neutral contract for external memory and continuity layers around Claude Code lifecycle boundaries:

```text
SessionStart → PreCompact → PostCompact → SessionEnd
```

It does not prescribe one memory implementation. It standardizes the event envelope and the safety properties needed for different memory providers to compose with Claude Code without intercepting transcripts ad hoc.

Machine-readable artifacts:

- [`schemas/claude-code-lifecycle-hook-event-v0.1.schema.json`](../../schemas/claude-code-lifecycle-hook-event-v0.1.schema.json)
- [`fixtures/claude-code-lifecycle-hooks-conformance-v0.1.json`](../../fixtures/claude-code-lifecycle-hooks-conformance-v0.1.json)

---

## 2. Core separation

```text
recalled context ≠ verified fact ≠ tool authorization
```

A lifecycle hook may save, retrieve, summarize, or inject context. It must not silently authorize a future tool call.

An external memory result should therefore expose:

```json
{
  "provider_id": "memory-provider-a",
  "source_event_id": "evt_postcompact_011",
  "context_ref": "memory://provider-a/recall/pack_77",
  "context_digest": "sha256:...",
  "provenance": "retrieved-from-external-memory",
  "authority": "context_only"
}
```

The only valid authority value in this profile is:

```text
context_only
```

Any tool or resource authorization remains subject to the current Claude Code permission and policy path.

---

## 3. Event envelope

Every event includes:

- `profile_version`;
- stable `event_id`;
- `event_type`;
- `session_id`;
- optional `trajectory_id` spanning related sessions;
- optional `correlation_id`;
- optional immediate `causation_id`;
- monotonic session-local `sequence`;
- `delivery_attempt`;
- timestamp;
- project/workspace identity;
- event-specific payload.

### Invariant

> A trajectory may preserve continuity across sessions, but it does not preserve authorization across sessions.

---

## 4. Lifecycle events

### 4.1 `SessionStart`

Emitted before the first user message and before external recalled context is injected.

Minimum payload:

```text
start_reason = new | resume | fork | restart
resume_checkpoint_ref?
parent_session_id?
```

A resume request should compare the checkpoint workspace identity with the current workspace.

Possible outcomes:

```text
CONTINUE
FORK
RESTART_REQUIRED
```

### 4.2 `PreCompact`

Emitted before the source context is flattened or discarded.

Minimum payload:

```text
reason
transcript_ref
context_usage
last_message_ref?
```

The transcript must remain readable for the duration of the hook.

The hook should be idempotent by `event_id`. Redelivery must not create duplicate checkpoints.

### 4.3 `PostCompact`

Emitted after built-in compaction completes.

Minimum payload:

```text
precompact_event_id
summary_ref
available_tokens
compaction_digest?
```

`PostCompact` must causally reference one `PreCompact` event from the same session.

External memory may return a token-budgeted context pack, but the pack must carry explicit provenance and `authority: context_only`.

### 4.4 `SessionEnd`

Emitted while the final transcript is still readable.

Minimum payload:

```text
exit_reason
transcript_ref
final_checkpoint_ref?
```

Graceful exits can guarantee the hook opportunity. Process crashes, forced termination, or power loss cannot guarantee delivery; implementations should state this limitation explicitly.

---

## 5. Ordering and causality

Within one session:

1. `sequence` is monotonic;
2. duplicate `event_id` delivery is idempotent;
3. `PostCompact.causation_id` and `payload.precompact_event_id` identify the matching `PreCompact` event;
4. cross-session compact references are rejected;
5. a stale checkpoint cannot overwrite a newer workspace state.

Recommended terminal classifications:

```text
ACCEPT
IDEMPOTENT_REPLAY
REJECT_OUT_OF_ORDER
REJECT_CAUSAL_MISMATCH
REJECT_SESSION_MISMATCH
RESTART_REQUIRED
REJECT_INVALID_AUTHORITY
```

---

## 6. Failure semantics

External hooks must not make built-in compaction permanently unavailable.

A practical default is:

```text
PreCompact timeout
→ continue built-in compaction
→ emit a visible diagnostic
→ record that external checkpoint capture was not confirmed
```

This is not equivalent to successful preservation. The runtime should not claim an external checkpoint exists when the hook timed out.

For `SessionEnd`, failure should be recorded as best-effort save failure. The previous confirmed checkpoint remains authoritative for continuity.

---

## 7. Resume safety

A recalled memory pack can inform the next session, but resume should independently evaluate:

- current workspace/repository identity;
- checkpoint lineage;
- files changed since checkpoint creation;
- current permission configuration;
- current model/provider if relevant to policy;
- duplicate resume attempts.

### Invariant

> Context may be resumed. Authority must be re-evaluated.

---

## 8. Conformance cases

The fixture includes:

- ordered lifecycle;
- duplicate `PreCompact` delivery;
- `PostCompact` without matching `PreCompact`;
- cross-session causal mismatch;
- sequence regression;
- resume with changed workspace state;
- valid external context injection;
- invalid attempt to convert recalled context into tool authorization;
- `PreCompact` hook timeout.

Every case asserts:

```text
expected authorization changes = 0
```

---

## 9. Relationship to external memory providers

This profile supports flat files, vector stores, knowledge graphs, belief graphs, event stores, or other memory representations.

Claude Code exposes lifecycle seams. Providers remain responsible for:

- selection and consolidation policy;
- provenance quality;
- conflict handling;
- storage and privacy;
- retrieval ranking;
- retention policy.

The runtime remains responsible for:

- event ordering;
- event identity;
- readable transcript timing;
- timeout behavior;
- current authorization enforcement.

---

## 10. Canonical principle

> The memory layer carries context across the boundary. The lifecycle event carries evidence that the boundary was crossed.
