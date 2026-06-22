# Liminal Agent Continuity Model v0.1

**Status:** Public architecture note  
**Project:** LIMINAL  
**Author:** Alexey Safalov / safal207  
**Date:** 2026-06-22

## Summary

The Liminal Agent Continuity Model describes a shared lifecycle for AI-agent memory, evidence, decisions, actions, and recovery.

The core idea is simple:

> An agent should not only remember and act. It should preserve where information came from, why a decision was made, what happened next, and how the process can be resumed or audited.

The model connects six public primitives:

- `MemoryArtifact`
- `EvidenceReference`
- `DecisionReceipt`
- `ActionReceipt`
- `ContinuationToken`
- `LifecycleEvent`

Together, they support this lifecycle:

```text
Observe → Remember → Ground → Decide → Act → Prove → Learn
```

This document is a public conceptual model. It intentionally does not disclose LIMINAL's full implementation, causal scoring, trust algorithms, storage topology, policy engine, replay engine, or commercial integration layer.

---

## 1. Problem

Modern AI agents often lose continuity at the boundaries between:

- one context window and the next;
- one session and the next;
- one agent and another;
- a proposed action and its real-world effect;
- a deferred approval and resumed execution;
- an event log and a human-understandable explanation.

Typical logs can show that something happened, but not reliably answer:

- Why does the agent remember this?
- Which source produced the memory?
- Did a newer correction replace an older belief?
- Which evidence supported the decision?
- Was the action actually executed?
- Was a retry a new action or a replay of the same logical action?
- Can the workflow resume without repeating side effects?

The Liminal Agent Continuity Model treats these questions as one connected systems problem.

---

## 2. Design principles

### 2.1 Provenance before persistence

A memory without origin is difficult to trust. Every durable artifact should remain traceable to the session, turn, tool result, human correction, or external event that produced it.

### 2.2 Explicit lifecycle states

Artifacts should not silently disappear between "created" and "used." Their state should be visible and machine-readable.

Examples:

```text
candidate → actionable → applied
candidate → quarantined → rejected
active → superseded
pending → resolved
```

### 2.3 Deterministic process boundaries

The model does not require deterministic LLM output. It requires deterministic boundaries around non-deterministic work.

A process should expose whether it:

```text
completed | skipped | failed | deferred
```

### 2.4 Idempotent retries

Repeated processing of the same logical memory or action should not create duplicate side effects or conflicting records.

### 2.5 Evidence-backed decisions

High-impact actions should be connected to the evidence and policy context that produced their authorization result.

### 2.6 Replayable evolution

The system should preserve enough structure to reconstruct how memory, decisions, and agent state evolved without requiring the full original transcript.

### 2.7 Human ownership

People must be able to inspect, correct, supersede, quarantine, delete, or restrict durable agent memory and high-impact decisions.

---

## 3. Core entities

### 3.1 MemoryArtifact

Represents a durable unit of learned or operational context.

Conceptual fields:

```json
{
  "memory_id": "mem_01...",
  "scope": "user | project | task | team",
  "kind": "preference | correction | decision | state | hypothesis",
  "status": "candidate | active | quarantined | rejected | superseded",
  "source_refs": ["src_01..."],
  "supersedes": ["mem_00..."],
  "created_at": "...",
  "expires_at": null
}
```

The public model defines the lifecycle, not the private retrieval or trust-scoring implementation.

### 3.2 EvidenceReference

Points to the material that supports a memory or decision.

Possible source classes:

```text
user statement
human correction
session turn
tool result
repository state
external document
system observation
agent-generated inference
```

An inference should be distinguishable from direct evidence.

### 3.3 DecisionReceipt

Records the outcome of a policy or governance evaluation before a sensitive action.

The minimum decision set is:

```text
ALLOW | BLOCK | DEFER
```

Conceptual fields:

```json
{
  "decision_id": "dec_01...",
  "action_id": "act_01...",
  "outcome": "DEFER",
  "reason_code": "human_approval_required",
  "evidence_refs": ["ev_01..."],
  "continuation_token": "cont_01...",
  "expires_at": "..."
}
```

### 3.4 ActionReceipt

Records the result of a real or attempted side effect.

Examples include:

- sending an email;
- modifying a repository;
- executing a deployment;
- creating a payment;
- changing access permissions;
- writing durable memory.

A retry of the same logical action should reference the original receipt when appropriate rather than create an indistinguishable duplicate.

### 3.5 ContinuationToken

Represents a resumable workflow boundary.

A continuation token connects:

```text
original intent
→ suspended or compacted state
→ external resolution or new context
→ resumed execution
```

The token is not necessarily a secret bearer token. It is a stable continuity reference whose security properties depend on the implementation.

### 3.6 LifecycleEvent

Captures a meaningful state transition.

Examples:

```text
MemoryProposed
MemoryQuarantined
MemoryActivated
MemorySuperseded
DecisionDeferred
DecisionResolved
ActionStarted
ActionCompleted
ActionReused
WorkflowResumed
```

Events enable replay and audit while allowing current state to remain compact.

---

## 4. Unified lifecycle

```text
1. Observe
   A user, tool, environment, or agent produces new information.

2. Remember
   The system creates a candidate MemoryArtifact with provenance.

3. Ground
   EvidenceReferences are attached and trust boundaries are evaluated.

4. Decide
   A DecisionReceipt returns ALLOW, BLOCK, or DEFER.

5. Act
   The authorized tool or agent operation executes with a stable action identity.

6. Prove
   An ActionReceipt records the result and links it to evidence and decision history.

7. Learn
   New outcomes update, confirm, supersede, or invalidate prior memory.
```

The cycle is recursive: an action result may become evidence for a later memory or decision.

---

## 5. Failure modes the model addresses

### Context compaction loss

Important user corrections or project decisions disappear after summarization.

**Model response:** durable memory with provenance, versioning, and supersession.

### Hidden invalid memory artifacts

Malformed or unsafe memory updates remain invisible but continue influencing future processing.

**Model response:** explicit quarantine and terminal lifecycle states.

### Duplicate side effects

A retry sends the same payment, email, or deployment twice.

**Model response:** stable action identity, idempotent processing, and ActionReceipt reuse.

### Approval deadlock

A worker remains blocked while waiting for human approval.

**Model response:** `DEFER` as a durable result plus checkpoint and continuation token.

### Stale operational state

An old project fact is recalled as if it were permanent truth.

**Model response:** scope, expiration, confirmation, supersession, and source revalidation.

### Memory poisoning

Untrusted tool or web content becomes durable instruction.

**Model response:** distinguish source classes, quarantine untrusted candidates, and retain provenance.

---

## 6. Public engineering observations

This model was refined while discussing concrete continuity and governance gaps in major open-source agent projects.

| Project | Public problem | LIMINAL contribution |
|---|---|---|
| OpenAI Codex | Observable completion of memory generation | Structured completion receipt and deterministic process boundary |
| Anthropic Claude Code | Persistent memory across compaction | Memory provenance, supersession, staleness, and selective recall |
| Google Gemini CLI | Invalid Auto Memory patch lifecycle | Explicit artifact states, reason codes, quarantine, and idempotency |
| CrewAI | Pre-tool-call authorization and asynchronous approval | `ALLOW / BLOCK / DEFER`, decision receipt, checkpoint, and continuation |

Public discussion links:

- [OpenAI Codex — memory generation receipt](https://github.com/openai/codex/issues/29430#issuecomment-4772703953)
- [Anthropic Claude Code — evidence-backed persistent memory](https://github.com/anthropics/claude-code/issues/34556#issuecomment-4772734257)
- [Google Gemini CLI — auditable memory artifact lifecycle](https://github.com/google-gemini/gemini-cli/issues/26523#issuecomment-4772804791)
- [CrewAI — durable DEFER and workflow continuation](https://github.com/crewAIInc/crewAI/issues/4877#issuecomment-4772839786)

These links are independent public engineering contributions. They do not imply endorsement, partnership, or adoption by the referenced projects.

---

## 7. Interoperability direction

A future interoperable envelope may allow different agent systems to exchange continuity records without sharing their internal storage or policy implementation.

Conceptual envelope:

```json
{
  "schema_version": "0.1",
  "record_id": "rec_01...",
  "record_type": "memory | evidence | decision | action | continuation | lifecycle_event",
  "scope": "...",
  "source_refs": [],
  "caused_by": [],
  "supersedes": [],
  "status": "...",
  "created_at": "..."
}
```

This is a direction for discussion, not a frozen protocol.

---

## 8. Open specification vs protected implementation

### Public in this document

- shared vocabulary;
- lifecycle concepts;
- minimal record relationships;
- interoperability direction;
- public problem mapping.

### Reserved to LIMINAL implementations

- causal validity and trust scoring;
- evidence sufficiency algorithms;
- memory-selection and consolidation logic;
- conflict resolution policy;
- tamper-evident receipt construction;
- storage and synchronization topology;
- graph traversal and replay algorithms;
- runtime risk evaluation;
- production SDKs and commercial integrations.

The public model makes the problem understandable. The protected implementation turns it into a reliable product.

---

## 9. Relationship to the LIMINAL ecosystem

The model connects existing LIMINAL technical directions:

- **LTP** — trace continuity and replayable execution;
- **CML** — causal memory and provenance;
- **CaPU** — permission-first action lifecycle;
- **PythiaLabs** — deterministic evidence gates for high-risk actions;
- **LiminalDB** — durable reactive state;
- **Осознание / Awareness** — accumulated experience and agent individuality.

The shared product position is:

> LIMINAL makes AI-agent memory and action continuity visible, evidence-backed, and auditable.

---

## 10. Next milestones

For v0.2:

- define state-transition invariants;
- publish a compact test matrix;
- add conflict and supersession examples;
- define deletion and tombstone behavior;
- specify concurrency and multi-agent scope isolation;
- add a minimal reference implementation without disclosing protected scoring logic.

---

## License and use

This architecture note is published as part of the LIMINAL repository. Repository license terms apply.

Discussion and citation are welcome. Implementation, redistribution, modification, or commercial use must follow the repository license.
