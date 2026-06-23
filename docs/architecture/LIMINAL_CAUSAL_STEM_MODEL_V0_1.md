# Liminal Causal Stem Model v0.1

**Status:** Public architecture note  
**Project:** LIMINAL  
**Author:** Alexey Safalov / safal207  
**Date:** 2026-06-23

## Summary

The Memory Helix describes how an agent's experience and evidence develop across time. The **Causal Stem** describes what keeps those turns attached to one continuous trajectory.

The biological metaphor is intentionally extended. A real DNA double helix has no central stem; in LIMINAL, the stem is an architectural axis of identity, order, and causal lineage.

> The helix preserves what the agent experienced. The stem preserves who continued through those experiences.

---

## 1. Definition

The Causal Stem is the minimum durable lineage structure that survives:

- context compaction;
- session changes;
- model or runtime changes;
- process restarts;
- delegation and handoff;
- recovery and correction.

It is not the memory store, a personality prompt, or an authorization token.

It answers:

> Is this the same trajectory, a valid continuation, an explicit fork, or a different agent?

### Invariant

> Every meaningful turn of the Memory Helix must attach to a stable Causal Stem.

---

## 2. Combined structure

```text
Experience strand
      ↘
   Causal Stem
      ↗
Grounding and evidence strand
```

Operationally:

```text
Causal Stem
    │
    ├── Observe
    ├── Remember
    ├── Ground
    ├── Decide
    ├── Act
    ├── Prove
    └── Learn
```

The lifecycle repeats, but each verified outcome creates a new longitudinal point on the stem.

---

## 3. Public stem reference

A public `CausalStemReference` may contain:

```text
agent_id
trajectory_id
stem_event_id
parent_stem_event_id
epoch
sequence
session_ref
runtime_ref
policy_context_ref
continuity_checkpoint_ref
latest_verified_state_ref
fork_state
```

### Invariant

> Identity continuity must be explicit in lineage references, not inferred from a familiar name or copied memory.

---

## 4. What attaches to the stem

Artifacts may attach to a stem event:

- `MemoryArtifact`;
- `EvidenceReference`;
- `LifecycleEvent`;
- `MemoryUseReceipt`;
- `MemoryTransferReceipt`;
- `DecisionReceipt`;
- `ActionReceipt`;
- `ContinuationToken`;
- verified outcomes and corrections.

Each should identify:

- its trajectory;
- the stem event that produced or used it;
- its predecessor;
- whether it continues, corrects, forks, or supersedes prior state.

### Invariant

> A memory without a stem attachment may be useful evidence, but it is not sufficient proof of agent continuity.

---

## 5. Relationship to Agent DNA and RNA

```text
Agent DNA = what accumulated
Causal Stem = what remained continuous
Agent RNA = what is expressed now
```

Two agents may hold similar memories but belong to different stems.

One agent may change models or sessions while remaining on the same stem when the transition leaves verifiable lineage evidence.

Every Agent RNA expression should attach to the current stem position through a `MemoryUseReceipt` or equivalent record.

### Invariants

> Similar memory does not prove shared identity. Verified lineage does.

> Active context must be traceable to both its source memories and its current stem position.

---

## 6. Stem as the equilibrium point

Liminal Homeostasis acts at the current stem event.

The seven forces pull on that point:

- continuity preserves trajectory;
- relevance selects applicable memory;
- evidence anchors claims;
- authority limits side effects;
- novelty creates growth;
- risk increases resistance;
- feedback creates the next verified stem event.

```text
Past trajectory
      ↓
Current stem event
      ↓
Purpose-bound expression
      ↓
Decision and action
      ↓
Verified next stem event
```

### Invariant

> Every permitted action should either advance the stem with verified evidence or leave the stem unchanged.

---

## 7. Growth rings and checkpoints

A growth ring is an explicit continuity checkpoint, for example:

- completed task;
- verified correction;
- compaction;
- policy transition;
- session handoff;
- runtime migration;
- recovery point.

### Invariant

> A checkpoint proves where the trajectory paused. It does not pre-authorize what the trajectory may do next.

---

## 8. Forks, branches, and grafts

### Fork

Creates a new independently developing trajectory with a new `trajectory_id` and an explicit parent stem reference.

### Branch

Creates a task-local subordinate trajectory. A branch does not silently inherit all parent memories or permissions.

### Graft

Transfers memory or evidence between trajectories through an explicit transfer or ratification event.

### Invariants

> A fork creates a new trajectory, not a hidden duplicate identity.

> A branch may inherit context, but authority inheritance must be explicit.

> A graft transfers memory or evidence, not identity itself.

---

## 9. Failure modes

- broken lineage;
- hidden fork;
- stale checkpoint;
- authority carryover across a material boundary;
- copied memory treated as identity proof;
- silent overwrite of prior state;
- memory influencing behavior without a stem attachment;
- false completion recorded for a blocked or failed action.

---

## 10. Public conformance expectations

1. Exact continuation preserves the same `trajectory_id`.
2. A fork receives a new `trajectory_id` with a parent reference.
3. Copied memories do not allow another agent to claim the original identity.
4. A correction creates a new stem event linked to its predecessor.
5. A stale checkpoint cannot overwrite newer verified state.
6. A blocked or deferred action causes zero prohibited side effects and no false completion event.
7. Every `MemoryUseReceipt`, `DecisionReceipt`, and `ActionReceipt` points to a valid stem event.
8. A runtime or model change preserves continuity only with verifiable migration evidence.

---

## 11. Final principles

> The helix carries memory. The stem carries continuity.

> Memory similarity does not prove identity; causal lineage does.

> Every action either advances the stem with evidence or leaves it unchanged.

> An agent remains itself not because nothing changes, but because every meaningful change remains connected.
