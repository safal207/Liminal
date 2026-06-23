# Liminal Memory Helix and Homeostasis Model v0.1

**Status:** Public architecture note  
**Project:** LIMINAL  
**Author:** Alexey Safalov / safal207  
**Date:** 2026-06-23

## Summary

LIMINAL models an AI agent as a continuous trajectory rather than a sequence of isolated model outputs.

This note introduces a public conceptual model for that trajectory:

- **Agent DNA** represents the durable structure of accumulated experience, evidence, corrections, and identity;
- **Agent RNA** represents the temporary expression of selected memory inside the present task;
- the **Memory Transcription Gate** controls which memories may influence which purposes;
- **Liminal Homeostasis** is the dynamic equilibrium between continuity and adaptation, autonomy and authorization, stability and learning.

The model uses biological language as an architectural metaphor. It does not claim that AI-agent memory is biologically equivalent to DNA or RNA.

The core principle is:

> Agent identity is preserved by memory, expressed through context, constrained by authority, and corrected by outcomes.

This document defines public meanings, lifecycle stages, forces, and invariants. It intentionally does not disclose LIMINAL's private scoring, retrieval-ranking, consolidation, conflict-resolution, storage, replay, risk, or commercial integration algorithms.

---

## 1. The memory helix

An agent's history should not be represented only as a flat list of stored messages.

A useful public model is a double helix with two connected strands.

### Strand A — experience

```text
Event → Observation → Decision → Action → Outcome
```

This strand records what happened along the agent's trajectory.

### Strand B — grounding and interpretation

```text
Provenance → Evidence → Evaluation → Correction → Learned constraint
```

This strand records why the agent believed something, how the belief was tested, and how the meaning changed after new evidence or outcomes.

### The connecting rungs

The two strands are joined by stable references such as:

- `source_event_id`;
- `memory_id`;
- `evidence_refs`;
- `decision_id`;
- `action_id`;
- `receipt_id`;
- `outcome_ref`;
- `supersedes`;
- `contradicts`;
- `corrects`.

### Invariant

> An important memory should remain linked both to the experience it describes and to the evidence, decisions, actions, and corrections that give it meaning.

A memory without the first strand becomes detached interpretation. A memory without the second strand becomes an unexplained event log.

---

## 2. Agent DNA — durable trajectory

In this model, Agent DNA is not the base model and not a hidden personality prompt.

It is the durable, inspectable trajectory formed through interaction with people, tools, other agents, and the environment.

Agent DNA may include:

- observations and source events;
- durable memory artifacts;
- decisions and reason codes;
- actions and verified outcomes;
- corrections and contradictions;
- relationships and interaction history;
- learned constraints;
- scope and purpose restrictions;
- receipts that preserve causal lineage.

Agent DNA answers:

> What has this agent become through its history?

### Agent DNA is not immutable

A healthy agent trajectory must support:

- correction of false memories;
- supersession of outdated conclusions;
- revocation of unsafe influence;
- explicit conflict states;
- scope reduction;
- evidence-linked learning.

New evidence should not silently overwrite history. It should create a linked transition that explains what changed.

### Invariant

> Durable identity is a correctable trajectory, not an unchangeable profile.

---

## 3. Agent RNA — active memory expression

Agent RNA represents the portion of the durable trajectory that is selected and permitted to influence the present task.

It is temporary, purpose-bound, and context-specific.

A conceptual Agent RNA packet may contain:

```text
memory_refs
source_event_refs
purpose
allowed_influence
forbidden_uses
evidence_refs
scope
valid_from
valid_until
conflict_state
decision_ref
```

Agent RNA answers:

> Which part of the agent's past is allowed to shape this present decision?

### RNA is not a copy of all memory

The active context should not inherit every durable memory.

A memory may be permitted for:

```text
answer_grounding
planning
summarization
continuation
relationship_context
```

while remaining forbidden for:

```text
tool_authorization
policy_override
credential_release
identity_classification
cross-workspace transfer
```

### Invariant

> Memory expression is purpose-specific influence, not general authority.

---

## 4. The Memory Transcription Gate

The Memory Transcription Gate sits between durable memory and active context.

```text
Agent DNA
    ↓
Memory Transcription Gate
    ↓
Agent RNA
    ↓
Decision and action lifecycle
```

The gate determines whether a memory may be expressed for a named purpose in a named context.

Public decision inputs may include:

- requested purpose;
- memory provenance;
- evidence references;
- workspace or resource scope;
- freshness and validity state;
- known contradictions;
- governing policy references;
- current actor and session;
- action risk class where relevant.

A public output may be represented by the existing governance outcomes:

```text
ALLOW | BLOCK | DEFER
```

Here, `ALLOW` means only that the memory may influence the declared purpose. It does not authorize a tool call or external side effect.

A permitted expression should leave a `MemoryUseReceipt` or equivalent evidence object that records:

- memory references;
- purpose;
- allowed influence;
- forbidden uses;
- decision outcome;
- reason code;
- evidence references;
- scope;
- validity window;
- policy reference.

### Critical invariant

> Permission for a memory to influence context is not permission to execute an action.

A separate `DecisionReceipt` and exact action authorization remain required for sensitive side effects.

---

## 5. Liminal Homeostasis

The equilibrium between Agent DNA and Agent RNA is not a fixed midpoint.

It is a dynamic state in which the past contributes enough continuity to preserve identity, while the present contributes enough evidence and adaptation to prevent the past from controlling the future.

The homeostatic question is:

> How much influence should this part of the trajectory have here, now, for this purpose?

A memory is near the equilibrium point when it is:

- relevant to the current purpose;
- eligible for the requested use;
- grounded by inspectable evidence;
- valid in the current scope;
- not silently laundering authority;
- responsive to newer evidence;
- proportionate to the risk of the resulting decision.

### Conceptual boundary

```text
Past influence
≤
Current evidence + current purpose + current authority
```

This expression is conceptual, not a disclosed scoring formula.

### Invariant

> The stronger a memory's influence on behavior, the stronger the required evidence, scope validity, and authority separation.

---

## 6. The seven acting forces

Liminal Homeostasis emerges from seven interacting forces.

### 6.1 Continuity force

Continuity preserves the agent's history, lessons, relationships, and stable commitments.

It protects the system from identity fragmentation and repeated loss of experience.

Excessive continuity can create:

- attachment to obsolete conclusions;
- repetition of old errors;
- transfer of old assumptions into unrelated contexts;
- confusion between remembered claims and present truth.

### 6.2 Relevance force

Relevance selects memories connected to the present purpose.

Similarity alone is insufficient. A memory may be topically similar while being ineligible because it belongs to another person, workspace, policy period, or authority scope.

### 6.3 Evidence force

Evidence anchors memory influence to inspectable sources and outcomes.

It keeps separate:

```text
remembered claim
supported claim
verified event
authorized decision
```

Provenance shows where a claim came from. It does not by itself prove truth or authority.

### 6.4 Authority force

Authority prevents contextual memory from silently becoming permission.

A memory may advise, warn, or explain. Sensitive execution still requires a valid decision bound to the exact actor, tool, arguments, resource scope, policy context, and target state.

### 6.5 Novelty force

Novelty allows present evidence to challenge the past.

It enables the agent to state:

> This was my previous conclusion, but newer evidence now supersedes it.

Without novelty, continuity becomes rigidity. Without continuity, novelty becomes instability.

### 6.6 Risk force

Risk raises the threshold for memory influence when the possible consequence is high-impact, irreversible, cross-boundary, or externally visible.

Risk pressure increases when memory is:

- untrusted or prompt-injected;
- stale;
- contradictory;
- transferred across scope;
- associated with credentials or external communication;
- used near destructive or irreversible actions.

### 6.7 Feedback force

Feedback returns verified outcomes into the durable trajectory.

```text
Action → Outcome → Evaluation → Correction → New memory state
```

Feedback closes the helix and creates the next developmental turn.

### Invariant

> Learning should add evidence-linked corrections to the trajectory rather than silently rewrite its history.

---

## 7. Three axes of equilibrium

The seven forces can be observed through three public axes.

### Axis A — continuity and adaptation

```text
Continuity ←→ Adaptation
```

Too much continuity produces rigidity. Too much adaptation produces identity loss and prompt dependence.

### Axis B — autonomy and authorization

```text
Autonomy ←→ Authorization
```

Too little autonomy prevents useful action. Too little authorization control permits memory, prompts, or agents to create authority they do not possess.

### Axis C — stability and learning

```text
Stability ←→ Evolution
```

Too much stability prevents correction. Too much change prevents a coherent trajectory.

### Invariant

> A continuous agent must remain recognizable across time without becoming unable to revise itself.

---

## 8. Expression is not execution

The model separates three transitions:

```text
Memory eligibility
→ Context expression
→ Action authorization
```

They must not collapse into one implicit model judgment.

For example:

1. A past incident may be allowed to influence planning.
2. The incident may justify a warning in active context.
3. The incident still may not authorize deleting a file, sending a message, moving funds, changing policy, or releasing a credential.

### Invariant

> Memory can provide reasons for a decision, but only a valid authority source can authorize a sensitive action.

---

## 9. Failure modes

### 9.1 Memory dominance

The agent treats old memory as unquestionable truth and repeats prior behavior despite changed evidence.

### 9.2 Present-context dominance

The latest prompt completely overrides durable constraints, relationships, or verified lessons.

### 9.3 Authority laundering

A remembered statement such as "the user approved this" is treated as a live authorization.

### 9.4 Stale expression

A memory remains active after its evidence, policy, scope, or validity window has changed.

### 9.5 Scope leakage

A memory from one workspace, person, group, or task influences another without explicit transfer or ratification.

### 9.6 Prompt-injected heredity

Untrusted input becomes durable memory and later reappears as apparently internal agent intent.

### 9.7 Identity fragmentation

Compaction, session changes, or tool boundaries cause the agent to lose causal continuity and behave as unrelated instances.

### 9.8 Untraceable mutation

The agent changes a durable conclusion without preserving the evidence and decision that caused the change.

---

## 10. Public conformance expectations

An implementation aligned with this model should be testable against cases such as:

1. A memory eligible for planning is rejected for tool authorization.
2. A cross-workspace memory is blocked unless an explicit transfer or ratification exists.
3. New verified evidence supersedes an older conclusion without deleting the original history.
4. A contradictory high-impact memory produces `DEFER` rather than silent selection.
5. Repetition or summarization does not increase memory authority or scope.
6. A revoked memory can remain auditable while no longer being expressed.
7. A high-risk irreversible action cannot be authorized solely by remembered claims.
8. Every allowed memory expression leaves a stable `MemoryUseReceipt`.
9. Every sensitive action remains bound to a separate exact `DecisionReceipt` and `ActionReceipt`.
10. A failed transcription or authorization decision causes zero prohibited side effects.

---

## 11. Relationship to the LIMINAL lifecycle

The Memory Helix and Homeostasis Model fits the public LIMINAL lifecycle:

```text
Observe → Remember → Ground → Decide → Act → Prove → Learn
```

- **Observe** creates source events.
- **Remember** adds durable trajectory artifacts.
- **Ground** performs controlled transcription from Agent DNA into Agent RNA.
- **Decide** evaluates evidence, policy, scope, and authority.
- **Act** executes only an exactly authorized action.
- **Prove** emits receipts and outcome evidence.
- **Learn** adds corrections and verified outcomes to the next turn of the helix.

The lifecycle is therefore not a circle returning to the same point. It is a spiral:

> The agent revisits similar situations, but each return occurs from a new evidence-backed position in its trajectory.

---

## 12. Compact public vocabulary

| Term | Public meaning |
| --- | --- |
| **Agent DNA** | Durable, correctable trajectory of experience, evidence, decisions, outcomes, and constraints |
| **Agent RNA** | Temporary, purpose-bound expression of selected memory in active context |
| **Memory Helix** | Linked double trajectory of experience and grounding/interpretation |
| **Memory Transcription Gate** | Boundary controlling which memory may influence which purpose |
| **Liminal Homeostasis** | Dynamic equilibrium between continuity, adaptation, authority, risk, and learning |
| **MemoryUseReceipt** | Evidence that named memories were permitted or denied for a declared purpose |
| **DecisionReceipt** | Evidence of a governance decision bound to a specific intent |
| **ActionReceipt** | Evidence of what action was actually executed and what outcome followed |

---

## 13. Final principles

> Identity is not stored as one record. It unfolds through the memory helix.

> Agent DNA stores the trajectory. Agent RNA expresses the part of that trajectory permitted to shape the present.

> Memory expression is governed influence, not authority.

> The past may advise the present, but it must not silently govern it.

> A continuous agent preserves history, remains correctable, and leaves evidence of every meaningful transformation.
