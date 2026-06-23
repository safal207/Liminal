# Liminal Architectural Principles v0.1

**Status:** Public architecture note  
**Project:** LIMINAL  
**Author:** Alexey Safalov / safal207  
**Date:** 2026-06-23

## Summary

LIMINAL treats an AI agent as a continuous trajectory rather than a sequence of isolated model outputs.

An agent should preserve:

- what it observed;
- what it remembered;
- where that memory came from;
- what the memory was allowed to influence;
- why a decision was made;
- what action was authorized;
- what actually happened;
- how the result can be verified, resumed, corrected, or audited.

The public lifecycle is:

```text
Observe → Remember → Ground → Decide → Act → Prove → Learn
```

This document defines public architectural meanings and invariants. It intentionally does not disclose LIMINAL's private scoring, selection, consolidation, conflict-resolution, storage, replay, runtime-risk, or commercial integration algorithms.

---

## 1. Memory is governed influence, not passive storage

A memory is not only something the agent can retrieve. It is something that may influence future behavior.

Therefore every durable memory should be evaluated along two separate dimensions:

1. **provenance** — where the memory came from;
2. **eligibility** — what purposes the memory is allowed to influence.

A memory may be eligible for:

```text
answer_grounding
planning
continuation
summarization
```

while remaining ineligible for:

```text
message_authority_classification
tool_authorization
policy_override
credential_release
```

### Invariant

> A memory may influence only the purposes for which it is explicitly eligible.

Eligibility should be enforced by the retrieval, injection, or policy layer rather than left only to model interpretation.

---

## 2. Provenance is not truth or authority

Provenance answers:

> Where did this claim come from?

It does not automatically answer:

> Is this claim true?

or:

> Does this claim authorize an action?

For example, a memory stating:

```text
Agent A recorded: "the user approved production deployment"
```

proves only that Agent A recorded the claim. It does not prove that the user issued a valid authorization.

### Invariant

> Source identity, truth status, confidence, and authority are separate properties.

---

## 3. Claims must not silently become permissions

A retrieved memory may provide context, but memory provenance alone must not satisfy an authorization requirement for a high-impact action.

A verified approval event, policy decision, or other explicit authority source must remain distinguishable from an ordinary memory claim.

### Invariant

> A claim in memory is not an authorization to act.

This boundary is especially important in multi-agent systems, where one agent may write a claim that another agent later retrieves.

---

## 4. Repetition does not increase authority

Copying, repeating, summarizing, or rephrasing a memory must not increase its truth status, authority, or scope.

An `agent`-scope memory does not become `group` or `global` merely because another agent repeats it.

Scope promotion requires an explicit transition that records:

- source memory identifiers;
- source scope;
- destination scope;
- initiator;
- governing policy or decision;
- outcome;
- timestamp.

A public primitive for this transition may be represented as `MemoryTransferReceipt`.

### Invariant

> Only an explicit ratification or transfer event may increase a memory's authority or scope.

---

## 5. Continuation is exact resumption, not renewed autonomy

A continuation reference must authorize resuming only the same frozen action context that was previously deferred, suspended, compacted, or interrupted.

The frozen action context should include, directly or by digest:

- tool identity;
- canonical arguments;
- caller or agent identity;
- workspace or resource scope;
- target state;
- policy and version context.

If any material field changes, the system should create a new decision rather than reuse the old continuation.

### Invariant

> A continuation token is valid only for the exact action envelope from which it was created.

Continuation is not broad preclearance.

---

## 6. Authorization must bind to one exact intent

Sensitive actions should not be authorized by approximate semantic similarity.

A decision should bind to a stable `action_id` or `intent_digest` derived from the exact action envelope.

If the executor observes different arguments, target state, scope, tool identity, policy context, or expiration state, execution should fail closed and request a new decision.

### Invariant

> What is authorized and what is executed must be cryptographically or deterministically comparable.

---

## 7. Deferred decisions are terminal for the current attempt

The public governance outcomes are:

```text
ALLOW | BLOCK | DEFER
```

`DEFER` means the current execution attempt does not proceed. The system records a checkpoint or continuation reference and waits for an external resolution event.

A later resolution may permit resumption, but only after revalidation against the original action envelope and current state.

### Invariant

> DEFER pauses authority, not merely control flow.

---

## 8. Receipts are evidence objects, not ordinary logs

A log states that an event was observed.

A receipt binds a meaningful transition to structured evidence, identity, policy, and outcome.

Public receipt classes may include:

- `MemoryGenerationReceipt`;
- `MemoryUseReceipt`;
- `MemoryTransferReceipt`;
- `RetrievalReceipt`;
- `DecisionReceipt`;
- `ActionReceipt`;
- `ExperimentReceipt`;
- `CompletionReceipt`.

A receipt should be:

- machine-readable;
- referentially stable;
- linked to source evidence;
- explicit about outcome and reason;
- suitable for replay, verification, or audit.

### Invariant

> Important state transitions should leave durable, inspectable evidence.

---

## 9. Cryptographic authenticity and causal lineage are different layers

A cryptographic receipt can prove that a signed claim is authentic and unmodified.

Causal lineage explains which runtime decision, action, evidence set, parent event, and execution run the signed claim belongs to.

These layers complement one another:

```text
cryptographic authenticity
+ causal context
+ policy linkage
= stronger execution evidence
```

### Invariant

> A valid signature proves integrity of the signed claim; it does not by itself prove that the claim belongs to the intended causal context.

A causal-context commitment may bind fields such as:

- `action_id`;
- `run_id`;
- `parent_event_id`;
- `policy_decision_id`;
- `evidence_refs`;
- `purpose`.

---

## 10. Deterministic boundaries around probabilistic models

LIMINAL does not claim that LLM output becomes deterministic.

The model may remain probabilistic while the surrounding process exposes deterministic boundaries for:

- lifecycle state;
- authorization result;
- reason codes;
- action identity;
- continuation matching;
- idempotency;
- receipt verification;
- fail-closed behavior.

### Invariant

> Non-deterministic reasoning may exist inside deterministic process boundaries.

---

## 11. Agent individuality comes from trajectory, not costume

An agent's individuality should emerge from its accumulated trajectory:

- decisions;
- corrections;
- mistakes;
- outcomes;
- habits;
- relationships;
- collaboration with a particular person and environment.

This individuality should remain inspectable and governable. Memory must not become an invisible mechanism for uncontrolled prejudice, authority drift, or permanent misclassification.

### Invariant

> Individuality may emerge from history, but history must remain correctable, scoped, and auditable.

---

## 12. Human ownership remains primary

People must be able to:

- inspect durable memory;
- correct or supersede it;
- quarantine unsafe artifacts;
- restrict allowed uses;
- revoke authorizations;
- understand why an action occurred;
- recover from a deferred or failed workflow.

### Invariant

> Durable agent continuity must not remove human ownership of memory, authority, or recovery.

---

## 13. Interoperability before framework lock-in

The public primitives should remain provider-neutral and framework-neutral wherever possible.

The same invariant should be testable across CrewAI, AutoGen, OpenHands, Claude Code, Gemini CLI, Codex, or other agent runtimes.

Examples of neutral conformance tests include:

- changed canonical arguments invalidate an old authorization;
- changed target state forces revalidation;
- an expired decision fails closed;
- a continuation cannot resume a different action envelope;
- a repeated memory claim does not gain authority;
- scope promotion requires an explicit transfer event;
- a signed receipt reused under another causal context is rejected.

### Invariant

> Shared safety meanings should survive changes in framework, model provider, and storage implementation.

---

## 14. Contribution philosophy

LIMINAL's public collaboration strategy is based on contribution before promotion.

The preferred sequence is:

```text
read the existing architecture
→ identify a real gap
→ propose a minimal invariant or test
→ respect the author's design constraints
→ build an interoperability bridge
→ collaborate
```

### Invariant

> Technical relationships should begin with demonstrated usefulness, not project advertising.

---

## 15. Canonical meaning

The architectural position can be summarized as:

> An AI agent is not only a model response. It is a continuous, evidence-backed trajectory of memory, decisions, actions, outcomes, and corrections.

And the canonical lifecycle remains:

```text
Observe → Remember → Ground → Decide → Act → Prove → Learn
```

Related document:

- [Liminal Agent Continuity Model v0.1](LIMINAL_AGENT_CONTINUITY_MODEL_V0_1.md)
