# DEFER and Continuation Conformance v0.1

**Status:** Draft framework-neutral conformance note  
**Project:** LIMINAL  
**Contributors:** Alexey Safalov and public GitHub discussion participants  
**Date:** 2026-06-23

## 1. Purpose

This note defines a small, reproducible conformance vector for deferred authorization and workflow continuation.

The core invariant is:

> A continuation is a locator for one frozen authorization context, not permission to execute whatever action is present at resume time.

The machine-readable fixture is:

- [`fixtures/defer-continuation-conformance-v0.1.json`](../../fixtures/defer-continuation-conformance-v0.1.json)

An independent dependency-free generator is:

- [`tools/generate_defer_continuation_vector.py`](../../tools/generate_defer_continuation_vector.py)

The generator intentionally does not read the fixture. It derives canonical bytes and digests independently from the published profile inputs.

---

## 2. Frozen authorization context

The frozen context is represented by two independently comparable digests.

### 2.1 Action identity

Action identity covers:

- caller identity;
- tool identity;
- resource scope.

Profile:

```text
org.liminal.defer.action-identity.v0.1
```

Preimage:

```json
{
  "profile_id": "org.liminal.defer.action-identity.v0.1",
  "caller_id": "agent:deploy",
  "tool_id": "tool:deploy",
  "resource_scope": "tenant:acme/environment:production"
}
```

The preimage is canonicalized with RFC 8785 JCS and hashed with SHA-256.

Raw concatenation of variable-length strings is forbidden because it is not injective.

### 2.2 Binding context

Binding context covers:

- canonical action arguments;
- policy context;
- target-state digest.

Profile:

```text
org.liminal.defer.binding.v0.1
```

Preimage:

```json
{
  "profile_id": "org.liminal.defer.binding.v0.1",
  "args": {
    "artifact": "api@sha256:abc123",
    "strategy": "rolling"
  },
  "policy_context": {
    "policy_id": "deploy-policy",
    "policy_version": "3"
  },
  "target_state_digest": "sha256:1111111111111111111111111111111111111111111111111111111111111111"
}
```

The preimage is canonicalized with RFC 8785 JCS and hashed with SHA-256.

### 2.3 Framework-local identifiers

A framework may maintain local identifiers such as:

```text
framework_local_action_id
framework_local_pending_id
framework_local_continuation_record_id
```

These identifiers may locate framework records, but they are not part of the normative action-identity or binding preimages unless a future comparison profile explicitly says otherwise.

Therefore:

- the same local identifier under a different profile is `NOT_COMPARABLE_PROFILE`;
- different local identifiers with the same profile IDs, canonical preimage bytes, and digests remain comparable;
- a framework may rename or migrate a local record without changing the portable conformance identity.

### Invariant

> Framework-local names may locate records, but they must not redefine portable comparison semantics.

---

## 3. DEFER record

A conforming DEFER record should expose at least:

```text
action_ref
action_identity_profile
binding_digest
binding_profile
hash_algorithm
canonicalization
decision_id
continuation_id
checkpoint_ref
issued_at
expires_at
consumed or consumed_at
```

It may additionally expose framework-local record identifiers, provided they are clearly marked non-normative for the declared comparison profiles.

The implementation must persist the frozen context and blocked checkpoint before returning control to the agent.

---

## 4. Comparison order

A resume implementation should evaluate in this order:

1. verify profile comparability;
2. verify continuation freshness;
3. resolve consumption/idempotency state;
4. recompute and compare action identity;
5. recompute and compare binding context;
6. re-check policy-required executor conditions;
7. execute the side effect at most once.

Framework-local identifiers may be used to find records before this procedure, but they must not replace any normative comparison above.

No external side effect may occur before these checks complete.

---

## 5. Terminal outcomes

The fixture distinguishes terminal outcomes instead of returning one generic reject:

```text
ACCEPT_EXECUTE_ONCE
RETURN_EXISTING_RESOLUTION
ACTION_IDENTITY_MISMATCH
BINDING_MISMATCH
NOT_COMPARABLE_PROFILE
EXPIRED_CONTINUATION
ALREADY_CONSUMED
```

### Meaning

- `ACCEPT_EXECUTE_ONCE` — unchanged frozen context, valid and unconsumed continuation;
- `RETURN_EXISTING_RESOLUTION` — unchanged idempotent retry after a known resolution, with no new side effect;
- `ACTION_IDENTITY_MISMATCH` — caller, tool, or resource scope changed;
- `BINDING_MISMATCH` — arguments, policy context, or target state changed;
- `NOT_COMPARABLE_PROFILE` — producer and verifier do not share the same preimage contract;
- `EXPIRED_CONTINUATION` — the continuation is outside its freshness bound;
- `ALREADY_CONSUMED` — continuation was spent and no reusable existing resolution applies.

---

## 6. Required cases

A minimal implementation should run:

1. unchanged first resume;
2. unchanged idempotent retry;
3. different framework-local IDs with identical profile bytes and digests;
4. changed caller;
5. changed tool;
6. changed resource scope;
7. changed arguments;
8. changed policy version;
9. changed target-state digest;
10. same framework-local IDs with an action-identity profile mismatch;
11. same framework-local IDs with a binding profile mismatch;
12. expired continuation;
13. replay after consumption.

Every reject case must assert zero additional side effects.

When the runtime can identify the changed dimension, the failure record should name it.

### Portable-profile boundary cases

Two cases are mandatory for framework neutrality:

```text
same local ID + different profile ID
→ NOT_COMPARABLE_PROFILE
```

```text
different local ID + same profile bytes + same digests
→ COMPARABLE
```

This avoids accidentally creating a universal framework action-ID namespace while preserving reproducible comparison behavior.

---

## 7. Test clock

The fixture uses an explicit `reference_clock` and per-case `evaluation_time` rather than the machine's wall clock.

This prevents a copied fixture from accidentally testing expiry instead of binding semantics.

---

## 8. Idempotency rule

An unchanged retry after successful resolution should return the existing resolved record rather than execute the side effect again.

This is distinct from an unknown replay of an already-consumed continuation, which should fail closed.

### Invariant

> Same frozen context plus known prior resolution returns the prior result; it does not repeat the action.

---

## 9. Independent generator rule

The fixture, prose note, and generator should be independently checkable.

The generator:

- does not read the fixture;
- starts from the published semantic inputs and profile IDs;
- emits canonical UTF-8 preimage bytes;
- emits SHA-256 digests for the baseline and one-field mutations;
- uses only the Python standard library.

A conforming implementation should be able to compare its own output against both the fixture and generator output.

### Invariant

> Conformance is stronger when no single artifact can pass only by reproducing its own hidden assumptions.

---

## 10. Framework neutrality

The vector does not prescribe:

- one policy engine;
- one orchestration framework;
- one checkpoint storage implementation;
- one framework-local action-ID syntax;
- one continuation-token security mechanism.

It standardizes:

- reproducible canonical preimage bytes;
- action and binding digests;
- declared profile identifiers;
- terminal mismatch classifications;
- side-effect behavior;
- the boundary between portable profile semantics and framework-local storage names.

The same fixture can be adapted to CrewAI, AutoGen, OpenHands, LangGraph, or another agent runtime.

---

## 11. Canonical principle

> Deferred authorization freezes both identity and binding. Resume is valid only when both remain comparable, fresh, unspent, and exactly matched; framework-local identifiers do not alter the declared comparison profile.
