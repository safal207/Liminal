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
3. changed caller;
4. changed tool;
5. changed resource scope;
6. changed arguments;
7. changed policy version;
8. changed target-state digest;
9. action-identity profile mismatch;
10. binding profile mismatch;
11. expired continuation;
12. replay after consumption.

Every reject case must assert zero additional side effects.

When the runtime can identify the changed dimension, the failure record should name it.

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

## 9. Framework neutrality

The vector does not prescribe:

- one policy engine;
- one orchestration framework;
- one checkpoint storage implementation;
- one action-ID syntax;
- one continuation-token security mechanism.

It standardizes observable comparison and side-effect behavior.

The same fixture can be adapted to CrewAI, AutoGen, OpenHands, LangGraph, or another agent runtime.

---

## 10. Canonical principle

> Deferred authorization freezes both identity and binding. Resume is valid only when both remain comparable, fresh, unspent, and exactly matched.
