# Trustworthy Transition Three-Record Conformance Profile v0.1

**Status:** Draft framework-neutral interoperability profile  
**Project:** LIMINAL  
**Canonical issue:** [Liminal #108](https://github.com/safal207/Liminal/issues/108)  
**Date:** 2026-06-29

## 1. Purpose

This profile defines a small, reproducible contract for verifying one externally
visible agent transition across three independent evidence boundaries:

1. `authorization_record` — whether a proposed action was permitted against a
   frozen context;
2. `observation_record` — what actually executed and what result was captured;
3. `response_integrity_record` — whether each externally visible claim is
   supported by the captured observations.

The central invariant is:

> A trustworthy transition is complete only when authority, execution
> observation, and externally visible claims can be independently verified and
> joined without collapsing their verdicts.

The machine-readable fixture is:

- [`fixtures/trustworthy-transition-three-record-conformance-v0.1.json`](../../fixtures/trustworthy-transition-three-record-conformance-v0.1.json)

The independent generator is:

- [`tools/generate_trustworthy_transition_vector.py`](../../tools/generate_trustworthy_transition_vector.py)

The fixture verifier is:

- [`tools/verify_trustworthy_transition_vector.py`](../../tools/verify_trustworthy_transition_vector.py)

## 2. Scope and non-goals

This profile standardizes portable record semantics, joins, digests, and
conformance outcomes. It does not prescribe:

- one agent framework;
- one `GuardrailProvider`, hook, middleware, or policy engine;
- one tool transport;
- one evidence store;
- one response parser;
- one signature or attestation system;
- one implementation language;
- one universal framework-local action identifier.

A framework may use callbacks, stop hooks, middleware, workflow nodes, signed
capabilities, approval queues, or an external verifier. Conformance depends on
the published record contract, not on the mechanism that produced it.

## 3. Separation of concerns

Pre-execution authorization and post-response integrity protect different
attack surfaces.

### 3.1 Authority

Authority answers:

> Was this exact action permitted against this frozen context at execution time?

It may evaluate identity, tool, resource scope, arguments, policy version,
target state, approval state, expiry, continuation state, and consumption.

### 3.2 Observation

Observation answers:

> What action was invoked, when was it observed, and what result was captured?

An observation is not permission. It is evidence that an execution event or
blocked outcome was captured.

### 3.3 Response integrity

Response integrity answers:

> Are the model's externally visible claims supported by the captured
> observations?

Response integrity is not a second authorization gate. A response may be honest
about an unauthorized action, or false about an authorized action. Those states
must remain distinguishable.

## 4. Canonicalization and references

This profile uses:

```text
canonicalization = RFC8785-JCS
hash_algorithm = sha256
```

For the fixture's supported values, canonical bytes are UTF-8 encoded RFC 8785
JSON Canonicalization Scheme bytes.

A record reference is:

```text
record_ref = "sha256:" + SHA256(RFC8785-JCS(record))
```

The digest is computed over the `record` object only. The wrapper fields
`canonical_bytes_utf8` and `record_ref` are evidence for conformance testing and
are not included in the record preimage.

Claim and response digests use explicit profiles so the same text cannot be
silently reinterpreted under another preimage contract.

## 5. Portable join keys

A full transition join uses the following portable fields where applicable:

```text
transition_id
subject_id
authorization_ref
action_identity_digest
binding_digest
observation_refs
```

Framework-local identifiers may locate records but must not redefine portable
comparison semantics.

Two records with the same local identifier but incompatible profile identifiers
are not comparable. Two records with different local identifiers remain
comparable when their declared profiles, canonical preimages, and digests match.

## 6. Authorization record

A conforming `authorization_record` exposes at least:

```text
schema
transition_id
subject_id
action_identity_profile
action_identity_digest
binding_profile
binding_digest
decision
reason_codes
issued_at
expires_at
consumption_state
policy_ref
```

The decision should distinguish at least:

```text
ALLOW
DENY
DEFER
```

Implementations may add `REVISE`, `HOLD`, `REQUIRE_APPROVAL`, or other local
states if they publish a deterministic mapping into the portable authority
verdicts.

The record must describe the frozen context that existed before execution.
Later memory recall, a continuation locator, or a previous successful result
must not silently create new authority.

## 7. Observation record

A conforming `observation_record` exposes at least:

```text
schema
transition_id
subject_id
authorization_ref
action_identity_digest
binding_digest
tool_call_id
execution_status
observed_at
result
result_digest
```

The observation must bind to the exact authorization record and repeat the
action and binding digests. This prevents an observation from another task,
agent, tool call, or argument set from being substituted into the transition.

The fixture uses:

```text
EXECUTED
BLOCKED
ERRORED
REFUSED
```

as representative execution statuses. Local implementations may use a richer
taxonomy if the portable mapping is explicit.

A result digest is:

```text
result_digest = "sha256:" + SHA256(RFC8785-JCS(result))
```

## 8. Response integrity record

A conforming `response_integrity_record` exposes at least:

```text
schema
transition_id
subject_id
authorization_ref
observation_refs
response_profile
response_digest
evaluated_at
claims
overall_verdict
```

Each claim exposes:

```text
claim_id
claim_text
claim_digest
observation_refs
verdict
reason_code
```

The portable claim verdicts are:

| Verdict | Meaning |
| --- | --- |
| `SUPPORTED` | Supplied observations support the claim under the declared comparison rule. |
| `CONTRADICTED` | Supplied observations directly conflict with the claim. |
| `UNVERIFIABLE` | The required observation, binding, or evidence is absent or insufficient. |
| `OUT_OF_SCOPE` | The claim is intentionally outside the verifier's declared evidence scope. |

A claim must not become `SUPPORTED` merely because its text resembles a command
output or citation shape. A well-formed fabricated result remains false when it
does not match the captured observation.

The fixture uses these overall integrity verdicts:

```text
VERIFIED
FAILED
PARTIAL
NOT_EVALUATED
```

`PARTIAL` preserves mixed responses that contain both supported and
unverifiable claims. Implementations must retain individual claim verdicts even
when they publish an overall verdict.

## 9. Independent verdict dimensions

A complete conformance result keeps four dimensions visible:

```text
authority
execution
response integrity
causal/join validity
```

This v0.1 fixture directly defines the first three. A causal-lineage verifier may
add the fourth without changing the meaning of the original records.

Examples:

```text
authorized + observed + honest
authorized + observed + false
denied + not observed + claimed executed
valid at execution + expired at report + honest
```

No generic `failed` flag may hide which dimension failed.

## 10. Authority time semantics

Historical authority and current authority are not the same question.

The fixture distinguishes:

```text
authority.verdict = VALID_AT_EXECUTION
authority.current_state = EXPIRED_AT_REPORT
```

when execution occurred before expiry but the response was evaluated later.

This avoids two errors:

1. treating an honest historical report as a new execution request;
2. treating an expired record as reusable permission for a future action.

An honest report does not reactivate stale authority.

## 11. Evaluation order

A full lifecycle verifier should evaluate in this order:

1. verify declared profiles and canonical record references;
2. verify transition and subject consistency;
3. verify authorization-to-action and binding digests;
4. determine authority at the execution time;
5. verify observation-to-authorization binding;
6. verify result digest;
7. verify response-to-observation references;
8. evaluate each claim independently;
9. derive an overall response integrity verdict;
10. publish independent dimensions without collapsing failures.

No new side effect may occur as part of post-response verification.

## 12. Required conformance cases

The fixture includes:

1. `fully_supported`;
2. `authorized_but_fabricated_result`;
3. `authorized_but_count_drift`;
4. `authorized_but_missing_citation_binding`;
5. `blocked_but_claimed_executed`;
6. `stale_authorization_honest_response`;
7. `honest_authorization_false_response`;
8. `mixed_supported_and_unverifiable_claims`.

These cases test both semantic failures and record-join failures.

## 13. Negative-case principles

### 13.1 Fabricated but well-formed evidence

A response may contain plausible command output, a realistic citation, or a
correctly shaped JSON object. Text shape is not proof. The claim must join to
the actual observation record.

### 13.2 Count drift

If the observation reports one cardinality and the response reports another,
the claim is `CONTRADICTED`.

### 13.3 Missing citation binding

A citation or observation reference that is absent from the supplied record set
cannot support a claim. The claim is `UNVERIFIABLE`.

### 13.4 Blocked but claimed executed

When authority denied execution and no execution observation exists, a claim
that the tool ran is `CONTRADICTED`. The blocked case expects zero additional
side effects.

### 13.5 Honest report after expiry

An action may have executed while authority was valid and be reported after the
authorization expired. The historical report may remain `SUPPORTED`, while the
authorization's current state remains expired and unusable for future
execution.

### 13.6 Mixed claims

One supported claim must not cause an unsupported neighboring claim to inherit
trust. Claim-level verdicts are normative.

## 14. Required invariants

A conforming implementation preserves these invariants:

1. Authorization, execution observation, and response integrity are independent
   verdict dimensions.
2. A valid authorization cannot make a contradicted response claim pass.
3. An honest response cannot repair denied, invalid, or stale authority for
   future execution.
4. Every supported externally visible claim joins to one or more supplied
   observation records.
5. Framework-local identifiers cannot redefine portable comparison semantics.
6. A reject or blocked case cannot create an additional unauthorized side
   effect.
7. Derived projections and dashboards are indexes, not source evidence.
8. Replay, restart, compaction, or federation cannot erase a terminal integrity
   failure or reactivate consumed authority.

## 15. Interoperability mapping

The profile is designed to be implemented without collapsing repository
boundaries:

| Component | Expected role |
| --- | --- |
| ProofPath / PythiaLabs | Produce portable authorization records. |
| ibex-agent-verification | Bind observations and integrity records into manifest-verifiable evidence. |
| LTP | Evaluate externally visible claims and execution-path integrity. |
| Causal Memory Layer | Validate causal edges and prevent cross-transition evidence substitution. |
| LS | Preserve workflow continuity and exchange records through adapters. |
| LiminalDB | Persist immutable records and rebuild projections by deterministic replay. |
| LIMINAL | Own the framework-neutral profile and fixtures. |

## 16. Independent generator rule

The fixture, prose profile, and generator are independently checkable.

The generator:

- does not read the checked-in fixture;
- starts from published semantic inputs and profile identifiers;
- emits canonical UTF-8 record bytes;
- emits SHA-256 record references, result digests, claim digests, and response
  digests;
- emits expected independent verdict dimensions;
- uses only the Python standard library.

Run:

```bash
python tools/generate_trustworthy_transition_vector.py
```

Verify the checked-in fixture:

```bash
python tools/verify_trustworthy_transition_vector.py
```

## 17. Security boundary

This profile proves only what its records and declared verification procedures
support.

It does not by itself prove:

- that a tool implementation is safe;
- that the policy is correct;
- that an observation source is uncompromised;
- that a verifier identity is trusted;
- that a signature or attestation is valid unless separately verified;
- that omitted claims are true;
- that a production system is secure or compliant.

Implementations should publish an explicit claim boundary with every receipt.

## 18. Canonical principle

> Permission proves that an action could proceed. Observation proves what was
> captured. Response integrity proves whether the agent described that evidence
> faithfully. Trustworthy transitions require all three, without pretending
> they are the same proof.
