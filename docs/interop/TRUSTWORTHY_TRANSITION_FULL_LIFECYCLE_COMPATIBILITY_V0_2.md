# Trustworthy Transition Full-Lifecycle Compatibility v0.2

**Status:** Draft stacked interoperability pack  
**Tracking issue:** [Liminal #112](https://github.com/safal207/Liminal/issues/112)  
**Base pack:** [Liminal PR #110](https://github.com/safal207/Liminal/pull/110)  
**Canonical three-record profile:** `org.liminal.trustworthy-transition.three-record.v0.1`

## Purpose

The v0.1 ecosystem pack normalizes three independent transition dimensions:

```text
authority
execution
response_integrity
```

This v0.2 pack consumes those checked-in v0.1 case results and adds three more
independent lifecycle boundaries:

```text
causal_validity
continuity_posture
durability
```

The resulting portable lifecycle is:

```text
authorization_record
        ↓
observation_record(s)
        ↓
response_integrity_record
        ↓
causal_audit_record
        ↓
continuity_snapshot_record
        ↓
durability_receipt
```

No downstream component may rewrite an upstream verdict.

## Files

- Fixture: [`fixtures/trustworthy-transition-full-lifecycle-compatibility-v0.2.json`](../../fixtures/trustworthy-transition-full-lifecycle-compatibility-v0.2.json)
- Verifier: [`tools/verify_trustworthy_transition_lifecycle.py`](../../tools/verify_trustworthy_transition_lifecycle.py)
- Base fixture: [`fixtures/trustworthy-transition-ecosystem-compatibility-v0.1.json`](../../fixtures/trustworthy-transition-ecosystem-compatibility-v0.1.json)

Run:

```bash
python3 tools/verify_trustworthy_transition_vector.py
python3 tools/verify_trustworthy_transition_ecosystem.py
python3 tools/verify_trustworthy_transition_lifecycle.py
```

Generate a deterministic receipt:

```bash
python3 tools/verify_trustworthy_transition_lifecycle.py \
  --output artifacts/trustworthy-transition-full-lifecycle-receipt.json
```

## Stacked dependency

This pack is intentionally based on draft PR #110. It does not duplicate the
first three implementation registries or their local vocabulary mappings.

Every v0.2 case names a `base_case_id` from the v0.1 fixture. The verifier loads
that case and copies only its already-derived values:

```text
authority
execution
response_integrity
required base records
```

The v0.2 expected object must repeat those values exactly. A mismatch fails
verification.

This turns verdict preservation into a machine-checked rule rather than prose.

## Pinned downstream implementations

| Boundary | Implementation | Exact PR head |
|---|---|---|
| Causal join validation | Causal-Memory-Layer PR #164 | `b5cae48e82e6958e8510999fe5907c50a3b3f0c9` |
| Pause/retry/resume continuity | LS PR #770 | `0ed661304e422f84dac375d334336ab1b8f76ca1` |
| Local durable event sourcing and replay | LiminalDB PR #89 | `47a2bf0323ab3c546013e03815d4ec0bdf59a9e0` |

Each registry entry declares:

- repository and pull request;
- exact immutable head SHA;
- implementation artifact path;
- local profile identifier;
- one owned lifecycle role;
- canonical record produced;
- canonical records consumed;
- local-to-canonical vocabulary mapping.

The pack is offline. It does not follow mutable branches during CI.

## Role ownership

### Causal-Memory-Layer

Role:

```text
CAUSAL_AUDITOR
```

Consumes:

```text
authorization_record
observation_record
response_integrity_record
```

Produces:

```text
causal_audit_record
```

Canonical values:

```text
VALID
INVALID
NOT_EVALUATED
UNKNOWN
```

Causal validity answers whether supplied records form one coherent transition.
It does not re-evaluate response truth or issue authority.

### LS

Role:

```text
CONTINUITY_EVALUATOR
```

Consumes the three base records plus `causal_audit_record` and produces:

```text
continuity_snapshot_record
```

Canonical postures:

```text
CONTINUE_SIDE_EFFECT
RETRY_SIDE_EFFECT
REPORT_ONLY
REMEDIATE_RESPONSE
REVALIDATE
BLOCKED
ALREADY_CONSUMED
NOT_EVALUATED
```

Continuity evaluates what may proceed after pause, retry, restart, or context
drift. It does not convert stored history into new permission.

### LiminalDB

Role:

```text
DURABLE_LEDGER
```

Consumes all five preceding records and produces:

```text
durability_receipt
```

Canonical durability values:

```text
LOCAL_REPLAY_VERIFIED
LOCAL_REPLAY_FAILED
NOT_EVALUATED
```

`LOCAL_REPLAY_VERIFIED` means the checked-in LiminalDB profile successfully
reconstructed its local projection through verified WAL/snapshot replay. It is
not external anti-rollback attestation and does not prove upstream semantics.

## Required joins

### Response integrity to causal audit

```text
transition_id
subject_id
authorization_ref
observation_refs
response_integrity_ref
```

### Causal audit to continuity snapshot

```text
transition_id
subject_id
action_identity_digest
binding_digest
authorization_ref
observation_refs
response_integrity_ref
causal_audit_ref
evidence_set_digest
```

### Continuity snapshot to durable ledger

```text
transition_id
subject_id
authorization_ref
observation_refs
response_integrity_ref
causal_audit_ref
continuity_snapshot_ref
```

Framework-local IDs may remain metadata, but cannot replace these portable
comparison fields.

## Six independent dimensions

A complete v0.2 receipt exposes:

```text
authority
execution
response_integrity
causal_validity
continuity_posture
durability
```

Examples:

```text
VALID + OBSERVED_EXECUTED + FAILED + VALID
  + REMEDIATE_RESPONSE + LOCAL_REPLAY_VERIFIED

VALID + OBSERVED_EXECUTED + VERIFIED + INVALID
  + BLOCKED + LOCAL_REPLAY_VERIFIED

VALID + OBSERVED_EXECUTED + VERIFIED + VALID
  + ALREADY_CONSUMED + LOCAL_REPLAY_FAILED
```

These are deliberately different failures:

- a false response does not imply broken causal lineage;
- broken causal lineage does not imply storage corruption;
- successful storage replay does not repair invalid authority or false claims;
- storage failure does not rewrite source verdicts.

## Derived full-lifecycle status

The verifier derives one reviewer-facing status while retaining all six source
dimensions.

Durability failure has reporting precedence because the local recovered view
cannot be trusted:

```text
DURABILITY_FAILED
```

Otherwise invalid causality yields:

```text
CAUSAL_INVALID
```

Valid causality maps continuity posture into:

```text
EXECUTABLE_CONTINUATION
HISTORICAL_REPORT_ONLY
RESPONSE_REMEDIATION
REVALIDATION_REQUIRED
BLOCKED
ALREADY_CONSUMED
CONTINUITY_NOT_EVALUATED
```

This derived status is an index. It never replaces the six independent values.

## Side-effect rule

A new resume side effect may be expected only when all of these are true:

```text
durability == LOCAL_REPLAY_VERIFIED
causal_validity == VALID
authority == VALID
continuity_posture == CONTINUE_SIDE_EFFECT
execution == NOT_OBSERVED
```

or for an explicitly retryable path:

```text
continuity_posture == RETRY_SIDE_EFFECT
execution == OBSERVED_ERRORED
```

A previously executed transition is never converted into a new side effect by
replay. Historical execution cases therefore expect zero resume side effects.

## Required conformance cases

The fixture proves:

1. fully supported historical execution restores as already consumed;
2. valid causal lineage plus false response routes to response remediation;
3. authority expired at report time remains report-only;
4. consumed replay remains non-executable;
5. missing authority becomes causally invalid and continuity-blocked;
6. denied/no-execution/false-response records may be causally coherent while
   still blocked;
7. a partial response requires revalidation;
8. cross-transition causal substitution remains invalid even when durably
   persisted;
9. semantic event-chain failure remains an independent durability failure.

## Machine-readable receipt

The verifier emits:

```text
profile
base_profile
fixture_digest
base_fixture_digest
immutable implementation pins
case count
status counts
durability counts
per-case six-dimensional results
verdict
claim boundary
```

No current timestamp is included, so identical inputs produce identical receipt
bytes.

## Boundary

This pack proves checked-in composition metadata, mappings, joins, and lifecycle
invariants. It does not:

- fetch mutable remote branches;
- verify current PR state;
- attest signer identity;
- replace repository-local tests and CI;
- prove policy correctness;
- prove observation-source trust;
- provide distributed consensus;
- provide external rollback resistance;
- replace CodeRabbit or mandatory Codex review.

## Merge order

1. merge and validate PR #110;
2. retarget this stacked PR to `main`;
3. rerun all three conformance verifiers;
4. complete all repository review gates;
5. only then merge v0.2.

## Canonical invariant

> Later lifecycle layers may restrict continuation or report storage failure.
> They may never rewrite the authority, execution, response-integrity, or causal
> facts they received.
