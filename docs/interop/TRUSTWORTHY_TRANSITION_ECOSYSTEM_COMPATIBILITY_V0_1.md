# Trustworthy Transition Ecosystem Compatibility Pack v0.1

**Status:** Draft interoperability pack  
**Canonical profile:** `org.liminal.trustworthy-transition.three-record.v0.1`  
**Tracking issue:** [Liminal #108](https://github.com/safal207/Liminal/issues/108)

## Purpose

The canonical three-record profile defines the portable contract. This pack
checks whether concrete ecosystem implementations can be normalized into that
contract without collapsing their local semantics.

The first pinned implementation set is:

| Boundary | Implementation | PR |
|---|---|---|
| Pre-execution authority | ProofPath | [#167](https://github.com/safal207/ProofPath/pull/167) |
| Pre-execution authority | PythiaLabs | [#205](https://github.com/safal207/pythiaLabs/pull/205) |
| Manifest-bound observation/evidence | ibex-agent-verification | [#58](https://github.com/safal207/ibex-agent-verification/pull/58) |
| Joined response-claim verification | LTP | [#481](https://github.com/safal207/L-THREAD-Liminal-Thread-Secure-Protocol-LTP-/pull/481) |

Each registry entry pins:

- repository;
- pull request;
- exact head commit SHA;
- implementation artifact path;
- local profile identifier;
- owned lifecycle roles;
- local-to-canonical vocabulary mappings.

The pack is intentionally offline and deterministic. It does not fetch mutable
remote branches during CI.

## Files

- Fixture: [`fixtures/trustworthy-transition-ecosystem-compatibility-v0.1.json`](../../fixtures/trustworthy-transition-ecosystem-compatibility-v0.1.json)
- Verifier: [`tools/verify_trustworthy_transition_ecosystem.py`](../../tools/verify_trustworthy_transition_ecosystem.py)

Run:

```bash
python3 tools/verify_trustworthy_transition_vector.py
python3 tools/verify_trustworthy_transition_ecosystem.py
```

Generate a machine-readable receipt:

```bash
python3 tools/verify_trustworthy_transition_ecosystem.py \
  --output artifacts/trustworthy-transition-ecosystem-receipt.json
```

## What is normalized

### Authority decisions

Provider-local decisions are mapped into:

```text
ALLOW
DENY
DEFER
```

The authority dimension remains more descriptive than the decision itself:

```text
VALID
DENIED
PENDING
EXPIRED
EXPIRED_AT_REPORT
CONSUMED
REVALIDATION_REQUIRED
NOT_EVALUATED
```

This preserves distinctions such as consumed authority and honest reporting
after authority expires.

### Execution observations

Runtime-local execution statuses are mapped into:

```text
OBSERVED_EXECUTED
OBSERVED_BLOCKED
OBSERVED_ERRORED
NOT_OBSERVED
```

### Response integrity

Claim verifiers map their overall result into:

```text
VERIFIED
FAILED
PARTIAL
NOT_EVALUATED
```

The LTP adapter also exposes the evidence level:

```text
TEXT_HEURISTIC
OBSERVATION_JOINED
FULL_LIFECYCLE_JOINED
```

## Required joins

Authorization to observation:

```text
transition_id
subject_id
authorization_ref
action_identity_digest
binding_digest
```

Observation to response integrity:

```text
transition_id
subject_id
authorization_ref
observation_refs
```

Framework-local identifiers may be retained as metadata, but they cannot replace
or redefine these portable keys.

## Compatibility cases

The pack currently proves these independent combinations:

1. ProofPath authority + ibex observation + LTP supported response;
2. PythiaLabs block + no execution + false execution claim;
3. ProofPath authority expired at report time + historical execution + honest response;
4. PythiaLabs valid authority + observed execution + false response;
5. ProofPath consumed replay authority + no execution + honest response;
6. ibex observation + LTP count-drift failure without imported authority;
7. PythiaLabs valid authority + observed execution + mixed partial response.

These cases deliberately distinguish:

```text
VALID + OBSERVED_EXECUTED + FAILED
EXPIRED_AT_REPORT + OBSERVED_EXECUTED + VERIFIED
CONSUMED + NOT_OBSERVED + VERIFIED
NOT_EVALUATED + OBSERVED_EXECUTED + FAILED
```

## Side-effect rule

`expected_additional_side_effects` is derived independently:

- `1` only for `VALID + OBSERVED_EXECUTED`;
- `0` for denied, deferred, expired, consumed, revalidation-required, or
  authority-not-evaluated cases.

`EXPIRED_AT_REPORT + OBSERVED_EXECUTED` may describe a legitimate historical
execution, but it still permits zero new side effects at report time.

## CI behavior

The workflow runs both layers:

1. canonical semantic fixture and independent generator verification;
2. pinned ecosystem registry, mapping, role, join, and dimension verification.

A repository PR changing its head SHA or portable vocabulary requires an
explicit registry update. This makes compatibility drift visible instead of
silently following a mutable branch.

## Boundary

This pack proves the consistency of checked-in metadata and mappings. It does
not:

- fetch or attest the current remote PR state;
- replace each repository's unit tests, CI, CodeRabbit, or mandatory Codex review;
- prove policy correctness or observation-source trust;
- allow one component's success to repair another dimension's failure.

The repository-specific PRs remain draft until their own review gates pass.
