# Trustworthy Transition Durability Assurance Compatibility v0.3

**Status:** Draft stacked interoperability profile  
**Tracking issue:** [Liminal #114](https://github.com/safal207/Liminal/issues/114)  
**Base lifecycle profile:** [Liminal PR #113](https://github.com/safal207/Liminal/pull/113)

## Purpose

The v0.2 lifecycle profile keeps six independent dimensions:

```text
authority
execution
response_integrity
causal_validity
continuity_posture
durability
```

This v0.3 layer adds two assurance dimensions without changing any of those six:

```text
checkpoint_assurance
process_crash_evidence
```

The key separation is:

```text
LOCAL_REPLAY_VERIFIED
        ≠
LOCAL_SIGNATURE_ONLY
        ≠
EXTERNAL_ANCHOR_VERIFIED
        ≠
PROCESS_CRASH_VERIFIED
        ≠
POWER_LOSS_VERIFIED
```

The final value is intentionally never produced by this profile.

## Pinned implementations

### Base lifecycle

- repository: `safal207/Liminal`
- PR: `#113`
- head: `b50ef4d90a76fbd5b5d2e835f85e348081f4110f`
- artifact: `tools/verify_trustworthy_transition_lifecycle.py`

### Signed checkpoints and external anchors

- repository: `safal207/LiminalDB`
- PR: `#92`
- head: `2cac499c3b73c19d566ac763bf32816f4a4bcadc`
- artifact: `liminal-db/crates/liminal-store/src/checkpoint.rs`

### Cross-platform process-crash evidence

- repository: `safal207/LiminalDB`
- PR: `#93`
- head: `0cd06391837588817ad5ac55d19f47794ba157e6`
- artifact: `tools/run_transition_durability_matrix.py`

Pinned evidence receipts:

```text
ubuntu  sha256:82ef417497a969b75f3ad3c1fc49e2935daf0bbfdc8a2885be6cc2d00593a13e
macos   sha256:9bc362e85307eae6b461c7ba03cb3a7ba86f60da4f2b995a2f7af4f48a9fac12
windows sha256:faf308dc19fd26390da1f66d3171c58c9df6be72809a8f390a058bd5eaabb589
```

## Checkpoint assurance

Canonical values:

```text
NOT_EVALUATED
LOCAL_SIGNATURE_ONLY
EXTERNAL_ANCHOR_VERIFIED
INVALID
```

### `LOCAL_SIGNATURE_ONLY`

A trusted key signed the exact checkpoint manifest bytes.

It does **not** establish resistance to rollback of all local files together,
because an attacker could replace the ledger, snapshot, checkpoint, and local
trust metadata with a mutually consistent older set.

### `EXTERNAL_ANCHOR_VERIFIED`

The supplied checkpoint chain descends from a caller-trusted external anchor.
This is the only v0.3 value that sets:

```text
external_anti_rollback = true
```

The profile does not itself trust an anchor provider. Provider trust remains a
caller responsibility.

### `INVALID`

The signature, ancestry, key state, expiry, fork relation, or external-anchor
relation failed. Stable errors include:

```text
SIGNATURE_VERIFICATION_FAILED
EXTERNAL_ANCHOR_ROLLBACK
EXTERNAL_ANCHOR_FORK
EXTERNAL_ANCHOR_NOT_IN_CHAIN
KEY_REVOKED
KEY_EXPIRED
CHECKPOINT_EXPIRED
PREVIOUS_CHECKPOINT_MISMATCH
NON_MONOTONIC_CHECKPOINT
```

An invalid checkpoint does not rewrite the original lifecycle verdict. It adds a
separate assurance failure.

## Process-crash evidence

Canonical values:

```text
NOT_EVALUATED
PARTIAL
VERIFIED
FAILED
```

Required platform set:

```text
ubuntu
macos
windows
```

Derivation:

- no receipts: `NOT_EVALUATED`;
- some required platforms passed and none failed: `PARTIAL`;
- every required platform passed: `VERIFIED`;
- any supplied platform failed: `FAILED`.

A failed Windows receipt cannot be reported as partial success merely because
Linux and macOS passed.

## Independent verdict preservation

For every v0.3 case, the following values must exactly equal the referenced v0.2
base case:

```text
authority
execution
response_integrity
causal_validity
continuity_posture
durability
full_lifecycle_status
```

Therefore:

- externally anchored storage cannot upgrade denied authority;
- three-platform crash evidence cannot repair a false response;
- invalid storage evidence cannot silently turn a valid historical transition
  into an authorization failure;
- unavailable assurance metadata cannot erase the original lifecycle result.

## Required fixture cases

The fixture covers:

1. external anchor plus all three platform receipts;
2. local signature without external anti-rollback;
3. durable replay with no checkpoint evaluation;
4. invalid signature with otherwise valid lifecycle;
5. explicit external-anchor rollback rejection;
6. partial platform coverage;
7. failed Windows evidence with Linux/macOS success;
8. strong storage assurance attached to denied authority;
9. strong storage assurance attached to a false response;
10. unavailable assurance metadata preserving all lifecycle verdicts.

## Verification

Run:

```bash
python3 tools/verify_trustworthy_transition_durability_assurance.py \
  --output artifacts/trustworthy-transition-durability-assurance-receipt.json
```

The verifier checks:

- exact implementation heads and artifact paths;
- canonical assurance vocabularies;
- exact platform artifact digests;
- checkpoint error codes;
- deterministic platform-status derivation;
- all required cases;
- full preservation of the six v0.2 dimensions;
- conservative claim boundaries.

## Claim boundary

This profile does not:

- issue authorization;
- observe execution;
- verify response truth;
- determine causal validity;
- select continuation posture;
- persist or replay records;
- sign checkpoints;
- establish anchor-provider trust;
- simulate sudden power loss;
- verify controller-cache loss;
- verify hostile or distributed filesystems.

It normalizes evidence already produced by the pinned implementations and
prevents stronger storage evidence from being misrepresented as stronger
behavioral truth.

## Merge order

```text
PR #110  ecosystem compatibility v0.1
    ↓
PR #113  full lifecycle compatibility v0.2
    ↓
this PR durability assurance compatibility v0.3
```

After the bases merge, retarget this PR to `main`, rerun all conformance checks,
and complete mandatory final review before merge.
