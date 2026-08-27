# Liminal Genesis / Historical Trust-Base Portability v0.1

## Status

Implementation gate. This document defines the fail-closed semantic comparator used before any claim of independent genesis/history is marked VERIFIED.

## Problem

Upstream Rotation-Authority Portability v0.1 moved rotation production and control across independent providers, but both paths intentionally inherited the same predecessor registry/manifest. That leaves one ambient shared trust assumption: historical origin.

Genesis / Historical Trust-Base Portability removes that assumption. Two paths must validate from distinct genesis identities and distinct history bytes, then converge only at the semantic trust-state layer.

## Required invariants

1. `distinct_genesis_identity` — genesis manifest digests differ.
2. `independent_genesis_authority` — genesis authority identities differ.
3. `independent_historical_chain` — no manifest digest is shared across histories.
4. `no_cross_root_dependency` — Path B carries no Path A manifest/registry identity, including nested aliases or unused evidence.
5. `equivalent_terminal_trust_state` — both independently valid histories normalize to the same `trust_state_digest`.

The comparator is intentionally ordered:

```text
validate history A
validate history B
        ↓
reject shared genesis / shared history / cross-root ancestry
        ↓
normalize terminal trust state A and B
        ↓
compare trust_state_digest
```

No semantic-equivalence verdict is allowed before both histories validate.

## Semantic trust state

`liminal-semantic-trust-state/v0.1` retains authorization semantics:

- trust domain;
- terminal authority IDs and threshold;
- rotation and authorization contract digests;
- active builder/verifier workflow identities and Git blob identities;
- active policy-material digests;
- authorization scope.

It deliberately excludes historical provenance:

- generation number;
- `previous_manifest_sha256`;
- registry manifest paths;
- policy `source_sha` commits.

This allows two distinct histories to converge on one currently authorized state without requiring identical history bytes.

## Receipt

A successful `liminal-historical-trust-base-portability-receipt/v0.1` records:

- both genesis manifest digests;
- both registry digests;
- both genesis authority IDs;
- both history tips;
- both semantic trust-state digests;
- `histories_independently_valid=true`;
- `cross_root_dependency=false`;
- `equivalent_terminal_state=true`.

## Fail-closed cases

The implementation rejects:

- copied/shared genesis bytes;
- shared manifest identity anywhere in history;
- a shared genesis authority;
- hidden Path A identity referenced by Path B;
- an invalid/corrupt history;
- failed upstream cryptographic verification;
- terminal authority drift;
- threshold drift;
- rotation-contract drift;
- authorization-contract drift;
- any other terminal semantic divergence.

A cryptographically valid but semantically different history must return:

```text
verified=false
reason=terminal_trust_state_mismatch
```

## Claim boundary

This layer does not itself mint or verify Ed25519/GitHub OIDC signatures. `HistoricalTrustPath.verified` is an input from the cryptographic/provider-specific verifier, matching the existing architecture where provider verification and portable semantic comparison are separate layers.

Therefore this implementation alone is not Signal 011. A VERIFIED claim additionally requires a material Root B, independently signed genesis/history evidence, an immutable proof workflow, and an independent audit that reconstructs both histories and reproduces the receipt bytes.
