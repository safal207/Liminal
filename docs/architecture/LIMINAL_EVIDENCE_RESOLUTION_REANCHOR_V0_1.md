# Liminal Evidence Resolution / Re-Anchor v0.1

## Purpose

Evidence used by a trust workflow has two different identities:

- **logical evidence identity** — what evidence record the workflow intends to consume;
- **physical artifact location** — where the record currently appears after transport, upload, download, packaging, or recovery.

Those identities must not be conflated. A valid trust record can move inside an artifact without changing its logical meaning, while a file found at a plausible path must not become trusted merely because the resolver found it.

The primitive therefore separates locator resolution from trust verification:

```text
ResolutionNode
    |
    v
expected EvidenceLocator
    |
    | missing
    v
bounded admissible locator field
    |
    | exactly one candidate
    v
ReAnchor
    |
    v
ResolutionOutcome
    |
    | existing signer / hash / policy verification
    v
VerifiedRecovery
```

## Objects

### `ResolutionNode`

Defines:

- stable `logical_id`;
- expected physical path;
- explicit allowed path prefixes for recovery;
- whether a verification path must exist before resolution can continue.

### `EvidenceLocator`

Represents one observed physical path for a stable logical evidence identity. `verification_available` means a verification path exists for the caller; it does **not** mean the evidence has already been verified.

### `ReAnchor`

Records a deterministic relocation from the expected path to one admissible candidate. A re-anchor is emitted only when the expected locator is absent and exactly one candidate with the same logical identity exists inside an explicitly allowed prefix.

### `ResolutionOutcome`

Represents locator resolution only. `RESOLVED` means the selected physical path may be handed to the existing verification layer. It is not trust authority.

### `VerifiedRecovery`

Is produced only after `confirm_verified_recovery(...)` receives the result of the existing external verification step. A required verification that is absent or false fails closed.

## Deterministic reason codes

- `expected_locator_resolved`
- `evidence_not_found`
- `ambiguous_evidence_candidates`
- `verification_path_required`
- `reanchor_resolved`
- `verification_failed`
- `verified_recovery`

## Safety properties

1. Logical identity is matched before physical location is considered.
2. Recovery paths are bounded by explicit prefixes.
3. Multiple plausible candidates never get ranked implicitly; ambiguity returns `DEFER`.
4. A required verification path overrides recovery convenience.
5. Locator resolution never claims cryptographic verification succeeded.
6. The resolver never mutates trust policy, registry state, signer identity, hashes, or evidence bytes.
7. `RESOLVED` is not action authority; only the existing verification result can produce `VerifiedRecovery`.

## Relationship to Recovery Router / Focus Field

The Recovery Router decides the **geometry of recovery**: sequential replay, Focus–Field, or defer.

Evidence Resolution operates one layer lower after a bounded recovery field exists. It answers:

> Which physical locator, if any, can represent this already-defined logical evidence identity without guessing?

The verification layer then answers a separate question:

> Does the resolved evidence satisfy the signer, hash, policy, and trust requirements?

This separation keeps Focus–Field recovery from silently turning candidate discovery into a trust decision.

## Empirical incident that motivated v0.1

During the immutable checkpoint witness proof, the logical generation-1 checkpoint was valid and attested, but its downloaded physical location was nested under:

```text
checkpoint-evidence/artifacts/trust-consumer-checkpoint-attested/
checkpoint-generation-1.json
```

while the witness expected:

```text
checkpoint-evidence/checkpoint-generation-1.json
```

The first failure was therefore a **ResolutionFailure**, not a trust failure. After a bounded re-anchor, the immutable witness passed. The external witness verifier then exposed the same geometry one level downstream: witness evidence was nested under `artifacts/trust-consumer-checkpoint-witness/`.

The observed recovery sequence was:

```text
ResolutionFailure
    -> observe actual evidence topology
    -> bounded ReAnchor
    -> exact-head verification
    -> immutable witness anchor
    -> independent signer verification
    -> VerifiedRecovery
```

This incident demonstrates why logical evidence identity must remain stable while physical locator resolution is explicit, bounded, and independently verifiable.

## Current scope

v0.1 is deliberately small and provider-agnostic. It does not discover files, query GitHub Actions, inspect artifact archives, or perform cryptographic verification. Adapters can supply `EvidenceLocator` candidates and external verification results later without changing the fail-closed resolution semantics.
