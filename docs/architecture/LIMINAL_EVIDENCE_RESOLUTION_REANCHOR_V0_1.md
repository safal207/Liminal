# Liminal Evidence Resolution / Re-Anchor v0.1

## Purpose

Evidence used by a trust workflow has two different identities:

- **logical evidence identity** — what evidence record the workflow intends to consume;
- **physical artifact location** — where the record currently appears after transport, upload, download, packaging, or recovery.

Those identities must not be conflated. A valid trust record can move inside an artifact without changing its logical meaning, while a file found at a plausible path must not become trusted merely because the resolver found it.

This primitive formalizes the narrow transition:

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
    | verification required and available
    v
VerifiedRecovery
    |
    v
existing signer / hash / policy verification
```

## Objects

### `ResolutionNode`

Defines:

- stable `logical_id`;
- expected physical path;
- explicit allowed path prefixes for recovery;
- whether verification is required before the locator may be returned.

### `EvidenceLocator`

Represents one observed physical path for a stable logical evidence identity. `verification_available` means the caller has a verification path available; it does **not** mean this resolver performs or replaces cryptographic verification.

### `ReAnchor`

Records a deterministic relocation from the expected path to one admissible candidate. A re-anchor is emitted only when the expected locator is absent and exactly one candidate with the same logical identity exists inside an explicitly allowed prefix.

### `VerifiedRecovery`

Returns either:

- `RESOLVED` — the locator may proceed to the normal verification layer; or
- `DEFER` — fail closed because the resolver cannot safely choose a locator.

## Deterministic reason codes

- `expected_locator_resolved`
- `evidence_not_found`
- `ambiguous_evidence_candidates`
- `verified_evidence_required`
- `verified_reanchor`
- `unverified_reanchor`

The final code is used only when the caller explicitly does not require verification. Trust-sensitive workflows should normally set `require_verified=True`.

## Safety properties

1. Logical identity is matched before physical location is considered.
2. Recovery paths are bounded by explicit prefixes.
3. Multiple plausible candidates never get ranked implicitly; ambiguity returns `DEFER`.
4. Required verification overrides recovery convenience.
5. The resolver never mutates trust policy, registry state, signer identity, hashes, or evidence bytes.
6. `RESOLVED` is not action authority. Existing verification remains authoritative.

## Relationship to Recovery Router / Focus Field

The Recovery Router decides the **geometry of recovery**: sequential replay, Focus–Field, or defer.

Evidence Resolution operates one layer lower after a bounded recovery field exists. It answers:

> Which physical locator, if any, can represent this already-defined logical evidence identity without guessing?

This keeps Focus–Field recovery from silently turning candidate discovery into a trust decision.

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

v0.1 is deliberately small and provider-agnostic. It does not discover files, query GitHub Actions, inspect artifact archives, or perform cryptographic verification. Those adapters can supply `EvidenceLocator` candidates later without changing the fail-closed resolution semantics.
