# Liminal GitHub Actions Evidence Locator Adapter v0.1

## Purpose

GitHub Actions artifact transport can preserve evidence bytes while changing the
physical path at which a downstream job observes them. Hard-coding that path in
consumer YAML turns packaging topology into an accidental runtime dependency.

This adapter converts a bounded downloaded artifact topology into
`EvidenceLocator[]` candidates for the existing Evidence Resolution / Re-Anchor
primitive.

```text
downloaded artifact topology
        |
        v
GitHubActionsEvidenceSpec
        |
        v
bounded filename + prefix discovery
        |
        v
EvidenceLocator[]
        |
        v
resolve_evidence()
        |
        +-- ambiguity / no verification path --> DEFER
        |
        v
external signer/hash/policy verification
        |
        v
confirm_verified_recovery()
```

## Boundary

The adapter performs **discovery only**. It does not:

- attest evidence;
- verify a GitHub signer;
- compare hashes;
- mutate trust policy or registry state;
- choose among multiple plausible evidence candidates;
- grant action authority.

The adapter therefore cannot turn a path match into trusted evidence.

## `GitHubActionsEvidenceSpec`

The caller supplies:

- stable logical evidence ID;
- expected artifact-relative path;
- exact evidence basename;
- explicit allowed recovery prefixes;
- whether a downstream verification path exists.

The adapter never learns arbitrary trust roots from the downloaded artifact.

## Discovery rules

1. Paths are normalized as relative POSIX artifact paths.
2. Absolute paths and parent traversal (`..`) are rejected.
3. Basename must match the declared evidence filename exactly.
4. A path is admitted only if it is the exact expected path or lies inside an
   explicit allowed prefix.
5. Duplicate identical paths collapse deterministically.
6. Multiple distinct admissible paths are preserved so the resolver can fail
   closed with `ambiguous_evidence_candidates`.

## Empirical checkpoint witness case

The witness originally expected:

```text
checkpoint-evidence/checkpoint-generation-1.json
```

but GitHub Actions exposed the valid evidence at:

```text
checkpoint-evidence/artifacts/trust-consumer-checkpoint-attested/
checkpoint-generation-1.json
```

With this adapter the downloaded topology can produce exactly one bounded
candidate for logical evidence:

```text
trust-consumer-checkpoint:generation-1
```

The resolver can then emit `reanchor_resolved`, after which the existing
`gh attestation verify` step remains responsible for cryptographic trust.

## Why this belongs below Focus Field

Focus Field chooses a bounded recovery region. The GitHub Actions adapter is a
provider-specific observation layer inside that region. It answers only:

> Which admissible physical artifact locations currently exist for this already
> defined logical evidence identity?

This preserves the architecture:

```text
Focus Field
   -> bounded topology observation
   -> EvidenceLocator[]
   -> deterministic ReAnchor
   -> verification
   -> VerifiedRecovery
```

## Current scope

v0.1 is a pure Python adapter over a supplied iterable of artifact-relative
paths. A later workflow integration can obtain that iterable from `find`, an
artifact manifest, or another deterministic listing mechanism without changing
resolution or trust semantics.
