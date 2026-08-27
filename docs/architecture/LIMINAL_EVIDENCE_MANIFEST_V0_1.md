# Liminal Evidence Manifest v0.1

## Purpose

Evidence identity should survive packaging and path drift without turning a
physical filename into a trust root.

Evidence Manifest v0.1 binds:

- stable `logical_id`;
- producer identity;
- evidence type;
- producer-relative locator;
- SHA-256 content digest;
- generation;
- verification expectations.

The physical path observed by a downstream job is deliberately not part of the
stable trust identity.

```text
logical_id + generation
        |
        v
Evidence Manifest
        |
        +-- producer
        +-- evidence_type
        +-- relative_locator
        +-- sha256
        +-- verification expectations
        |
        v
bounded physical candidate field
        |
        v
digest match
        |
        +-- 0 matches -> DEFER
        +-- >1 paths  -> DEFER
        |
        v
EvidenceLocator
        |
        v
existing resolution / signer / hash / policy verification
```

## Why digest, not basename

The checkpoint witness incident showed that GitHub Actions can preserve valid
evidence bytes while changing the downloaded path.

A basename such as `checkpoint-generation-1.json` is useful for discovery, but
it is not sufficient for identity. The manifest therefore binds the logical
evidence generation to SHA-256. A downstream bounded topology adapter may find
physical candidates at different paths; the manifest accepts only the content
whose digest matches the declared evidence identity.

## Objects

### `EvidenceManifestEntry`

One stable evidence generation:

- `logical_id`
- `producer`
- `evidence_type`
- `relative_locator`
- `sha256`
- `generation`
- `verification`

### `VerificationExpectation`

Describes the verification contract expected after manifest resolution:

- whether verification is required;
- verifier class/name;
- optional expected signer;
- optional expected signer digest.

The manifest records expectations but does not execute or replace external
verification.

### `ManifestCandidate`

A physical candidate already obtained from a bounded topology observation:

- observed path;
- observed SHA-256;
- whether an external verification path is available.

The manifest layer intentionally does not perform unbounded filesystem search.

## Fail-closed rules

1. SHA-256 values must be lowercase 64-character hexadecimal strings.
2. Manifest locators and observed candidate paths must be relative.
3. Parent traversal (`..`) is rejected.
4. Required identity fields must be non-empty.
5. Generations must be non-negative.
6. Conflicting entries with the same `logical_id + generation` are rejected.
7. Distinct manifest documents that claim different entries for the same
   `logical_id + generation` resolve to `DEFER`.
8. No digest match resolves to `DEFER`.
9. More than one distinct physical path with the expected digest resolves to
   `DEFER` instead of selecting a winner.
10. Manifest resolution never grants tool/action authority and never bypasses
    signer, policy, registry, or attestation verification.

## Canonical serialization

`canonical_manifest_bytes()` sorts entries deterministically by logical ID,
generation, and digest and emits compact canonical JSON with a trailing newline.
This gives later layers a stable byte representation for manifest commitment or
attestation without changing the v0.1 resolution semantics.

## Relationship to Evidence Locator / Re-Anchor

The current stack becomes:

```text
Focus Field
   -> bounded topology observation
   -> physical candidates + observed digests
   -> Evidence Manifest
   -> logical identity / generation / digest binding
   -> EvidenceLocator
   -> ReAnchor / resolution
   -> signer + hash + policy verification
   -> VerifiedRecovery
```

The provider-specific GitHub Actions locator adapter may still reduce the
candidate field by safe topology constraints. Evidence Manifest adds a stronger
identity layer so final binding can rely on bytes rather than basename/path.

## Current scope

v0.1 is a pure Python primitive and does not modify the immutable witness
workflow, witness trust anchor, signer policy, registry semantics, or evidence
bytes. Workflow integration should occur only after exact-head CI proves the
manifest primitive independently.
