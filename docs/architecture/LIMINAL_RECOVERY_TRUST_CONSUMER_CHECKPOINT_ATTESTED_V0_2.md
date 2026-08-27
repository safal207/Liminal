# Liminal Recovery Trust Consumer Checkpoint — Attested v0.2

## Purpose

The trust-root registry can reject an explicit downgrade only when a verifier retains the previously accepted registry state. This layer makes that remembered state explicit as a canonical consumer checkpoint and requires cryptographically verified rotation authorization before advancing it.

The goal is to reject an old registry that is still structurally valid and may still have historically valid provenance.

## Trust chain

```text
accepted checkpoint gen0
        |
        v
candidate registry gen1
        |
        v
immutable rotation authorizer
        |
        v
GitHub OIDC / Sigstore attestation
        |
        v
exact rotation-result digest
        |
        +--> exact candidate registry digest
        +--> exact candidate manifest digest
        |
        v
consumer checkpoint policy
        |
        v
checkpoint gen1
        |
        v
GitHub OIDC / Sigstore attestation
```

## Separation of responsibilities

The genesis checkpoint records the provenance of the already accepted generation-0 registry using the immutable registry-attestor root.

Future advancement uses a separate `advance_authorizer`. In v0.2 that authorizer is the immutable trust-root rotation drill workflow. Its attested result binds the exact candidate registry and candidate manifest digests to a machine-readable `registry_rotation_authorized` decision.

This is intentionally not described as a direct registry attestation by the old registry-attestor root. The candidate registry is accepted through cryptographically verified rotation-authorization evidence.

## Monotonic rules

For a trusted checkpoint at generation `N`:

- candidate generation `< N` → `stale_registry`;
- candidate generation `N` with the exact accepted registry and manifest digests → idempotent `registry_already_accepted`;
- candidate generation `N` with different accepted bytes → `same_generation_registry_conflict`;
- candidate generation `> N + 1` → `registry_generation_gap`;
- candidate generation `N + 1` requires verified rotation-authorization evidence from the pinned authorizer;
- the evidence must bind the exact candidate registry and active manifest digests;
- the candidate history prefix must reproduce the registry digest and active manifest digest stored in the trusted checkpoint;
- the underlying registry rotation must independently pass the registry anti-downgrade policy.

## Checkpoint chain

A generation-1 checkpoint records:

- accepted registry digest;
- accepted active manifest digest;
- digest of the previous checkpoint;
- provenance of the evidence that authorized the accepted registry;
- the immutable workflow identity allowed to authorize the next advance.

The previous-checkpoint digest creates a monotonic consumer-side chain independent of the registry's own manifest history.

## Cryptographic gate

The trusted workflow first runs `gh attestation verify` against the rotation result with:

- repository `safal207/Liminal`;
- exact rotation-authorizer workflow path;
- exact immutable signer digest;
- expected source ref;
- self-hosted runners denied.

Only after that command succeeds is the rotation evidence passed into the checkpoint advancement logic.

The resulting generation-1 checkpoint and the consumer authorization result are separately attested with GitHub OIDC/Sigstore.

## Non-goals

This layer does not:

- make the registry globally non-rollbackable if a consumer deliberately erases all trusted checkpoint state;
- prove provider or model output truth;
- grant tool or financial authority;
- replace the trust-root registry's own rotation policy;
- make GitHub-hosted execution hermetic.

It makes the consumer's remembered trust state explicit, hash-linked, machine-verifiable, and externally attestable.
