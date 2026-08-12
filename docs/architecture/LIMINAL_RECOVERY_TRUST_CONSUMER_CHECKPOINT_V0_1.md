# Liminal Recovery Trust Consumer Checkpoint v0.1

## Purpose

The trust-root registry can reject a downgrade when a verifier compares a candidate registry with the previously trusted registry. A downstream consumer still needs durable memory of the newest registry it has accepted. Without that memory, an attacker can replay an older registry that remains internally valid and historically attested.

The consumer checkpoint is that monotonic memory boundary.

```text
accepted registry gen0
        |
        v
consumer checkpoint gen0
        |
        | verified authorized registry gen1
        v
consumer checkpoint gen1
        |
        | replay old registry gen0
        v
REJECT: stale_registry
```

## Checkpoint contents

The canonical checkpoint records only compact trust state:

- repository identity;
- accepted registry generation;
- accepted canonical registry SHA-256;
- accepted active-manifest SHA-256;
- previous checkpoint SHA-256 for generation > 0;
- the pinned registry-attestor workflow identity associated with the accepted state.

It does not contain credentials, provider prompts, model output, or private signing material.

## Update rules

A candidate registry is accepted only when:

1. upstream trust evidence has already been verified;
2. the registry is structurally valid;
3. its generation is either the exact already-accepted generation or exactly one generation newer;
4. for a new generation, the candidate history prefix reconstructs the exact registry digest stored in the checkpoint;
5. the candidate manifest prefix matches the checkpoint's accepted manifest digest;
6. the registry rotation policy authorizes the one-generation transition.

A successful transition produces a new checkpoint whose `previous_checkpoint_sha256` is the digest of the prior checkpoint.

Stable rejection reasons include:

- `candidate_trust_evidence_unverified`;
- `registry_attestor_mismatch`;
- `stale_registry`;
- `same_generation_registry_conflict`;
- `registry_generation_gap`;
- `checkpoint_registry_prefix_mismatch`;
- `checkpoint_manifest_prefix_mismatch`;
- registry-rotation rejection reasons such as `verifier_root_downgrade`.

## Trust boundary

This primitive deliberately does **not** implement Sigstore or GitHub certificate verification. Cryptographic provenance stays in the existing GitHub/Sigstore verification layer. The checkpoint consumes a boolean representing the result of that upstream trust-evidence verification and then enforces monotonic state.

The v0.1 drill uses deterministic registry-rotation authorization as its candidate trust-evidence basis and records that a fresh cryptographic attestation of the simulated generation-1 registry was **not** exercised. The drill result itself is GitHub OIDC/Sigstore attested by an immutable checkpoint-drill workflow. Production adoption should wire the checkpoint update to a cryptographically verified candidate registry or a cryptographically verified rotation authorization that binds the exact candidate registry digest.

## Non-destructive drill

The drill creates an in-memory/artifact-only generation 1, advances an artifact-only consumer checkpoint, then presents the permanent genesis registry again and requires `REJECT: stale_registry`.

Neither the permanent trust-root registry nor the permanent genesis consumer checkpoint is modified by the drill.

## Claim boundary

The checkpoint proves monotonic consumer memory only when the consumer actually preserves the newest accepted checkpoint. A verifier that discards its checkpoint state can still be rolled back to an older trust view. External durable storage, replication, or anchoring of the checkpoint is a separate concern.
