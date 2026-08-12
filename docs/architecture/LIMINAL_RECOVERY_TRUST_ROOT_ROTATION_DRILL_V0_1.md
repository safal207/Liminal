# Recovery Trust Root Rotation Drill v0.1

Status: experimental, non-normative.

## Purpose

Exercise trust-root rotation and rollback rejection without changing the active production-style registry.

The drill starts from the attested generation-0 registry and constructs ephemeral successor states only inside the workflow artifact.

## Controlled rotation

The v0.1 drill rotates the verifier signer identity from:

`0aa3dce24f9aeb0c90f955fa5f68d12685e5654a`

into:

`ca69a7d342a4e00dfc2d11b13016a8eb7733da6b`

The verifier workflow Git blob is required to remain identical across both commits. This makes the exercise a signer-identity rotation without a verifier-workflow semantic change.

The candidate commit must already have ordinary exact-head CI evidence before it is used by the drill.

## Positive case

The runner constructs an ephemeral generation 1 manifest that:

- points to the exact generation-0 manifest digest;
- changes only the verifier workflow commit identity;
- verifies the referenced historical Git blob and pinned policy material;
- appends exactly one registry generation.

The expected machine verdict is:

`AUTHORIZED: registry_rotation_authorized`

## Negative case

The runner then constructs an ephemeral generation 2 manifest that is structurally valid and correctly chained to generation 1, but attempts to restore the superseded generation-0 verifier root.

The registry remains structurally valid so the negative result is specifically an anti-downgrade policy decision rather than malformed input.

The expected machine verdict is:

`REJECT: verifier_root_downgrade`

## Evidence

The immutable drill workflow:

1. checks out its own `job.workflow_sha` with full Git history;
2. runs the deterministic drill with no model/provider calls;
3. emits canonical generation-1 and downgrade-candidate artifacts;
4. asserts both machine-readable decisions;
5. attests `rotation-drill-result.json` using GitHub OIDC/Sigstore;
6. uploads the ephemeral evidence bundle.

An external consumer should verify the result attestation against the exact drill workflow signer digest.

## Boundary

This drill does **not** advance `policies/recovery-trust-root-registry-v0.1.json`. It proves that the rotation policy can accept a controlled forward rotation and reject a later rollback while preserving the current active genesis registry.

Anti-downgrade across real accepted states still requires the consumer to retain its previously trusted registry/attestor anchor. A consumer that deliberately forgets all trusted history can still be presented with an older validly signed state.
