# Liminal Trusted Recovery Verifier Root v0.1

Status: experimental, non-normative.

## Purpose

The recovery proof builder is already pinned to an immutable workflow commit and
its proof bundle is signed through GitHub OIDC/Sigstore. The remaining trust gap
is the verifier itself: if authorization rules are read from a mutable branch,
that branch can silently change the rules used to declare a proof trusted.

Trusted Recovery Verifier Root v0.1 separates proof production from trust
policy evaluation.

## Trust chain

```text
live recovery evidence
        |
        v
immutable trusted builder
        |
        v
GitHub/Sigstore-attested proof bundle
        |
        v
immutable verifier workflow
        |
        +--> verify builder attestation identity
        |
        +--> authorize embedded builder environment
        |
        v
cross-linked Recovery Trust Authorization Receipt
        |
        v
GitHub/Sigstore attestation signed by verifier workflow identity
```

## Recovery Trust Authorization Receipt

The verifier emits canonical JSON containing:

- SHA-256 of the verified recovery proof bundle;
- SHA-256 of the builder identity authorization result;
- SHA-256 of the builder environment authorization result;
- trusted builder repository, workflow path, and immutable workflow SHA;
- source ref carried by the verified builder attestation;
- verifier repository, workflow path, and immutable workflow SHA.

Before the receipt can be emitted, the verifier requires both authorization
results to be explicitly successful and cross-links the builder identity to the
builder environment receipt:

- `identity.signer_digest == environment.builder.workflow_sha`;
- the verified signer URI must equal the repository/workflow/SHA named by the
  environment receipt;
- the proof bundle must pass canonical bundle membership and manifest-hash
  verification.

## Immutable verifier identity

The reusable verifier workflow checks out its own `job.workflow_sha` and verifies
that the checkout HEAD equals that value before running policy code. Once a
candidate verifier commit passes ordinary CI, callers can pin the reusable
workflow to that exact commit SHA.

The verifier then signs `recovery-trust-authorization.json` with GitHub Artifact
Attestations. External consumers can require both the verifier workflow path and
the immutable verifier signer digest.

## Separation of concerns

The layers intentionally prove different things:

1. The inner decision receipt proves deterministic replay relative to recorded
   evidence and policy inputs.
2. The builder attestation proves which workflow produced the proof-bundle
   bytes.
3. Builder identity/environment policies decide whether that production context
   is authorized.
4. The verifier attestation proves which immutable verifier/policy root issued
   the final authorization receipt.

A valid builder attestation is not sufficient authorization. A valid verifier
attestation is also not evidence that the underlying model output is true; it
only proves provenance for a policy verdict over the supplied proof evidence.

## Deliberate limits

v0.1 does not prove:

- correctness of external telemetry sources;
- truth of model output;
- authorization of downstream tool execution;
- non-equivocation outside the transparency properties supplied by GitHub and
  Sigstore;
- hermeticity of GitHub-hosted runner infrastructure.

The verifier root makes policy changes explicit trust rotations instead of
silent mutable-branch changes.
