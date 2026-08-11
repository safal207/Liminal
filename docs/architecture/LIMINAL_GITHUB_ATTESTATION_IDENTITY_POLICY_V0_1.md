# Liminal GitHub Attestation Identity Policy v0.1

## Purpose

GitHub/Sigstore cryptographic validity is necessary but not sufficient for a trusted recovery proof. A valid attestation from another repository, workflow, ref, deployment environment, runner class, or OIDC issuer must not become authorized evidence merely because its signature verifies.

This policy therefore runs **after** successful `gh attestation verify` and adds exact, fail-closed authorization of the workflow identity that produced the proof bundle.

## Verification boundary

```text
proof bundle bytes
      |
      v
GitHub/Sigstore cryptographic verification
      |
      | success only
      v
normalized verified certificate claims
      |
      v
Liminal identity authorization policy
      |
      +-- repository URI + immutable repository ID
      +-- permanent signer workflow URI
      +-- exact source ref
      +-- deployment environment
      +-- GitHub-hosted runner
      +-- GitHub Actions OIDC issuer
      |
      v
authorized recovery provenance
```

The Liminal policy does **not** reimplement Sigstore cryptographic verification. It consumes JSON emitted by a successful `gh attestation verify --format json` call. This separation avoids treating a local claim parser as a substitute for Sigstore trust-root, signature, timestamp, transparency-log, subject-digest, and certificate-chain verification.

## Current pinned identity

For the experimental recovery proof path:

- repository: `safal207/Liminal`
- immutable GitHub repository ID: `1005410203`
- signer workflow: `.github/workflows/live-recovery-decision-proof.yml`
- deployment environment: `live-provider-trace`
- runner environment: `github-hosted`
- OIDC issuer: `https://token.actions.githubusercontent.com`
- source ref: supplied exactly from the workflow run (`github.ref`)

No wildcard matching or trust-on-first-use is used.

## Deployment environment claim

Fulcio's provider-generic certificate extension
`1.3.6.1.4.1.57264.1.23` carries the deployment environment. The policy decodes that DER UTF8String directly from the certificate embedded in the already-verified Sigstore bundle because the current GitHub CLI parsed certificate view does not expose the environment field.

Missing, malformed, empty, or mismatched environment data fails closed.

## Reusable workflow boundary

`live-recovery-decision-proof.yml` supports both manual dispatch and `workflow_call`. When invoked as a reusable workflow, it remains the **Build Signer URI** in the Fulcio certificate. This lets a temporary caller trigger a live experiment without becoming the trusted signer itself.

The initiating caller remains visible separately as build configuration provenance; it is not substituted for the signer workflow.

## Security properties

- cryptographic validity and authorization remain separate;
- repository name is paired with immutable repository ID;
- exact permanent workflow path is pinned;
- exact source ref is pinned;
- deployment environment is pinned;
- self-hosted runners are rejected in the current policy;
- non-GitHub Actions OIDC issuers are rejected;
- ambiguous multiple verified attestation results fail closed;
- missing certificate claims fail closed.

## Non-claims

This policy does not prove that model output is correct. Provider output still becomes evidence only after deterministic verification. It also does not grant tool or action authority, and it does not make the temporary inner Ed25519 key a long-term identity root.
