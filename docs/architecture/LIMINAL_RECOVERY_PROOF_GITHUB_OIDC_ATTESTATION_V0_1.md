# Liminal Recovery Proof — GitHub OIDC Attestation v0.1

## Purpose

A recovery Decision Receipt already proves that a deterministic routing decision
matches explicit signals, policy, and a signed durable evidence-ledger head.
That inner Ed25519 signature protects integrity and replayability, but an
ephemeral CI key is not a durable identity root.

This layer gives the complete proof an external workflow identity.

## Two-layer proof

```text
provider recovery attempts
        |
        v
verified recovery evidence
        |
        v
SHA-256 chained ledger
        |
        v
Decision Receipt + deterministic replay
        |        inner Ed25519 integrity
        v
canonical recovery-proof-bundle.zip
        |        stable subject digest
        v
GitHub OIDC -> Sigstore certificate
        |
        v
GitHub Artifact Attestation
```

The inner proof answers: **does the recorded decision follow from the recorded
evidence and policy?**

The outer attestation answers: **which GitHub Actions workflow/repository/commit
produced this exact proof-bundle digest?**

Neither layer grants tool authority or proves that provider output is true.
Provider output still requires deterministic verification before it can become
recovery evidence.

## Canonical bundle

`src/liminal/recovery_proof_bundle.py` builds a deterministic ZIP containing:

- `decision-receipt.json`
- `public-key.json`
- `recovery-evidence.jsonl`
- `summary.json`
- `proof-manifest.json`

The manifest contains SHA-256 and byte length for every evidence member. The ZIP
uses sorted entries, fixed timestamps and permissions, and no compression so
identical evidence produces identical bundle bytes.

Raw prompts, raw provider responses, model reasoning, credentials, and private
keys are excluded.

## GitHub identity root

`.github/workflows/live-recovery-decision-proof.yml` requests only the permissions
needed to generate provenance:

- `contents: read`
- `id-token: write`
- `attestations: write`

After the live proof self-verifies, `actions/attest@v4` attests
`recovery-proof-bundle.zip`. GitHub OIDC supplies the workload identity used for
the short-lived signing certificate. The resulting attestation is associated
with the repository and can be independently verified against the bundle digest.

## Trust boundary

The ephemeral key stored in `public-key.json` remains explicitly scoped as
`ephemeral_ci_integrity_only_not_long_term_identity`. It is not promoted to a
trusted long-term signer merely because it appears inside the bundle.

The GitHub attestation is the outer provenance root for this workflow execution.
A verifier should still validate the expected repository/workflow/commit policy
rather than treating any valid GitHub attestation as authorized.

## Failure behavior

The workflow fails closed if:

1. Gonka configuration is unavailable;
2. live recovery execution fails;
3. Decision Receipt offline replay/signature verification fails;
4. a required proof member is missing;
5. canonical bundle self-verification fails;
6. GitHub cannot issue/persist the artifact attestation.

The evidence artifact upload remains `if: always()` so partial diagnostic
evidence can be retained without being mistaken for a successful attested proof.
