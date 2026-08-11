# Liminal Trusted Recovery Proof Builder v0.1

Status: experimental.

## Goal

Bind a recovery proof to an immutable builder implementation, not merely to a mutable workflow path or branch.

## Trusted builder v0.1

The reviewed builder version is pinned to:

`0a02df376a91ef870573e811370ff62ce2461111`

Its reusable workflow is:

`.github/workflows/trusted-recovery-proof-builder.yml`

The permanent trust wrapper is:

`.github/workflows/trusted-recovery-decision-proof.yml`

The wrapper invokes the builder using the exact commit SHA, never a branch or tag.

## Builder identity

GitHub exposes the workflow that defines the current reusable job through `job.workflow_repository`, `job.workflow_ref`, and `job.workflow_sha`.

The builder checks out:

- repository: `job.workflow_repository`;
- ref: `job.workflow_sha`.

It then verifies that `git rev-parse HEAD` equals `job.workflow_sha` before executing proof code. This prevents the reusable builder from silently executing mutable caller source.

## Fixed execution surface

The builder intentionally exposes no model or token-budget inputs. v0.1 fixes:

- provider path: Gonka broker configuration from the protected `live-provider-trace` environment;
- model: `MiniMaxAI/MiniMax-M2.7`;
- maximum output: `1536` tokens per recovery attempt;
- direct Python dependency versions used by the live proof;
- GitHub Actions by immutable action commit SHA.

The recovery proof itself remains deterministic-verification-first: raw provider output does not become trusted evidence until the verifier accepts it.

## Outer provenance and authorization

The builder emits the canonical `recovery-proof-bundle.zip` and creates a GitHub Artifact Attestation for that exact bundle using GitHub OIDC / Sigstore.

The trust wrapper independently runs `gh attestation verify` and requires:

1. repository `safal207/Liminal`;
2. signer workflow `.github/workflows/trusted-recovery-proof-builder.yml`;
3. signer digest `0a02df376a91ef870573e811370ff62ce2461111`;
4. expected source ref;
5. GitHub-hosted runner.

After GitHub's cryptographic verification succeeds, Liminal's claim policy separately requires:

1. immutable repository ID `1005410203`;
2. the same signer workflow;
3. signer ref `0a02df376a91ef870573e811370ff62ce2461111`;
4. the same signer digest;
5. deployment environment `live-provider-trace`;
6. GitHub-hosted runner;
7. GitHub Actions OIDC issuer.

The signer digest is the trust anchor for the builder version. Updating it is an explicit trust rotation and must not follow a branch automatically.

## Trust boundary

This design proves that the attested proof bundle was produced by the code and workflow stored at the pinned builder commit, subject to the GitHub-hosted runner, GitHub Actions, OIDC/Sigstore, package indexes, Gonka provider, and configured environment secrets.

It does **not** claim full hermetic or SLSA-level reproducibility. Direct Python package versions are fixed, but transitive package resolution and external provider behavior remain outside the builder's complete control. A stronger future version can use a hash-locked dependency set and additional isolation.

The verification wrapper is an operator convenience, not a replacement for the external trust anchor: an independent consumer can verify the same artifact with GitHub CLI using the pinned signer workflow and signer digest.

## Rotation

A new builder version should be introduced as a new reviewed commit. Only after its ordinary CI and a live proof pass should the trusted wrapper update its pinned signer digest. Historical trusted builder SHAs remain independently identifiable.
