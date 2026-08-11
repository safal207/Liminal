# Liminal Recovery Trust Root Registry v0.1

Status: experimental, non-normative.

## Purpose

The recovery proof chain already pins an immutable builder and an immutable verifier/policy root. This registry adds one machine-readable root-of-roots view over those trust anchors and makes future trust changes explicit rotations rather than silent branch edits.

## Objects

### Trust Root Manifest

A manifest records one trust generation:

- immutable builder workflow commit and Git blob;
- immutable verifier workflow commit and Git blob;
- SHA-256 of critical policy material;
- authorization scope;
- SHA-256 of the previous manifest for generations after zero.

Generation zero is the genesis manifest and therefore has `previous_manifest_sha256 = null`.

### Rotation Registry

The registry contains an ordered history of manifest digests and identifies the active generation. Validation fails unless:

- generations are contiguous from zero;
- every manifest digest matches canonical JSON;
- every non-genesis manifest points to the exact preceding manifest digest;
- the active digest equals the final history entry.

### Registry Attestor

The reusable registry-attestor workflow checks out its own immutable workflow commit, fetches complete Git history, validates the registry chain, and verifies historical material against Git objects. It then produces two GitHub OIDC/Sigstore attestations:

1. the canonical registry JSON itself;
2. a verification receipt stating which registry digest and active roots were verified.

A downstream consumer must pin the registry-attestor workflow digest when deciding whether either attestation is trusted.

## Genesis roots

Builder v0.3:

`02beb48b9c8a61d67c585573aac6c5781c000e89`

Verifier / Policy Root v0.1:

`0aa3dce24f9aeb0c90f955fa5f68d12685e5654a`

Genesis manifest canonical SHA-256:

`bd8aaa6162d0f7e9627e10ee6d495810820fd6fd8cd07d9d48e5d585786537b5`

Genesis registry canonical SHA-256:

`bd43cb039d29245f3d7eb8b78a7a5fcde14d7bf638c4dfe98bb300b00f8670e1`

## Rotation procedure

A future rotation should:

1. create generation `N+1` with `previous_manifest_sha256` equal to generation `N` canonical digest;
2. append the new manifest to registry history without rewriting earlier entries;
3. set the active generation/digest to the new manifest;
4. run exact-head CI;
5. freeze a new immutable registry-attestor commit if the attestor logic changes;
6. produce and externally verify fresh GitHub/Sigstore attestations.

## Trust boundary

The registry does not prove that the model output is true or that a policy is desirable. It proves which builder/verifier/policy material is declared active, that the rotation chain is internally consistent, and—when externally verified—that a specific immutable registry-attestor workflow produced the provenance.
