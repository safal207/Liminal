# Independent Verifier Portability v0.1

## Status

Experimental verification-portability contract.

## Goal

Prove that two genuinely distinct verifier implementations can independently validate the same GitHub/Sigstore attestation contract and converge on the same normalized verification identity.

```text
GitHub CLI verifier
        +
Sigstore/Cosign verifier
        ↓
independent authoritative verdicts
        ↓
exact security-semantic comparison
        ↓
Normalized Verification Receipt A == B
        ↓
Evidence Bundle v0.2 A == B
        ↓
same witness transition
```

This is stronger than verifier-output representation independence because the second verdict must not derive from the first verifier output.

## Claim boundary

v0.1 targets **independent verifier implementation portability over a shared GitHub/Sigstore attestation substrate**.

It does not yet prove:

- independent trust-root providers;
- independent transparency logs;
- independent signing infrastructure;
- independent CI providers;
- independence from GitHub's attestation storage transport.

The expected pair for the first live proof is:

- primary: `gh attestation verify`;
- secondary: Sigstore `cosign verify-blob-attestation` using the stored Sigstore bundle.

The current secure Cosign line must be used. Versions affected by known verification vulnerabilities must not be introduced into the proof workflow.

## Portable semantics versus implementation identity

Verifier implementation identity is audit metadata, not portable trust identity.

```text
implementation: github-cli/attestation-verify
implementation: sigstore/cosign
                 ↓
        deliberately different

verification_scheme: github_attestation
subject_sha256: ...
repository: safal207/Liminal
signer_workflow: ...
signer_digest: ...
source_ref: ...
runner policy: deny self-hosted
verified: true
                 ↓
        must be exactly equal
```

Both implementations therefore produce the same `verification_scheme` when they verify the same GitHub artifact-attestation scheme. Recording the verifier implementation as the scheme would incorrectly make implementation choice part of portable trust identity.

## Fail-closed agreement model

`independent_verifier_portability.py` compares two `VerifierObservation` values.

Agreement requires:

1. distinct non-empty verifier implementation identities;
2. successful verification from both implementations;
3. exact verification-scheme match;
4. exact subject SHA-256 match;
5. exact repository scope match;
6. exact signer workflow match;
7. exact immutable signer digest match;
8. exact source ref match;
9. exact runner-policy match;
10. identical normalized receipt SHA-256.

Any mismatch returns a deterministic failure reason and no portable receipt hash.

## Trust boundary

The portability comparator is not a verifier.

```text
external verifier A ─┐
                     ├─ VerifierObservation[]
external verifier B ─┘
                            ↓
                 semantic agreement check
                            ↓
                 normalized receipt equality
```

The comparator never:

- validates signatures;
- validates transparency-log inclusion;
- fetches trusted roots;
- invents a verifier verdict;
- upgrades a failed verdict into success;
- chooses one verifier as authoritative when they disagree.

## Planned live workflow

The live workflow should preserve two independent raw evidence paths.

### Primary verifier

Use `gh attestation verify` with the exact expected repository, signer workflow, immutable signer digest, source ref and `--deny-self-hosted-runners` policy.

### Secondary verifier

Use a pinned Sigstore Cosign installer and a patched Cosign release. Retrieve the Sigstore bundle for the exact subject digest from GitHub's attestation API and verify it with Cosign rather than feeding the GitHub CLI output into the second path.

The secondary path must independently enforce the same signer/source policy claims available from the signing certificate and bundle. If the runner-environment constraint cannot be independently proven, the secondary observation must record the weaker policy and the portability comparison must fail closed rather than claiming equivalence.

## First falsifiable live gate

```text
subject bytes
   ├─ gh verifier → authoritative observation A
   └─ cosign      → authoritative observation B

A.implementation != B.implementation
A.verified == B.verified == true
A.security_semantics == B.security_semantics
        ↓
receipt A == receipt B
        ↓
Bundle v0.2 A == Bundle v0.2 B
        ↓
witness decision A == witness decision B
```

The external one-shot verifier must then recompute the equality and independently verify the immutable proof-workflow attestations.

## Expected next boundary

After this gate succeeds, the next meaningful experiment is **Trust-Provider Portability**: preserve the normalized contract while changing more of the verification substrate itself rather than only the verifier implementation.
