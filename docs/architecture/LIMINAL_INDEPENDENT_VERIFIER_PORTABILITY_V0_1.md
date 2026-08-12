# Independent Verifier Portability v0.1

## Status

Verified experimental verification-portability contract.

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

This is stronger than verifier-output representation independence because the second verdict does not derive from the first verifier output.

## Claim boundary

v0.1 proves **independent verifier implementation portability over a shared GitHub/Sigstore attestation substrate**.

It does not prove:

- independent trust-root providers;
- independent transparency logs;
- independent signing infrastructure;
- independent CI providers;
- independence from GitHub's attestation storage transport.

The verified pair is:

- primary: `gh attestation verify`;
- secondary: Sigstore Cosign `v3.0.6` using `cosign verify-blob-attestation` over stored Sigstore bundles.

Cosign was installed through immutable installer commit:

`6f9f17788090df1f26f669e9d70d6ae9567deba6`

## Portable semantics versus implementation identity

Verifier implementation identity is audit metadata, not portable trust identity.

```text
implementation: github-cli/attestation-verify
implementation: sigstore/cosign-v3.0.6
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

## Live verification paths

### Primary verifier

`gh attestation verify` verifies the exact expected repository, signer workflow, immutable signer digest, source ref and `--deny-self-hosted-runners` policy.

### Secondary verifier

Sigstore Cosign independently verifies:

- the Sigstore bundle;
- the signed DSSE attestation;
- subject SHA-256;
- SLSA provenance predicate type;
- certificate identity/SAN;
- OIDC issuer;
- transparency-log evidence.

The workflow then independently inspects the Fulcio leaf certificate and requires the same policy facts needed by the normalized contract:

- producer signer commit SHA;
- repository identity;
- source ref;
- `github-hosted` runner environment.

If any of these facts cannot be proven, the secondary observation is not accepted and portability fails closed.

## Transport separation

The first pinned experiment exposed a transport-only failure before Cosign verification. GitHub's attestation bundle URL payload is not ordinary UTF-8 JSON; the official GitHub CLI transport decodes the stored representation before materializing the bundle.

The repaired workflow therefore uses `gh attestation download` only for supported bundle retrieval/decompression into JSONL. It does **not** reuse `gh attestation verify` output as the secondary verdict. Cosign remains the independent secondary cryptographic verifier.

This preserves the separation:

```text
GitHub storage transport
        ↓
gh attestation download
        ↓
Sigstore bundle bytes
        ↓
Cosign verification
        ↓
secondary authoritative observation
```

Transport assistance is not verifier agreement.

## Immutable live proof

Reusable workflow:

`fa20161f4e0c77f4caa97e2e0febfe0cea240d82`

Pinned one-shot caller:

`ff9c14da8e35b3bbf02fa53fd4a64f0243da9755`

Successful one-shot run:

`31623698930` — **SUCCESS**

Immutable upstream chain:

- checkpoint producer: `f31b56a5e21a668bcb98791b05542652760dcc27`;
- trust-root rotation workflow: `e2cb6a014236bc561d03c405f4986146026041fa`.

The reusable workflow bytes passed exact-head Python CI, Python Integration and Artillery before being pinned.

## Verified subjects and normalized identities

Manifest subject SHA-256:

`5f80518cb671ea0622336adbd9a0a9bd16b72ea803ad09d0ac2abd4415f58be2`

Checkpoint subject SHA-256:

`74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`

Both `gh` and Cosign independently produced `verified_semantics_agree` for both subjects.

Manifest normalized receipt SHA-256:

`05367cac13290c50dbd413c37b3741a6d1977f19f2b12a29f0e1e154d79e73ca`

Checkpoint normalized receipt SHA-256:

`fc14a91512662d58a6db21263bf0dd71ce5ad2abcc09a431c027c4bb73a4db70`

Receipt-backed Evidence Bundle v0.2 SHA-256:

`63110899de2feb57152232b07e63a48921e3822320d6b1eb5e7cd6b016bd9892`

These are exactly the same portable identities produced by the preceding verifier-output-representation proof. Changing the verifier implementation therefore did not change portable trust identity.

## Witness equivalence

Both independent verifier observations produced:

```text
authorized: true
reason: checkpoint_witness_advanced
next_witness_sha256: cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f
```

The canonical proof result SHA-256 is:

`2b857ced0b8ae39ac700844358ef7017b1badc7149d063a6de3fad30b355c6b3`

## External independent recomputation

A separate job did not trust the proof-result boolean. It independently:

1. recomputed manifest and checkpoint digests;
2. confirmed GH/Cosign receipt byte equality and canonical serialization;
3. recomputed normalized receipt hashes;
4. recomputed Evidence Bundle v0.2 identity;
5. confirmed distinct verifier implementation identities;
6. confirmed witness decision equivalence;
7. reran Cosign v3.0.6 on the selected stored Sigstore bundles;
8. rechecked signer SHA, repository, source ref and `github-hosted` certificate policy;
9. reverified the immutable producer signer with GitHub CLI;
10. verified the immutable proof-workflow signer on both GH/Cosign receipt copies, both bundle copies and the proof result.

External recomputation record SHA-256:

`e128b187b776b3e1da2adacd05cc4e6c299a6d2992d9537a9047c50e01b5f0f8`

## Evidence artifacts

Independent verifier proof:

- artifact id: `9152287850`;
- digest: `sha256:4ec977fcb559ba2f84bf91c5641798f98a5ccea59d028a4cd22d908c104662e3`;
- retention: 30 days;
- expires: 2026-09-11.

External independent verification:

- artifact id: `9152310899`;
- digest: `sha256:4ebc4978164bad5708f24fcb610ee35136fcfedc693f61b2c1bd0dd958398b44`;
- retention: 30 days;
- expires: 2026-09-11.

Producer evidence used by this run:

- artifact id: `9152268182`;
- digest: `sha256:f8efc1f874076a6a5e57a6bda0839e6e20d65e44ba99f972f81069670e350b43`.

## Proven invariant

```text
same subject bytes
   ├─ GitHub CLI verifier ─┐
   └─ Sigstore Cosign ─────┘
              ↓
exact signer/source/policy agreement
              ↓
Normalized Receipt A == B
              ↓
Evidence Bundle v0.2 A == B
              ↓
witness transition A == B
```

For this GitHub/Sigstore attestation substrate, verifier implementation choice between GitHub CLI and Cosign is not part of portable trust identity when both independently prove the same security contract.

## What this does not prove

The experiment still shares important infrastructure:

- GitHub-hosted artifact-attestation storage;
- the same Sigstore/Fulcio signing substrate;
- the same transparency-log ecosystem;
- the same producer workflow and subject bytes;
- GitHub Actions as the CI environment.

So the result must not be described as independent trust-provider portability.

## Expected next boundary

The next meaningful experiment is **Trust-Provider Portability**: preserve the normalized security contract while changing more of the verification substrate itself rather than only the verifier implementation.

Candidate direction:

```text
GitHub/Sigstore proof path
        +
independent trust/provider proof path
        ↓
normalized subject / signer / policy semantics
        ↓
provider-independent Evidence Receipt
        ↓
portable recovery / witness decision
```

Any disagreement in subject identity, authorization lineage, signer binding or policy must remain a hard failure rather than being normalized away.
