# Normalized Verification Receipt v0.1

## Status

Experimental verification/evidence contract with an immutable GitHub Actions proof of verifier-output representation independence.

## Goal

Make verification identity depend on security-relevant semantics rather than the byte layout of one verifier implementation's JSON output.

The boundary is:

```text
raw verifier output A
raw verifier output B
        ↓
exact same verified security contract
        ↓
Normalized Verification Receipt
        ↓
same canonical receipt SHA-256
```

Raw outputs may differ. The normalized receipt must not.

## Why

The Evidence Bundle v0.1 binds raw verification JSON SHA-256 values. That is useful for auditability inside one verifier path, but it prevents meaningful verifier/provider portability because equivalent verifiers can emit different JSON fields, ordering, timestamps, versions, certificate decoration, or implementation metadata.

The normalized receipt separates:

```text
raw verifier bytes            → audit evidence
normalized verification facts → portable trust identity input
```

## Canonical security semantics

v0.1 records only:

- verification scheme;
- subject SHA-256;
- repository scope;
- signer workflow identity;
- immutable signer commit SHA;
- fully qualified source ref;
- self-hosted-runner denial policy;
- verification verdict.

The canonical receipt deliberately excludes:

- physical artifact path;
- filename or extension;
- raw verifier JSON;
- raw verifier JSON SHA-256;
- CLI/tool version;
- timestamp;
- certificate presentation bytes;
- output field ordering;
- transport-local metadata.

## Trust boundary

`verification_receipt.py` is not a verifier.

The caller must obtain the verification outcome from an external verification implementation. The receipt only canonicalizes the exact contract that was checked and the resulting boolean verdict.

Therefore:

```text
finding bytes            != verifying bytes
normalizing a result     != verifying a result
receipt construction     != trust authorization
```

Downstream authorization must still require `verified == true` and must independently enforce any expected signer, subject and policy bindings.

## Canonical model

```text
NormalizedVerificationReceipt
├ schema
├ verification_scheme
├ subject_sha256
├ repository
├ signer_workflow
├ signer_digest
├ source_ref
├ policy
│  └ deny_self_hosted_runners
└ verified
```

Serialization is deterministic JSON with sorted keys and compact separators, followed by one newline.

## Raw evidence retention

Raw verifier output remains important for debugging, independent audit, regression analysis and provider-specific detail. It remains an artifact beside the normalized receipt, but its bytes are not part of the normalized receipt SHA-256.

```text
verification evidence
├ normalized-receipt.json      ← portable semantic identity
└ raw/
   ├ verifier-a.json            ← provider-specific audit evidence
   └ verifier-b.json
```

## v0.1 falsifiable properties

The tests require:

1. deterministic canonical bytes and round-trip parsing;
2. different raw verifier JSON bytes can map to the same normalized receipt when the security semantics are identical;
3. physical path/name are absent from receipt identity;
4. signer rotation changes receipt identity;
5. source-ref change changes receipt identity;
6. verification failure changes receipt identity;
7. runner-policy relaxation changes receipt identity;
8. malformed signer/ref/repository fields fail closed.

## Verified immutable proof

The primitive exact head passed Python CI, Python Integration and Artillery before workflow integration. The reusable proof workflow was then itself exact-head green and pinned immutably at:

`608061196ef8504a5bed8208797a14bc2dc71c50`

Pinned caller:

`dd069652dd38ef11410650da9385b1fd923ecfd4`

One-shot run:

`31620226592` — **SUCCESS**

The run used immutable upstream workflows:

- normalized receipt proof: `608061196ef8504a5bed8208797a14bc2dc71c50`;
- checkpoint/manifest producer: `f31b56a5e21a668bcb98791b05542652760dcc27`;
- trust-root rotation drill: `e2cb6a014236bc561d03c405f4986146026041fa`.

The proof captured a real successful `gh attestation verify --format json` event as capture A and created an explicitly non-authoritative byte-distinct audit envelope as capture B for the same verification event.

Raw capture SHA-256 values were different:

```text
manifest A  1014a62cadb75b00bc40b0934904afefa82d827d62f171b71e1adb36412089c6
manifest B  9c9657efa7fd179c077fb672bcedf98983a998675ea59d97e7db8b4b5427e45a
checkpoint A a86adb43d8fef225a073cddf7c77ff2df3dfada1f8062450252a38bf08b1e206
checkpoint B bc9b89634bf62035289562b595d477b7ea7e0006be83da49b2cee7fc81dd8d2d
```

Yet both representations normalized to the same security identities:

- manifest receipt SHA-256: `05367cac13290c50dbd413c37b3741a6d1977f19f2b12a29f0e1e154d79e73ca`;
- checkpoint receipt SHA-256: `fc14a91512662d58a6db21263bf0dd71ce5ad2abcc09a431c027c4bb73a4db70`.

Both then produced the same receipt-backed Evidence Bundle v0.2:

`63110899de2feb57152232b07e63a48921e3822320d6b1eb5e7cd6b016bd9892`

Both witness evaluations authorized the same transition:

- reason: `checkpoint_witness_advanced`;
- next-witness SHA-256: `cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f`.

The attested proof-result SHA-256 is:

`49e4e3706645fb47b70251d8ad2ea0714ba4e03595cbf91c16b980d47c1c36da`

The independent external job recomputed the subject, receipt and bundle equivalence, reverified the immutable producer on the manifest/checkpoint, and verified the immutable proof signer on both A/B copies of the normalized receipts and Evidence Bundle v0.2. The B copies passed because their canonical content identity matched the attested A copies.

Evidence artifacts:

- proof artifact `9150941935` — `sha256:ffc420fe9f81ba6e823a212c8c4d32ecfc90752e9a926f483327b8158c25c74a`;
- external verification `9150963798` — `sha256:db4e60a85fe698be68f017b346aeac3df5ebe27d28d767045325e0c8e8e33d58`.

The pinned caller head also passed Python CI, Python Integration and Artillery.

## What this proves

For one successful GitHub Attestations verification event, byte-level verifier-output representation is not required to be part of portable trust identity. Different audit representations can normalize to the same receipt, the same Evidence Bundle v0.2, and the same witness transition when the security-relevant contract is unchanged.

## What this does not prove

Capture B is not an independent second verifier. It is an explicitly marked alternate byte representation of the same successful GitHub verification event.

Therefore this does **not** yet prove:

- independent verifier agreement;
- CI-provider independence;
- transport independence;
- semantic equivalence across different attestation ecosystems.

## Next verification gate

Use a genuinely independent verifier adapter that obtains the same security facts without reusing the original `gh` verification result as its authoritative verdict.

```text
Verifier A: GitHub / gh
Verifier B: independent implementation
        ↓
subject + signer + source + policy semantics
        ↓
Normalized Verification Receipt A/B
        ↓
receipt SHA equality
        ↓
Evidence Bundle v0.2 equality
        ↓
same recovery / witness transition
```

That is the next meaningful provider/verifier portability experiment.
