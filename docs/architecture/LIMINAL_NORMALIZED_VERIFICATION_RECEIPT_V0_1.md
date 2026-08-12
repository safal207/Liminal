# Normalized Verification Receipt v0.1

## Status

Verified experimental verification/evidence contract.

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
- transport-local metadata;
- verifier implementation identity.

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

Raw verifier output remains important for debugging, independent audit, regression analysis and provider-specific detail. It should remain an artifact beside the normalized receipt, but its bytes are not part of the normalized receipt SHA-256.

A verification evidence container can therefore retain both roles:

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

## Verified representation-independence proof

The first immutable proof established that byte-distinct representations of the same successful GitHub verification event normalize to the same receipt, Evidence Bundle v0.2 and witness transition.

Reusable workflow:

`608061196ef8504a5bed8208797a14bc2dc71c50`

One-shot run:

`31620226592` — **SUCCESS**

Portable identities:

- manifest receipt: `05367cac13290c50dbd413c37b3741a6d1977f19f2b12a29f0e1e154d79e73ca`;
- checkpoint receipt: `fc14a91512662d58a6db21263bf0dd71ce5ad2abcc09a431c027c4bb73a4db70`;
- Evidence Bundle v0.2: `63110899de2feb57152232b07e63a48921e3822320d6b1eb5e7cd6b016bd9892`;
- next witness: `cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f`.

That proof was intentionally limited to output-representation independence and did not claim a second independent verifier.

## Verified independent-verifier proof

The next gate replaced the second representation with a genuinely distinct verifier implementation:

```text
GitHub CLI attestation verifier
        +
Sigstore Cosign v3.0.6
        ↓
independent verifier implementations
        ↓
exact same subject / signer / source / policy semantics
        ↓
same Normalized Verification Receipt
```

Immutable independent-verifier workflow:

`fa20161f4e0c77f4caa97e2e0febfe0cea240d82`

Pinned caller:

`ff9c14da8e35b3bbf02fa53fd4a64f0243da9755`

Successful one-shot:

`31623698930` — **SUCCESS**

The independent verifier proof produced the **same** portable identities as the earlier representation-only proof:

- manifest receipt: `05367cac13290c50dbd413c37b3741a6d1977f19f2b12a29f0e1e154d79e73ca`;
- checkpoint receipt: `fc14a91512662d58a6db21263bf0dd71ce5ad2abcc09a431c027c4bb73a4db70`;
- Evidence Bundle v0.2: `63110899de2feb57152232b07e63a48921e3822320d6b1eb5e7cd6b016bd9892`;
- next witness: `cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f`.

The external job reran Cosign, recomputed the receipt/bundle/witness equivalence and reverified immutable signer bindings.

This proves verifier-implementation portability for the tested GitHub/Sigstore attestation substrate. It still does not prove independence from the shared GitHub/Sigstore trust provider infrastructure.

## Next integration gate

The next meaningful boundary is **Trust-Provider Portability**: preserve the normalized subject/signer/source/policy contract while changing more of the verification substrate itself.

```text
provider path A
        +
provider path B
        ↓
independently established security semantics
        ↓
Normalized Verification Receipt equality or hard failure
        ↓
portable Evidence Bundle
        ↓
same recovery / witness decision
```

A provider disagreement must remain visible and fail closed; normalization must never manufacture equivalence.
