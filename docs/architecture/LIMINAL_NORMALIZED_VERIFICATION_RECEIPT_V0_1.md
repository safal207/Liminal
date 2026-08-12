# Normalized Verification Receipt v0.1

## Status

Experimental verification/evidence contract.

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

Raw verifier output remains important for debugging, independent audit, regression analysis and provider-specific detail. It should remain an artifact beside the normalized receipt, but its bytes are not part of the normalized receipt SHA-256.

A later evidence container may link both without collapsing their roles:

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

## Next integration gate

After exact-head CI is green, Evidence Bundle v0.2 should replace raw `verification_json_sha256` identity fields with normalized verification receipt SHA-256 fields while continuing to retain raw verifier JSON as separate audit artifacts.

Then a real workflow can prove:

```text
same subject + signer + source + policy
        ↓
raw verifier receipt A != raw verifier receipt B
        ↓
normalized receipt A == normalized receipt B
        ↓
Evidence Bundle A == Evidence Bundle B
        ↓
same recovery / witness decision
```

Only after that gate is green does a second verifier implementation or transport become a meaningful provider-portability experiment.
