# Evidence Bundle v0.2 — Receipt-Backed Verification

## Status

Experimental evidence contract. Backward-compatible with the existing v0.1 implementation, which remains available for historical proof chains.

## Goal

Remove raw verifier-output bytes from portable Evidence Bundle identity without removing the verification semantics that authorize trust.

```text
Evidence Bundle v0.1
subject SHA
+ raw verifier JSON SHA
+ signer fields

                 ↓ normalize verification boundary

Evidence Bundle v0.2
subject SHA
+ normalized verification receipt SHA
+ canonical verification semantics
```

## Why a new schema

v0.1 intentionally bound raw verification JSON so every verifier output was directly traceable. The topology-portability experiment showed that this becomes the next portability barrier: different verifier implementations or output representations can have different raw JSON bytes while enforcing the same security contract.

v0.2 keeps raw outputs as audit evidence but removes them from the bundle's canonical identity.

## Subject model

Each manifest/evidence subject contains:

```text
sha256
verification
├ receipt_sha256
├ scheme
├ repository
├ signer_workflow
├ signer_digest
├ source_ref
└ policy
   └ deny_self_hosted_runners
```

The normalized receipt digest is recomputed from the embedded semantics during bundle validation. A tampered receipt hash therefore fails closed.

## Construction rules

A v0.2 bundle can be built only when:

1. the Evidence Manifest is valid;
2. the manifest verification receipt has `verified == true`;
3. the evidence verification receipt has `verified == true`;
4. the manifest receipt subject SHA-256 equals the canonical Evidence Manifest SHA-256;
5. exactly one manifest entry matches `logical_id + generation`;
6. the evidence receipt subject SHA-256 equals the manifest entry SHA-256;
7. verification scheme matches the manifest verification expectation;
8. signer workflow matches the manifest expectation;
9. immutable signer digest matches the manifest expectation.

The bundle builder does not perform cryptographic verification. It accepts only already-normalized receipts whose verdict was obtained from an external verifier.

## Separation of roles

```text
raw verifier output
        ↓ retained separately
external verification result
        ↓
Normalized Verification Receipt
        ↓ canonical semantics
Evidence Bundle v0.2
        ↓
recovery / witness policy
```

The following are deliberately absent from v0.2 canonical identity:

- raw verifier JSON bytes;
- raw verifier JSON SHA-256;
- verifier CLI version;
- timestamp;
- certificate presentation bytes;
- physical artifact path;
- filename/extension;
- archive topology.

## Fail-closed properties

v0.2 rejects:

- unverified manifest/evidence receipts;
- manifest receipt subject mismatch;
- evidence digest mismatch;
- verification-scheme drift;
- signer workflow drift;
- signer digest drift;
- tampered normalized-receipt digests;
- malformed canonical fields.

## Next real workflow gate

After exact-head CI is green, wire v0.2 into a dedicated immutable proof without altering the existing v0.1 trust anchors.

The proof should retain one real `gh attestation verify --format json` output and materialize a second byte-distinct audit representation of the same successful verification event. Both must produce identical normalized receipt SHA-256 values and therefore the same Evidence Bundle v0.2 SHA-256 and witness decision.

```text
raw capture A != raw capture B
        ↓
same verified contract
        ↓
receipt A == receipt B
        ↓
bundle v0.2 A == bundle v0.2 B
        ↓
same recovery / witness transition
```

This proves verifier-output representation independence. A later experiment can replace the second representation with a genuinely independent verifier implementation or CI transport.
