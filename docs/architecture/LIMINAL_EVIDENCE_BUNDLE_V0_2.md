# Evidence Bundle v0.2 — Receipt-Backed Verification

## Status

Verified experimental evidence contract. Backward-compatible with the existing v0.1 implementation, which remains available for historical proof chains.

## Goal

Remove raw verifier-output bytes and verifier-implementation identity from portable Evidence Bundle identity without removing the verification semantics that authorize trust.

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
- verifier implementation identity;
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

## Verified representation-independence proof

The first v0.2 live proof retained one real `gh attestation verify --format json` output and created a byte-distinct audit representation of the same successful verification event.

Reusable workflow:

`608061196ef8504a5bed8208797a14bc2dc71c50`

Successful one-shot:

`31620226592`

Both representations produced:

- manifest receipt SHA-256: `05367cac13290c50dbd413c37b3741a6d1977f19f2b12a29f0e1e154d79e73ca`;
- checkpoint receipt SHA-256: `fc14a91512662d58a6db21263bf0dd71ce5ad2abcc09a431c027c4bb73a4db70`;
- Evidence Bundle v0.2 SHA-256: `63110899de2feb57152232b07e63a48921e3822320d6b1eb5e7cd6b016bd9892`;
- same `checkpoint_witness_advanced` transition;
- next witness SHA-256: `cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f`.

This proved output-representation independence, not independent verifier agreement.

## Verified independent-verifier proof

The next live gate used two genuinely distinct verifier implementations:

```text
GitHub CLI attestation verification
        +
Sigstore Cosign v3.0.6 verification
        ↓
independently established exact security semantics
        ↓
same normalized receipts
        ↓
same Evidence Bundle v0.2
        ↓
same witness transition
```

Immutable workflow:

`fa20161f4e0c77f4caa97e2e0febfe0cea240d82`

Pinned caller:

`ff9c14da8e35b3bbf02fa53fd4a64f0243da9755`

Successful one-shot:

`31623698930` — **SUCCESS**

The independent verifier implementations produced exactly the same portable identities as the preceding representation-only proof:

- manifest receipt SHA-256: `05367cac13290c50dbd413c37b3741a6d1977f19f2b12a29f0e1e154d79e73ca`;
- checkpoint receipt SHA-256: `fc14a91512662d58a6db21263bf0dd71ce5ad2abcc09a431c027c4bb73a4db70`;
- Evidence Bundle v0.2 SHA-256: `63110899de2feb57152232b07e63a48921e3822320d6b1eb5e7cd6b016bd9892`;
- next witness SHA-256: `cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f`.

Canonical proof-result SHA-256:

`2b857ced0b8ae39ac700844358ef7017b1badc7149d063a6de3fad30b355c6b3`

External recomputation record SHA-256:

`e128b187b776b3e1da2adacd05cc4e6c299a6d2992d9537a9047c50e01b5f0f8`

The external job independently recomputed bundle/receipt/witness equivalence and reran Cosign on the stored selected Sigstore bundles before rechecking immutable signer attestations.

Evidence artifacts:

- proof `9152287850` — `sha256:4ec977fcb559ba2f84bf91c5641798f98a5ccea59d028a4cd22d908c104662e3`;
- external verification `9152310899` — `sha256:4ebc4978164bad5708f24fcb610ee35136fcfedc693f61b2c1bd0dd958398b44`.

## Current proven boundary

```text
physical topology drift
        ↓ survives
raw verifier representation drift
        ↓ survives
verifier implementation drift
        ↓ survives
canonical subject + signer + source + policy semantics
        ↓
portable Evidence Bundle v0.2 identity
        ↓
portable witness transition
```

The claim remains limited to a shared GitHub/Sigstore attestation substrate. Verifier portability is not yet trust-provider portability.

## Next real workflow gate

**Trust-Provider Portability v0.1** should test whether the same normalized security contract can be established through a materially different trust/provider substrate.

```text
trust provider A
        +
trust provider B
        ↓
independent security observations
        ↓
normalized semantic equality or hard failure
        ↓
Evidence Bundle
        ↓
same recovery / witness transition
```

Normalization must never hide a provider-level disagreement.
