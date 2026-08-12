# Evidence Bundle v0.2 — Receipt-Backed Verification

## Status

Experimental evidence contract with a successful immutable representation-independence proof. Backward-compatible with the existing v0.1 implementation, which remains available for historical proof chains.

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

## Verified immutable proof

Receipt-backed Bundle v0.2 passed unit/static/integration gates before the immutable proof workflow was pinned.

Immutable proof workflow:

`608061196ef8504a5bed8208797a14bc2dc71c50`

Pinned caller:

`dd069652dd38ef11410650da9385b1fd923ecfd4`

Successful one-shot:

`31620226592`

The proof used the same manifest/checkpoint subjects in two byte-distinct verification-audit representations. The raw capture digests differed, while the normalized receipt digests were identical between A and B.

Canonical identities:

- manifest SHA-256: `5f80518cb671ea0622336adbd9a0a9bd16b72ea803ad09d0ac2abd4415f58be2`;
- checkpoint SHA-256: `74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`;
- normalized manifest receipt SHA-256: `05367cac13290c50dbd413c37b3741a6d1977f19f2b12a29f0e1e154d79e73ca`;
- normalized checkpoint receipt SHA-256: `fc14a91512662d58a6db21263bf0dd71ce5ad2abcc09a431c027c4bb73a4db70`;
- Evidence Bundle v0.2 SHA-256: `63110899de2feb57152232b07e63a48921e3822320d6b1eb5e7cd6b016bd9892`;
- proof-result SHA-256: `49e4e3706645fb47b70251d8ad2ea0714ba4e03595cbf91c16b980d47c1c36da`.

The witness decision was identical for both representations:

```text
authorized: true
reason: checkpoint_witness_advanced
next_witness_sha256: cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f
```

The external verification job independently:

- recomputed raw-capture inequality;
- recomputed normalized receipt equality;
- recomputed Bundle v0.2 equality;
- checked canonical subject/receipt/bundle bindings;
- verified the immutable producer signer on manifest and checkpoint;
- verified the immutable proof signer on both A/B normalized receipt copies;
- verified the immutable proof signer on both A/B Bundle v0.2 copies;
- verified the proof-result signer.

Only A receipt/bundle files were directly attested in the producer proof job; B copies independently verified against those attestations because their canonical bytes were identical. This is an additional content-identity check, not a second-verifier claim.

Evidence artifacts:

- normalized proof `9150941935` — `sha256:ffc420fe9f81ba6e823a212c8c4d32ecfc90752e9a926f483327b8158c25c74a`;
- external verification `9150963798` — `sha256:db4e60a85fe698be68f017b346aeac3df5ebe27d28d767045325e0c8e8e33d58`.

The pinned caller head also passed Python CI, Python Integration and Artillery.

## Proven boundary

The proof establishes:

```text
raw verifier representation A != B
        ↓
normalized security semantics A == B
        ↓
Verification Receipt SHA A == B
        ↓
Evidence Bundle v0.2 SHA A == B
        ↓
witness transition A == B
```

for two representations of the **same successful GitHub Attestations verification event**.

It does not establish independent verifier/provider agreement.

## Next real workflow gate

Introduce a second verifier adapter whose authoritative verdict is independently obtained rather than derived from the first verifier's output. Normalize both independently obtained verification events and compare their receipts, Bundle v0.2 identities and witness transitions.

```text
GitHub verifier
        +
independent verifier
        ↓
Normalized Verification Receipt A/B
        ↓
semantic equality or fail closed
        ↓
Evidence Bundle v0.2
        ↓
recovery / witness policy
```

A mismatch in subject, signer, source, policy or verdict must remain a hard portability failure rather than being normalized away.
