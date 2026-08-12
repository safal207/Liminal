# Evidence Topology Portability v0.1

## Status

Verified experimental recovery/evidence contract.

## Goal

Prove that physical artifact packaging can change without changing trusted evidence identity or the final recovery decision.

The v0.1 portability condition is:

```text
same logical_id
+ same generation
+ same evidence bytes / SHA-256
+ same verified signer contract

Topology A != Topology B
        ↓
manifest-backed resolution succeeds in both
        ↓
physical resolved paths differ
        ↓
same canonical Evidence Bundle SHA-256
        ↓
same verified recovery decision
```

Physical path equality is not accepted as a portability proof.

## Boundary

This layer does not make artifact paths trustworthy and does not perform cryptographic verification itself.

It composes existing primitives:

- `evidence_manifest.py` — logical identity + generation → expected content digest;
- `evidence_resolution.py` — bounded physical re-anchor and explicit verification boundary;
- `evidence_bundle.py` — path-independent receipt for externally verified manifest + evidence.

The caller supplies verification results from the existing signer/attestation layer.

## Topology observation

Each topology declares:

```text
name
expected_path
allowed_prefixes[]
observed candidates[]
```

Candidates remain physical observations only. They do not become trust anchors.

A topology is authorized only when:

1. the manifest contains exactly one entry for the requested `logical_id + generation`;
2. exactly one bounded physical candidate matches the manifest SHA-256;
3. normal evidence resolution succeeds;
4. the external verification result is explicitly successful;
5. the Evidence Bundle can be built against the manifest's signer expectations.

## Portability verdict

`topology_portability_verified` requires both topologies to:

- resolve successfully;
- authorize as `verified_recovery`;
- resolve to different physical paths;
- produce the same canonical Evidence Bundle SHA-256;
- produce the same authorization reason.

The contract fails closed when either topology has:

- digest drift;
- multiple digest matches;
- no candidate in the bounded field;
- unavailable/failed verification;
- a signer contract mismatch;
- the same physical path when a distinct-topology proof is required.

## Example

```text
Topology A
flat/checkpoint-generation-1.json

Topology B
deep/transport/layers/opaque-blob.dat

             same bytes
                 ↓
          same manifest digest
                 ↓
      same logical_id + generation
                 ↓
          same Evidence Bundle
                 ↓
            same decision
```

The filename may change because the manifest-backed evidence resolver selects by stable logical identity and SHA-256, not basename.

## Why this matters

A transport, archive extractor, CI provider, or packaging step may legitimately reshape physical storage. Trust should not silently inherit those packaging choices.

The desired separation is:

```text
packaging / physical topology  → retrieval concern
logical_id + generation        → semantic identity
SHA-256                        → content identity
signer + attestation           → verification authority
Evidence Bundle                → portable receipt
recovery policy                → authorization decision
```

## Verified GitHub Actions proof

The first immutable portability experiment passed on 12 Aug 2026.

- immutable workflow: `2a71b4c77f7a9271dd47ffc5002d3fc254dc635a`
- pinned one-shot caller: `cf258a247c9ea4393d16d3508b6dc03618b2b768`
- successful run: `31617370441`

The producer-attested manifest and checkpoint bytes were materialized into two distinct layouts:

```text
Topology A
  topology-a/evidence-manifest-v0.1.json
  topology-a/checkpoint-generation-1.json

Topology B
  topology-b/meta/opaque-evidence-index.bin
  topology-b/transport/layers/opaque-blob.dat
```

Verified content identities:

- manifest SHA-256: `5f80518cb671ea0622336adbd9a0a9bd16b72ea803ad09d0ac2abd4415f58be2`
- checkpoint SHA-256: `74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`
- portable Evidence Bundle SHA-256: `e3a11b8e98e1f5c7d5c56326d91a641848536f3bedb4be3f51fc1237d0a30d13`
- portability result SHA-256: `b83584388985c82b88204835ffb4fa59d99e44598a6fa86a515f65b88ee57493`

Both layouts produced the same:

- `verified_recovery` authorization reason;
- `checkpoint_witness_advanced` witness decision;
- next-witness SHA-256;
- canonical Evidence Bundle SHA-256.

The external verifier independently recomputed digest/decision equivalence and verified the immutable producer attestation on all four physical copies, including the renamed nested subjects:

- `opaque-evidence-index.bin` — verified;
- `opaque-blob.dat` — verified.

It also verified the immutable portability workflow attestation on the Evidence Bundle and portability result.

Evidence artifacts:

- portability evidence artifact `9149843672`, digest `sha256:e8b4861ed9c4ff65b7a50861a18f1b2760b2cbea1b419ccce657b91d22a403af`;
- external verification artifact `9149864501`, digest `sha256:7a4c001786457ac7b4d9e039c9744eb3a7660b0a2dcdf08053dbfdeca54d8543`.

This proves packaging-topology independence for this GitHub Actions/Sigstore recovery chain: changing directory depth and filenames without changing bytes did not change manifest/evidence identity, the Evidence Bundle, recovery decision, witness transition, or signer verification result.

## Current limit

This experiment uses two packaging layouts inside the same GitHub Actions / GitHub Attestations transport. It does not yet prove verifier- or CI-provider independence.

The current Evidence Bundle also binds raw verification JSON digests. Those receipts can legitimately vary across executions or verifier implementations even when the semantic verification result is equivalent. Therefore a cross-provider experiment should not assume that the current raw-receipt-based Evidence Bundle SHA will remain identical.

## Next portability boundary

Before a cross-provider proof, define a canonical **Normalized Verification Receipt v0.1** that preserves the security-relevant semantics of verification while referencing raw verifier evidence separately.

A later experiment can then test:

```text
same logical evidence identity
→ different artifact transport / verifier implementation
→ normalized verification semantics agree
→ same trust decision
→ portable verification receipt
```

That is the next falsifiable portability boundary.
