# Evidence Topology Portability v0.1

## Status

Experimental recovery/evidence contract.

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

## v0.1 falsifiable proof gate

The real GitHub Actions experiment must use the same producer-attested checkpoint/manifest bytes and materialize them into two distinct layouts:

```text
A: flat expected-name layout
B: deep nested renamed-blob layout
```

The workflow must then independently run manifest-backed resolution for both layouts and record:

- both resolved physical paths;
- both resolution reasons;
- both canonical Evidence Bundle digests;
- both trust decisions;
- both witness decision reasons/digests when the witness policy is included.

The proof passes only if the bundle and trust outputs are equal while the physical paths are different.

## Next portability boundary

Passing two layouts inside GitHub Actions proves packaging-topology independence inside one CI transport. It does not yet prove provider independence.

A later experiment should preserve the same logical evidence contract across a second artifact transport or CI implementation and compare the resulting portable receipt and trust decision.
