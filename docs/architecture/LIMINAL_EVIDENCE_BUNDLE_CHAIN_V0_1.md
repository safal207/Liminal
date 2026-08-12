# Liminal Evidence Bundle / Manifest Chain v0.1

## Goal

Bind a verified Evidence Manifest and its verified evidence subject into one deterministic, path-independent receipt that downstream recovery evidence can reference by SHA-256.

The bundle exists to make this chain explicit:

```text
attested manifest
→ manifest logical_id + generation
→ expected evidence SHA-256
→ bounded digest resolution
→ evidence attestation verification
→ Evidence Bundle
→ bundle SHA-256
→ recovery result
→ witness SHA-256
→ external verification
```

## Identity boundary

Physical artifact paths are deliberately excluded from the bundle.

A path is retrieval context. It is not evidence identity and it is not authority.

The v0.1 bundle contains:

- `logical_id`
- `generation`
- producer
- evidence type
- verified manifest SHA-256
- manifest attestation-verification JSON SHA-256
- manifest signer workflow + immutable digest
- verified evidence SHA-256
- evidence attestation-verification JSON SHA-256
- evidence signer workflow + immutable digest

## Construction rule

`build_verified_evidence_bundle()` fails closed unless:

1. manifest verification explicitly succeeded;
2. evidence verification explicitly succeeded;
3. the requested `logical_id + generation` resolves to exactly one manifest entry;
4. the manifest entry SHA-256 equals the verified evidence SHA-256;
5. the evidence signer workflow equals the manifest's expected signer;
6. the evidence signer digest equals the manifest's expected immutable signer digest.

The bundle does not perform attestation verification itself. It records externally obtained verification results after those checks succeed.

## Canonical receipt

The bundle has deterministic canonical JSON bytes and a stable `bundle_sha256`.

Downstream recovery should bind that digest into its result rather than copying physical locator assumptions into the trust state.

```text
EvidenceBundle SHA-256
        ↓
checkpoint recovery result
        ├─ checkpoint SHA-256
        ├─ witness SHA-256
        └─ bundle SHA-256
        ↓
attested recovery result
```

An external verifier can then independently recompute the manifest digest, evidence digest, bundle digest, recovery-result links and witness digest before accepting the proof chain.

## Safety properties

- no physical path in bundle trust identity;
- manifest verification precedes bundle construction;
- evidence verification precedes bundle construction;
- digest mismatch fails closed;
- signer expectation mismatch fails closed;
- one manifest logical generation must be unique;
- the bundle does not grant authority by itself;
- policy, registry and witness rules remain separate.

## Initial integration target

`trusted-recovery-trust-consumer-checkpoint-witness.yml` will create `evidence-bundle-v0.1.json` only after manifest and checkpoint signer verification succeed.

The checkpoint witness drill will record the bundle SHA-256 in `checkpoint-witness-drill-result.json`.

The one-shot external verifier will then recompute:

```text
manifest bytes
→ manifest SHA-256
→ checkpoint bytes
→ checkpoint SHA-256
→ evidence bundle
→ bundle SHA-256
→ recovery result links
→ witness SHA-256
→ signer attestations
```

Ambiguity or any broken link remains a hard failure.
