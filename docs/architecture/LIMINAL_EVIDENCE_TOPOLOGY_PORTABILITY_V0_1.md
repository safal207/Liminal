# Evidence Topology Portability v0.1

Verified. See immutable workflow `2a71b4c77f7a9271dd47ffc5002d3fc254dc635a` and successful one-shot run `31617370441`.

## Verified invariant

```text
same logical_id + generation + bytes + signer contract
+ different physical topology
→ same Evidence Bundle
→ same verified_recovery
→ same checkpoint_witness_advanced decision
→ same next witness digest
```

Topology A used normal JSON filenames. Topology B moved and renamed the same manifest/checkpoint bytes to `opaque-evidence-index.bin` and `opaque-blob.dat`. External `gh attestation verify` succeeded on both renamed subjects against immutable producer `f31b56a5e21a668bcb98791b05542652760dcc27`.

Content identities:

- manifest SHA-256: `5f80518cb671ea0622336adbd9a0a9bd16b72ea803ad09d0ac2abd4415f58be2`
- checkpoint SHA-256: `74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`
- Evidence Bundle SHA-256: `e3a11b8e98e1f5c7d5c56326d91a641848536f3bedb4be3f51fc1237d0a30d13`
- result SHA-256: `b83584388985c82b88204835ffb4fa59d99e44598a6fa86a515f65b88ee57493`

Evidence artifacts:

- `9149843672` — `sha256:e8b4861ed9c4ff65b7a50861a18f1b2760b2cbea1b419ccce657b91d22a403af`
- `9149864501` — `sha256:7a4c001786457ac7b4d9e039c9744eb3a7660b0a2dcdf08053dbfdeca54d8543`

## Safety boundary

This proves packaging-topology independence inside the tested GitHub Actions / GitHub Attestations chain. It does not prove CI-provider or verifier-output portability.

The current Evidence Bundle binds raw verification JSON SHA-256 values, so equivalent verification semantics may still produce different bundle bytes across runs/verifiers.

## Next boundary

**Normalized Verification Receipt v0.1** should separate canonical verification semantics from raw verifier-output evidence. That enables the next falsifiable experiment: cross-verifier / cross-transport portability without weakening signer, subject-digest, source-ref, runner-policy, or attestation checks.
