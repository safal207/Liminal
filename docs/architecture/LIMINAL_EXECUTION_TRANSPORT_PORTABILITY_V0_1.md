# Execution-Provider + Evidence-Transport Portability v0.1

## Status

Experimental fail-closed portability contract. The comparison model is implemented; no live execution/transport portability claim is valid until one path is actually executed outside GitHub-hosted Actions and consumes the evidence through a non-GitHub-artifact transport provider.

## Goal

Prove that a trusted transition is not an accidental property of one CI execution substrate or one artifact transport service.

```text
path A
GitHub-hosted execution
+ GitHub Actions artifact transport
        ↓
verified transition semantics

path B
independent execution provider
+ independent evidence transport provider
        ↓
verified transition semantics

A == B on portable semantics
        ↓
same trusted transition
```

## Why this is a distinct gate

Earlier gates proved that the system survives:

- physical path/topology drift;
- raw verifier-output drift;
- verifier implementation drift;
- trust-root drift;
- signing-authority drift;
- signature-scheme drift.

Trust-Provider Portability v0.1 still used GitHub Actions as the execution environment and GitHub Actions artifacts as the evidence transport mechanism. Those shared dependencies remain outside the portable proof boundary.

## Observation model

`ExecutionTransportObservation` contains two classes of data.

### Audit / independence metadata

- `execution_provider`
- `execution_environment_id`
- `transport_provider`
- `transport_channel_id`

These fields identify how and where one path actually ran. They are retained for audit and independence checks but excluded from portable receipt identity.

### Portable transition semantics

- `subject_sha256`
- `portable_trust_receipt_sha256`
- `authorization_policy_sha256`
- `witness_reason`
- `next_witness_sha256`
- `verified`

These fields must match exactly across independent paths.

## Portable Execution-Transport Receipt v0.1

Canonical schema:

`liminal-execution-transport-receipt/v0.1`

The receipt intentionally excludes provider and channel identifiers. Those identifiers prove independence; they are not the semantic identity that should survive portability.

```text
subject identity
+ portable trust identity
+ local authorization-policy identity
+ witness decision class
+ next trusted-state identity
+ successful verification
        ↓
Portable Execution-Transport Receipt
```

## Independence requirements

Agreement is impossible unless:

1. `execution_provider` differs;
2. `transport_provider` differs;
3. both paths report successful externally established verification.

A different runner label under the same execution provider is not enough.

A self-hosted GitHub runner is not automatically a different execution provider merely because the machine is different; if GitHub Actions remains the authority/orchestrator for the asserted execution boundary, that distinction must not be overstated.

A different GitHub artifact URL, mirror path, filename or extraction directory is not a different transport provider.

## Fail-closed semantic agreement

After independence checks, these values must match exactly:

1. subject SHA-256;
2. Portable Trust Receipt SHA-256;
3. authorization-policy SHA-256;
4. witness reason;
5. next-witness SHA-256;
6. canonical portable execution-transport receipt SHA-256.

Any mismatch returns a deterministic failure reason and no portable receipt identity.

## Trust boundary

`execution_transport_portability.py` is not an execution verifier, transport verifier or cryptographic verifier.

It must only consume observations whose claims were established outside this comparison layer.

It never:

- launches or authenticates a remote execution environment;
- proves that a file really traversed a claimed transport provider;
- validates cryptographic signatures;
- infers provider independence from labels;
- treats a different filename/path as a different transport provider;
- treats a different runner name as a different execution provider;
- chooses one path as authoritative if portable semantics disagree.

## Planned first live experiment

The first candidate secondary path is deliberately outside GitHub-hosted Actions:

```text
secondary execution:
OpenAI / ChatGPT isolated Linux workspace

secondary evidence transport:
Google Drive file transport
```

The proposed live procedure is:

1. obtain the immutable checkpoint evidence from the already successful trust-provider proof;
2. copy the bounded evidence package through Google Drive as a separate transport/storage authority;
3. consume the Drive-delivered copy in the non-GitHub execution environment;
4. recompute the subject SHA-256;
5. independently verify the pinned offline Ed25519 claims;
6. reconstruct the Portable Trust Receipt;
7. apply the same local authorization policy;
8. recompute the witness transition;
9. compare that observation to the immutable GitHub-hosted/GitHub-artifact observation;
10. fail closed on any identity drift.

The Drive round trip must be content-addressed. A successful upload/download operation is not evidence identity; the SHA-256 after retrieval must match the trusted subject identity.

## Claim boundary for the planned experiment

If successful, the first live gate may claim independence only for the concrete tested boundaries:

- execution provider: GitHub Actions hosted vs OpenAI/ChatGPT workspace;
- transport provider: GitHub Actions artifacts vs Google Drive;
- trust path used by the secondary execution: pinned offline Ed25519 root.

It must not imply independent organizational governance, hardware provenance, network path, transparency log or long-term storage durability unless those are separately demonstrated.

## Next falsifiable question

Can evidence transported through a non-GitHub provider and verified in a non-GitHub execution environment reproduce the exact same Portable Trust Receipt and next-witness digest as the immutable GitHub-hosted proof?
