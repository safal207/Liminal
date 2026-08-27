# Execution-Provider + Evidence-Transport Portability v0.1

## Status

**VERIFIED — live immutable proof completed successfully.**

The first live gate executed the secondary verification path outside GitHub-hosted Actions and transported the source evidence through Google Drive rather than GitHub Actions artifacts. Both paths converged on the same Portable Trust Receipt, authorization-policy identity, witness decision, next-witness identity and Portable Execution-Transport Receipt.

## Goal

Prove that a trusted transition is not an accidental property of one CI execution substrate or one artifact transport service.

```text
path A
GitHub Actions hosted
+ GitHub Actions artifact transport
        ↓
verified transition semantics

path B
OpenAI / ChatGPT isolated Linux workspace
+ Google Drive transport
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

Trust-Provider Portability v0.1 still used GitHub Actions as the execution environment and GitHub Actions artifacts as the evidence transport mechanism. This gate crossed both of those boundaries for the secondary path.

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

Verified receipt SHA-256:

`644b575189d18d61e7ed8415d59087c69d0ddc02ba5743b24c86dfacc7b24b49`

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

It only consumes observations whose claims were established outside this comparison layer.

It never:

- launches or authenticates a remote execution environment;
- proves that a file really traversed a claimed transport provider;
- validates cryptographic signatures;
- infers provider independence from labels;
- treats a different filename/path as a different transport provider;
- treats a different runner name as a different execution provider;
- chooses one path as authoritative if portable semantics disagree.

## Live secondary path

The successful secondary path was deliberately outside GitHub-hosted Actions:

```text
immutable GitHub source artifact
        ↓
Google Drive upload
        ↓
Google Drive raw retrieval
        ↓
OpenAI / ChatGPT isolated Linux workspace
        ↓
SHA-256 recomputation
+ offline Ed25519 verification
+ Portable Trust Receipt reconstruction
+ local authorization-policy application
+ witness transition recomputation
        ↓
externally signed result
```

Source GitHub artifact:

- run: `31658743875`
- artifact ID: `9165388614`
- artifact ZIP SHA-256: `c3822f1d5658d4c9965a67b1a4264dafc04cb5ea6f64c516ced7a920b49cd161`

Google Drive transport:

- file ID: `1wLKINhrx6BGilKvUKI_OZQLOV5oEiDdN`
- retrieved size: `52911` bytes
- retrieved ZIP SHA-256: `c3822f1d5658d4c9965a67b1a4264dafc04cb5ea6f64c516ced7a920b49cd161`

The Drive-retrieved bytes therefore matched the immutable source artifact identity exactly.

Checkpoint subject SHA-256:

`74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`

Portable Trust Receipt SHA-256:

`2235b07a4188628091cbe94af6a16dc30516d0acea743f9b4517b58a5cbd1a80`

Authorization-policy SHA-256:

`22fcc3c556528d080591041bc10c1a35f85bfbad348b8f669bfff4bb1b88b47f`

Witness decision:

`checkpoint_witness_advanced`

Next-witness SHA-256:

`cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f`

## External execution proof

The OpenAI-workspace result was signed outside GitHub with a dedicated Ed25519 key before the GitHub verifier consumed it.

Pinned external root:

`ed25519-sha256:72c2477f78a0a901f6f1cef45ccec69053842eb980c666f0cbdc01589dcd69d9`

External result SHA-256:

`70d9413ef99348ab495b4fe173cba9493372ec9ee25a4ac5deb64a5b9c94a979`

External proof SHA-256:

`77f27ac764ea1aff2a13eda215492ac4bd22830aba54825e3dee6804d140f999`

The external private key was not committed to the repository and was not available to the GitHub proof workflow.

## Immutable GitHub verification gate

Reusable workflow:

`118a136cd63d43216399be10d66bcb589655e92d`

Exact-head checks before pinning:

- Python CI `31659994873` — SUCCESS
- Python Integration `31659994855` — SUCCESS
- Artillery WebSocket Smoke `31659994840` — SUCCESS

Pinned one-shot caller:

`9e4709dc638418e5124f62799b68baa1b21fa661`

Successful one-shot:

`31660230947` — **FULL SUCCESS**

The reusable proof independently:

1. re-downloaded the immutable source through GitHub artifact transport;
2. reverified the GitHub/Sigstore checkpoint path;
3. verified the externally signed OpenAI + Drive result;
4. recomputed the GitHub-side Portable Trust Receipt and witness transition;
5. compared GitHub Actions + GitHub artifact against OpenAI workspace + Google Drive;
6. required exact portable semantic equality;
7. attested the combined result.

Canonical combined result SHA-256:

`c1135ab49d5d81225f9b5cfff5441557415b99d4996cb2beed3de1cea6519d01`

## Independent audit job

The caller's second job did not trust the combined `verified: true` flag. It independently:

- verified the immutable reusable-workflow signer on the proof result;
- reverified the external Ed25519 result signature;
- rechecked the exact external root and result identities;
- recomputed the execution/transport provider independence decision;
- rechecked the checkpoint, Portable Trust Receipt, policy and next-witness identities;
- reproduced the Portable Execution-Transport Receipt identity;
- attested its own audit result.

Independent audit result SHA-256:

`ef222cc90aff884e47e40c0924738012c98d0139643aed561da6daba73884a02`

Evidence artifacts, retained for 30 days:

- proof artifact `9165906664` — `sha256:f739a009dfd16678583b7ef1d2c7f229793ffd94d22854ea90d989916a5c1052`
- external verification artifact `9165911241` — `sha256:c3d3aeed1ad96b95ac9a0c7e6b6e1c034f13013bc10d8dd5b977d2751130bdc7`

## Proven boundary

The live gate proves the following concrete portability boundary:

```text
GitHub Actions hosted execution
        ≠
OpenAI / ChatGPT isolated workspace execution

GitHub Actions artifact transport
        ≠
Google Drive file transport

                    ↓
identical checkpoint subject
+ identical Portable Trust Receipt
+ identical authorization policy
+ identical witness decision
+ identical next trusted state
                    ↓
identical Portable Execution-Transport Receipt
```

This demonstrates execution-provider and evidence-transport-provider portability for the tested transition.

## Claim boundary

This experiment **does prove** independence of the tested:

- execution provider;
- evidence transport provider;
- secondary execution-result signing root;
- secondary trust verification path.

It **does not prove** independence of:

- the source checkpoint producer — it remains the immutable GitHub producer workflow;
- repository / policy control plane — GitHub remains the repository of pinned policy and public-root metadata;
- organizational governance;
- hardware provenance;
- network path;
- transparency-log portability for the offline path;
- long-term multi-provider storage durability.

## Next falsifiable question

Can a producer outside the current GitHub workflow/repository authority generate evidence that binds to a provider-neutral logical producer identity and authorization contract, yet still reproduces the same trusted transition?

That is the next frontier: **Source-Producer + Control-Plane Portability v0.1**.
