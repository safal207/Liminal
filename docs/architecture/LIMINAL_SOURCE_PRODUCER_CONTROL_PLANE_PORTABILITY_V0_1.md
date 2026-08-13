# Source-Producer + Control-Plane Portability v0.1

## Status

Experimental fail-closed portability contract. The comparison model is implemented; no live source/control-plane portability claim is valid until an external producer and an independent control plane actually produce and authorize the tested evidence path.

## Goal

Prove that a trusted transition is not an accidental property of one checkpoint producer implementation or one repository/policy control plane.

```text
path A
GitHub workflow producer
+ GitHub repository/policy control plane
        ↓
verified source/control claims

path B
independent producer
+ independent control plane
        ↓
verified source/control claims

A == B on portable semantics
        ↓
same trusted transition
```

## Why this gate exists

Execution-Provider + Evidence-Transport Portability v0.1 proved that the same transition survived movement from GitHub-hosted execution + GitHub artifact transport to an OpenAI/ChatGPT workspace + Google Drive transport.

That proof intentionally retained a shared source lineage:

- the checkpoint bytes were originally produced by the GitHub checkpoint workflow;
- the producer revision and repository were still part of the earlier Portable Trust Receipt;
- authorization policy remained anchored in the GitHub repository control plane.

Therefore execution and transport portability did not yet prove producer or control-plane portability.

## Critical safety constraint

The existing witness v0.2 pins a concrete GitHub workflow path and SHA as `checkpoint_signer`.

An external producer MUST NOT be relabeled as that GitHub signer merely to satisfy the legacy witness adapter.

```text
external producer
        ≠
legacy GitHub signer identity
```

Doing so would manufacture authority instead of proving portability.

This v0.1 layer therefore compares producer/control-plane observations without converting the external producer into legacy `VerifiedCheckpointEvidence`.

## Observation model

`SourceControlObservation` separates concrete infrastructure identity from portable semantics.

### Audit / independence metadata

- `producer_provider`
- `producer_instance_id`
- `control_plane_provider`
- `control_plane_id`

These fields prove that the two tested paths are materially distinct. They are deliberately excluded from portable receipt identity.

### Portable transition semantics

- `subject_sha256`
- `logical_producer_id`
- `producer_contract_sha256`
- `authorization_contract_sha256`
- `evidence_type`
- `generation`
- `witness_reason`
- `next_witness_sha256`
- `verified`

These fields must match exactly across independent source/control paths.

## Logical producer identity

`logical_producer_id` names the semantic producer role, not a vendor workflow or repository path.

Example:

`liminal:trusted-recovery-checkpoint-producer`

A concrete GitHub workflow and a non-GitHub producer may both implement that role only if each independently satisfies the same producer contract.

## Producer contract

`producer_contract_sha256` binds the rules for constructing the evidence subject.

The contract should eventually specify at least:

- input evidence identities;
- canonical serialization;
- generation transition rules;
- anti-rollback constraints;
- required invariants;
- output evidence type;
- deterministic subject construction.

Changing those semantics changes the contract digest and therefore breaks portable agreement.

## Authorization contract

`authorization_contract_sha256` binds the local rules that allow a logical producer output to participate in a trusted transition.

This is distinct from producer construction logic.

```text
producer contract
    = what valid evidence must be

authorization contract
    = under which verified conditions that evidence may advance trust state
```

A producer cannot choose its own authorization contract.

## Portable Source-Control Receipt v0.1

Canonical schema:

`liminal-source-control-receipt/v0.1`

Receipt identity excludes concrete provider names and instance IDs.

```text
subject identity
+ logical producer identity
+ producer contract identity
+ authorization contract identity
+ evidence type
+ generation
+ witness decision class
+ next trusted-state identity
+ successful verification
        ↓
Portable Source-Control Receipt
```

## Independence requirements

Agreement is impossible unless:

1. `producer_provider` differs;
2. `control_plane_provider` differs;
3. both observations were externally established as verified.

A different workflow SHA inside the same producer authority is not sufficient.

A fork, branch, alternate repository path or copied policy file is not automatically an independent control plane. The authority that can define or mutate the active authorization contract must be materially distinct for the tested secondary path.

## Fail-closed agreement

After independence checks, these values must match exactly:

1. subject SHA-256;
2. logical producer ID;
3. producer-contract SHA-256;
4. authorization-contract SHA-256;
5. evidence type;
6. generation;
7. witness reason;
8. next-witness SHA-256;
9. canonical Portable Source-Control Receipt SHA-256.

Any mismatch rejects portability.

## Trust boundary

`source_control_portability.py` is a comparison layer only.

It never:

- runs a producer;
- authenticates producer identity;
- proves control-plane independence from labels;
- verifies cryptographic signatures;
- grants checkpoint authority;
- rewrites an external producer into the GitHub signer pinned by witness v0.2;
- chooses a winner when source/control semantics disagree.

`verified=True` must be established outside this module.

## Proposed first live experiment

The next live proof should use a secondary path whose subject is produced from canonical input evidence outside the GitHub checkpoint workflow.

Candidate shape:

```text
primary producer:
immutable GitHub checkpoint workflow

primary control plane:
GitHub repository + pinned policy bytes

secondary producer:
OpenAI/ChatGPT isolated workspace producer

secondary control plane:
independently signed portable producer + authorization contract bundle
whose signing key is unavailable to GitHub Actions
```

Procedure:

1. freeze a canonical Producer Contract v0.1;
2. freeze a canonical Authorization Contract v0.1;
3. bind both by SHA-256;
4. provide the same bounded input evidence to both producers;
5. have the secondary producer construct the checkpoint subject independently rather than copy the primary output;
6. cryptographically bind secondary output to the independent control-plane contract;
7. verify each path independently;
8. compare subject and portable source/control semantics;
9. evaluate transition semantics without impersonating the legacy GitHub signer;
10. fail closed on any subject, contract, generation, or transition drift.

## Migration requirement for witness authority

A full live proof cannot honestly stop at the v0.2 witness signer model because that model encodes a GitHub workflow as authority identity.

The next implementation stage should therefore introduce a provider-neutral witness authority representation, for example:

```text
checkpoint_authority:
  logical_producer_id
  producer_contract_sha256
  authorization_contract_sha256
```

Concrete signer/provider observations remain evidence about that authority, not the authority identity itself.

A compatibility adapter may preserve old witnesses, but it must never make a new external producer appear to have been signed by the legacy GitHub workflow.

## Claim boundary

Until the live experiment and provider-neutral witness authority are complete, this milestone proves only that the comparison semantics are explicit and fail closed.

It does not yet prove source-producer or control-plane independence in production or CI.

## Next falsifiable question

Can two materially distinct producers, governed by materially distinct control planes, independently construct and authorize the same evidence subject under the same provider-neutral producer and authorization contracts and reproduce the same trusted transition without either path impersonating the other's concrete signer identity?
