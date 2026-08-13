# Checkpoint Witness Authority v0.3

## Status

Experimental fail-closed authority model for Source-Producer + Control-Plane Portability.

This document defines the model only. It does **not** rotate any immutable witness workflow, trust root, producer, or control-plane policy. A live workflow must not be pinned until the v0.3 implementation passes exact-head Python CI, Python Integration and Artillery.

## Problem

Checkpoint witness v0.1/v0.2 correctly binds checkpoint advancement to a concrete GitHub workflow signer:

```text
checkpoint_signer
  = workflow_path
  + workflow_sha
```

That historical model is strong inside one producer/control plane, but it cannot represent producer portability. If an external producer is simply relabeled as the GitHub workflow signer, the system has not demonstrated portability; it has forged the authority model.

The required split is:

```text
logical authority
        !=
concrete proof of authority
```

## v0.3 authority identity

Checkpoint Witness Authority v0.3 binds advancement to:

```text
logical_producer_id
+ producer_contract_sha256
+ authorization_contract_sha256
+ evidence_type
```

Concrete producer/provider identities, signer schemes, repositories and control-plane locations remain evidence about that logical authority. They are not copied into the portable authority identity.

The v0.3 authority schema is:

`liminal.checkpoint-authority/v0.1`

```json
{
  "schema": "liminal.checkpoint-authority/v0.1",
  "logical_producer_id": "liminal.trusted-recovery.checkpoint-producer",
  "producer_contract_sha256": "<sha256>",
  "authorization_contract_sha256": "<sha256>",
  "evidence_type": "trusted-recovery-consumer-checkpoint"
}
```

## Explicit migration from v0.1/v0.2

v0.3 is not allowed to appear as an uncaused replacement for a legacy witness.

The first v0.3 witness is created only through `migrate_legacy_genesis_witness_to_v3(...)`, which requires externally verified migration evidence binding:

- the exact legacy witness SHA-256;
- the exact legacy signer workflow path;
- the exact legacy signer workflow SHA;
- the trust domain;
- the logical producer identity;
- the producer contract SHA-256;
- the authorization contract SHA-256;
- the evidence type.

The migration is deliberately restricted to a validated generation-0 legacy witness. Later generations must advance through the normal v0.3 predecessor chain rather than being independently re-rooted.

### Portable migration identity

The witness must **not** bind raw verifier output or a verifier-specific verification-record digest into its identity. Doing that would recreate the verifier-output coupling removed by Normalized Verification Receipt.

Instead, v0.3 computes:

`migration_claim_sha256`

from canonical semantic mapping fields only:

```text
legacy witness identity
+ exact legacy signer pin
+ trust domain
+ logical producer id
+ producer contract digest
+ authorization contract digest
+ evidence type
+ migration reason
        ↓
canonical migration claim SHA-256
```

`verified` is deliberately excluded from this claim identity. External verification still must succeed before migration is authorized, but two verifier implementations that establish the same mapping can produce the same v0.3 witness root even when their raw verification outputs differ.

The new witness retains an immutable `authority_origin` record:

```text
legacy witness hash
+ legacy signer pin
+ migration reason
+ migration claim digest
        ↓
logical authority contract
```

This preserves causality while removing the concrete signer from future authority identity.

## Candidate authorization evidence

`PortableCheckpointAuthorityEvidence` contains only provider-neutral claims already established by an external verifier:

- `verified`;
- `subject_sha256`;
- `logical_producer_id`;
- `producer_contract_sha256`;
- `authorization_contract_sha256`;
- `evidence_type`;
- `generation`.

It deliberately excludes:

- GitHub workflow path;
- GitHub workflow SHA;
- producer provider;
- control-plane provider;
- signature scheme;
- verification implementation.

Those facts remain in the verification evidence that produced the authority observation. They do not define the logical authority consumed by the witness.

## Fail-closed advancement

A generation advance is rejected when any of these conditions holds:

- trusted v0.3 witness body invalid;
- candidate checkpoint invalid;
- stale checkpoint;
- same-generation conflict;
- generation gap;
- predecessor checkpoint missing;
- predecessor hash does not match the trusted witness;
- authority evidence missing or malformed;
- authority evidence not externally verified;
- candidate subject digest mismatch;
- authority-evidence generation mismatch;
- logical producer mismatch;
- producer contract mismatch;
- authorization contract mismatch;
- evidence type mismatch.

Only exact agreement produces:

```text
authorized = true
reason = checkpoint_witness_advanced
```

The next v0.3 witness carries the same logical authority and authority-origin record and links to the previous v0.3 witness by canonical SHA-256.

## Historical compatibility

This work does not modify the existing v0.1/v0.2 witness implementation or historical witness JSON.

```text
v0.1 / v0.2
concrete signer authority
        ↓
explicit verified migration
        ↓
v0.3
logical producer/authorization authority
```

Historical proofs therefore remain reproducible under their original authority semantics.

## Important non-circularity rule

The post-transition `PortableSourceControlReceipt` from Source-Producer + Control-Plane Portability includes `witness_reason` and `next_witness_sha256`. It is therefore an **output comparison receipt**, not an input authorization credential.

v0.3 must not consume that post-transition receipt to authorize the same transition. Doing so would create a circular proof.

Instead:

```text
pre-transition verified authority evidence
        ↓
v0.3 witness decision
        ↓
witness_reason + next_witness
        ↓
SourceControlObservation
        ↓
PortableSourceControlReceipt
```

This causal ordering is mandatory.

## Trust boundary

`checkpoint_witness_authority_v3.py` does not:

- verify signatures;
- determine whether two producer providers are truly independent;
- determine whether two control planes are organizationally independent;
- issue producer authority;
- infer authority from a provider label;
- mutate permanent policy;
- rotate an immutable workflow anchor.

It only consumes already-verified authority claims and evaluates them against an already-trusted v0.3 witness.

## Next verification sequence

1. Exact-head CI for the v0.3 model and tests.
2. Materialize the real producer-contract and authorization-contract digests into a canonical migration claim.
3. Verify that claim through independent proof paths without changing `migration_claim_sha256`.
4. Produce a signed external producer/control-plane observation outside the current GitHub producer authority.
5. Reproduce the same candidate checkpoint bytes from both producer paths.
6. Feed independently verified pre-transition authority evidence into the same v0.3 witness root.
7. Require the same `checkpoint_witness_advanced` decision and the same v0.3 next-witness SHA-256.
8. Compare the two post-transition `SourceControlObservation` values.
9. Only after exact agreement, create/pin a live immutable workflow proof.

## Falsifiable question

Can two materially different producer/control-plane authorities independently prove the same logical producer and authorization contracts, produce the same checkpoint subject, and advance the same v0.3 witness to the exact same next-witness digest?

If yes, checkpoint authority is no longer a property of one GitHub workflow identity. It becomes a portable contract with independently provable implementations.
