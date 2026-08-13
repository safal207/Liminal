# Upstream Rotation-Authority Portability v0.1

## Status

Experimental fail-closed model. No live portability claim is valid until an independent rotation producer and independent rotation control plane have produced and authorized the same rotation semantics and the result has passed immutable external verification.

## Goal

Remove the remaining causal dependency on the existing GitHub rotation authority.

```text
primary path
immutable GitHub rotation producer
+ GitHub rotation control plane
        ↓
verified rotation authority A

secondary path
independent rotation producer
+ independent signed control plane
        ↓
verified rotation authority B

A == B on portable semantics
        ↓
same checkpoint-v0.3 state
        ↓
checkpoint producer authority v0.2
        ↓
same witness-v0.4 trusted transition
```

The historical GitHub rotation workflow `e2cb6a014236bc561d03c405f4986146026041fa` remains immutable evidence. It is not rewritten or re-labelled as the independent authority.

## Portable rotation identity

Concrete signer/provider identity is evidence about authority, not the authority identity itself.

Portable identity binds:

- `logical_rotation_id`;
- `rotation_contract_sha256`;
- `authorization_contract_sha256`;
- previous registry SHA-256;
- current registry SHA-256;
- previous manifest SHA-256;
- current manifest SHA-256;
- source generation;
- target generation;
- `registry_rotation_authorized` decision class.

Concrete `rotation_producer_provider`, producer instance, control-plane provider and control-plane ID are excluded from the portable receipt and retained only as audit/independence metadata.

## Independence requirements

Portability cannot be accepted unless:

1. the rotation producer providers differ;
2. the rotation control-plane providers differ;
3. both observations were cryptographically established outside the comparison module;
4. both paths bind the exact same predecessor registry and manifest;
5. both paths bind the exact same next registry and manifest;
6. both paths implement the same rotation producer and authorization contract digests;
7. both authorize exactly one generation advance;
8. both return `registry_rotation_authorized`;
9. both feed the same Portable Rotation-Authority Receipt into checkpoint v0.3 construction;
10. downstream checkpoint producer authority is explicitly migrated to contracts that actually describe checkpoint v0.3 semantics.

A second workflow SHA inside the same authority is not sufficient independence.

## Rotation producer contract

`portable-rotation-producer-contract-v0.1.json` defines the deterministic semantic transition:

- one-generation increment;
- append-only registry history;
- exact predecessor binding;
- exact manifest binding;
- canonical JSON serialization;
- root downgrade rejection;
- policy-material downgrade rejection.

The producer contract describes what a valid rotation result must be. It does not grant authority.

## Rotation authorization contract

`portable-rotation-authorization-contract-v0.1.json` defines when a verified rotation result can count as an authorized causal input.

The producer cannot choose its own authorization contract. The control plane that signs or otherwise authenticates that contract must be independently established.

## Portable Rotation-Authority Receipt

Schema:

`liminal-rotation-authority-receipt/v0.1`

The receipt excludes concrete providers and contains only the portable semantics listed above. If two independently verified paths agree, canonical receipt bytes and receipt SHA-256 must be identical.

## Downstream checkpoint contract migration

A model review exposed an important semantic mismatch before live proof: checkpoint producer contract v0.1 explicitly named checkpoint v0.2 and the legacy rotation-result schema, so it could not honestly authorize checkpoint v0.3 merely because the logical producer role stayed the same.

The live chain therefore uses:

- `portable-checkpoint-producer-contract-v0.2.json`;
- `portable-checkpoint-authorization-contract-v0.2.json`;
- witness v0.4 with an explicit old-authority → new-authority contract migration.

The logical checkpoint producer identity and evidence type remain stable, while producer/authorization contract digests change explicitly and are bound into the canonical witness migration claim.

This prevents a schema change from silently inheriting authority from a contract that described different semantics.

## Trust boundary

`rotation_authority_portability.py` is comparison-only. It does not:

- run a rotation producer;
- verify signatures;
- infer control-plane independence from labels;
- manufacture `verified=True`;
- authorize a transition from its own post-transition receipt;
- choose one provider when semantics disagree.

Checkpoint v0.3 and witness v0.4 likewise consume already verified authority evidence; they do not perform cryptographic verification internally.

All mismatches fail closed.

## Live proof plan

1. Freeze the rotation producer contract and rotation authorization contract.
2. Compute their canonical SHA-256 identities.
3. Keep `e2cb6a...` as the immutable primary rotation path.
4. Create an external rotation control-plane root whose private key is unavailable to GitHub Actions.
5. Sign the exact portable rotation contracts and predecessor/target rotation claim with that external control-plane root.
6. Give a standalone external producer only predecessor registry/manifest inputs plus the signed control-plane claim.
7. Require the external producer to independently construct the current registry/manifest and Portable Rotation-Authority Receipt rather than copy primary output.
8. Sign the external producer result with a separate producer root unavailable to GitHub Actions.
9. Verify both roots and signatures in an immutable GitHub verifier.
10. Recompute both rotation observations and require exact Portable Rotation-Authority Receipt equality.
11. Migrate the historical checkpoint genesis to checkpoint v0.3 under provider-neutral rotation authority.
12. Explicitly migrate checkpoint producer/authorization authority from v0.1 contracts to the v0.2 contracts that describe checkpoint v0.3 semantics.
13. Feed each authorized rotation path into checkpoint-v0.3 generation and require identical checkpoint subjects.
14. Advance witness v0.4 with externally verified checkpoint authority evidence and require the same trusted transition.
15. Run a second audit job that does not trust the first result flag and independently repeats signer checks and semantic recomputation.

## Pre-live correction record

The first witness-v0.4 candidate said checkpoint producer authority was unchanged across checkpoint schema migration. That candidate was never used for a live proof. Review found that the historical producer contract explicitly bound checkpoint-v0.2 inputs, so reusing its digest for checkpoint v0.3 would have overextended authority.

The active candidate corrects this by preserving the logical producer role while explicitly migrating producer and authorization contract identities. No historical verified proof is rewritten.

## Claim boundary

Until the live experiment succeeds, this milestone proves only that the rotation-authority comparison semantics and fail-closed causal chain are explicit.

Even after a successful first live proof, it would not by itself prove organizational governance independence, hardware provenance independence, network-path independence, or universal provider independence.
