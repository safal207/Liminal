# Witness Authority v0.3

## Status

Experimental compatibility layer for source-producer and control-plane portability.

v0.1/v0.2 witness formats remain historical and valid for their proven scopes. v0.3 does not rewrite those records. It introduces a new authority identity for future portable transitions.

## Problem

The v0.2 checkpoint witness binds authority to a concrete GitHub Actions signer:

```text
checkpoint_signer =
  workflow_path
  + workflow_sha
```

That is strong for an immutable GitHub workflow, but it prevents an independently controlled producer from representing the same authorized logical producer contract without pretending to be that GitHub signer.

For source-producer/control-plane portability, that would be the wrong abstraction:

```text
concrete signer/provider != logical production authority
```

The signer is evidence about authority. It is not necessarily the portable authority identity itself.

## v0.3 authority identity

v0.3 replaces `checkpoint_signer` with:

```text
checkpoint_authority =
  logical_producer_id
  + producer_contract_sha256
  + authorization_contract_sha256
  + evidence_type
```

Concrete provider, repository, workflow, key, signature scheme, execution substrate and control-plane location remain upstream audit/verification evidence. They do not silently become the portable authority identity.

## Witness schema

`liminal.recovery-trust-consumer-checkpoint-witness.v0.3`

Canonical body:

```text
schema_version
trust_domain
observed_generation
checkpoint_sha256
accepted_registry_sha256
accepted_manifest_sha256
previous_witness_sha256
checkpoint_authority
  logical_producer_id
  producer_contract_sha256
  authorization_contract_sha256
  evidence_type
authority_migration
  from_schema
  from_witness_sha256
  reason
```

The v0.3 witness intentionally has no `checkpoint_signer` field.

## Migration rule

A v0.2 witness cannot simply be relabelled as v0.3.

Migration requires `VerifiedAuthorityMigrationEvidence` bound to the exact SHA-256 of the trusted v0.2 predecessor:

```text
valid v0.2 witness
        +
verified migration evidence
        +
exact predecessor witness digest
        +
explicit logical checkpoint authority
        ↓
v0.3 witness
```

Required migration reason:

`concrete_signer_to_logical_producer_authority`

The migration layer performs no cryptographic verification itself. `verified=True` must be established externally by the live proof workflow or another verifier.

Failure to verify the migration, predecessor-digest drift, invalid authority contracts or an invalid v0.2 predecessor all fail closed.

## Candidate evidence

Future candidate checkpoints use `VerifiedPortableCheckpointEvidence`:

```text
verified
subject_sha256
logical_producer_id
producer_contract_sha256
authorization_contract_sha256
evidence_type
```

The evidence contains no GitHub workflow path/SHA.

A concrete GitHub/Sigstore producer and an independent offline/external producer may therefore produce different provider-specific evidence while converging on the same portable authority claims.

## Advancement rule

For generation `N → N+1`, v0.3 requires:

1. valid current v0.3 witness;
2. exact checkpoint generation increment;
3. valid checkpoint chain to the witnessed predecessor;
4. verified candidate evidence;
5. exact candidate subject digest;
6. exact logical producer ID;
7. exact producer-contract digest;
8. exact authorization-contract digest;
9. exact evidence type.

Any mismatch is rejected deterministically.

Successful advancement preserves the same `checkpoint_authority` and links the next witness to the current v0.3 witness digest.

## Separation of authority and evidence

```text
provider-specific proof A ─┐
                           ├→ portable producer claims
provider-specific proof B ─┘
                                  ↓
                        checkpoint_authority
                                  ↓
                         witness authorization
                                  ↓
                          trusted transition
```

This means:

- a provider cannot choose its own authority merely by asserting metadata;
- changing providers does not require changing authority when the same independently verified contracts are satisfied;
- changing a producer or authorization contract changes authority and must fail closed unless explicitly migrated;
- a GitHub workflow SHA remains useful evidence and historical lineage, but is not the portable authority identity in v0.3.

## Safety boundary

`recovery_trust_consumer_checkpoint_witness_v3.py` does **not**:

- cryptographically verify a provider or signer;
- prove source-producer independence;
- prove control-plane independence;
- create or authorize migration evidence;
- infer logical producer identity from a provider name;
- allow a provider-specific signer to overwrite local authority contracts;
- mutate the v0.1/v0.2 witness history.

It only evaluates already-established portable authority evidence.

## Compatibility

v0.1/v0.2 remain unchanged.

The only supported cross-schema transition in v0.3 is the explicit migration entry point:

`migrate_witness_v2_to_v3(...)`

Once in v0.3, future witness links are v0.3-to-v0.3.

## Next live gate

The live Source-Producer + Control-Plane Portability proof should:

1. start from the currently trusted immutable v0.2 witness;
2. create and independently verify one explicit authority-migration record;
3. derive the same logical producer/authorization contracts for the existing GitHub producer path and an independently controlled producer path;
4. have both paths produce/verify the same checkpoint subject semantics;
5. map both to identical `VerifiedPortableCheckpointEvidence`;
6. advance the same v0.3 witness;
7. require the same next-witness digest;
8. fail closed if producer/control-plane independence or contract equality is absent.

Only after that live proof may source-producer/control-plane portability be marked verified.
