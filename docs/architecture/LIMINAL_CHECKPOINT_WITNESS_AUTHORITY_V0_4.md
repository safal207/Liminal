# Checkpoint Witness Authority v0.4

## Status

Experimental fail-closed schema + authority-contract adapter for Upstream Rotation-Authority Portability. It is intentionally separate from witness v0.3 so already verified v0.3 history remains semantically stable.

## Why v0.4 exists

Witness v0.3 made checkpoint **producer identity** provider-neutral, but its accepted checkpoint semantics are still checkpoint v0.2 and its authority points at checkpoint producer/authorization contracts v0.1.

Checkpoint v0.3 introduces provider-neutral upstream rotation authority and new checkpoint fields (`rotation_authority`, `accepted_rotation`). Reusing the old producer-contract digest would incorrectly let a contract that explicitly names checkpoint v0.2 and the legacy rotation-result schema authorize new checkpoint-v0.3 semantics.

v0.4 therefore performs one explicit migration that binds both changes:

```text
witness v0.3 genesis
+ checkpoint v0.3 genesis
+ verified rotation-authority migration
+ verified checkpoint producer-contract migration
        ↓
witness v0.4 genesis
        ↓
same logical checkpoint producer role
new checkpoint producer contract v0.2
new checkpoint authorization contract v0.2
checkpoint schema = v0.3
```

## Checkpoint producer contracts

Historical contracts remain immutable:

- producer v0.1: checkpoint-v0.2 / legacy rotation-result semantics;
- authorization v0.1: binds producer-contract v0.1.

New contracts for checkpoint v0.3:

- `policies/portable-checkpoint-producer-contract-v0.2.json`;
- `policies/portable-checkpoint-authorization-contract-v0.2.json`.

Producer v0.2 explicitly binds:

- predecessor checkpoint schema `liminal.recovery-trust-consumer-checkpoint.v0.3`;
- Portable Rotation-Authority Receipt schema `liminal-rotation-authority-receipt/v0.1`;
- predecessor checkpoint digest;
- registry and manifest digests;
- rotation-authority receipt digest;
- one-generation advancement;
- verified rotation authority and exact rotation decision class.

Authorization v0.2 explicitly binds checkpoint schema v0.3 and requires an explicit authority migration.

## Migration evidence

`WitnessCheckpointSchemaMigrationEvidence` binds:

- trust domain;
- exact witness-v0.3 genesis SHA-256;
- exact historical checkpoint SHA-256 observed by that witness;
- exact checkpoint-v0.3 genesis SHA-256;
- exact checkpoint rotation-authority migration-claim SHA-256;
- logical checkpoint producer ID and evidence type;
- old producer-contract SHA-256;
- old authorization-contract SHA-256;
- new producer-contract SHA-256;
- new authorization-contract SHA-256.

The canonical migration claim excludes verifier output and `verified` status.

## Authority migration rule

The logical producer role is preserved, but contract authority is not silently reused.

```text
same logical_producer_id
same evidence_type
        +
old contract digests
        ↓ explicit verified migration
new contract digests
```

The v0.4 witness records both `from_checkpoint_authority` and `to_checkpoint_authority` in `checkpoint_schema_migration`. The active `checkpoint_authority` MUST equal `to_checkpoint_authority`.

The migration fails closed if:

- the old digests do not exactly match witness v0.3;
- the logical producer ID or evidence type changes;
- producer-contract digest is not migrated;
- authorization-contract digest is not migrated;
- checkpoint rotation-authority migration claim does not match checkpoint v0.3 origin.

## Advancement

After migration, witness v0.4 accepts only checkpoint v0.3 candidates.

Advancement requires:

1. valid checkpoint-v0.3 predecessor chain;
2. exact witness → checkpoint predecessor binding;
3. exact logical checkpoint producer ID;
4. exact **v0.2 checkpoint producer-contract** SHA-256;
5. exact **v0.2 checkpoint authorization-contract** SHA-256;
6. exact evidence type and generation;
7. exact checkpoint subject SHA-256;
8. externally established verified checkpoint authority evidence.

Old v0.1 checkpoint authority evidence is rejected after migration.

The witness does not verify cryptographic signatures itself.

## Causal stack after v0.4

```text
portable rotation authority
        ↓
checkpoint v0.3
        ↓
checkpoint producer contract v0.2
+ checkpoint authorization contract v0.2
        ↓
portable checkpoint producer authority
        ↓
witness v0.4
        ↓
trusted state transition
```

This separates four facts that were previously coupled:

- who may authorize registry rotation;
- what checkpoint construction semantics are authorized;
- who may satisfy the logical checkpoint producer role;
- what trusted state the witness accepts.

## Claim boundary

v0.4 is a schema/authority adapter, not a live portability proof. Upstream Rotation-Authority Portability is VERIFIED only after an independent rotation producer/control plane, the historical GitHub path, checkpoint-v0.3 construction, migrated checkpoint producer authority, witness-v0.4 transition, immutable verifier and independent recomputation all agree fail closed.
