# Checkpoint Witness Authority v0.4

## Status

Experimental schema adapter for Upstream Rotation-Authority Portability. It is intentionally separate from witness v0.3 so already verified v0.3 history remains semantically stable.

## Why v0.4 exists

Witness v0.3 made checkpoint **producer** authority provider-neutral, but its candidate validation still consumes historical checkpoint v0.2.

Checkpoint v0.3 makes upstream **rotation** authority provider-neutral. Silently changing witness v0.3 to accept the new checkpoint schema would retroactively change an already verified witness protocol.

v0.4 therefore introduces an explicit checkpoint-schema migration:

```text
witness v0.3 genesis
+ checkpoint v0.3 genesis
+ verified schema-migration mapping
        ↓
witness v0.4 genesis
        ↓
checkpoint producer authority unchanged
checkpoint schema = v0.3
```

## Migration evidence

Migration binds:

- trust domain;
- exact witness-v0.3 genesis SHA-256;
- exact historical checkpoint SHA-256 observed by that witness;
- exact checkpoint-v0.3 genesis SHA-256;
- exact checkpoint rotation-authority migration-claim SHA-256.

The canonical schema-migration claim excludes verifier output and `verified` status.

## Authority preservation

`checkpoint_authority` and its historical `authority_origin` are copied exactly from the stronger witness-v0.3 root. The migration changes only which checkpoint schema represents the accepted state.

Therefore:

```text
checkpoint producer authority before migration
        ==
checkpoint producer authority after migration
```

No new producer authority is granted by changing checkpoint schema.

## Advancement

After migration, witness v0.4 accepts only checkpoint v0.3 candidates.

Advancement requires:

1. valid checkpoint-v0.3 predecessor chain;
2. exact witness → checkpoint predecessor binding;
3. exact logical checkpoint producer ID;
4. exact checkpoint producer-contract SHA-256;
5. exact checkpoint authorization-contract SHA-256;
6. exact evidence type and generation;
7. exact checkpoint subject SHA-256;
8. externally established verified checkpoint authority evidence.

The witness does not verify cryptographic signatures itself.

## Causal stack after v0.4

```text
portable rotation authority
        ↓
checkpoint v0.3
        ↓
portable checkpoint producer authority
        ↓
witness v0.4
        ↓
trusted state transition
```

This separates three different facts that were previously coupled to GitHub workflow identities:

- who may authorize registry rotation;
- who may construct/authorize the checkpoint;
- what trusted state the witness accepts.

## Claim boundary

v0.4 is a schema adapter, not a live portability proof. Upstream Rotation-Authority Portability is VERIFIED only after an independent rotation producer/control plane, the historical GitHub path, checkpoint-v0.3 construction, witness-v0.4 transition, immutable verifier and independent recomputation all agree fail closed.
