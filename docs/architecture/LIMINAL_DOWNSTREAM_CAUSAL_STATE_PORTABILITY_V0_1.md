# Liminal Downstream Causal-State Portability Across Independent Histories v0.1

## Status

Implementation gate. This document records the Fractal Causal Refactoring diagnosis and the fail-closed semantic layer that removes concrete history bytes from downstream causal identity.

This document is **not** a VERIFIED proof claim by itself. VERIFIED status additionally requires an immutable reusable workflow, exact-head CI, a pinned one-shot caller, and an independent audit that reconstructs the result from proof artifact bytes.

## Fractal Causal Refactoring diagnosis

### Idea

A downstream checkpoint or witness should answer:

> What semantic trust state is currently authorized, and under which portable authority?

It should not answer:

> Which concrete registry, manifest, provider, or historical generation happened to establish that state?

The latter facts are provenance and evidence. They matter for verification, but they are not the logical identity of the downstream causal state.

### Project before refactoring

Checkpoint v0.3 and witness v0.4 successfully removed concrete signer/provider dependence from several authority paths, but their state identity still contains concrete historical values:

```text
accepted_registry_sha256
accepted_manifest_sha256
previous_checkpoint_sha256
checkpoint_sha256
previous_witness_sha256
```

The first two are history-specific. Once they enter a checkpoint, every downstream checkpoint and witness inherits that concrete history through the hash chain.

### Reality after Genesis / Historical Trust-Base Portability

Signal 011 established that two independently rooted histories can be simultaneously true:

```text
registry A != registry B
manifest A != manifest B
genesis authority A != genesis authority B

but

semantic trust state A == semantic trust state B
```

Therefore raw registry/manifest equality can no longer be treated as the identity criterion for portable downstream causal state.

### First Meaningful Divergence

The first meaningful divergence is **checkpoint v0.3**, not the witness.

The witness only inherits the checkpoint's decision. The earlier model error is that checkpoint v0.3 promotes provenance bytes into causal identity via `accepted_registry_sha256` and `accepted_manifest_sha256`.

Patching only witness v0.4 would preserve the wrong abstraction one level higher.

## Refactor point

Introduce a new primitive:

```text
CausalStateRef
```

Schema:

```text
schema
trust_domain
logical_state_id
causal_epoch
semantic_state_sha256
```

`CausalStateRef` deliberately excludes:

- provider ID;
- genesis authority ID;
- raw registry digest;
- raw manifest digest;
- historical generation number.

Those remain in path-specific verification evidence and in the portability receipt.

## Causal epoch versus historical generation

Historical generation is provenance. Two independent histories may require different numbers of internal steps to reach the same semantic state.

Therefore:

```text
historical_generation != causal_epoch
```

`causal_epoch` belongs to the portable downstream state machine. The initial convergence point is anchored at causal epoch `0`, regardless of whether History A reached it at generation 1 and History B at generation 9.

This prevents another accidental history dependency from entering through a counter instead of a digest.

## New downstream objects

### Causal checkpoint

```text
schema_version
state_ref
previous_checkpoint_sha256
checkpoint_authority
```

The checkpoint hashes only previous **portable checkpoints**. It does not carry registry or manifest identities.

### Causal witness

```text
schema_version
state_ref
checkpoint_sha256
previous_witness_sha256
witness_authority
```

The witness binds to the portable checkpoint and semantic state reference, not to historical registry/manifest bytes.

## Portability proof invariant

Given independently verified historical observations A and B:

```text
provider A != provider B
genesis authority A != genesis authority B
registry A != registry B
manifest A != manifest B
semantic state A == semantic state B
```

then the downstream layer must produce:

```text
CausalStateRef(A) == CausalStateRef(B)
Checkpoint(A)     == Checkpoint(B)
Witness(A)        == Witness(B)
```

and none of the raw historical identities may occur anywhere inside the portable checkpoint or witness.

## Fail-closed cases

The implementation rejects:

- invalid historical observations;
- unverified historical observations;
- the same history provider on both paths;
- the same genesis authority on both paths;
- the same terminal registry on both paths;
- the same terminal manifest on both paths;
- trust-domain divergence;
- terminal semantic-state divergence;
- invalid checkpoint or witness authority contracts;
- invalid logical state IDs or causal epochs;
- any raw history identity embedded into portable downstream objects;
- any checkpoint or witness byte divergence between the two paths.

## Time traversal

### Past

Legacy downstream state was correct while the system assumed one concrete historical predecessor.

### Present

Independent histories now converge semantically, exposing the fact that raw predecessor identity is too strong for downstream portability.

### Future

Without this refactor, every new provider, genesis root, recovery path, or historical reconstitution would force a new raw checkpoint/witness lineage even when the currently authorized semantic state is identical.

With `CausalStateRef`, new historical paths can converge into one portable downstream causal chain after their provenance has been independently verified.

## Upward verification

After the refactor:

```text
historical cryptographic verification
        ↓
independent history validation
        ↓
semantic trust-state convergence
        ↓
CausalStateRef
        ↓
portable checkpoint
        ↓
portable witness
```

History is still verified. It is simply no longer confused with downstream identity.

## Claim boundary

This implementation separates **history provenance** from **downstream causal identity** at the semantic layer.

It does not by itself establish:

- organizational-governance independence;
- hardware or storage independence;
- network-path independence;
- universal provider independence;
- indefinite durability;
- an immutable public proof run.

Those remain separate evidence and governance questions.
