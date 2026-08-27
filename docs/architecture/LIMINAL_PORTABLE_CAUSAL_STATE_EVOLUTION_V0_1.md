# Portable Causal-State Evolution / Multi-Epoch Convergence v0.1

## Status

Implementation gate. Proof workflow follows only after exact-head tests pass.

## First Meaningful Divergence

Downstream Causal-State Portability v0.1 established an epoch-0 portable anchor and proved that two independently rooted histories produce identical downstream checkpoint/witness bytes.

The next failure appears when that anchor is advanced beyond one step.

The original single-link validator answers two questions at once:

1. is this checkpoint/witness object structurally valid?
2. is its full predecessor chain valid back to the trusted anchor?

For epoch 1, one predecessor object is enough. For epoch 2, the predecessor itself has a predecessor, but the API no longer carries that context. A recursive call therefore sees an epoch-1 object without epoch-0 context and rejects it.

This is the first meaningful divergence:

```text
intended model
trusted anchor -> epoch 1 -> epoch 2 -> ... -> epoch N

old validator context
current object + one predecessor
```

The fix is not to add ad-hoc `previous_previous` parameters. Chain validity becomes a first-class operation.

## Causal model

Historical provenance and portable causal identity remain distinct.

```text
History A: generation 1 -> 3 -> 4
History B: generation 9 -> 10 -> 13
                       |
                       v
Portable causal epochs: 0 -> 1 -> 2
```

Historical generation may advance by different amounts on each path. Causal epoch advances exactly once per portable logical transition.

## Portable transition identity

A portable transition reference contains only semantic transition facts:

```text
CausalTransitionRef
  trust_domain
  logical_state_id
  logical_transition_id
  from_causal_epoch
  to_causal_epoch
  from_state_ref_sha256
  to_state_ref_sha256
  transition_contract_sha256
  authorization_contract_sha256
```

It deliberately excludes:

- provider IDs;
- genesis authority IDs;
- registry digests;
- manifest digests;
- history generation numbers;
- path-specific transition provenance digests.

Those remain available in the proof receipt for audit.

## Checkpoint v0.2

```text
schema_version
state_ref
transition_ref
previous_checkpoint_sha256
checkpoint_authority
```

The transition reference binds the next state to the previous portable state and to the logical transition contract. The predecessor digest binds the portable checkpoint prefix.

## Witness v0.2

```text
schema_version
state_ref
transition_ref_sha256
checkpoint_sha256
previous_witness_sha256
witness_authority
```

The witness binds both the checkpoint and the portable transition that produced it.

## Chain validation

Two validators are explicit:

- `validate_evolution_checkpoint_chain(anchor, checkpoints)`
- `validate_evolution_witness_chain(anchor_witness, anchor_checkpoint, checkpoints, witnesses)`

Both validate every link from epoch 0 to the terminal epoch. No intermediate checkpoint or witness is accepted merely because its immediate child points to it.

## Invariants

1. The epoch-0 anchor must already be a valid history-free portable checkpoint/witness.
2. Path A and Path B remain distinct in provider, genesis authority, registry, manifest, and transition provenance.
3. Each historical path is prefix-continuous in its own provenance.
4. Both paths start from the same portable semantic anchor.
5. Each causal transition advances exactly one causal epoch.
6. History generation is not used as causal identity.
7. Both paths agree on logical transition ID and transition/authorization contracts.
8. Both paths agree on the next semantic state.
9. Every portable checkpoint/witness byte sequence is identical across the two paths.
10. Raw history/provenance identities never appear inside portable transition/checkpoint/witness objects.
11. The full checkpoint and witness prefixes validate back to epoch 0.

## Fail-closed cases

The implementation rejects:

- invalid/unverified transition observations;
- mismatched transition sequence lengths;
- broken historical prefixes;
- shared path transition provenance where independence is required;
- semantic predecessor divergence;
- semantic successor divergence;
- logical transition mismatch;
- transition contract mismatch;
- authorization contract mismatch;
- raw history smuggled into a portable transition ID;
- checkpoint prefix tampering;
- witness prefix tampering.

## Claim boundary

This layer proves portable multi-epoch causal evolution **given independently verified history-bound transition observations**.

It does not by itself prove that every future historical registry transition is produced by independent organizations, hardware, storage, or network paths. Those provenance claims require their own producer/signature evidence and remain separately auditable.

The next proof workflow must consume the immutable epoch-0 downstream portability proof and independently reproduce at least two later causal epochs before this gate can be marked VERIFIED.
