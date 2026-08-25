# Repeated Fork / Reconciliation Lineage Compaction v0.1

## Intent

A reconciled portable causal state must be able to fork and reconcile again without
copying every earlier branch object into the next portable tip.

The target property is not deletion of history. It is separation of:

```text
raw proof material          evidence needed to reproduce an earlier gate
immediate parent lineages   the two parents of the current reconciliation
lineage accumulator         a bounded recursive commitment to earlier joins
```

## Fractal Causal Refactoring diagnosis

### Idea

```text
verified reconciled tip
        ↓
second fork
        ↓
second reconciliation
        ↓
both current parents preserved
+
all earlier joins remain committed
+
portable tip shape remains bounded
```

### Previous project model

The first fork gate produced a correct two-parent DAG join. Its reconciliation
checkpoint preserved both immediate parents.

However, the entry point for the next cycle still assumed that the common tip was
a linear `anchor + evolution[]` prefix. A reconciliation checkpoint was not a typed,
compactly reusable verified tip.

### First Meaningful Divergence

The divergence was not the size of a SHA-256 digest and not the two immediate
parents. It was the absence of a portable proof state for an already verified DAG
tip:

```text
lineage preservation
        was conflated with
replaying the complete predecessor proof
```

Relaxing validation would allow an arbitrary digest to masquerade as a trusted tip.
Copying the complete predecessor proof into every later portable result would make
lineage payload grow recursively.

## Refactor

### `CausalLineageEvent`

A fixed-shape event summarizes one verified two-parent reconciliation:

- reconciliation sequence number;
- common tip state/checkpoint/witness digests;
- canonical parent-set digest;
- reconciliation-ref digest;
- result state/checkpoint/witness digests;
- semantic target;
- reconciliation and authorization contracts;
- lineage-compaction contract.

It does not contain provider identities, signer identities, vote provenance, or raw
parent-lineage objects.

### `CausalLineageAccumulator`

A fixed-shape recursive commitment contains:

- fixed-width reconciliation and branch counters;
- previous accumulator digest;
- previous lineage-root digest;
- latest event digest;
- newly derived lineage-root digest;
- current state/checkpoint/witness tip digests;
- compaction-contract digest.

The root transition is:

```text
lineage_root[n] = SHA256(
    previous_lineage_root
  + lineage_event[n]
  + fixed-width reconciliation counter
  + compaction contract
)
```

The accumulator is accepted only when an attested predecessor result and the exact
checkpoint/witness tip bind the same digests.

## Tested topology

```text
cycle 1
  common epoch 2
      ├─ branch A1 epoch 3 ─┐
      └─ branch B1 epoch 3 ─┴─ reconciliation epoch 4
                                  ↓
                           accumulator #1
                                  ↓
cycle 2
  reconciled epoch 4
      ├─ branch A2 epoch 5 ─┐
      └─ branch B2 epoch 5 ─┴─ reconciliation epoch 6
                                  ↓
                           accumulator #2
```

The implementation falsification suite also advances a synthetic third cycle to
epoch 8 and checks that the accumulator byte length remains unchanged.

## Evidence compaction boundary

The compact proof result carries only:

- the predecessor result object;
- its signer-verification record;
- its independent-audit summary;
- the previous fixed-shape lineage event and accumulator.

It does not recursively embed the predecessor proof's nested raw materials. The
independent audit downloads the predecessor proof as a separate artifact, reverifies
it, and recomputes the compact result.

This establishes bounded portable-tip and compact-result shape for the tested
second cycle. It does not establish constant-time verification from genesis, because
a verifier still relies on the attested predecessor result or must retrieve earlier
proof artifacts.

## Fail-closed properties

The gate rejects:

- an accumulator not bound to the exact reconciled tip;
- a modified previous lineage root or event digest;
- non-independent branch provider, authority, or provenance;
- duplicate branch identity or non-divergent branch semantics;
- branch evidence not descending from the exact compacted tip;
- vote/branch binding mismatch;
- reconciliation target, contract, or authorization disagreement;
- missing, duplicated, or non-canonical parents;
- accumulator shape or byte-length drift;
- prior raw parent lineages copied into the accumulator;
- provider identity smuggled into portable logical identity.

## Claim boundary

The v0.1 gate targets one real second fork/reconciliation cycle and a model-level
third-cycle falsification. It does not claim:

- arbitrary unbounded cycles;
- constant-time full-history verification;
- cryptographic set-membership proofs for arbitrary historical events;
- Byzantine quorum or governance correctness;
- automatic conflict-resolution safety;
- organizational, hardware, storage, or network independence;
- indefinite artifact durability.
