# Liminal Causal Fork / Reconciliation Portability v0.1

## Status

Implementation and falsification gate. A `VERIFIED` claim requires an immutable one-shot proof, provider attestations/signatures, and an independent recomputation job.

## Fractal Causal Refactoring diagnosis

The previous multi-epoch gate proved two independent histories could follow the same semantic trajectory. It intentionally rejected any step where the two target semantic states differed.

The next question is not whether that equality check can be relaxed. Relaxing it would preserve a linear checkpoint shape with only one predecessor and would silently discard one fork lineage at reconciliation.

The First Meaningful Divergence is therefore the transition topology:

```text
intended reconciliation = one state authorized by both divergent lineages
linear checkpoint        = one state with one predecessor
```

A reconciliation is a DAG join, not another linear transition.

## Portable model

```text
                         branch A checkpoint/witness
                        /
common checkpoint/witness
                        \
                         branch B checkpoint/witness

branch A tip + branch B tip
             ↓
canonical parent-lineage set
             ↓
CausalReconciliationRef
             ↓
reconciliation checkpoint/witness
```

Provider-specific attestations, signer identities, and provenance digests authorize the branch observations and reconciliation votes. They are evidence, not portable identity.

## Core primitives

### ForkBranchRef

A provider-free reference binding:

- common state reference;
- logical branch ID;
- next branch state reference;
- branch contract;
- branch authorization contract.

### CausalReconciliationRef

A provider-free two-parent join binding:

- exact common ancestor state, checkpoint, and witness;
- both branch state refs;
- both branch refs;
- both branch checkpoint tips;
- both branch witness tips;
- canonical parent-set digest;
- reconciled result state ref;
- reconciliation contract;
- reconciliation authorization contract.

Parent order is canonicalized by branch checkpoint digest. Swapping input order must produce byte-identical reconciliation objects.

### Reconciliation checkpoint and witness

The reconciliation checkpoint contains a sorted two-parent checkpoint digest set rather than a single `previous_checkpoint_sha256`.

The reconciliation witness contains a sorted two-parent witness digest set. Neither parent may be omitted, duplicated, or replaced.

## Required invariants

```text
F1 common prefix is fully valid
F2 branch tips descend from the exact same common checkpoint/witness
F3 branch semantics are genuinely different
F4 branch evidence providers, authorities, and provenance are independent
F5 each reconciliation vote is bound to one exact branch ref/state/checkpoint/witness
F6 both votes authorize the same new target state and contracts
F7 reconciliation identity commits both lineages in canonical order
F8 raw provider evidence is absent from portable branch/reconciliation objects
F9 removing or duplicating either parent fails closed
F10 reversing branch input order does not change portable bytes
```

## Tested fail-closed cases

- invalid branch evidence;
- same provider, authority, or provenance for both branches;
- duplicate logical branch identity;
- non-divergent branch semantic states;
- branch not descended from the common tip;
- vote bound to the wrong branch checkpoint or witness;
- vote target, contract, or authorization mismatch;
- duplicate or missing reconciliation parent;
- tampered common prefix;
- provider identity smuggled into a portable logical ID.

## Claim boundary

A successful v0.1 proof establishes one explicit two-parent fork/reconciliation construction over a previously verified portable multi-epoch prefix.

It does not establish:

- arbitrary N-parent joins;
- repeated nested forks;
- Byzantine quorum or governance correctness;
- automatic conflict-resolution policy safety;
- organizational, hardware, storage, or network-path independence;
- indefinite durability.

## Next falsifiable question

After a successful two-parent reconciliation, can the reconciled chain fork and reconcile repeatedly while retaining bounded, independently verifiable lineage rather than growing an unbounded ancestry payload?
