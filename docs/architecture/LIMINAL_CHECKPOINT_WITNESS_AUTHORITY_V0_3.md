# Checkpoint Witness Authority v0.3

## Status

**VERIFIED in the stronger Source-Producer + Control-Plane Portability proof.**

The current v0.3 model replaces concrete checkpoint signer identity as the active authority identity with a provider-neutral logical authority contract, while preserving an explicit causal migration from the historical v0.2 signer-pinned witness.

## Authority identity

```text
checkpoint_authority
  = logical_producer_id
  + producer_contract_sha256
  + authorization_contract_sha256
  + evidence_type
```

Concrete producer, signer, verifier and control-plane identities remain evidence about that authority. They do not become the portable authority identity.

Current contracts:

- logical producer: `liminal.trusted-recovery.checkpoint-producer`
- producer contract: `72bba8eddc81e88c2e9ad24e266713e9534f6c332fec7ad5ecaa264f922b7ca3`
- authorization contract: `576da1fa0c5cd70313ad1d89de88f4a7048e13fa5d0ce05c833f7bef4233a553`
- evidence type: `trusted-recovery-consumer-checkpoint`

## Explicit migration

The v0.3 root is not allowed to appear without cause. Migration requires already-verified evidence binding:

- exact legacy v0.2 witness SHA-256;
- exact legacy checkpoint signer workflow path;
- exact legacy checkpoint signer workflow SHA;
- trust domain;
- logical producer ID;
- producer contract SHA-256;
- authorization contract SHA-256;
- evidence type.

Legacy witness SHA-256:

`af12743396296c788223d3087f427b1f93d3086a5aeb9b7c8c0f38d49347e9f9`

Legacy signer SHA:

`f31b56a5e21a668bcb98791b05542652760dcc27`

Provider-neutral migration claim SHA-256:

`aec92a1c1100e6ea5944e042cd5e7c56f3ebc01b5a957782482231732d504f10`

Resulting v0.3 generation-0 witness SHA-256:

`8d2e44dab167f1f4613ef66257ca3c3be19f2168a87b620483628389b771ca8c`

Raw verifier output is excluded from migration identity. `verified` is a precondition for migration, not a field that changes `migration_claim_sha256`.

## Causal ordering

A post-transition Source-Control Receipt must never authorize the transition that produced it.

```text
verified pre-transition authority evidence
        ↓
v0.3 witness decision
        ↓
checkpoint_witness_advanced
+ next witness
        ↓
SourceControlObservation
        ↓
Portable Source-Control Receipt
```

This prevents circular proof.

## Fail-closed advancement

Generation advancement requires:

- valid trusted v0.3 witness;
- exactly next generation;
- valid predecessor checkpoint chain;
- exact predecessor/witness prefix binding;
- externally verified authority evidence;
- exact checkpoint subject digest;
- exact generation;
- exact logical producer ID;
- exact producer contract;
- exact authorization contract;
- exact evidence type.

Mismatch returns a deterministic rejection reason. Same-generation replay of the exact witnessed checkpoint remains idempotently accepted; stale/conflicting/gapped checkpoints remain rejected.

## Verified live proof

Stronger immutable verifier:

`d4d498288afac1d26e37f62ff8a8c17746d25d8d`

Pinned caller:

`b6cf8dbe1f3e846e2abc430f905e69a07a5fb78f`

One-shot:

`31673608370` — **FULL SUCCESS**

Both primary and secondary paths independently produced/verified the same checkpoint subject:

`74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`

Both advanced the same v0.3 root with:

`checkpoint_witness_advanced`

and produced the same next-witness SHA-256:

`efc242be9ebeb3bf898c3cee301391525d1609d499f44c7ae4eac9ce4e5cb4ed`

Portable Source-Control Receipt SHA-256:

`9d6a90e5f079b8c8bde01ab858fa9b9050603f3245d5008b0a90d61301a5c73a`

## Historical compatibility

v0.1/v0.2 witness code and immutable proofs are not rewritten. The first source/control proof (`31669188983`) used an earlier v0.3 migration model and remains historical evidence. It is superseded for current authority semantics by this stronger exact-signer + canonical-migration-claim binding.

See `LIMINAL_WITNESS_AUTHORITY_V0_3.md` for the historical note.

## Trust boundary

`checkpoint_witness_authority_v3.py` does not verify signatures or decide whether providers are organizationally independent. It consumes already-verified authority claims and evaluates them against the trusted logical authority contract.

The latest proof does not yet remove the upstream GitHub rotation-authority dependency. That is the next causal boundary.
