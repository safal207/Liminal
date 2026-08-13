# Witness Authority v0.3 — historical first proof

## Status

**Superseded in the current branch by the stricter Checkpoint Witness Authority v0.3 model.**

This document is retained as a historical pointer because the first immutable Source-Producer + Control-Plane Portability proof used this earlier migration model.

Historical immutable verifier:

`32152ef2b8f7f134b7830743a70c6bc903b64c1c`

Historical successful one-shot:

`31669188983`

Historical proof artifacts:

- `9169187861` — `sha256:062c509d840557e798cd20f161982df16393ddaaf33589683c063f0a692d4c1b`
- `9169232567` — `sha256:020c8e8aed36889e1fa19401e3c4180ef2ce806a84b3547ed7d1b05927833cb7`

## Why it was superseded

The first model migrated a legacy witness into logical producer authority using the predecessor witness digest and logical authority contract. It produced a successful, immutable proof, but subsequent review exposed a stronger causal requirement:

```text
legacy witness hash
+ exact legacy signer mapping
+ canonical provider-neutral migration claim
        ↓
logical checkpoint authority
```

Without the exact legacy signer mapping and canonical migration claim in the migration origin, the transition from concrete signer authority to logical authority was less tightly bound than the rest of the portability stack.

The current implementation is therefore:

[`LIMINAL_CHECKPOINT_WITNESS_AUTHORITY_V0_3.md`](LIMINAL_CHECKPOINT_WITNESS_AUTHORITY_V0_3.md)

Current module:

`src/liminal/checkpoint_witness_authority_v3.py`

The current model additionally requires:

- exact legacy witness SHA-256;
- exact legacy signer workflow path and SHA;
- canonical `migration_claim_sha256` over provider-neutral mapping semantics;
- trust-domain binding;
- pre-transition authority evidence;
- explicit rejection of circular post-transition receipt authorization.

## Historical integrity

The historical run is **not revoked or rewritten**. Its immutable workflow SHA, run, and artifacts remain reproducible as evidence of the earlier model.

The current branch no longer carries the earlier executable implementation as a competing active authority model. The stronger revision has its own immutable proof lineage and different v0.3 witness identities.
