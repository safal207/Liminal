# Liminal Recovery Trust Consumer Checkpoint Witness v0.1

## Purpose

The consumer checkpoint prevents rollback only while the consumer retains its latest accepted
checkpoint. If that local state is lost, an older checkpoint can still be historically valid and
cryptographically well formed.

The checkpoint witness externalizes the consumer's latest accepted checkpoint digest so a
consumer can recover monotonic anti-rollback state after local checkpoint loss.

## Trust chain

```text
verified rotation authorization
        ↓
attested consumer checkpoint
        ↓
cryptographically verify checkpoint signer + exact subject digest
        ↓
checkpoint witness
        ↓
GitHub OIDC / Sigstore attestation
        ↓
recovered consumer
        ↓
old checkpoint → REJECT: stale_checkpoint
```

## Witness contents

A canonical witness records only compact trust state:

- repository;
- observed checkpoint generation;
- exact checkpoint SHA-256;
- accepted registry SHA-256;
- accepted manifest SHA-256;
- previous witness SHA-256;
- immutable checkpoint signer workflow path and commit SHA.

No provider output, prompt, model reasoning, credentials, or private keys are recorded.

## Authorization rules

For a candidate checkpoint:

1. generation below the witness is rejected as `stale_checkpoint`;
2. an exact same-generation checkpoint is idempotently accepted;
3. a conflicting same-generation checkpoint is rejected as
   `same_generation_checkpoint_conflict`;
4. generation gaps are rejected;
5. a one-generation advance must validate against the checkpoint immediately represented by the
   witness;
6. the candidate checkpoint must have a cryptographically verified attestation from the exact
   checkpoint signer pinned in the witness;
7. the attested subject SHA-256 must equal the canonical candidate checkpoint SHA-256.

A successful advance creates a new canonical witness whose `previous_witness_sha256` commits to
the exact predecessor witness bytes.

## Recovery model

The drill intentionally models this sequence:

```text
witness gen0
    ↓
verify + accept checkpoint gen1
    ↓
witness gen1
    ↓
local checkpoint gen1 is unavailable
    ↓
recover witness gen1
    ↓
present old checkpoint gen0
    ↓
REJECT: stale_checkpoint
```

The old checkpoint may remain historically valid. The rejection is based on recovered monotonic
consumer state, not on pretending the old artifact became cryptographically invalid.

## Evidence boundary

This layer assumes the consumer has obtained and cryptographically verified a witness it is
willing to treat as its recovery anchor. It does **not** by itself solve discovery of the globally
latest witness from an untrusted mirror, nor does it provide Byzantine quorum across independent
organizations.

Those are separate problems. A later witness-discovery or witness-quorum layer can build on this
canonical witness format without changing the checkpoint semantics.

## Non-authority

A witness proves and remembers accepted trust state. It does not grant tool execution authority,
financial authority, or permission to mutate the underlying registry/checkpoint policies.
