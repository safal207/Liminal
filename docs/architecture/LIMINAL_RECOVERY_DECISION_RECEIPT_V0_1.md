# Liminal Recovery Decision Receipt v0.1

**Status:** experimental / non-normative  
**Project:** LIMINAL

## Purpose

A recovery decision should be independently reproducible, not merely logged.

The receipt binds one Recovery Router decision to:

- the compact `RecoverySignals` used by the router;
- the explicit `RecoveryPolicy` thresholds;
- the exact `RecoveryDecision` produced;
- a signed attestation for the durable recovery-evidence ledger head.

The receipt is signed with Ed25519 and can be verified offline against a separately trusted public key.

## Evidence path

```text
recovery attempts
      ↓
hash-chained evidence ledger
      ↓
signed ledger-head attestation
      ↓
RecoverySignals + RecoveryPolicy
      ↓
deterministic Recovery Router
      ↓
RecoveryDecision
      ↓
signed decision receipt
```

## Independent verification

A verifier can:

1. verify the decision-receipt Ed25519 signature;
2. verify the embedded ledger-head attestation with the trusted public key;
3. optionally verify that the attestation matches a supplied current ledger;
4. reconstruct `RecoverySignals` and `RecoveryPolicy` from the receipt;
5. rerun `choose_recovery_mode(signals, policy)`;
6. require the recomputed decision to exactly equal the signed decision.

A valid signature is therefore not sufficient by itself. A contradictory decision fails deterministic replay even if the receipt bytes were signed by the expected key.

## Runtime integration

`EvidenceAwareRecoveryRuntime.decide_with_receipt(...)` requires a durable `RecoveryEvidenceLedger`.

It performs the same evidence enrichment used by the normal `decide(...)` path, then signs the verified ledger head and the resulting decision receipt. The private key is caller-supplied for the operation and is not retained by the runtime.

If no durable ledger is configured, receipt emission fails closed.

## Data minimization

The receipt does **not** contain:

- raw prompts;
- raw model responses or hidden reasoning;
- API credentials;
- private keys.

It contains only compact routing inputs, policy thresholds, decision outputs, and signed evidence-head metadata.

## Trust boundary

The receipt proves integrity and reproducibility relative to a separately trusted public key. It does not establish the real-world identity or authority of a signer by itself.

v0.1 requires the ledger attestation and decision receipt to use the same trusted signer/key id. Future versions may support delegated or multi-party provenance.

## Control / evidence boundary

This receipt is the narrow bridge between:

```text
Liminal CONTROL
      ↓
reproducible decision receipt
      ↓
ProofPath-style EVIDENCE
```

The router still controls recovery geometry only. A receipt does not grant tool or action authority.
