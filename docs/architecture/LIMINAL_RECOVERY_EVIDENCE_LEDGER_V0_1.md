# Liminal Recovery Evidence Ledger v0.1

**Status:** experimental / non-normative  
**Project:** LIMINAL

## Purpose

The evidence-aware Recovery Router can adapt its next routing decision from prior
comparable recovery outcomes. An in-memory window is sufficient inside one
process, but process restarts must not silently erase the evidence used to
calibrate routing.

This ledger provides a narrow durable layer for that outcome evidence.

## Persisted fields

Each append-only record contains only:

- schema version;
- SHA-256 of the previous record;
- recovery class;
- selected recovery mode;
- deterministic verification result;
- explicit provider finish reason;
- SHA-256 of the current canonical record.

It does **not** persist:

- raw model output or reasoning;
- prompts or retrieved context;
- credentials or provider keys;
- hidden state;
- inferred psychological/cognitive properties.

## Tamper evidence

Records form a SHA-256 chain:

```text
GENESIS
   ↓
record 1 hash
   ↓
record 2.previous_sha256
   ↓
record 2 hash
   ↓
...
```

On load, the complete file is validated. Malformed JSON, unsupported schemas,
invalid fields, record-hash mismatch, or a broken previous-hash link fail
closed. Corrupted history is never silently converted into routing evidence.

This is tamper-evident, not a cryptographic identity/signature system. A future
ProofPath integration can add signed provenance if cross-system trust requires
it.

## Runtime path

```text
verified recovery outcome
        ↓
append durable evidence record
        ↓
RecoveryEvidenceWindow
  (bounded per recovery_class)
        ↓
field reliability summary
        ↓
RuntimeTelemetry
        ↓
Recovery Router
```

Persistence happens before the in-memory window is mutated. If durable append
or chain validation fails, runtime evidence does not diverge from the durable
source of truth.

At process startup, the verified ledger is replayed into the bounded window.
Only the newest configured number of attempts per recovery class influence the
router, even if the durable ledger contains a longer audit history.

## Scope and limits

- The ledger is an outcome audit primitive, not a general memory store.
- It does not make Focus–Field universally preferable.
- Evidence remains scoped by recovery class.
- The current file-backed implementation assumes a single writer; multi-process
  locking/transactionality is future work.
- Hash chaining detects accidental or unsophisticated modification but is not a
  substitute for signed attestations or an external append-only transparency
  log.
