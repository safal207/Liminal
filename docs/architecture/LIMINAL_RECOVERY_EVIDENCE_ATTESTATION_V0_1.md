# Liminal Recovery Evidence Attestation v0.1

**Status:** experimental / non-normative  
**Project:** LIMINAL

## Purpose

The recovery evidence ledger is locally tamper-evident because each JSONL record is linked by SHA-256 and the complete chain is verified before use. v0.1 attestation adds a portable cryptographic statement over the verified ledger head so another process or reviewer can validate the evidence snapshot offline.

## Signed statement

The Ed25519 signature covers canonical JSON containing only:

```text
attestation_schema_version
ledger_schema_version
record_count
ledger_head_sha256
```

No prompt, raw model response, credential, hidden reasoning, or private key is signed into or persisted by the attestation object.

## Verification path

```text
recovery ledger JSONL
       ↓
verify complete SHA-256 chain
       ↓
ledger head + record count
       ↓
canonical statement
       ↓
Ed25519 signature
       ↓
portable attestation

external verifier
       ↓
trusted/pinned public key
       + attestation
       + optional ledger copy
       ↓
verify signature
       ↓
optionally recompute ledger head
       ↓
exact provenance match
```

## Trust boundary

A valid signature proves that the holder of the corresponding private key signed the exact statement. It does **not** by itself establish who controls that key or whether that signer should be trusted.

Signer identity/trust therefore remains external to this primitive and should be bound through a pinned public key, trusted key registry, release identity, Sigstore identity, ProofPath, or another explicit trust anchor.

`key_id` is metadata for selecting/identifying the intended trust anchor. It is included in the attestation envelope but signer trust must still be established separately.

## Fail-closed behavior

Attestation creation first verifies the complete local ledger chain. A malformed or tampered ledger cannot be signed through this API.

Verification fails when:

- the Ed25519 signature is invalid;
- the supplied trusted public key is wrong;
- an expected `key_id` does not match;
- the attestation schema is malformed or unsupported;
- the attested head no longer equals the current verified ledger head.

## Scope

This is deliberately a small detached-signature primitive. It does not yet implement:

- key distribution or rotation;
- hardware-backed signing;
- Sigstore/Fulcio/Rekor publication;
- ProofPath receipt packaging;
- multi-signature/quorum policy;
- timestamp authority.

Those can be layered above the same canonical ledger-head statement without changing Recovery Router semantics.
