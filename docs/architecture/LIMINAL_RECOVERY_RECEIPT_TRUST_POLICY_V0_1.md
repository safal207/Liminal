# Liminal Recovery Receipt Trust Policy v0.1

Status: experimental.

## Purpose

A valid Ed25519 signature proves that a receipt was signed by the private key
corresponding to a supplied public key. It does **not** by itself prove that the
signer is trusted to attest Liminal recovery decisions.

This policy adds the missing authorization layer.

```text
Recovery Decision Receipt
        |
        v
cryptographic verification
        |
        v
pinned key id + public-key fingerprint
        |
        v
allowed receipt schema
        |
        v
allowed recovery class
        |
        v
TRUSTED / UNTRUSTED
```

## Trust anchor

Each `TrustedSignerRule` explicitly pins:

- `key_id`;
- SHA-256 of the raw Ed25519 public key;
- allowed `recovery_class` values;
- allowed receipt schema versions.

Trust is therefore caller-controlled and offline. There is no network key
discovery and no trust-on-first-use behavior in v0.1.

## Verification order

`verify_trusted_decision_receipt(...)` fails closed unless all of the following
hold:

1. the receipt declares a valid recovery class;
2. the receipt `key_id` is present in the trust policy;
3. the supplied public key matches the pinned SHA-256 fingerprint;
4. the receipt schema is authorized for that signer;
5. the recovery class is authorized for that signer;
6. the existing decision-receipt verifier validates the Ed25519 signature,
   nested ledger-head attestation, optional exact ledger binding, and
   deterministic Recovery Router replay.

A mathematically valid signature from an unpinned key remains untrusted.

## Ephemeral CI keys

The live Gonka decision-proof experiment used an ephemeral Ed25519 key created
inside one GitHub Actions job. That key proves artifact integrity for that run,
but it is **not** a long-term Liminal identity.

Under this trust policy the ephemeral signer is untrusted unless a caller
explicitly pins its key id and fingerprint. This separation prevents an
artifact-generated key from silently becoming an identity root.

## Key rotation

v0.1 keeps rotation explicit:

1. add a new signer rule with a distinct `key_id` and fingerprint;
2. deploy verifiers that trust the new rule;
3. begin signing with the new private key;
4. remove the old rule only after the intended verification window closes.

The policy intentionally does not contain wall-clock validity or remote
revocation in v0.1. Those require a separately trustworthy time/revocation
source and should not be simulated locally.

## Non-goals

This policy does not prove:

- human or organizational legal identity;
- GitHub workflow identity;
- certificate-chain ownership;
- hardware-backed key custody;
- timestamp validity;
- remote revocation status.

Those can be layered later using a durable signing identity, GitHub OIDC /
Sigstore-style provenance, HSM/KMS-backed signing, or another externally
verifiable identity root.

## Safety properties

- signature validity and signer authorization are separate checks;
- no wildcard signer identities;
- no implicit first-use trust;
- recovery-class scope is explicit;
- receipt-schema scope is explicit;
- public-key substitution is detected by fingerprint mismatch;
- post-signature receipt mutation still fails through the underlying receipt
  verifier;
- no private key, provider output, prompt, credential, or reasoning is persisted
  by this layer.
