# Trust-Provider Portability v0.1

## Status

Experimental provider-portability contract. The pure comparison model and provider-neutral witness adapter are implemented. A one-shot offline Ed25519 public root and signed claim records are pinned for the first live experiment; live provider portability must not be claimed until the workflow independently verifies both trust paths.

## Goal

Prove that materially distinct trust providers can independently establish the same security contract for the same evidence and converge on one portable trust identity and recovery transition.

```text
provider A proof path
        +
provider B proof path
        ↓
independently established trust claims
        ↓
provider-neutral semantic comparison
        ↓
Portable Trust Receipt A == B
        ↓
local witness authorization adapter
        ↓
same recovery / witness transition
```

This is a stronger boundary than Independent Verifier Portability. Two verifier implementations over the same GitHub/Sigstore substrate are still one trust-provider family.

## Why the existing normalized verification receipt is not enough

`NormalizedVerificationReceipt v0.1` intentionally contains GitHub-specific semantics such as:

- signer workflow;
- signer Git SHA;
- source ref;
- self-hosted-runner policy.

Those fields were correct for verifier-output and verifier-implementation portability inside one provider substrate. Reusing them unchanged across distinct providers would either:

1. force the second provider to impersonate GitHub-specific metadata; or
2. normalize away a real provider-level disagreement.

Trust-Provider Portability therefore introduces a higher semantic layer.

## Provider-specific evidence versus portable claims

Each provider emits a `TrustProviderObservation` with two classes of fields.

### Provider audit / independence metadata

- `provider`
- `verification_scheme`
- `trust_root_id`

These must be retained for audit and independence checks, but are excluded from portable trust identity.

### Portable trust claims

- `subject_sha256`
- `authority_id`
- `repository`
- `producer_revision`
- `source_ref`
- `execution_policy`
- `authorization_policy_sha256`
- `verified`

Each provider must independently establish these claims from its own trust path.

## Portable Trust Receipt v0.1

The provider-neutral receipt contains only portable claims:

```text
subject bytes
+ logical authority
+ source repository
+ producer revision
+ source ref
+ execution policy
+ authorization policy digest
+ successful verification
```

Canonical schema:

`liminal-portable-trust-receipt/v0.1`

Provider identity, trust-root identity and verification scheme are intentionally absent from the receipt hash. They are not security semantics shared across providers; they are evidence about how the semantics were established.

## Independence requirements

Agreement is impossible unless:

1. provider identifiers are distinct;
2. trust-root identifiers are distinct;
3. both providers independently report successful verification.

A different verifier executable against the same root is not sufficient.

A second key generated inside the same proof workflow is also not sufficient. Such a workflow could manufacture its own trust anchor and then verify itself.

## Fail-closed semantic agreement

After independence checks, the following claims must match exactly:

1. subject SHA-256;
2. logical authority ID;
3. repository;
4. producer revision;
5. source ref;
6. execution policy;
7. authorization policy SHA-256;
8. canonical Portable Trust Receipt SHA-256.

Any mismatch returns a deterministic failure reason and no portable receipt hash.

## Provider-neutral witness authorization

The existing checkpoint witness deliberately pins a concrete local producer workflow. Provider portability must not let an external provider overwrite that policy.

`portable_checkpoint_authorization.py` therefore maps an already verified `PortableTrustReceipt` into the legacy `VerifiedCheckpointEvidence` only after checking it against:

- the logical authority expected by local policy;
- the repository pinned in the trusted witness;
- the producer revision pinned by `checkpoint_signer.workflow_sha`;
- the expected source ref;
- the expected execution policy;
- the exact authorization-policy SHA-256.

The workflow path and signer revision placed into `VerifiedCheckpointEvidence` come from the trusted witness, not from provider metadata.

```text
provider-neutral receipt
        ↓
portable policy checks
        +
trusted witness signer pin
        ↓
VerifiedCheckpointEvidence
        ↓
existing witness evaluator
```

This preserves the current witness implementation while removing provider-specific proof machinery from the authorization boundary.

## Trust boundary

Neither `trust_provider_portability.py` nor `portable_checkpoint_authorization.py` is a verifier or trust provider.

```text
external provider A ─┐
                     ├─ TrustProviderObservation[]
external provider B ─┘
                            ↓
                  independence checks
                            ↓
                  semantic agreement
                            ↓
                  portable receipt
                            ↓
                  local authorization mapping
```

These layers never:

- validate a provider signature themselves;
- create or rotate a trust root;
- accept a provider because its claims look similar;
- map a provider-specific failure to success;
- choose one provider as authoritative when they disagree;
- manufacture a second provider identity from one proof path;
- allow provider metadata to replace the producer signer pinned by local witness policy.

## First independent root fixture

The first live experiment uses an experimental one-shot offline Ed25519 root pinned under:

`trust/experimental/offline-ed25519-root-v0.1/`

It contains:

- `public-key.pem`;
- `manifest-proof.json`;
- `checkpoint-proof.json`.

The private key is not committed to the repository and must not be available to the GitHub Actions proof workflow. The signed claim records bind the known deterministic manifest/checkpoint subject digests to the portable producer, source and authorization-policy claims.

Pinned public-root identity:

`ed25519-sha256:4b690cae29f41bea47c2beaca52e92dcb606c69638b9f48d8e540a981af1e402`

This fixture is intentionally experimental and one-shot. It is not a production key-management design and does not claim independent organizational governance.

## Forbidden shortcut

Do not generate the secondary key pair inside the same GitHub Actions run and call that provider independence.

```text
proof workflow
   ├─ generate key
   ├─ sign subject
   └─ verify signature
```

This proves cryptographic round-trip correctness only. It does not prove an independent trust provider because the verifier run created its own authority.

## Planned live gate

```text
checkpoint / manifest bytes
   ├─ GitHub + Sigstore attestation path
   │      ↓
   │  provider observation A
   │
   └─ pre-existing offline Ed25519 root
          ↓
      provider observation B

A.provider != B.provider
A.trust_root_id != B.trust_root_id
A.verified == B.verified == true
A.portable_claims == B.portable_claims
        ↓
Portable Trust Receipt A == B
        ↓
portable authorization adapter A/B
        ↓
same witness decision and next-witness digest
```

The external verifier must then recompute the portable receipt, independently rerun the offline Ed25519 verification, reverify the GitHub/Sigstore path and recompute both witness transitions instead of trusting a combined result flag.

## Current claim boundary

The implemented code proves the comparison and authorization-adapter contracts. The pinned offline root proves only that an independent trust root exists before the future proof workflow; it does not by itself prove provider portability until the live gate succeeds.

A live milestone must state exactly which properties are independent:

- trust root;
- signing authority;
- verification substrate;
- storage/transport;
- CI environment;
- transparency log, if any.

For the planned one-shot offline-root experiment, the expected independence claim is limited to **trust root + signing authority + signature scheme**. GitHub remains shared for CI execution and artifact transport; the offline path has no transparency log.

## Next falsifiable question

Can the pinned offline Ed25519 root establish the same portable claims as GitHub/Sigstore for the real checkpoint evidence while producing the same Portable Trust Receipt and recovery transition?
