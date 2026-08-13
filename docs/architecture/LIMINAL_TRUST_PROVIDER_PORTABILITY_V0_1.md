# Trust-Provider Portability v0.1

## Status

Experimental provider-portability contract. The pure model is implemented; a live proof must not be claimed until a second trust root exists independently of the proof workflow.

## Goal

Prove that materially distinct trust providers can independently establish the same security contract for the same evidence and converge on one portable trust identity.

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
portable evidence / recovery decision
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

## Trust boundary

`trust_provider_portability.py` is not a verifier and not a trust provider.

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
```

The comparator never:

- validates a signature;
- creates or rotates a trust root;
- accepts a provider because its claims look similar;
- maps a provider-specific failure to success;
- chooses one provider as authoritative when they disagree;
- manufactures a second provider identity from one proof path.

## Candidate second trust path

The first live experiment should use a trust root whose authority exists before the proof workflow starts and whose private signing authority is unavailable to that workflow except through the intended signing interface.

Acceptable shapes include, for example:

- a pre-existing offline Ed25519 public trust root with the private key kept outside the proof repository/workflow;
- an external KMS/HSM-backed key with a pinned public identity;
- a second attestation provider with independently managed roots and issuance infrastructure.

Cosign supports verification against on-disk public keys and KMS key references, so a provider-independent cryptographic path can still use Cosign as tooling without making Sigstore keyless identity the trust provider. The provider claim must be based on the independent key/root, not on GitHub's Fulcio identity.

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

Once an independent root is available:

```text
checkpoint / manifest bytes
   ├─ GitHub + Sigstore attestation path
   │      ↓
   │  provider observation A
   │
   └─ independent-root signature path
          ↓
      provider observation B

A.provider != B.provider
A.trust_root_id != B.trust_root_id
A.verified == B.verified == true
A.portable_claims == B.portable_claims
        ↓
Portable Trust Receipt A == B
        ↓
portable Evidence Bundle / witness decision
```

The external verifier must then recompute the portable receipt and independently verify both provider-specific proof paths rather than trusting a combined result flag.

## Current claim boundary

The implemented pure model proves only the **comparison contract** and fail-closed semantics. It does not yet prove live provider independence.

A live milestone must state exactly which properties are independent:

- trust root;
- signing authority;
- verification substrate;
- storage/transport;
- CI environment;
- transparency log, if any.

Provider portability should be claimed only for the properties actually changed.

## Next falsifiable question

Can a pre-existing independent trust root establish the same portable claims as GitHub/Sigstore for the real checkpoint evidence while producing the same Portable Trust Receipt and recovery transition?
