# Trust-Provider Portability v0.1

## Status

**Verified experimental provider-portability proof.** The pure comparison model, provider-neutral witness adapter, pre-existing offline Ed25519 root, immutable dual-provider workflow and external recomputation have all completed successfully.

The proven independence boundary is deliberately limited to:

- independent trust root;
- independent signing authority;
- independent signature scheme / proof path.

GitHub Actions execution and artifact transport remain shared. The offline path has no transparency log and this experiment does not claim independent organizational governance.

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

`portable_checkpoint_authorization.py` maps an already verified `PortableTrustReceipt` into the legacy `VerifiedCheckpointEvidence` only after checking it against:

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

## Independent root fixture

The verified experiment uses a one-shot offline Ed25519 root pinned under:

`trust/experimental/offline-ed25519-root-v0.1/`

It contains:

- `public-key.pem`;
- `manifest-proof.json`;
- `checkpoint-proof.json`.

The private key is not committed to the repository and was not available to the GitHub Actions proof workflow. The signed claim records bind the deterministic manifest/checkpoint subject digests to the portable producer, source and authorization-policy claims.

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

## Verified live gate

Immutable reusable workflow:

`dc9f236d590f15ee005d2688f91da92460c512d2`

Pinned one-shot caller:

`cc34af1ed5bf7a997ff5c2d94f72001d0429d824`

Successful one-shot run:

`31658743875` — **SUCCESS**

The workflow first passed exact-head Python CI, Python Integration and Artillery before being pinned.

The successful run proved:

```text
checkpoint / manifest bytes
   ├─ GitHub + Sigstore Public Good path
   │      ↓
   │  provider observation A
   │
   └─ pre-existing offline Ed25519 root
          ↓
      provider observation B

A.provider != B.provider
A.trust_root_id != B.trust_root_id
A.verification_scheme != B.verification_scheme
A.verified == B.verified == true
A.portable_claims == B.portable_claims
        ↓
Portable Trust Receipt A == B
        ↓
portable authorization adapter A/B
        ↓
checkpoint_witness_advanced A/B
        ↓
same next-witness digest
```

Both offline signatures were independently verified with OpenSSL and the pinned Ed25519 public root before their provider observations were admitted.

### Subject identities

Manifest subject SHA-256:

`5f80518cb671ea0622336adbd9a0a9bd16b72ea803ad09d0ac2abd4415f58be2`

Checkpoint subject SHA-256:

`74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`

Authorization policy SHA-256:

`22fcc3c556528d080591041bc10c1a35f85bfbad348b8f669bfff4bb1b88b47f`

### Provider-neutral identities

Manifest Portable Trust Receipt SHA-256:

`e3558d426d560bd202bd7e16ef0364b378cc2956c36feccc78eeaf40bfaa084e`

Checkpoint Portable Trust Receipt SHA-256:

`2235b07a4188628091cbe94af6a16dc30516d0acea743f9b4517b58a5cbd1a80`

Both provider paths produced:

```text
authorized: true
reason: checkpoint_witness_advanced
next_witness_sha256: cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f
```

The next-witness identity is the same one produced by the preceding GitHub-only verifier portability experiments. Provider-specific trust machinery therefore changed while the authorized state transition remained stable.

Canonical proof-result SHA-256:

`4ee314a71bd08f469d369c5689be653729d3dbad37b328b9da91409241d1da3d`

External verification record SHA-256:

`3b0a43d6915f002d1189cf9b98d527e22ec8b51cca8ab498d515aeaf8d731224`

### External verification

The separate external job did not trust the combined proof result. It independently:

- reverified the GitHub/Sigstore producer attestations;
- recomputed the offline Ed25519 public-root fingerprint;
- revalidated both offline claim payloads against the real subject bytes;
- reran both Ed25519 signature verifications;
- rebuilt both provider observations;
- recomputed provider independence and Portable Trust Receipt equality;
- rebound both receipts through the trusted witness authorization policy;
- recomputed both `checkpoint_witness_advanced` transitions and the next-witness digest;
- checked byte identity of GitHub/offline receipt copies;
- verified the immutable proof-workflow signer on both provider receipt copies and the result;
- attested its own external verification record.

### Evidence artifacts

Trust-provider proof:

- artifact `9165397390`
- `sha256:ae84a4638808f923ed5633822be5e159048de94e6e2b11137c05173366445b0f`

External verification:

- artifact `9165410163`
- `sha256:803aebec55db62b1ec0acbcbef390a832cdbbc8fbd3e65b0106215c9f065e064`

Both artifacts have 30-day retention and expire on 12 Sep 2026.

## Proven independence matrix

| Property | Independent in v0.1? |
| --- | --- |
| Trust root | Yes |
| Signing authority | Yes |
| Signature / proof scheme | Yes |
| Verifier path | Yes |
| CI provider | No |
| Artifact transport | No |
| Transparency log | No — offline path has none |
| Organizational governance | Not claimed |

The precise milestone is therefore **trust-root + signing-authority + signature-scheme portability**, not total infrastructure independence.

## Current proven boundary

```text
physical topology drift         ✅
raw verifier output drift       ✅
verifier implementation drift   ✅
trust root drift                ✅
signing authority drift         ✅
signature scheme drift          ✅
                                ↓
portable subject / authority / source / policy claims
                                ↓
Portable Trust Receipt
                                ↓
local witness authorization
                                ↓
stable trusted state transition
```

## Next falsifiable question

Can the portable contract survive **execution-provider and evidence-transport independence** as well — for example when one proof is produced and verified outside GitHub Actions and its evidence is transported independently of GitHub artifacts?

That is the next boundary. Normalization must never hide disagreement about subject, authority, source, execution policy or authorization policy.
