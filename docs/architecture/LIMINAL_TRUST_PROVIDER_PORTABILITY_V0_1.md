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

## Portable trust contract

Provider audit metadata remains outside portable identity:

- `provider`
- `verification_scheme`
- `trust_root_id`

Portable claims must match exactly:

- `subject_sha256`
- `authority_id`
- `repository`
- `producer_revision`
- `source_ref`
- `execution_policy`
- `authorization_policy_sha256`
- `verified`

Canonical schema:

`liminal-portable-trust-receipt/v0.1`

Agreement is impossible unless provider IDs and trust-root IDs are distinct, both paths independently verify successfully, and every portable security claim matches exactly. Any mismatch returns a deterministic failure and no portable receipt hash.

## Provider-neutral witness authorization

`portable_checkpoint_authorization.py` maps an already verified `PortableTrustReceipt` into the existing `VerifiedCheckpointEvidence` only after checking it against local trusted witness policy.

The workflow path and signer revision placed into checkpoint evidence come from the trusted witness, not from provider metadata. An external provider therefore cannot appoint itself as the local authorized producer.

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

## Independent root fixture

The verified experiment uses a one-shot offline Ed25519 root pinned under:

`trust/experimental/offline-ed25519-root-v0.1/`

Pinned public-root identity:

`ed25519-sha256:4b690cae29f41bea47c2beaca52e92dcb606c69638b9f48d8e540a981af1e402`

The private signing key is not committed to the repository and was not available to the GitHub Actions proof workflow. The root existed before the live verification run, so the proof workflow could verify the offline signatures but could not manufacture new signatures from that authority.

This remains an experimental one-shot ceremony, not a production KMS/HSM design or a claim of independent organizational governance.

## Verified live gate

Immutable reusable workflow:

`dc9f236d590f15ee005d2688f91da92460c512d2`

Pinned one-shot caller:

`cc34af1ed5bf7a997ff5c2d94f72001d0429d824`

Successful one-shot run:

`31658743875` — **SUCCESS**

The reusable workflow passed exact-head Python CI, Python Integration and Artillery before being pinned.

The live chain was:

```text
same manifest / checkpoint bytes
   ├─ GitHub + Sigstore Public Good trust path
   │      ↓
   │  provider observation A
   │
   └─ pre-existing offline Ed25519 trust path
          ↓
      provider observation B

provider A != provider B
trust root A != trust root B
signature scheme A != signature scheme B
verified A == verified B == true
portable claims A == portable claims B
        ↓
Portable Trust Receipt A == B
        ↓
local witness authorization A/B
        ↓
checkpoint_witness_advanced A/B
        ↓
same next-witness digest
```

Both offline signatures were independently verified against the pinned Ed25519 public root before the secondary provider observations were admitted.

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

That next-witness digest is unchanged from the preceding GitHub-only verifier portability proof: provider-specific trust machinery changed while the authorized state transition remained stable.

Canonical proof-result SHA-256:

`4ee314a71bd08f469d369c5689be653729d3dbad37b328b9da91409241d1da3d`

External verification record SHA-256:

`3b0a43d6915f002d1189cf9b98d527e22ec8b51cca8ab498d515aeaf8d731224`

## External verification

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

## Evidence artifacts

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
| Verification path | Yes |
| CI provider | No |
| Artifact transport | No |
| Transparency log | No — offline path has none |
| Organizational governance | Not claimed |

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

Can the portable contract survive **execution-provider and evidence-transport independence** as well — when at least one proof is produced and verified outside GitHub Actions and its evidence is transported independently of GitHub artifacts?

That is the next boundary. Normalization must never hide disagreement about subject, authority, source, execution policy or authorization policy.
