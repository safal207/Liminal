# Trust-Provider Portability v0.1

## Status

**VERIFIED.** Experimental provider-portability milestone completed on 13 Aug 2026.

The pure comparison model, provider-neutral witness adapter, pre-existing offline Ed25519 root, immutable dual-provider workflow and independent external recomputation all completed successfully.

Proven independence is deliberately limited to:

- independent trust root;
- independent signing authority;
- independent signature / proof scheme;
- independent verification path.

GitHub Actions execution and artifact transport remain shared. The offline path has no transparency log, and independent organizational governance is not claimed.

## Core result

```text
same evidence bytes
   ├─ GitHub + Sigstore Public Good trust path
   └─ pre-existing offline Ed25519 trust path
                    ↓
different provider / root / authority / scheme
                    ↓
exact same portable security claims
                    ↓
Portable Trust Receipt A == B
                    ↓
local witness policy binding
                    ↓
checkpoint_witness_advanced A == B
                    ↓
same next-witness digest
```

Provider-specific trust machinery is audit evidence, not portable authorization identity. Authorization still belongs to local witness policy.

## Portable trust contract

Provider audit metadata:

- `provider`
- `verification_scheme`
- `trust_root_id`

Portable claims:

- `subject_sha256`
- `authority_id`
- `repository`
- `producer_revision`
- `source_ref`
- `execution_policy`
- `authorization_policy_sha256`
- `verified`

Canonical receipt schema:

`liminal-portable-trust-receipt/v0.1`

Agreement fails closed unless provider IDs and trust roots are distinct, both verification paths succeed, and every portable claim matches exactly.

## Local authorization remains authoritative

`portable_checkpoint_authorization.py` maps a verified `PortableTrustReceipt` into the existing checkpoint evidence only after matching it against the trusted local witness policy.

The concrete producer workflow path and revision are taken from the trusted witness, not from external-provider metadata. A provider therefore cannot nominate itself as an authorized local producer.

## Offline trust root

Fixture:

`trust/experimental/offline-ed25519-root-v0.1/`

Pinned public root:

`ed25519-sha256:4b690cae29f41bea47c2beaca52e92dcb606c69638b9f48d8e540a981af1e402`

The private signing key is not committed to the repository and was unavailable to the GitHub Actions proof workflow. The root and signed claims existed before live verification, so the workflow could verify that authority but could not mint new signatures from it.

This is an experimental one-shot ceremony, not a production KMS/HSM design.

## Immutable proof

Reusable workflow:

`dc9f236d590f15ee005d2688f91da92460c512d2`

Pinned caller:

`cc34af1ed5bf7a997ff5c2d94f72001d0429d824`

Successful one-shot:

`31658743875` — **SUCCESS**

The reusable workflow passed exact-head Python CI, Python Integration and Artillery before immutable pinning.

Upstream immutable producer:

`f31b56a5e21a668bcb98791b05542652760dcc27`

## Verified identities

Manifest subject SHA-256:

`5f80518cb671ea0622336adbd9a0a9bd16b72ea803ad09d0ac2abd4415f58be2`

Checkpoint subject SHA-256:

`74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`

Authorization-policy SHA-256:

`22fcc3c556528d080591041bc10c1a35f85bfbad348b8f669bfff4bb1b88b47f`

Manifest Portable Trust Receipt:

`e3558d426d560bd202bd7e16ef0364b378cc2956c36feccc78eeaf40bfaa084e`

Checkpoint Portable Trust Receipt:

`2235b07a4188628091cbe94af6a16dc30516d0acea743f9b4517b58a5cbd1a80`

Both providers independently produced:

```text
authorized: true
reason: checkpoint_witness_advanced
next_witness_sha256: cc389524836b013bb5a416f0a9f6647d9ff252d2de79598e4df119c6e5760d2f
```

The next-witness identity is unchanged from the preceding verifier-portability proof even though the trust root, signing authority and signature scheme changed.

Proof-result SHA-256:

`4ee314a71bd08f469d369c5689be653729d3dbad37b328b9da91409241d1da3d`

External verification record SHA-256:

`3b0a43d6915f002d1189cf9b98d527e22ec8b51cca8ab498d515aeaf8d731224`

## External recomputation

The external job independently:

- reverified GitHub/Sigstore producer provenance;
- recomputed the offline Ed25519 public-root fingerprint;
- rebound both offline claims to actual subject bytes;
- reran both Ed25519 signature checks;
- rebuilt both provider observations;
- recomputed provider independence and Portable Trust Receipt equality;
- rebound both receipts through local witness policy;
- recomputed both witness transitions;
- checked byte identity of provider receipt copies;
- verified immutable proof attestations on both receipt copies and the proof result;
- attested its own external verification record.

## Evidence artifacts

Proof artifact `9165397390`:

`sha256:ae84a4638808f923ed5633822be5e159048de94e6e2b11137c05173366445b0f`

External verification artifact `9165410163`:

`sha256:803aebec55db62b1ec0acbcbef390a832cdbbc8fbd3e65b0106215c9f065e064`

Retention: 30 days, expiring 12 Sep 2026.

## Independence matrix

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

## Proven progression

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

## Next boundary

**Execution-Provider + Evidence-Transport Portability v0.1**.

The next falsifiable experiment should place at least one proof production/verification path outside GitHub Actions and move its evidence independently of GitHub artifact storage. The portable contract must remain identical or fail closed; normalization must never hide disagreement about subject, authority, source, execution policy or authorization policy.
