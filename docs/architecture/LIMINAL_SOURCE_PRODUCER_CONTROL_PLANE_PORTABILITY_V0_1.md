# Source-Producer + Control-Plane Portability v0.1

## Status

**VERIFIED for the concrete tested checkpoint transition.**

The stronger revision proves that the same checkpoint subject and the same trusted state transition survive a material change of checkpoint producer and authorization control plane without impersonating the legacy GitHub signer.

## Proven shape

```text
primary path
immutable GitHub checkpoint producer
+ GitHub repository policy/control plane
        ↓
verified pre-transition authority claims

secondary path
standalone producer in OpenAI isolated workspace
+ separately signed offline Ed25519 control plane
        ↓
verified pre-transition authority claims

same logical producer
+ same producer contract
+ same authorization contract
+ same checkpoint subject
        ↓
same Checkpoint Witness Authority v0.3 root
        ↓
same checkpoint_witness_advanced transition
        ↓
same next-witness SHA-256
        ↓
Portable Source-Control Receipt
```

## Portable contracts

- logical producer: `liminal.trusted-recovery.checkpoint-producer`
- producer contract SHA-256: `72bba8eddc81e88c2e9ad24e266713e9534f6c332fec7ad5ecaa264f922b7ca3`
- authorization contract SHA-256: `576da1fa0c5cd70313ad1d89de88f4a7048e13fa5d0ce05c833f7bef4233a553`
- evidence type: `trusted-recovery-consumer-checkpoint`

Concrete producer/control-plane identities are audit metadata and must differ for a portability claim. They are excluded from Portable Source-Control Receipt identity.

## Stronger Witness Authority v0.3 migration

The legacy v0.2 witness pins a concrete GitHub workflow signer. The stronger v0.3 model never relabels an external producer as that signer.

Migration binds:

```text
legacy witness SHA-256
+ exact legacy signer workflow path
+ exact legacy signer workflow SHA
+ trust domain
+ logical producer id
+ producer contract SHA-256
+ authorization contract SHA-256
+ evidence type
+ migration reason
        ↓
canonical migration_claim_sha256
        ↓
logical checkpoint authority
```

Key identities:

- legacy witness: `af12743396296c788223d3087f427b1f93d3086a5aeb9b7c8c0f38d49347e9f9`
- canonical migration claim: `aec92a1c1100e6ea5944e042cd5e7c56f3ebc01b5a957782482231732d504f10`
- stronger v0.3 generation-0 witness: `8d2e44dab167f1f4613ef66257ca3c3be19f2168a87b620483628389b771ca8c`

Raw verifier output is excluded from migration identity. Verification must succeed, but verifier-specific bytes do not change the logical migration claim.

## Independent secondary path

External control-plane root:

`ed25519-sha256:ecd3d6167557ed9d8dfbd3cccb75c72ea38da3ed09b89fa4f277cbcac3c51bb6`

External producer root:

`ed25519-sha256:452f19f3bcee0a79e3907224474803a45cca5edaa2b1dff5e43b1fb7ea764408`

Standalone producer implementation SHA-256:

`e45233b9432f00f21d82c5a29875e445045f705f2d2cd1560d1312d7a5f6eccb`

The secondary producer did not import `liminal.*`. It consumed bounded rotation inputs plus signed external control-plane contracts and independently constructed the generation-1 checkpoint before comparison with the GitHub producer output.

Both producers produced exactly:

`74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`

## Dual-signed external envelope

Current public evidence:

`trust/experimental/source-control-portability-v0.1/external-source-control-proof-v0.1.json`

It contains public roots, portable contracts, canonical migration claim, independently produced checkpoint, pre-transition authority evidence, v0.3 witnesses and post-transition observation. It contains no private key.

- external payload SHA-256: `0a6d137d8d976675bae84466e14a08a85d335ba80a5829a559f23b0f4c773c70`
- external envelope SHA-256: `c1a2675c26ab9c8c2b0064259fac2c18b1e1146c57863e72a09e0a68711cf7b3`

The canonical payload is signed independently by both the control-plane root and producer root.

## Stronger immutable proof

Reusable verifier:

`d4d498288afac1d26e37f62ff8a8c17746d25d8d`

It passed exact-head checks before pinning:

- Python CI `31673332543` — SUCCESS
- Python Integration `31673332545` — SUCCESS
- Artillery `31673332564` — SUCCESS

Pinned caller:

`b6cf8dbe1f3e846e2abc430f905e69a07a5fb78f`

Successful one-shot:

`31673608370` — **FULL SUCCESS**

Immutable upstream checkpoint producer:

`f31b56a5e21a668bcb98791b05542652760dcc27`

Immutable upstream rotation workflow:

`e2cb6a014236bc561d03c405f4986146026041fa`

## Stronger proof identities

- checkpoint subject: `74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`
- witness reason: `checkpoint_witness_advanced`
- next v0.3 witness: `efc242be9ebeb3bf898c3cee301391525d1609d499f44c7ae4eac9ce4e5cb4ed`
- Portable Source-Control Receipt: `9d6a90e5f079b8c8bde01ab858fa9b9050603f3245d5008b0a90d61301a5c73a`
- canonical result: `e57156c8645c1c68ad73bff06513ccf14bb15e44f7c41d0ad96f8c814cf9aada`
- independent audit record: `6821468c5c6f1543ff63554c313cd306e6d812a9b9850f9a040b3dcae1683069`

Evidence:

- proof artifact `9170683259` — `sha256:638de3db91e4d45e9208ac7d2b093dfb023357f3f98f36cf8c6f5ae19e64a4ab`
- audit artifact `9170705052` — `sha256:2023aa344dc5c95036dfe6dfdca8c6876b160d68d76756a674c1d0eb97bcb2ac`

The audit independently reverified the stronger workflow signer, fresh GitHub checkpoint signer, both external Ed25519 signatures, public-key fingerprints, portable contracts, migration claim, v0.3 witness transition and exact result bytes.

## Historical first proof

The first immutable source/control implementation also produced a successful proof:

- verifier `32152ef2b8f7f134b7830743a70c6bc903b64c1c`
- run `31669188983` — SUCCESS
- proof artifact `9169187861` — `sha256:062c509d840557e798cd20f161982df16393ddaaf33589683c063f0a692d4c1b`
- audit artifact `9169232567` — `sha256:020c8e8aed36889e1fa19401e3c4180ef2ce806a84b3547ed7d1b05927833cb7`

That run remains immutable historical evidence. It is superseded for current authority semantics because the stronger revision additionally binds the exact legacy signer mapping and canonical provider-neutral migration claim. The current branch carries only the stronger executable authority path.

## Fail-closed boundary

The current path rejects producer/control-plane non-independence, unverified observations, subject/logical-producer/contract/evidence-type/generation/witness drift, invalid legacy witness migration, exact legacy signer mismatch, migration trust-domain mismatch and checkpoint authority mismatch.

The post-transition Portable Source-Control Receipt is an output comparison receipt and never authorizes the same transition.

## Claim boundary

This proof establishes checkpoint-source producer and checkpoint-authorization control-plane portability for the concrete tested transition.

**It does not yet establish upstream rotation-authority portability.** The independent checkpoint producer consumed rotation evidence whose causal authorization still originates in the existing GitHub rotation chain.

It also does not claim organizational-governance, hardware-provenance, network-path or universal provider independence.

## Next falsifiable question

Can the upstream rotation-authority input itself be independently produced and authorized under provider-neutral contracts, so both the checkpoint producer and the causal authorization that permits its generation advance survive a control-plane change?
