# Source-Producer + Control-Plane Portability v0.1

## Status

**VERIFIED for the concrete tested transition.**

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

Logical producer:

`liminal.trusted-recovery.checkpoint-producer`

Producer Contract SHA-256:

`72bba8eddc81e88c2e9ad24e266713e9534f6c332fec7ad5ecaa264f922b7ca3`

Authorization Contract SHA-256:

`576da1fa0c5cd70313ad1d89de88f4a7048e13fa5d0ce05c833f7bef4233a553`

Evidence type:

`trusted-recovery-consumer-checkpoint`

These identities are portable semantics. Concrete producer/control-plane identities are retained as audit metadata and must differ for a portability claim.

## Witness Authority v0.3

The legacy v0.2 witness pins a concrete GitHub workflow signer. The stronger v0.3 model does not relabel an external producer as that signer.

Instead it performs an explicit migration:

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

Legacy witness SHA-256:

`af12743396296c788223d3087f427b1f93d3086a5aeb9b7c8c0f38d49347e9f9`

Canonical migration claim SHA-256:

`aec92a1c1100e6ea5944e042cd5e7c56f3ebc01b5a957782482231732d504f10`

Stronger v0.3 generation-0 witness SHA-256:

`8d2e44dab167f1f4613ef66257ca3c3be19f2168a87b620483628389b771ca8c`

The migration claim excludes raw verifier output. Verification must succeed, but verifier-specific bytes do not become witness identity.

## Independent secondary producer/control plane

The secondary control plane was created outside GitHub Actions and signed with an Ed25519 root unavailable to the GitHub proof workflow.

Control-plane root:

`ed25519-sha256:ecd3d6167557ed9d8dfbd3cccb75c72ea38da3ed09b89fa4f277cbcac3c51bb6`

Secondary producer root:

`ed25519-sha256:452f19f3bcee0a79e3907224474803a45cca5edaa2b1dff5e43b1fb7ea764408`

Standalone producer implementation SHA-256:

`e45233b9432f00f21d82c5a29875e445045f705f2d2cd1560d1312d7a5f6eccb`

The standalone producer did not import `liminal.*`. It consumed the bounded rotation inputs and signed external control-plane contracts, independently constructed generation-1 checkpoint bytes, and only then compared its output to the GitHub producer output.

Both producers produced exactly:

`74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`

## External proof envelope

Committed public envelope:

`trust/experimental/source-control-portability-v0.1/external-source-control-proof-v0.1.json`

It contains public keys, contracts, migration claim, independently produced checkpoint, pre-transition authority evidence, v0.3 witnesses and post-transition observation. It contains **no private key**.

External payload SHA-256:

`0a6d137d8d976675bae84466e14a08a85d335ba80a5829a559f23b0f4c773c70`

Envelope SHA-256:

`c1a2675c26ab9c8c2b0064259fac2c18b1e1146c57863e72a09e0a68711cf7b3`

The whole canonical payload is independently signed by both the control-plane root and the producer root.

## Stronger immutable proof

Reusable verifier:

`d4d498288afac1d26e37f62ff8a8c17746d25d8d`

The reusable verifier passed exact-head gates before pinning:

- Python CI `31673332543` — SUCCESS
- Python Integration `31673332545` — SUCCESS
- Artillery `31673332564` — SUCCESS

Pinned caller:

`b6cf8dbe1f3e846e2abc430f905e69a07a5fb78f`

Successful one-shot:

`31673608370` — **FULL SUCCESS**

The run used immutable upstream producer:

`f31b56a5e21a668bcb98791b05542652760dcc27`

and immutable rotation workflow:

`e2cb6a014236bc561d03c405f4986146026041fa`

## Stronger proof identities

Checkpoint subject:

`74096c48cd730c55dd2f486f1af4b211b4f7f1ce38613134be645055ff1f946a`

Witness decision:

`checkpoint_witness_advanced`

Next v0.3 witness SHA-256:

`efc242be9ebeb3bf898c3cee301391525d1609d499f44c7ae4eac9ce4e5cb4ed`

Portable Source-Control Receipt SHA-256:

`9d6a90e5f079b8c8bde01ab858fa9b9050603f3245d5008b0a90d61301a5c73a`

Canonical result SHA-256:

`e57156c8645c1c68ad73bff06513ccf14bb15e44f7c41d0ad96f8c814cf9aada`

Independent audit record SHA-256:

`6821468c5c6f1543ff63554c313cd306e6d812a9b9850f9a040b3dcae1683069`

## Evidence artifacts

Proof:

- artifact `9170683259`
- `sha256:638de3db91e4d45e9208ac7d2b093dfb023357f3f98f36cf8c6f5ae19e64a4ab`

Independent audit:

- artifact `9170705052`
- `sha256:2023aa344dc5c95036dfe6dfdca8c6876b160d68d76756a674c1d0eb97bcb2ac`

The audit did not trust the proof result boolean. It independently:

1. verified the immutable stronger workflow signer on the result;
2. reverified the fresh GitHub checkpoint producer attestation;
3. reconstructed the external canonical payload;
4. reverified both external Ed25519 signatures;
5. recomputed both public-key fingerprints;
6. recomputed contracts and migration claim;
7. reconstructed the v0.3 witness root;
8. recomputed both checkpoint transitions;
9. recomputed source/control agreement;
10. required the recomputed result bytes to equal the original result bytes exactly.

## Historical v0.1 proof

Before the stronger migration binding was introduced, the first immutable implementation also produced a successful result:

- immutable verifier `32152ef2b8f7f134b7830743a70c6bc903b64c1c`
- one-shot `31669188983` — SUCCESS
- proof artifact `9169187861` — `sha256:062c509d840557e798cd20f161982df16393ddaaf33589683c063f0a692d4c1b`
- audit artifact `9169232567` — `sha256:020c8e8aed36889e1fa19401e3c4180ef2ce806a84b3547ed7d1b05927833cb7`

That proof remains historical evidence. It is superseded for current authority semantics because the stronger revision additionally binds the exact legacy signer mapping and a canonical provider-neutral migration claim. The historical immutable commit/run are not rewritten.

## Fail-closed boundary

The current implementation rejects:

- same producer provider;
- same control-plane provider;
- unverified observations;
- subject drift;
- logical producer drift;
- producer-contract drift;
- authorization-contract drift;
- evidence-type drift;
- generation drift;
- witness-decision drift;
- next-witness drift;
- unverified migration evidence;
- legacy witness digest mismatch;
- legacy signer mismatch;
- migration trust-domain mismatch;
- checkpoint authority subject/generation/contract mismatch.

The post-transition `PortableSourceControlReceipt` is an output comparison receipt. It is never used as the credential that authorizes the same transition.

## Claim boundary

This gate proves the tested checkpoint transition survived a change from:

```text
GitHub Actions checkpoint producer
+ GitHub repository policy control plane
```

to:

```text
standalone OpenAI-isolated-workspace producer
+ separately signed offline Ed25519 control plane
```

while preserving the exact subject and trusted transition.

It does **not** prove independence of the upstream rotation producer/control plane. The secondary checkpoint producer still consumed rotation evidence whose upstream authority originates in the existing GitHub rotation chain.

It also does not claim organizational governance, hardware provenance, network-path independence, or universal provider independence.

## Next falsifiable question

Can the remaining upstream rotation-authority dependency itself become portable, so that both the checkpoint producer **and the causal authorization input that permits its generation advance** are independently produced under provider-neutral authority contracts?
