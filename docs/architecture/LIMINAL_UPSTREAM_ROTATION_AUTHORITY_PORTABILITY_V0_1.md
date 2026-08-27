# Upstream Rotation-Authority Portability v0.1

## Status

**VERIFIED — 2026-08-13.**

A live immutable proof established the same portable rotation authority semantics across:

- the historical immutable GitHub rotation producer/control plane; and
- a standalone producer executed outside GitHub Actions under a separately signed offline Ed25519 rotation control plane.

Both paths produced the same generation-1 manifest and registry, the same Portable Rotation-Authority Receipt, the same checkpoint-v0.3 generation-1 state, and the same witness-v0.4 trusted transition. A second audit job independently repeated signer checks, all four external Ed25519 signature checks, semantic recomputation, and exact result-byte comparison.

RESONANCE journal entry: `signals/010-upstream-rotation-authority-portability.md`, commit `2b8513e734d79121c57df7f9414f6e2771d09371`.

## Goal

Remove the remaining causal dependency on the existing GitHub rotation authority.

```text
primary path
immutable GitHub rotation producer
+ GitHub rotation control plane
        ↓
verified rotation authority A

secondary path
standalone independent rotation producer
+ separately signed offline control plane
        ↓
verified rotation authority B

A == B on portable semantics
        ↓
same checkpoint-v0.3 state
        ↓
checkpoint producer authority v0.2
        ↓
same witness-v0.4 trusted transition
```

The historical GitHub rotation workflow `e2cb6a014236bc561d03c405f4986146026041fa` remains immutable evidence. It is not rewritten or re-labelled as the independent authority.

## Portable rotation identity

Concrete signer/provider identity is evidence about authority, not the authority identity itself.

Portable identity binds:

- `logical_rotation_id`;
- `rotation_contract_sha256`;
- `authorization_contract_sha256`;
- previous registry SHA-256;
- current registry SHA-256;
- previous manifest SHA-256;
- current manifest SHA-256;
- source generation;
- target generation;
- `registry_rotation_authorized` decision class.

Concrete rotation-producer provider/instance and control-plane provider/ID are retained as audit metadata and excluded from the portable receipt.

## Verified live proof

### Immutable chain

- primary rotation workflow: `e2cb6a014236bc561d03c405f4986146026041fa`
- final reusable verifier revision: `28d96de36267fde8e1c66ce0c5f36c2c30e44813`
- pinned caller: `5a22e30b3248416d638dc441a4fba0edd9c95edd`
- successful one-shot: **`31690895530` — FULL SUCCESS**

The reusable verifier revision passed exact-head gates before being pinned:

- Python CI `31690292430` — SUCCESS
- Python Integration `31690292458` — SUCCESS
- Artillery `31690292372` — SUCCESS

### Independent external roots

- rotation control plane: `ed25519-sha256:972ce43feacdb0747fad6e0fe02d1fbe06270727600fca65b185cd94f55c1062`
- standalone rotation producer: `ed25519-sha256:26976b8f46f2d970cda4667f64a814c1a3316747fde791e4102fd5fb54942f8c`
- standalone implementation SHA-256: `e8e3e0c177e018ea525c264ba6290444a803834afd6e4515895dfa120a2a548c`
- external envelope SHA-256: `27186c859f6793305a8c405927db84a6f1df7e0152a645ec4dfb4597bbe4ffbb`

The external control plane signed three pre-transition objects: the producer contract, authorization contract, and exact rotation intent. A separate producer root signed the independently produced rotation result. The GitHub proof and independent audit each verified all four Ed25519 signatures and recomputed both public-root fingerprints.

### Portable contracts and state identities

- logical rotation ID: `liminal.trusted-recovery.registry-rotation`
- rotation producer contract: `58e797025b5326f10bc73666034ed407a73f728ea9126170c185b5e59266889b`
- rotation authorization contract: `f23cfdb4f78f6980420c1511f7624e2aaa8d876509937c9577595c32ecbc7a1e`
- rotation authorization intent: `d090275e16a7ad0fe161c9c05339858aa018879fd413be0e1ca7aae4ebb6c29d`
- predecessor registry: `bd43cb039d29245f3d7eb8b78a7a5fcde14d7bf638c4dfe98bb300b00f8670e1`
- predecessor manifest: `bd8aaa6162d0f7e9627e10ee6d495810820fd6fd8cd07d9d48e5d585786537b5`
- generation-1 registry: `5441072b0e550995a9ad0b27b4f3af7c7b5bf531f59e27c870ab1a8cf61789a1`
- generation-1 manifest: `b9cb0b37da2d74ece6c1cf780b06b17fbbb96f02e073ac64fb26be49cae24277`
- Portable Rotation-Authority Receipt: `9576a9f96acd278d873c65f4dcaf974a661bf5547319ba1fd60b874f89aef368`

### Downstream causal continuity

The verified rotation receipt was consumed through the provider-neutral downstream chain:

- checkpoint-v0.3 genesis: `0833f2463235554ab80f374fee9f14f887391e4939b7f5d082fabce4f57b821f`
- checkpoint-v0.3 generation 1: `cfe0ede206da217fa774cd980c20032857692c461c421ffceeeacfe863276e1a`
- witness-v0.4 genesis: `46c7758d25958216c07363176bea3106eceaad58f2f0bdb28ff983b56349f7c9`
- witness decision: `checkpoint_witness_advanced`
- next witness: `ed385f07200b424937498374035ce11d0e4327a4c42ff701c7842bc74cee8dc6`

This is the important causal claim: rotation-provider independence survived downstream checkpoint construction and witness advancement rather than stopping at rotation-result equality.

### Proof and independent audit

Canonical proof result:

- schema: `liminal-upstream-rotation-authority-portability-proof/v0.1`
- reason: `independent_upstream_rotation_authority_semantics_agree`
- result SHA-256: `9e80d1dd529055b78269660301e59b94afb12cb102c0450053b5e1036418c34f`

Independent audit:

- schema: `liminal-upstream-rotation-authority-external-audit/v0.1`
- reason: `upstream_rotation_authority_recomputed`
- audit SHA-256: `909ce2af98e20b910d7460bb5fda45fa1ab8b703a0666e8391de6986445d03ea`

Run artifacts:

- proof `9177340936` — `sha256:6b3aff98b6a5a2b63692dbd92180acb65adde417aa72847013e978244595f700`
- external audit `9177355189` — `sha256:20c4a07561b40caf749934ba5fbb39ce1110fa96df5a006f572cb0a75f07feec`
- primary rotation evidence `9177328748` — `sha256:7926c741f547b0bf689c119b953a017ea42fa93750daf3b82bbad66505e3cff9`

These run artifacts were configured with 30-day retention and reported GitHub expiry timestamps on 2026-09-12.

## Independence requirements

Portability is accepted only because:

1. the rotation producer providers differ;
2. the rotation control-plane providers differ;
3. both paths were cryptographically established outside the comparison module;
4. both paths bind the exact same predecessor registry and manifest;
5. both paths bind the exact same next registry and manifest;
6. both paths implement the same rotation producer and authorization contract digests;
7. both authorize exactly one generation advance;
8. both return `registry_rotation_authorized`;
9. both feed the same Portable Rotation-Authority Receipt into checkpoint-v0.3 construction;
10. downstream checkpoint producer authority is explicitly migrated to contracts that describe checkpoint-v0.3 semantics.

A second workflow SHA inside the same authority would not satisfy this independence requirement.

## Downstream checkpoint contract migration

Model review exposed an important semantic mismatch before live proof: checkpoint producer contract v0.1 explicitly named checkpoint v0.2 and the legacy rotation-result schema, so it could not honestly authorize checkpoint v0.3 merely because the logical producer role stayed the same.

The verified chain therefore uses:

- `portable-checkpoint-producer-contract-v0.2.json`;
- `portable-checkpoint-authorization-contract-v0.2.json`;
- witness v0.4 with an explicit old-authority → new-authority contract migration.

The logical checkpoint producer identity and evidence type remain stable, while producer/authorization contract digests change explicitly and are bound into the canonical witness migration claim.

## First pin failure and correction trail

The first immutable caller run, `31689958160`, failed at semantic recomputation with:

`standalone_source_digest_mismatch`

This was **not** a cryptographic or rotation-authority failure. The primary signer verification and all four external Ed25519 signatures had already succeeded. Two comment-only source lines were omitted when the standalone producer was published, so the repository bytes no longer matched the implementation SHA bound into the signed producer result.

The correction restored exactly the originally signed standalone source bytes without changing executable semantics, contracts, roots, signatures, rotation claims, or authority. The corrected source/verifier revision `28d96de3...` then passed exact-head CI before being repinned. The failed run remains immutable historical evidence of the provenance gate working as intended.

## Trust boundary

`rotation_authority_portability.py` is comparison-only. It does not run a producer, verify signatures, infer independence from labels, manufacture `verified=True`, or authorize a transition from its own post-transition receipt.

Checkpoint v0.3 and witness v0.4 likewise consume already verified authority evidence; they do not perform cryptographic verification internally.

## Claim boundary

This milestone proves **upstream rotation-producer and rotation-control-plane portability for the tested generation-0 → generation-1 transition**, including downstream checkpoint-v0.3 and witness-v0.4 convergence.

It does **not** prove:

- independent genesis or historical trust-base creation;
- organizational-governance independence;
- hardware-provenance independence;
- network-path independence;
- universal provider independence;
- indefinite multi-provider durability.

Both tested paths intentionally share the existing Liminal predecessor registry/manifest history. Therefore the strongest remaining causal dependency is the shared genesis/history itself.

## Next falsifiable gate

**Genesis / Historical Trust-Base Portability v0.1**.

Can materially independent trust-base providers establish semantically equivalent genesis/history under an explicit portable genesis contract and still reproduce the same downstream rotation, checkpoint, and witness semantics without inheriting the same historical Liminal predecessor bytes as ambient authority?
