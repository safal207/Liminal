# Consumer Checkpoint Rotation Authority v0.3

## Status

Experimental fail-closed schema bridge for Upstream Rotation-Authority Portability. Historical checkpoint v0.2 remains unchanged and continues to represent its original GitHub-workflow-pinned authority semantics.

## Why v0.3 is required

Checkpoint v0.2 stores concrete GitHub rotation authority in the checkpoint itself:

```text
accepted_evidence.signer_workflow_path
accepted_evidence.signer_workflow_sha
advance_authorizer.workflow_path
advance_authorizer.workflow_sha
```

An independent Ed25519 rotation authority cannot reproduce those bytes without pretending to be the GitHub workflow. That would manufacture portability rather than prove it.

v0.3 therefore changes the representation of authority, not the historical meaning of v0.2.

## Migration

Migration is explicit and limited to the legacy generation-0 checkpoint.

Required externally verified mapping:

- exact legacy checkpoint SHA-256;
- exact legacy `advance_authorizer.workflow_path`;
- exact legacy `advance_authorizer.workflow_sha`;
- trust domain;
- logical rotation ID;
- portable rotation producer-contract SHA-256;
- portable rotation authorization-contract SHA-256.

The canonical migration claim excludes verifier output and `verified` status. Different verifier implementations can establish the same semantic migration claim without changing the v0.3 checkpoint identity.

```text
legacy checkpoint v0.2
+ exact legacy authorizer mapping
+ provider-neutral migration claim
        ↓
checkpoint v0.3 genesis
        ↓
logical rotation authority
```

## Rotation authority identity

`rotation_authority` contains only:

- schema;
- `logical_rotation_id`;
- `rotation_contract_sha256`;
- `authorization_contract_sha256`.

Concrete signer/provider identity is deliberately absent from active authority identity.

`authority_origin` preserves the historical causal link to the legacy GitHub authorizer and migration claim.

## Accepted rotation

For generation > 0 the checkpoint binds the portable rotation transition:

- Portable Rotation-Authority Receipt SHA-256;
- previous/current registry SHA-256;
- previous/current manifest SHA-256;
- from/to generation;
- `registry_rotation_authorized` decision class.

The receipt is evidence of an already verified pre-transition authorization. A post-transition receipt never authorizes the transition that produced it.

## Fail-closed advancement

Advancement requires:

1. exact one-generation increment;
2. valid candidate registry and manifests;
3. exact predecessor registry and manifest binding;
4. exact logical rotation authority contract match;
5. externally established `verified=True` rotation evidence;
6. exact current registry and manifest binding;
7. recomputed Portable Rotation-Authority Receipt digest match;
8. append-only registry history and root/policy rotation validation;
9. canonical v0.3 checkpoint predecessor hash.

Any mismatch rejects the transition.

## Separation from witness authority

Checkpoint v0.3 solves a different dependency from Checkpoint Witness Authority v0.3.

```text
rotation authority
    ↓
checkpoint construction / registry acceptance
    ↓
checkpoint producer authority
    ↓
witness transition
```

The source/control portability proof already made checkpoint producer authority logical. Upstream Rotation-Authority Portability now makes the preceding rotation authorization logical as well.

The existing witness v0.3 validator still consumes historical checkpoint v0.2. A later explicit adapter/revision must consume checkpoint v0.3 without silently changing the meaning of already verified witness v0.3 history.

## Claim boundary

This schema alone does not prove upstream rotation-authority portability. A live claim requires independent control-plane and producer signatures, exact portable rotation agreement, independent checkpoint-v0.3 construction, immutable verifier execution, and independent audit recomputation.
