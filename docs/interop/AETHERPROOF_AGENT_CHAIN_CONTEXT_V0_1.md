# AetherProof × LIMINAL Agent Chain Context Extension v0.1

**Status:** Draft interoperability note  
**Projects:** AetherProof / LIMINAL  
**Authors:** Alexey Safalov and collaborators  
**Date:** 2026-06-23

## 1. Purpose

AetherProof establishes a cryptographic authenticity boundary around an AI output receipt. LIMINAL adds a causal boundary that identifies which runtime action, policy decision, evidence set, actor, resource scope, and parent event the receipt belongs to.

The combined invariant is:

> A cryptographic receipt proves that the signed claim is authentic and unmodified; an agent-chain context proves which runtime decision and causal path the claim belongs to.

This note defines a provider-neutral extension object and two integration profiles.

The extension schema is available at:

- [`schemas/agent-chain-context-v0.1.schema.json`](../../schemas/agent-chain-context-v0.1.schema.json)

---

## 2. Threat model

A receipt may be cryptographically valid while being replayed in the wrong context.

Examples:

- a receipt generated for one `action_id` is reused for another action;
- a receipt authorized by one `policy_decision_id` is presented under another decision;
- an answer-generation receipt is reused as evidence of tool authorization;
- a receipt from one workspace or tenant is replayed in another resource scope;
- a valid child receipt is detached from its original parent event in a multi-agent chain.

The extension is designed to make those context substitutions detectable.

---

## 3. Extension namespace

Canonical namespace:

```text
org.liminal.agent_chain_context/v0.1
```

Example extension object:

```json
{
  "schema_version": "0.1",
  "action_id": "act_042",
  "run_id": "run_017",
  "parent_event_id": "evt_991",
  "policy_decision_id": "dec_204",
  "evidence_refs": ["file_88", "mem_12"],
  "purpose": "tool_authorization",
  "actor_id": "agent_deployer",
  "resource_scope": "tenant/acme/repository/api/environment/production"
}
```

### Field meanings

- `schema_version` — version of this extension object;
- `action_id` — stable identity or digest of the exact action envelope;
- `run_id` — execution run containing the action;
- `parent_event_id` — immediate causal parent, if present;
- `policy_decision_id` — governing decision or authorization record, if present;
- `evidence_refs` — stable references to supporting evidence;
- `purpose` — declared function for which the output or action was produced;
- `actor_id` — responsible agent, human, service, or composite actor;
- `resource_scope` — bounded workspace, tenant, repository, account, or environment.

---

## 4. Canonicalization and commitment

Before hashing, an implementation MUST:

1. validate the object against the JSON Schema;
2. remove no fields and add no implicit fields after validation;
3. ensure `evidence_refs` contains unique values;
4. sort `evidence_refs` lexicographically by Unicode code point;
5. serialize the object using RFC 8785 JSON Canonicalization Scheme (JCS);
6. hash the UTF-8 canonical bytes with SHA-256.

Commitment representation:

```text
sha256:<64 lowercase hexadecimal characters>
```

Pseudocode:

```text
normalized = validate_and_sort(agent_chain_context)
canonical_bytes = JCS(normalized).encode("utf-8")
context_commitment = "sha256:" + SHA256(canonical_bytes).hex()
```

### Invariant

> Semantically identical extension objects must produce the same commitment, while any material context change must produce a different commitment.

---

## 5. Profile A — native receipt v1.2

This is the preferred integrated profile.

A receipt adds:

```json
{
  "receipt_version": "1.2",
  "signed_extensions": {
    "org.liminal.agent_chain_context/v0.1": {
      "schema_version": "0.1",
      "action_id": "act_042",
      "run_id": "run_017",
      "parent_event_id": "evt_991",
      "policy_decision_id": "dec_204",
      "evidence_refs": ["file_88", "mem_12"],
      "purpose": "tool_authorization",
      "actor_id": "agent_deployer",
      "resource_scope": "tenant/acme/repository/api/environment/production"
    }
  },
  "signed_extensions_hash": "sha256:..."
}
```

`signed_extensions_hash` is computed over the RFC 8785 canonical form of the complete `signed_extensions` object.

For receipt version `1.2`, the canonical signing preimage appends `signed_extensions_hash` after the existing v1.1 canonical fields.

Conceptual field order:

```text
receipt_version
model_weight_root
model_root_type
input_commitment
output_hash
timestamp_ms
log_sequence
canonical_hw_evidence
log_anchor
signed_extensions_hash
```

Each field remains encoded with the existing injective length-prefix format:

```text
<len>:<field>
```

### Verification

A v1.2 verifier MUST:

1. validate the receipt's required core fields;
2. validate every recognized signed extension;
3. canonicalize the full `signed_extensions` map;
4. recompute `signed_extensions_hash`;
5. reject on a hash mismatch;
6. reconstruct the v1.2 signing preimage;
7. verify the receipt signature;
8. optionally compare expected runtime context values.

### Compatibility statement

- Existing v1.1 receipts remain valid and unchanged.
- A v1.2-aware verifier can verify both v1.1 and v1.2 receipts.
- A v1.1-only verifier is not expected to verify a v1.2 signature because the signed preimage is intentionally extended.

This is receipt-format backward compatibility, not binary compatibility with an unmodified legacy verifier.

---

## 6. Profile B — legacy chained binding for v1.1

This optional profile preserves an existing v1.1 receipt and its signature byte-for-byte.

A separate binding artifact signs:

```json
{
  "binding_version": "0.1",
  "base_receipt_id": "receipt_...",
  "base_signature_hash": "sha256:...",
  "extension_namespace": "org.liminal.agent_chain_context/v0.1",
  "extension_hash": "sha256:..."
}
```

The binding artifact has its own signature, preferably produced by the same signer identity or by an explicitly identified trusted binding service.

A legacy verifier can still verify the base receipt. A context-aware verifier additionally verifies the binding artifact and extension hash.

### Trade-off

Profile B provides stronger legacy-tool compatibility but introduces a second signed artifact and an additional verification step.

---

## 7. Purpose semantics

The `purpose` field is not descriptive decoration. It is part of the signed causal context.

Initial values:

```text
tool_authorization
experiment_verification
answer_generation
memory_transfer
workflow_continuation
other
```

A verifier MAY enforce an expected purpose.

Example:

```text
expected purpose: tool_authorization
receipt purpose: answer_generation
result: CONTEXT_MISMATCH
```

### Invariant

> Authentic output produced for one declared function must not silently authorize a different function.

---

## 8. Verification outcomes

A context-aware verifier should distinguish at least:

```text
VALID
INVALID_SIGNATURE
INVALID_EXTENSION_SCHEMA
EXTENSION_HASH_MISMATCH
CONTEXT_MISMATCH
UNSUPPORTED_RECEIPT_VERSION
UNSUPPORTED_EXTENSION
```

`UNSUPPORTED_EXTENSION` may remain non-fatal when the caller requests core-only verification. It must be fatal when context verification is required by policy.

---

## 9. Minimal conformance vectors

### Vector 1 — action identity changes

- same model output;
- same hardware evidence;
- same policy decision;
- different `action_id`.

Expected:

```text
different context commitment
different valid receipt signature in Profile A
```

### Vector 2 — policy decision replay

- receipt is valid;
- runtime expects `policy_decision_id=dec_205`;
- receipt contains `policy_decision_id=dec_204`.

Expected:

```text
core cryptographic verification: VALID
context verification: CONTEXT_MISMATCH
```

### Vector 3 — JSON key order changes

- same extension values;
- different object key order and whitespace.

Expected:

```text
same RFC 8785 canonical bytes
same context commitment
```

### Vector 4 — evidence reference order changes

- same unique evidence references;
- different input array order.

Expected after required normalization:

```text
same sorted evidence_refs
same context commitment
```

### Vector 5 — purpose changes

- all fields identical;
- `purpose` changes from `answer_generation` to `tool_authorization`.

Expected:

```text
different context commitment
```

### Vector 6 — legacy receipt

- receipt version is `1.1`;
- no signed extensions are present.

Expected:

```text
v1.2-aware core verification: VALID
context-required policy: UNSUPPORTED_EXTENSION or policy-defined rejection
```

### Vector 7 — resource-scope replay

- valid receipt for `environment/staging`;
- presented for `environment/production`.

Expected:

```text
CONTEXT_MISMATCH
```

---

## 10. Multi-agent chain aggregation

Each hop in a multi-agent pipeline may carry its own receipt and `parent_event_id`.

A chain index may aggregate receipt commitments into a Merkle root while preserving parent links separately.

The Merkle root proves set integrity. Parent links preserve causal structure.

### Invariant

> Aggregation must not erase which event caused which later event.

This note does not prescribe the full aggregation format; that may be defined in a later Agent Chain Manifest specification.

---

## 11. Non-goals

This extension does not:

- prove that an LLM's reasoning was correct;
- replace policy evaluation;
- convert a memory claim into authorization;
- define private trust or risk scoring;
- require one model provider or agent framework;
- make probabilistic model output deterministic.

It binds a signed output receipt to a declared causal context that can be verified deterministically.

---

## 12. Open questions for co-authors

1. Should Profile A become AetherProof receipt `1.2`, or use another version identifier?
2. Should `signed_extensions_hash` bind the complete extension map or a sorted list of per-extension commitments?
3. Should `parent_event_id` identify an event, a receipt, or permit either with a typed prefix?
4. Should `actor_id` include an explicit actor type?
5. Should Profile B be standardized now or left as an implementation note?
6. Which `purpose` values belong in the initial stable enum?

---

## 13. Canonical principle

> Integrity proves that the receipt was not changed. Causal binding proves that the receipt was not moved.
