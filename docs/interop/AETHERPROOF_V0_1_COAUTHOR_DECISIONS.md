# AetherProof × LIMINAL v0.1 Co-author Decisions

**Status:** Draft decision log  
**Date:** 2026-06-23

This note records the currently agreed design direction for the AetherProof Agent Chain Context extension.

## Agreed direction

1. **Native receipt v1.2 is the preferred profile.**
   - `signed_extensions_hash` is appended to the canonical signing preimage.
   - Context integrity is first-class and covered by the receipt signature.
   - Legacy chained binding remains an optional compatibility note, not the recommended path.

2. **RFC 8785 JCS is the canonicalization scheme.**
   - Structured extension objects are canonicalized with RFC 8785.
   - SHA-256 commits the canonical UTF-8 bytes.

3. **`purpose` is extensible but registry-bound.**
   - The signed context contains `purpose_registry`.
   - The initial registry is `org.liminal.agent_chain_purpose/v0.1`.
   - Unknown syntactically valid values are schema-valid but policy-dependent.
   - A verifier requiring registered semantics returns `UNSUPPORTED_PURPOSE` for unknown values.

4. **Policy-decision replay rejection is mandatory conformance behavior.**
   - A cryptographically valid receipt presented under a different expected `policy_decision_id` passes core verification but fails context verification with `CONTEXT_MISMATCH`.

## Machine-readable artifacts

- Schema: `schemas/agent-chain-context-v0.1.schema.json`
- Purpose registry: `registries/agent-chain-purpose-v0.1.json`
- Mandatory replay fixture: `fixtures/aetherproof-agent-chain-context-replay-v0.1.json`
- Main interoperability draft: `docs/interop/AETHERPROOF_AGENT_CHAIN_CONTEXT_V0_1.md`

## Remaining co-author questions

- final receipt version identifier;
- full extension-map hash versus per-extension commitments;
- typed semantics for `parent_event_id`;
- explicit actor typing;
- whether legacy chained binding belongs in the normative or informative section;
- IETF companion-document structure and terminology.

## Canonical principle

> Integrity proves that the receipt was not changed. Causal binding proves that the receipt was not moved.
