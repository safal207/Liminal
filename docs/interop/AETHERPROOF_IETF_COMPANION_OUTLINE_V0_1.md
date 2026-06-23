# AetherProof × LIMINAL IETF Companion Outline v0.1

**Status:** Early outline  
**Date:** 2026-06-23

## Working title

**Causal Context Binding for AI Execution Receipts**

## Intended relationship

This document is designed as a companion to a core receipt-format draft. The core receipt draft defines authenticity and integrity of the receipt. This companion defines how a signed receipt is bound to the runtime decision, action, actor, evidence set, resource scope, declared purpose, and causal parent event for which it was produced.

## Problem statement

A receipt can be cryptographically valid yet replayed in the wrong runtime context. Examples include reuse under another policy decision, action, purpose, actor, workspace, or parent event.

## Proposed mechanism

- Namespaced signed extension: `org.liminal.agent_chain_context/v0.1`
- RFC 8785 canonicalization
- SHA-256 commitment
- Native receipt profile with `signed_extensions_hash` included in the canonical signing preimage
- Registry-bound, extensible `purpose` semantics
- Context-aware verification outcomes

## Mandatory conformance behavior

A valid receipt bound to `policy_decision_id=dec_204`, when presented where the verifier expects `dec_205`, must produce:

```text
core verification: VALID
context verification: CONTEXT_MISMATCH
```

## Security considerations

- receipt replay across policy or resource boundaries;
- purpose substitution;
- parent-event detachment;
- unknown purpose semantics;
- extension downgrade;
- verifier behavior when context validation is required;
- separation of authenticity, truth, and authority.

## Candidate document sections

1. Introduction
2. Terminology
3. Threat Model
4. Agent Chain Context Extension
5. Canonicalization and Commitment
6. Native Signed-Extension Profile
7. Purpose Registry
8. Verification Procedure
9. Error and Outcome Semantics
10. Conformance Vectors
11. Multi-agent Chain Aggregation
12. Security Considerations
13. IANA Considerations or Equivalent Registry Considerations
14. Privacy Considerations
15. Examples

## Canonical principle

> Integrity proves that the receipt was not changed. Causal binding proves that the receipt was not moved.
