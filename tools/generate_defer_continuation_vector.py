#!/usr/bin/env python3
"""Generate independent DEFER/continuation conformance bytes and digests.

This generator intentionally does not read the JSON fixture. It starts from the
published profile definitions and semantic inputs, then emits canonical UTF-8
bytes and SHA-256 digests that can be compared with the fixture independently.

The supported values in this vector avoid floating-point numbers, so Python's
sorted, minimal JSON encoding is equivalent to RFC 8785 JCS for these inputs.
"""

from __future__ import annotations

import copy
import hashlib
import json
from typing import Any

ACTION_PROFILE = "org.liminal.defer.action-identity.v0.1"
BINDING_PROFILE = "org.liminal.defer.binding.v0.1"


def canonical_json(value: Any) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )


def sha256_uri(canonical_text: str) -> str:
    digest = hashlib.sha256(canonical_text.encode("utf-8")).hexdigest()
    return f"sha256:{digest}"


def digests(action_preimage: dict[str, Any], binding_preimage: dict[str, Any]) -> dict[str, str]:
    action_bytes = canonical_json(action_preimage)
    binding_bytes = canonical_json(binding_preimage)
    return {
        "canonical_action_identity_bytes_utf8": action_bytes,
        "action_ref": sha256_uri(action_bytes),
        "canonical_binding_bytes_utf8": binding_bytes,
        "binding_digest": sha256_uri(binding_bytes),
    }


def main() -> None:
    base_action = {
        "profile_id": ACTION_PROFILE,
        "caller_id": "agent:deploy",
        "tool_id": "tool:deploy",
        "resource_scope": "tenant:acme/environment:production",
    }
    base_binding = {
        "profile_id": BINDING_PROFILE,
        "args": {
            "artifact": "api@sha256:abc123",
            "strategy": "rolling",
        },
        "policy_context": {
            "policy_id": "deploy-policy",
            "policy_version": "3",
        },
        "target_state_digest": "sha256:" + ("1" * 64),
    }

    cases: dict[str, Any] = {
        "base": digests(base_action, base_binding),
    }

    mutations = {
        "mutate_caller": ("action", "caller_id", "agent:other"),
        "mutate_tool": ("action", "tool_id", "tool:restart"),
        "mutate_scope": (
            "action",
            "resource_scope",
            "tenant:acme/environment:staging",
        ),
        "mutate_args": ("binding", "args.strategy", "blue_green"),
        "mutate_policy": ("binding", "policy_context.policy_version", "4"),
        "mutate_target_state": (
            "binding",
            "target_state_digest",
            "sha256:" + ("2" * 64),
        ),
    }

    for case_id, (target, path, value) in mutations.items():
        action = copy.deepcopy(base_action)
        binding = copy.deepcopy(base_binding)
        obj = action if target == "action" else binding

        cursor: dict[str, Any] = obj
        parts = path.split(".")
        for part in parts[:-1]:
            cursor = cursor[part]
        cursor[parts[-1]] = value

        cases[case_id] = digests(action, binding)

    output = {
        "generator_version": "0.1",
        "canonicalization": "RFC8785-JCS",
        "hash_algorithm": "sha256",
        "action_identity_profile": ACTION_PROFILE,
        "binding_profile": BINDING_PROFILE,
        "framework_local_ids_in_preimage": False,
        "cases": cases,
    }

    print(json.dumps(output, ensure_ascii=False, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
