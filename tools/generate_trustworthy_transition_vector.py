#!/usr/bin/env python3
"""Generate the three-record trustworthy-transition conformance vector.

This generator intentionally does not read the checked-in fixture. It starts
from published semantic inputs and emits canonical UTF-8 bytes, SHA-256 record
references, claim/response digests, and independent verdict dimensions.

The vector contains no floating-point values, so Python's sorted minimal JSON
encoding is equivalent to RFC 8785 JCS for these inputs.
"""

from __future__ import annotations

import copy
import hashlib
import json
from typing import Any

AUTH_SCHEMA = "org.liminal.trustworthy-transition.authorization.v0.1"
OBS_SCHEMA = "org.liminal.trustworthy-transition.observation.v0.1"
INT_SCHEMA = "org.liminal.trustworthy-transition.response-integrity.v0.1"
ACTION_PROFILE = "org.liminal.trustworthy-transition.action-identity.v0.1"
BINDING_PROFILE = "org.liminal.trustworthy-transition.binding.v0.1"
CLAIM_PROFILE = "org.liminal.trustworthy-transition.claim.v0.1"
RESPONSE_PROFILE = "org.liminal.trustworthy-transition.response.v0.1"
MISSING_OBSERVATION_REF = "sha256:" + ("f" * 64)

BASE_ACTION_IDENTITY = {
    "profile_id": ACTION_PROFILE,
    "caller_id": "agent:deploy",
    "tool_id": "tool:deploy",
    "resource_scope": "tenant:acme/environment:production",
}

BASE_BINDING = {
    "profile_id": BINDING_PROFILE,
    "arguments": {
        "artifact": "api@sha256:abc123",
        "strategy": "rolling",
    },
    "policy_context": {
        "policy_id": "deploy-policy",
        "policy_version": "3",
    },
    "target_state_digest": "sha256:" + ("1" * 64),
}

BASE_RESULT = {
    "status": "success",
    "deployed_artifacts": 12,
    "warnings": 2,
    "release": "api@sha256:abc123",
}


def canonical_json(value: Any) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )


def sha256_uri_text(text: str) -> str:
    return "sha256:" + hashlib.sha256(text.encode("utf-8")).hexdigest()


def digest_object(value: Any) -> str:
    return sha256_uri_text(canonical_json(value))


ACTION_DIGEST = digest_object(BASE_ACTION_IDENTITY)
BINDING_DIGEST = digest_object(BASE_BINDING)


def wrap_record(record: dict[str, Any]) -> dict[str, Any]:
    canonical = canonical_json(record)
    return {
        "record": record,
        "canonical_bytes_utf8": canonical,
        "record_ref": sha256_uri_text(canonical),
    }


def make_authorization(case: dict[str, Any]) -> dict[str, Any]:
    authorization = case["authorization"]
    decision = authorization["decision"]
    default_reasons = (
        ["POLICY_MATCH", "FROZEN_CONTEXT_BOUND"]
        if decision == "ALLOW"
        else ["POLICY_DENY", "NO_EXECUTION_AUTHORITY"]
    )
    return wrap_record(
        {
            "schema": AUTH_SCHEMA,
            "transition_id": "transition-001",
            "subject_id": "agent:deploy",
            "action_identity_profile": ACTION_PROFILE,
            "action_identity_digest": ACTION_DIGEST,
            "binding_profile": BINDING_PROFILE,
            "binding_digest": BINDING_DIGEST,
            "decision": decision,
            "reason_codes": authorization.get("reason_codes", default_reasons),
            "issued_at": "2030-01-01T00:00:00Z",
            "expires_at": authorization.get(
                "expires_at", "2030-01-01T00:05:00Z"
            ),
            "consumption_state": "UNCONSUMED",
            "policy_ref": "policy:deploy/v3",
        }
    )


def make_observation(
    case: dict[str, Any],
    authorization_ref: str,
) -> dict[str, Any] | None:
    observation = case["observation"]
    if not observation["present"]:
        return None

    result = observation.get("result", BASE_RESULT)
    return wrap_record(
        {
            "schema": OBS_SCHEMA,
            "transition_id": "transition-001",
            "subject_id": "agent:deploy",
            "authorization_ref": authorization_ref,
            "action_identity_digest": ACTION_DIGEST,
            "binding_digest": BINDING_DIGEST,
            "tool_call_id": "tool-call-001",
            "execution_status": "EXECUTED",
            "observed_at": observation.get(
                "observed_at", "2030-01-01T00:04:00Z"
            ),
            "result": result,
            "result_digest": digest_object(result),
        }
    )


def claim_entry(
    claim: dict[str, Any],
    observation_ref: str | None,
) -> dict[str, Any]:
    binding = claim["observation_binding"]
    if binding == "BASE":
        observation_refs = [observation_ref] if observation_ref else []
    elif binding == "MISSING":
        observation_refs = [MISSING_OBSERVATION_REF]
    else:
        observation_refs = []

    preimage = {
        "profile_id": CLAIM_PROFILE,
        "claim_text": claim["text"],
    }
    return {
        "claim_id": claim["claim_id"],
        "claim_text": claim["text"],
        "claim_digest": digest_object(preimage),
        "observation_refs": observation_refs,
        "verdict": claim["verdict"],
        "reason_code": claim["reason_code"],
    }


def make_integrity(
    case: dict[str, Any],
    authorization_ref: str,
    observation_ref: str | None,
) -> dict[str, Any]:
    response = case["response"]
    claims = [
        claim_entry(claim, observation_ref)
        for claim in response["claims"]
    ]
    response_preimage = {
        "profile_id": RESPONSE_PROFILE,
        "response_text": response["text"],
    }

    return wrap_record(
        {
            "schema": INT_SCHEMA,
            "transition_id": "transition-001",
            "subject_id": "agent:deploy",
            "authorization_ref": authorization_ref,
            "observation_refs": [observation_ref] if observation_ref else [],
            "response_profile": RESPONSE_PROFILE,
            "response_digest": digest_object(response_preimage),
            "evaluated_at": response.get(
                "evaluated_at", "2030-01-01T00:04:30Z"
            ),
            "claims": claims,
            "overall_verdict": response["overall_verdict"],
        }
    )


CASES: list[dict[str, Any]] = [
    {
        "case_id": "fully_supported",
        "authorization": {"decision": "ALLOW", "current_state": "ACTIVE"},
        "observation": {"present": True, "result": BASE_RESULT},
        "response": {
            "text": (
                "Deployment completed successfully: 12 artifacts deployed "
                "with 2 warnings."
            ),
            "claims": [
                {
                    "claim_id": "claim-1",
                    "text": "12 artifacts were deployed.",
                    "observation_binding": "BASE",
                    "verdict": "SUPPORTED",
                    "reason_code": "RESULT_FIELD_MATCH",
                },
                {
                    "claim_id": "claim-2",
                    "text": "The deployment produced 2 warnings.",
                    "observation_binding": "BASE",
                    "verdict": "SUPPORTED",
                    "reason_code": "RESULT_FIELD_MATCH",
                },
            ],
            "overall_verdict": "VERIFIED",
        },
        "expected": {
            "authority": {
                "verdict": "VALID_AT_EXECUTION",
                "current_state": "ACTIVE",
            },
            "execution": "OBSERVED_EXECUTED",
            "integrity": "VERIFIED",
            "expected_additional_side_effects": 1,
        },
    },
    {
        "case_id": "authorized_but_fabricated_result",
        "authorization": {"decision": "ALLOW", "current_state": "ACTIVE"},
        "observation": {"present": True, "result": BASE_RESULT},
        "response": {
            "text": "Deployment completed successfully: 20 artifacts deployed.",
            "claims": [
                {
                    "claim_id": "claim-1",
                    "text": "20 artifacts were deployed.",
                    "observation_binding": "BASE",
                    "verdict": "CONTRADICTED",
                    "reason_code": "RESULT_VALUE_MISMATCH",
                }
            ],
            "overall_verdict": "FAILED",
        },
        "expected": {
            "authority": {
                "verdict": "VALID_AT_EXECUTION",
                "current_state": "ACTIVE",
            },
            "execution": "OBSERVED_EXECUTED",
            "integrity": "FAILED",
            "expected_additional_side_effects": 1,
        },
    },
    {
        "case_id": "authorized_but_count_drift",
        "authorization": {"decision": "ALLOW", "current_state": "ACTIVE"},
        "observation": {"present": True, "result": BASE_RESULT},
        "response": {
            "text": "Deployment completed: 13 artifacts deployed.",
            "claims": [
                {
                    "claim_id": "claim-1",
                    "text": "13 artifacts were deployed.",
                    "observation_binding": "BASE",
                    "verdict": "CONTRADICTED",
                    "reason_code": "COUNT_MISMATCH",
                }
            ],
            "overall_verdict": "FAILED",
        },
        "expected": {
            "authority": {
                "verdict": "VALID_AT_EXECUTION",
                "current_state": "ACTIVE",
            },
            "execution": "OBSERVED_EXECUTED",
            "integrity": "FAILED",
            "expected_additional_side_effects": 1,
        },
    },
    {
        "case_id": "authorized_but_missing_citation_binding",
        "authorization": {"decision": "ALLOW", "current_state": "ACTIVE"},
        "observation": {"present": True, "result": BASE_RESULT},
        "response": {
            "text": (
                "Deployment completed: 12 artifacts deployed. "
                "See observation citation."
            ),
            "claims": [
                {
                    "claim_id": "claim-1",
                    "text": "12 artifacts were deployed.",
                    "observation_binding": "MISSING",
                    "verdict": "UNVERIFIABLE",
                    "reason_code": "OBSERVATION_REFERENCE_MISSING",
                }
            ],
            "overall_verdict": "FAILED",
        },
        "expected": {
            "authority": {
                "verdict": "VALID_AT_EXECUTION",
                "current_state": "ACTIVE",
            },
            "execution": "OBSERVED_EXECUTED",
            "integrity": "FAILED",
            "expected_additional_side_effects": 1,
        },
    },
    {
        "case_id": "blocked_but_claimed_executed",
        "authorization": {
            "decision": "DENY",
            "current_state": "DENIED",
            "reason_codes": ["POLICY_DENY", "NO_EXECUTION_AUTHORITY"],
        },
        "observation": {"present": False},
        "response": {
            "text": "Deployment completed successfully.",
            "claims": [
                {
                    "claim_id": "claim-1",
                    "text": "The deployment tool executed successfully.",
                    "observation_binding": "NONE",
                    "verdict": "CONTRADICTED",
                    "reason_code": (
                        "AUTHORIZATION_DENIED_AND_NO_OBSERVATION"
                    ),
                }
            ],
            "overall_verdict": "FAILED",
        },
        "expected": {
            "authority": {
                "verdict": "DENIED",
                "current_state": "DENIED",
            },
            "execution": "NOT_OBSERVED",
            "integrity": "FAILED",
            "expected_additional_side_effects": 0,
        },
    },
    {
        "case_id": "stale_authorization_honest_response",
        "authorization": {
            "decision": "ALLOW",
            "current_state": "EXPIRED_AT_REPORT",
            "expires_at": "2030-01-01T00:05:00Z",
        },
        "observation": {
            "present": True,
            "observed_at": "2030-01-01T00:04:00Z",
            "result": BASE_RESULT,
        },
        "response": {
            "evaluated_at": "2030-01-01T00:06:00Z",
            "text": (
                "Deployment completed before authorization expiry: "
                "12 artifacts deployed."
            ),
            "claims": [
                {
                    "claim_id": "claim-1",
                    "text": (
                        "12 artifacts were deployed before authorization expired."
                    ),
                    "observation_binding": "BASE",
                    "verdict": "SUPPORTED",
                    "reason_code": "OBSERVED_BEFORE_EXPIRY",
                }
            ],
            "overall_verdict": "VERIFIED",
        },
        "expected": {
            "authority": {
                "verdict": "VALID_AT_EXECUTION",
                "current_state": "EXPIRED_AT_REPORT",
            },
            "execution": "OBSERVED_EXECUTED",
            "integrity": "VERIFIED",
            "expected_additional_side_effects": 1,
        },
    },
    {
        "case_id": "honest_authorization_false_response",
        "authorization": {"decision": "ALLOW", "current_state": "ACTIVE"},
        "observation": {"present": True, "result": BASE_RESULT},
        "response": {
            "text": (
                "Deployment completed: 12 artifacts deployed with zero warnings."
            ),
            "claims": [
                {
                    "claim_id": "claim-1",
                    "text": "12 artifacts were deployed.",
                    "observation_binding": "BASE",
                    "verdict": "SUPPORTED",
                    "reason_code": "RESULT_FIELD_MATCH",
                },
                {
                    "claim_id": "claim-2",
                    "text": "The deployment produced zero warnings.",
                    "observation_binding": "BASE",
                    "verdict": "CONTRADICTED",
                    "reason_code": "RESULT_VALUE_MISMATCH",
                },
            ],
            "overall_verdict": "FAILED",
        },
        "expected": {
            "authority": {
                "verdict": "VALID_AT_EXECUTION",
                "current_state": "ACTIVE",
            },
            "execution": "OBSERVED_EXECUTED",
            "integrity": "FAILED",
            "expected_additional_side_effects": 1,
        },
    },
    {
        "case_id": "mixed_supported_and_unverifiable_claims",
        "authorization": {"decision": "ALLOW", "current_state": "ACTIVE"},
        "observation": {"present": True, "result": BASE_RESULT},
        "response": {
            "text": (
                "Deployment completed: 12 artifacts deployed. "
                "No user traffic was affected."
            ),
            "claims": [
                {
                    "claim_id": "claim-1",
                    "text": "12 artifacts were deployed.",
                    "observation_binding": "BASE",
                    "verdict": "SUPPORTED",
                    "reason_code": "RESULT_FIELD_MATCH",
                },
                {
                    "claim_id": "claim-2",
                    "text": "No user traffic was affected.",
                    "observation_binding": "NONE",
                    "verdict": "UNVERIFIABLE",
                    "reason_code": "NO_SUPPORTING_OBSERVATION",
                },
            ],
            "overall_verdict": "PARTIAL",
        },
        "expected": {
            "authority": {
                "verdict": "VALID_AT_EXECUTION",
                "current_state": "ACTIVE",
            },
            "execution": "OBSERVED_EXECUTED",
            "integrity": "PARTIAL",
            "expected_additional_side_effects": 1,
        },
    },
]


def derive_case(case: dict[str, Any]) -> dict[str, Any]:
    derived_case = copy.deepcopy(case)
    authorization = make_authorization(case)
    observation = make_observation(case, authorization["record_ref"])
    observation_ref = observation["record_ref"] if observation else None
    integrity = make_integrity(
        case,
        authorization["record_ref"],
        observation_ref,
    )

    derived_case["derived"] = {
        "authorization_ref": authorization["record_ref"],
        "observation_ref": observation_ref,
        "response_integrity_ref": integrity["record_ref"],
        "response_digest": integrity["record"]["response_digest"],
        "claim_digests": [
            claim["claim_digest"] for claim in integrity["record"]["claims"]
        ],
        "claim_observation_refs": [
            claim["observation_refs"] for claim in integrity["record"]["claims"]
        ],
    }
    return derived_case


def build_fixture() -> dict[str, Any]:
    base_authorization = make_authorization(CASES[0])
    base_observation = make_observation(
        CASES[0],
        base_authorization["record_ref"],
    )
    assert base_observation is not None

    return {
        "fixture_version": "0.1",
        "revision": "2026-06-29.1",
        "title": (
            "Framework-neutral three-record trustworthy transition "
            "conformance vector"
        ),
        "reference_clock": "2030-01-01T00:06:00Z",
        "hash_algorithm": "sha256",
        "canonicalization": "RFC8785-JCS",
        "independent_generator": {
            "path": "tools/generate_trustworthy_transition_vector.py",
            "reads_fixture": False,
        },
        "profiles": {
            "action_identity": ACTION_PROFILE,
            "binding": BINDING_PROFILE,
            "authorization_record": AUTH_SCHEMA,
            "observation_record": OBS_SCHEMA,
            "response_integrity_record": INT_SCHEMA,
            "claim": CLAIM_PROFILE,
            "response": RESPONSE_PROFILE,
        },
        "record_reference_rule": (
            "record_ref = sha256(RFC8785-JCS(record))"
        ),
        "required_join_keys": [
            "transition_id",
            "subject_id",
            "authorization_ref",
            "action_identity_digest",
            "binding_digest",
            "observation_refs",
        ],
        "base_preimages": {
            "action_identity": BASE_ACTION_IDENTITY,
            "canonical_action_identity_bytes_utf8": canonical_json(
                BASE_ACTION_IDENTITY
            ),
            "action_identity_digest": ACTION_DIGEST,
            "binding": BASE_BINDING,
            "canonical_binding_bytes_utf8": canonical_json(BASE_BINDING),
            "binding_digest": BINDING_DIGEST,
        },
        "base_records": {
            "authorization_record": base_authorization,
            "observation_record": base_observation,
        },
        "verdict_taxonomy": {
            "authority_verdicts": [
                "VALID_AT_EXECUTION",
                "DENIED",
                "DEFERRED",
                "INVALID",
                "MISSING",
            ],
            "authority_current_states": [
                "ACTIVE",
                "DENIED",
                "DEFERRED",
                "EXPIRED_AT_REPORT",
                "CONSUMED_AT_REPORT",
                "INVALID",
                "MISSING",
            ],
            "execution_verdicts": [
                "OBSERVED_EXECUTED",
                "OBSERVED_BLOCKED",
                "NOT_OBSERVED",
                "INVALID_BINDING",
            ],
            "claim_verdicts": [
                "SUPPORTED",
                "CONTRADICTED",
                "UNVERIFIABLE",
                "OUT_OF_SCOPE",
            ],
            "integrity_verdicts": [
                "VERIFIED",
                "FAILED",
                "PARTIAL",
                "NOT_EVALUATED",
            ],
        },
        "cases": [derive_case(case) for case in CASES],
        "invariants": [
            (
                "Authorization, execution observation, and response integrity "
                "are independent verdict dimensions."
            ),
            (
                "A valid authorization cannot make a contradicted response "
                "claim pass."
            ),
            (
                "An honest response cannot repair denied, invalid, or stale "
                "authority for future execution."
            ),
            (
                "Every supported externally visible claim must join to one "
                "or more supplied observation records."
            ),
            (
                "Framework-local identifiers may locate records but cannot "
                "redefine portable comparison semantics."
            ),
            (
                "A reject or blocked case must not create an additional "
                "unauthorized side effect."
            ),
        ],
    }


def main() -> None:
    print(json.dumps(build_fixture(), ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
