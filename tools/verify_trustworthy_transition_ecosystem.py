#!/usr/bin/env python3
"""Verify the cross-repository trustworthy-transition compatibility pack."""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from pathlib import Path, PurePosixPath
from typing import Any


PROFILE = "org.liminal.trustworthy-transition.ecosystem-compatibility.v0.1"
CANONICAL_PROFILE = "org.liminal.trustworthy-transition.three-record.v0.1"
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
REPOSITORY_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")

AUTHORIZATION_ROLE = "AUTHORIZATION_PRODUCER"
OBSERVATION_ROLE = "OBSERVATION_BINDER"
INTEGRITY_ROLE = "CLAIM_VERIFIER"

CANONICAL_RECORDS = {
    "authorization_record",
    "observation_record",
    "response_integrity_record",
}
AUTHORITY_VALUES = {
    "VALID",
    "DENIED",
    "PENDING",
    "EXPIRED",
    "EXPIRED_AT_REPORT",
    "CONSUMED",
    "REVALIDATION_REQUIRED",
    "NOT_EVALUATED",
}
EXECUTION_VALUES = {
    "OBSERVED_EXECUTED",
    "OBSERVED_BLOCKED",
    "OBSERVED_ERRORED",
    "NOT_OBSERVED",
}
INTEGRITY_VALUES = {"VERIFIED", "FAILED", "PARTIAL", "NOT_EVALUATED"}
VERIFICATION_LEVELS = {
    "TEXT_HEURISTIC",
    "OBSERVATION_JOINED",
    "FULL_LIFECYCLE_JOINED",
}


class EcosystemVerificationError(ValueError):
    """Raised when the ecosystem compatibility pack is inconsistent."""


def load_json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise EcosystemVerificationError(f"cannot read {path}: {error}") from error
    except json.JSONDecodeError as error:
        raise EcosystemVerificationError(
            f"invalid JSON in {path}: {error.msg}"
        ) from error
    if not isinstance(value, dict):
        raise EcosystemVerificationError("fixture root must be an object")
    return value


def require_text(value: Any, *, label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise EcosystemVerificationError(f"{label} must be a non-empty string")
    return value.strip()


def require_string_list(value: Any, *, label: str) -> list[str]:
    if not isinstance(value, list) or any(
        not isinstance(item, str) or not item for item in value
    ):
        raise EcosystemVerificationError(f"{label} must be a string array")
    if len(value) != len(set(value)):
        raise EcosystemVerificationError(f"{label} must not contain duplicates")
    return value


def require_map(value: Any, *, label: str) -> dict[str, str]:
    if not isinstance(value, dict) or not value:
        raise EcosystemVerificationError(f"{label} must be a non-empty object")
    if any(
        not isinstance(key, str)
        or not key
        or not isinstance(mapped, str)
        or not mapped
        for key, mapped in value.items()
    ):
        raise EcosystemVerificationError(
            f"{label} must map non-empty strings to non-empty strings"
        )
    return value


def verify_artifact_path(value: Any, *, label: str) -> str:
    path = require_text(value, label=label)
    parsed = PurePosixPath(path)
    if parsed.is_absolute() or ".." in parsed.parts or "." in parsed.parts:
        raise EcosystemVerificationError(
            f"{label} must be a normalized relative repository path"
        )
    return path


def verify_implementation(
    value: Any,
    *,
    index: int,
) -> tuple[str, dict[str, Any]]:
    label = f"implementations[{index}]"
    if not isinstance(value, dict):
        raise EcosystemVerificationError(f"{label} must be an object")

    required = {
        "implementation_id",
        "repository",
        "pull_request",
        "head_sha",
        "artifact_path",
        "local_profile",
        "roles",
        "canonical_records",
    }
    optional = {
        "decision_map",
        "authority_state_map",
        "execution_status_map",
        "integrity_verdict_map",
        "verification_level_map",
    }
    unknown = set(value) - required - optional
    missing = required - set(value)
    if missing or unknown:
        raise EcosystemVerificationError(
            f"{label} keys invalid: missing={sorted(missing)}, "
            f"unknown={sorted(unknown)}"
        )

    implementation_id = require_text(
        value["implementation_id"], label=f"{label}.implementation_id"
    )
    repository = require_text(value["repository"], label=f"{label}.repository")
    if not REPOSITORY_RE.fullmatch(repository):
        raise EcosystemVerificationError(f"{label}.repository is invalid")
    pull_request = value["pull_request"]
    if not isinstance(pull_request, int) or isinstance(pull_request, bool) or pull_request <= 0:
        raise EcosystemVerificationError(
            f"{label}.pull_request must be a positive integer"
        )
    head_sha = require_text(value["head_sha"], label=f"{label}.head_sha")
    if not SHA_RE.fullmatch(head_sha):
        raise EcosystemVerificationError(
            f"{label}.head_sha must be a lowercase 40-character commit SHA"
        )
    verify_artifact_path(value["artifact_path"], label=f"{label}.artifact_path")
    require_text(value["local_profile"], label=f"{label}.local_profile")

    roles = set(require_string_list(value["roles"], label=f"{label}.roles"))
    records = set(
        require_string_list(
            value["canonical_records"],
            label=f"{label}.canonical_records",
        )
    )
    if not records <= CANONICAL_RECORDS:
        raise EcosystemVerificationError(
            f"{label} declares unknown canonical records: "
            f"{sorted(records - CANONICAL_RECORDS)}"
        )

    if AUTHORIZATION_ROLE in roles:
        decision_map = require_map(
            value.get("decision_map"), label=f"{label}.decision_map"
        )
        state_map = require_map(
            value.get("authority_state_map"),
            label=f"{label}.authority_state_map",
        )
        if not set(decision_map.values()) <= {"ALLOW", "DENY", "DEFER"}:
            raise EcosystemVerificationError(
                f"{label}.decision_map has non-canonical values"
            )
        if not set(state_map.values()) <= AUTHORITY_VALUES - {"NOT_EVALUATED"}:
            raise EcosystemVerificationError(
                f"{label}.authority_state_map has non-canonical values"
            )
        if "authorization_record" not in records:
            raise EcosystemVerificationError(
                f"{label} authority producer must export authorization_record"
            )

    if OBSERVATION_ROLE in roles:
        execution_map = require_map(
            value.get("execution_status_map"),
            label=f"{label}.execution_status_map",
        )
        if not set(execution_map.values()) <= EXECUTION_VALUES:
            raise EcosystemVerificationError(
                f"{label}.execution_status_map has non-canonical values"
            )
        if "observation_record" not in records:
            raise EcosystemVerificationError(
                f"{label} observation binder must export observation_record"
            )

    if INTEGRITY_ROLE in roles:
        verdict_map = require_map(
            value.get("integrity_verdict_map"),
            label=f"{label}.integrity_verdict_map",
        )
        level_map = require_map(
            value.get("verification_level_map"),
            label=f"{label}.verification_level_map",
        )
        if not set(verdict_map.values()) <= INTEGRITY_VALUES:
            raise EcosystemVerificationError(
                f"{label}.integrity_verdict_map has non-canonical values"
            )
        if set(level_map.values()) != VERIFICATION_LEVELS:
            raise EcosystemVerificationError(
                f"{label}.verification_level_map must expose all evidence levels"
            )
        if "response_integrity_record" not in records:
            raise EcosystemVerificationError(
                f"{label} claim verifier must export response_integrity_record"
            )

    normalized = dict(value)
    normalized["roles"] = roles
    normalized["canonical_records"] = records
    normalized["source_url"] = (
        f"https://github.com/{repository}/pull/{pull_request}"
    )
    return implementation_id, normalized


def derive_authority(
    authorization: dict[str, Any] | None,
    outcomes: dict[str, Any],
    *,
    case_id: str,
) -> str:
    if authorization is None:
        if outcomes.get("authorization_decision") is not None or outcomes.get(
            "authorization_state"
        ) is not None:
            raise EcosystemVerificationError(
                f"{case_id} has authority outcomes without an authority producer"
            )
        return "NOT_EVALUATED"

    decision = outcomes.get("authorization_decision")
    state = outcomes.get("authorization_state")
    if not isinstance(decision, str) or not isinstance(state, str):
        raise EcosystemVerificationError(
            f"{case_id} requires string authorization decision and state"
        )

    decision_map = authorization["decision_map"]
    state_map = authorization["authority_state_map"]
    if decision not in decision_map:
        raise EcosystemVerificationError(
            f"{case_id} unknown local authorization decision: {decision}"
        )
    if state not in state_map:
        raise EcosystemVerificationError(
            f"{case_id} unknown local authorization state: {state}"
        )

    normalized_state = state_map[state]
    if normalized_state != "VALID":
        return normalized_state

    normalized_decision = decision_map[decision]
    if normalized_decision == "ALLOW":
        return "VALID"
    if normalized_decision == "DENY":
        return "DENIED"
    return "PENDING"


def verify_case(
    value: Any,
    *,
    index: int,
    implementations: dict[str, dict[str, Any]],
) -> dict[str, Any]:
    label = f"cases[{index}]"
    if not isinstance(value, dict):
        raise EcosystemVerificationError(f"{label} must be an object")
    required = {
        "case_id",
        "participants",
        "local_outcomes",
        "required_record_roles",
        "expected",
    }
    if set(value) != required:
        raise EcosystemVerificationError(
            f"{label} must contain exactly {sorted(required)}"
        )

    case_id = require_text(value["case_id"], label=f"{label}.case_id")
    participants = value["participants"]
    outcomes = value["local_outcomes"]
    expected = value["expected"]
    if not all(isinstance(item, dict) for item in (participants, outcomes, expected)):
        raise EcosystemVerificationError(
            f"{case_id} participants, local_outcomes, and expected must be objects"
        )
    if set(participants) != {"authorization", "observation", "integrity"}:
        raise EcosystemVerificationError(f"{case_id} participant roles are invalid")

    def implementation_for(
        participant_role: str,
        required_role: str,
        *,
        nullable: bool = False,
    ) -> dict[str, Any] | None:
        implementation_id = participants[participant_role]
        if implementation_id is None and nullable:
            return None
        if not isinstance(implementation_id, str) or implementation_id not in implementations:
            raise EcosystemVerificationError(
                f"{case_id} references unknown {participant_role} implementation"
            )
        implementation = implementations[implementation_id]
        if required_role not in implementation["roles"]:
            raise EcosystemVerificationError(
                f"{case_id} {participant_role} implementation lacks {required_role}"
            )
        return implementation

    authorization = implementation_for(
        "authorization", AUTHORIZATION_ROLE, nullable=True
    )
    observation = implementation_for("observation", OBSERVATION_ROLE)
    integrity = implementation_for("integrity", INTEGRITY_ROLE)
    assert observation is not None and integrity is not None

    authority = derive_authority(
        authorization,
        outcomes,
        case_id=case_id,
    )

    execution_status = outcomes.get("execution_status")
    if not isinstance(execution_status, str):
        raise EcosystemVerificationError(
            f"{case_id}.execution_status must be a string"
        )
    execution_map = observation["execution_status_map"]
    if execution_status not in execution_map:
        raise EcosystemVerificationError(
            f"{case_id} unknown local execution status: {execution_status}"
        )
    execution = execution_map[execution_status]

    integrity_verdict = outcomes.get("integrity_verdict")
    if not isinstance(integrity_verdict, str):
        raise EcosystemVerificationError(
            f"{case_id}.integrity_verdict must be a string"
        )
    integrity_map = integrity["integrity_verdict_map"]
    if integrity_verdict not in integrity_map:
        raise EcosystemVerificationError(
            f"{case_id} unknown local integrity verdict: {integrity_verdict}"
        )
    response_integrity = integrity_map[integrity_verdict]

    level = outcomes.get("verification_level")
    if not isinstance(level, str) or level not in integrity["verification_level_map"]:
        raise EcosystemVerificationError(
            f"{case_id} unknown verification level: {level}"
        )
    normalized_level = integrity["verification_level_map"][level]
    if normalized_level == "FULL_LIFECYCLE_JOINED" and authorization is None:
        raise EcosystemVerificationError(
            f"{case_id} full lifecycle verification requires authorization"
        )
    if normalized_level in {"OBSERVATION_JOINED", "FULL_LIFECYCLE_JOINED"} and execution == "NOT_OBSERVED":
        if response_integrity == "VERIFIED":
            raise EcosystemVerificationError(
                f"{case_id} cannot verify observation-backed claims without observation"
            )

    record_roles = set(
        require_string_list(
            value["required_record_roles"],
            label=f"{case_id}.required_record_roles",
        )
    )
    if not record_roles <= CANONICAL_RECORDS:
        raise EcosystemVerificationError(
            f"{case_id} requires unknown canonical record roles"
        )
    if authorization is not None and "authorization_record" not in record_roles:
        raise EcosystemVerificationError(
            f"{case_id} omits required authorization_record"
        )
    if execution != "NOT_OBSERVED" and "observation_record" not in record_roles:
        raise EcosystemVerificationError(
            f"{case_id} omits required observation_record"
        )
    if "response_integrity_record" not in record_roles:
        raise EcosystemVerificationError(
            f"{case_id} omits required response_integrity_record"
        )

    expected_keys = {
        "authority",
        "execution",
        "response_integrity",
        "expected_additional_side_effects",
    }
    if set(expected) != expected_keys:
        raise EcosystemVerificationError(
            f"{case_id}.expected must contain exactly {sorted(expected_keys)}"
        )
    derived = {
        "authority": authority,
        "execution": execution,
        "response_integrity": response_integrity,
    }
    for key, actual in derived.items():
        if expected.get(key) != actual:
            raise EcosystemVerificationError(
                f"{case_id} {key} mismatch: expected {expected.get(key)}, "
                f"derived {actual}"
            )

    expected_side_effects = 1 if (
        authority == "VALID" and execution == "OBSERVED_EXECUTED"
    ) else 0
    side_effects = expected.get("expected_additional_side_effects")
    if side_effects != expected_side_effects:
        raise EcosystemVerificationError(
            f"{case_id} expected_additional_side_effects mismatch: "
            f"expected {side_effects}, derived {expected_side_effects}"
        )

    return {
        "case_id": case_id,
        "participants": dict(participants),
        "verification_level": normalized_level,
        "dimensions": derived,
        "required_record_roles": sorted(record_roles),
        "expected_additional_side_effects": expected_side_effects,
    }


def verify_join_contract(value: Any) -> dict[str, list[str]]:
    if not isinstance(value, dict):
        raise EcosystemVerificationError("join_contract must be an object")
    required = {
        "authorization_to_observation",
        "observation_to_integrity",
        "independent_dimensions",
        "claim_verdicts",
    }
    if set(value) != required:
        raise EcosystemVerificationError(
            f"join_contract must contain exactly {sorted(required)}"
        )
    normalized = {
        key: require_string_list(items, label=f"join_contract.{key}")
        for key, items in value.items()
    }
    auth_join = set(normalized["authorization_to_observation"])
    if not {
        "transition_id",
        "subject_id",
        "authorization_ref",
        "action_identity_digest",
        "binding_digest",
    } <= auth_join:
        raise EcosystemVerificationError(
            "authorization_to_observation omits required portable join keys"
        )
    integrity_join = set(normalized["observation_to_integrity"])
    if not {
        "transition_id",
        "subject_id",
        "authorization_ref",
        "observation_refs",
    } <= integrity_join:
        raise EcosystemVerificationError(
            "observation_to_integrity omits required portable join keys"
        )
    if normalized["independent_dimensions"] != [
        "authority",
        "execution",
        "response_integrity",
    ]:
        raise EcosystemVerificationError(
            "independent_dimensions order or values changed"
        )
    if set(normalized["claim_verdicts"]) != {
        "SUPPORTED",
        "CONTRADICTED",
        "UNVERIFIABLE",
        "OUT_OF_SCOPE",
    }:
        raise EcosystemVerificationError("claim verdict vocabulary mismatch")
    return normalized


def verify_fixture(path: Path) -> dict[str, Any]:
    fixture = load_json(path)
    required = {
        "schema_version",
        "profile",
        "canonical_profile",
        "implementations",
        "join_contract",
        "cases",
    }
    if set(fixture) != required:
        raise EcosystemVerificationError(
            f"fixture must contain exactly {sorted(required)}"
        )
    if fixture["schema_version"] != 1:
        raise EcosystemVerificationError("unsupported schema_version")
    if fixture["profile"] != PROFILE:
        raise EcosystemVerificationError("ecosystem profile mismatch")
    if fixture["canonical_profile"] != CANONICAL_PROFILE:
        raise EcosystemVerificationError("canonical profile mismatch")

    raw_implementations = fixture["implementations"]
    if not isinstance(raw_implementations, list) or not raw_implementations:
        raise EcosystemVerificationError("implementations must be a non-empty array")
    implementations: dict[str, dict[str, Any]] = {}
    for index, item in enumerate(raw_implementations):
        implementation_id, normalized = verify_implementation(item, index=index)
        if implementation_id in implementations:
            raise EcosystemVerificationError(
                f"duplicate implementation_id: {implementation_id}"
            )
        implementations[implementation_id] = normalized

    all_roles = set().union(
        *(implementation["roles"] for implementation in implementations.values())
    )
    for required_role in (AUTHORIZATION_ROLE, OBSERVATION_ROLE, INTEGRITY_ROLE):
        if required_role not in all_roles:
            raise EcosystemVerificationError(
                f"ecosystem pack has no implementation for {required_role}"
            )

    join_contract = verify_join_contract(fixture["join_contract"])

    raw_cases = fixture["cases"]
    if not isinstance(raw_cases, list) or not raw_cases:
        raise EcosystemVerificationError("cases must be a non-empty array")
    case_reports: list[dict[str, Any]] = []
    seen_case_ids: set[str] = set()
    for index, case in enumerate(raw_cases):
        report = verify_case(
            case,
            index=index,
            implementations=implementations,
        )
        case_id = report["case_id"]
        if case_id in seen_case_ids:
            raise EcosystemVerificationError(f"duplicate case_id: {case_id}")
        seen_case_ids.add(case_id)
        case_reports.append(report)

    dimensions = Counter(
        (
            report["dimensions"]["authority"],
            report["dimensions"]["execution"],
            report["dimensions"]["response_integrity"],
        )
        for report in case_reports
    )
    if len(dimensions) < 5:
        raise EcosystemVerificationError(
            "fixture does not exercise enough independent verdict combinations"
        )

    return {
        "schema_version": 1,
        "status": "VERIFIED",
        "profile": PROFILE,
        "canonical_profile": CANONICAL_PROFILE,
        "implementations": [
            {
                "implementation_id": implementation_id,
                "repository": implementation["repository"],
                "pull_request": implementation["pull_request"],
                "head_sha": implementation["head_sha"],
                "source_url": implementation["source_url"],
                "roles": sorted(implementation["roles"]),
                "canonical_records": sorted(
                    implementation["canonical_records"]
                ),
            }
            for implementation_id, implementation in sorted(
                implementations.items()
            )
        ],
        "join_contract": join_contract,
        "cases": case_reports,
        "summary": {
            "implementation_count": len(implementations),
            "case_count": len(case_reports),
            "independent_dimension_combinations": len(dimensions),
        },
        "claim_boundary": (
            "This offline pack verifies pinned implementation metadata, local-to-"
            "canonical vocabulary mappings, record-role coverage, and independent "
            "verdict derivation. It does not fetch remote code, attest current PR "
            "heads, or replace each repository's own tests and review gates."
        ),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "fixture",
        nargs="?",
        type=Path,
        default=Path(
            "fixtures/trustworthy-transition-ecosystem-compatibility-v0.1.json"
        ),
    )
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    try:
        receipt = verify_fixture(args.fixture)
        rendered = json.dumps(receipt, indent=2, sort_keys=True) + "\n"
        if args.output:
            args.output.parent.mkdir(parents=True, exist_ok=True)
            args.output.write_text(rendered, encoding="utf-8")
        else:
            print(rendered, end="")
        return 0
    except EcosystemVerificationError as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
