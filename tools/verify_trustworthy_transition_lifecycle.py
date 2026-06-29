#!/usr/bin/env python3
"""Verify the stacked full-lifecycle trustworthy-transition compatibility pack."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from collections import Counter
from pathlib import Path, PurePosixPath
from typing import Any


PROFILE = "org.liminal.trustworthy-transition.full-lifecycle-compatibility.v0.2"
BASE_PROFILE = "org.liminal.trustworthy-transition.ecosystem-compatibility.v0.1"
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
REPOSITORY_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")

CAUSAL_ROLE = "CAUSAL_AUDITOR"
CONTINUITY_ROLE = "CONTINUITY_EVALUATOR"
DURABILITY_ROLE = "DURABLE_LEDGER"

CANONICAL_RECORDS = {
    "authorization_record",
    "observation_record",
    "response_integrity_record",
    "causal_audit_record",
    "continuity_snapshot_record",
    "durability_receipt",
}
DOWNSTREAM_RECORDS = {
    "causal_audit_record",
    "continuity_snapshot_record",
    "durability_receipt",
}
BASE_DIMENSIONS = {"authority", "execution", "response_integrity"}
DOWNSTREAM_DIMENSIONS = {
    "causal_validity",
    "continuity_posture",
    "durability",
}
CAUSAL_VALUES = {"VALID", "INVALID", "NOT_EVALUATED", "UNKNOWN"}
CONTINUITY_VALUES = {
    "CONTINUE_SIDE_EFFECT",
    "RETRY_SIDE_EFFECT",
    "REPORT_ONLY",
    "REMEDIATE_RESPONSE",
    "REVALIDATE",
    "BLOCKED",
    "ALREADY_CONSUMED",
    "NOT_EVALUATED",
}
DURABILITY_VALUES = {
    "LOCAL_REPLAY_VERIFIED",
    "LOCAL_REPLAY_FAILED",
    "NOT_EVALUATED",
}
EXECUTABLE_POSTURES = {"CONTINUE_SIDE_EFFECT", "RETRY_SIDE_EFFECT"}

EXPECTED_JOIN_CONTRACT = {
    "integrity_to_causal": [
        "transition_id",
        "subject_id",
        "authorization_ref",
        "observation_refs",
        "response_integrity_ref",
    ],
    "causal_to_continuity": [
        "transition_id",
        "subject_id",
        "action_identity_digest",
        "binding_digest",
        "authorization_ref",
        "observation_refs",
        "response_integrity_ref",
        "causal_audit_ref",
        "evidence_set_digest",
    ],
    "continuity_to_durability": [
        "transition_id",
        "subject_id",
        "authorization_ref",
        "observation_refs",
        "response_integrity_ref",
        "causal_audit_ref",
        "continuity_snapshot_ref",
    ],
    "canonical_records": [
        "authorization_record",
        "observation_record",
        "response_integrity_record",
        "causal_audit_record",
        "continuity_snapshot_record",
        "durability_receipt",
    ],
    "independent_dimensions": [
        "authority",
        "execution",
        "response_integrity",
        "causal_validity",
        "continuity_posture",
        "durability",
    ],
}


class LifecycleVerificationError(ValueError):
    """Raised when the full-lifecycle compatibility pack is inconsistent."""


def load_json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise LifecycleVerificationError(f"cannot read {path}: {error}") from error
    except json.JSONDecodeError as error:
        raise LifecycleVerificationError(
            f"invalid JSON in {path}: {error.msg}"
        ) from error
    if not isinstance(value, dict):
        raise LifecycleVerificationError(f"{path} root must be an object")
    return value


def canonical_json(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def digest_json(value: Any) -> str:
    payload = canonical_json(value).encode("utf-8")
    return "sha256:" + hashlib.sha256(payload).hexdigest()


def require_text(value: Any, *, label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise LifecycleVerificationError(f"{label} must be a non-empty string")
    return value.strip()


def require_positive_int(value: Any, *, label: str) -> int:
    if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
        raise LifecycleVerificationError(f"{label} must be a positive integer")
    return value


def require_string_list(value: Any, *, label: str) -> list[str]:
    if not isinstance(value, list) or any(
        not isinstance(item, str) or not item for item in value
    ):
        raise LifecycleVerificationError(f"{label} must be a string array")
    if len(value) != len(set(value)):
        raise LifecycleVerificationError(f"{label} must not contain duplicates")
    return value


def require_map(value: Any, *, label: str) -> dict[str, str]:
    if not isinstance(value, dict) or not value:
        raise LifecycleVerificationError(f"{label} must be a non-empty object")
    for key, mapped in value.items():
        if not isinstance(key, str) or not key or not isinstance(mapped, str) or not mapped:
            raise LifecycleVerificationError(
                f"{label} must map non-empty strings to non-empty strings"
            )
    return value


def verify_artifact_path(value: Any, *, label: str) -> str:
    path = require_text(value, label=label)
    parsed = PurePosixPath(path)
    if parsed.is_absolute() or "." in parsed.parts or ".." in parsed.parts:
        raise LifecycleVerificationError(
            f"{label} must be a normalized relative repository path"
        )
    return path


def verify_implementation(value: Any, *, index: int) -> tuple[str, dict[str, Any]]:
    label = f"implementations[{index}]"
    if not isinstance(value, dict):
        raise LifecycleVerificationError(f"{label} must be an object")

    required = {
        "implementation_id",
        "repository",
        "pull_request",
        "head_sha",
        "artifact_path",
        "local_profile",
        "roles",
        "canonical_records",
        "consumes_records",
    }
    optional = {
        "causal_validity_map",
        "continuity_posture_map",
        "durability_status_map",
    }
    missing = required - set(value)
    unknown = set(value) - required - optional
    if missing or unknown:
        raise LifecycleVerificationError(
            f"{label} keys invalid: missing={sorted(missing)}, unknown={sorted(unknown)}"
        )

    implementation_id = require_text(
        value["implementation_id"], label=f"{label}.implementation_id"
    )
    repository = require_text(value["repository"], label=f"{label}.repository")
    if not REPOSITORY_RE.fullmatch(repository):
        raise LifecycleVerificationError(f"{label}.repository is invalid")
    require_positive_int(value["pull_request"], label=f"{label}.pull_request")
    head_sha = require_text(value["head_sha"], label=f"{label}.head_sha")
    if not SHA_RE.fullmatch(head_sha):
        raise LifecycleVerificationError(
            f"{label}.head_sha must be a lowercase 40-character commit SHA"
        )
    verify_artifact_path(value["artifact_path"], label=f"{label}.artifact_path")
    require_text(value["local_profile"], label=f"{label}.local_profile")

    roles = set(require_string_list(value["roles"], label=f"{label}.roles"))
    if len(roles) != 1 or not roles <= {CAUSAL_ROLE, CONTINUITY_ROLE, DURABILITY_ROLE}:
        raise LifecycleVerificationError(
            f"{label}.roles must contain exactly one downstream lifecycle role"
        )
    records = set(
        require_string_list(value["canonical_records"], label=f"{label}.canonical_records")
    )
    consumes = set(
        require_string_list(value["consumes_records"], label=f"{label}.consumes_records")
    )
    if not records <= CANONICAL_RECORDS or not consumes <= CANONICAL_RECORDS:
        raise LifecycleVerificationError(f"{label} declares unknown canonical records")

    if CAUSAL_ROLE in roles:
        expected_records = {"causal_audit_record"}
        expected_consumes = {
            "authorization_record",
            "observation_record",
            "response_integrity_record",
        }
        mapping = require_map(
            value.get("causal_validity_map"), label=f"{label}.causal_validity_map"
        )
        if not set(mapping.values()) <= CAUSAL_VALUES:
            raise LifecycleVerificationError(f"{label} has unknown causal values")
    elif CONTINUITY_ROLE in roles:
        expected_records = {"continuity_snapshot_record"}
        expected_consumes = {
            "authorization_record",
            "observation_record",
            "response_integrity_record",
            "causal_audit_record",
        }
        mapping = require_map(
            value.get("continuity_posture_map"),
            label=f"{label}.continuity_posture_map",
        )
        if set(mapping.values()) != CONTINUITY_VALUES:
            raise LifecycleVerificationError(
                f"{label} must expose all canonical continuity postures"
            )
    else:
        expected_records = {"durability_receipt"}
        expected_consumes = CANONICAL_RECORDS - {"durability_receipt"}
        mapping = require_map(
            value.get("durability_status_map"),
            label=f"{label}.durability_status_map",
        )
        if not set(mapping.values()) <= DURABILITY_VALUES:
            raise LifecycleVerificationError(f"{label} has unknown durability values")
        if set(mapping.values()) != DURABILITY_VALUES:
            raise LifecycleVerificationError(
                f"{label} must expose verified, failed, and not-evaluated durability"
            )

    if records != expected_records:
        raise LifecycleVerificationError(
            f"{label}.canonical_records must equal {sorted(expected_records)}"
        )
    if consumes != expected_consumes:
        raise LifecycleVerificationError(
            f"{label}.consumes_records must equal {sorted(expected_consumes)}"
        )

    normalized = dict(value)
    normalized["roles"] = roles
    normalized["canonical_records"] = records
    normalized["consumes_records"] = consumes
    normalized["source_url"] = (
        f"https://github.com/{repository}/pull/{value['pull_request']}"
    )
    return implementation_id, normalized


def verify_base_fixture(value: dict[str, Any]) -> dict[str, dict[str, Any]]:
    if value.get("profile") != BASE_PROFILE:
        raise LifecycleVerificationError("base fixture profile mismatch")
    cases = value.get("cases")
    if not isinstance(cases, list) or not cases:
        raise LifecycleVerificationError("base fixture must contain cases")

    indexed: dict[str, dict[str, Any]] = {}
    for index, case in enumerate(cases):
        label = f"base.cases[{index}]"
        if not isinstance(case, dict):
            raise LifecycleVerificationError(f"{label} must be an object")
        case_id = require_text(case.get("case_id"), label=f"{label}.case_id")
        if case_id in indexed:
            raise LifecycleVerificationError(f"duplicate base case_id: {case_id}")
        expected = case.get("expected")
        if not isinstance(expected, dict) or not BASE_DIMENSIONS <= set(expected):
            raise LifecycleVerificationError(
                f"{case_id} must expose all base independent dimensions"
            )
        required_roles = set(
            require_string_list(
                case.get("required_record_roles"),
                label=f"{case_id}.required_record_roles",
            )
        )
        if not required_roles <= CANONICAL_RECORDS - DOWNSTREAM_RECORDS:
            raise LifecycleVerificationError(
                f"{case_id} base case declares downstream records"
            )
        indexed[case_id] = case
    return indexed


def implementation_for(
    participants: dict[str, Any],
    participant_role: str,
    required_role: str,
    implementations: dict[str, dict[str, Any]],
    *,
    case_id: str,
) -> dict[str, Any]:
    implementation_id = participants.get(participant_role)
    if not isinstance(implementation_id, str) or implementation_id not in implementations:
        raise LifecycleVerificationError(
            f"{case_id} references unknown {participant_role} implementation"
        )
    implementation = implementations[implementation_id]
    if required_role not in implementation["roles"]:
        raise LifecycleVerificationError(
            f"{case_id} {participant_role} implementation lacks {required_role}"
        )
    return implementation


def derive_full_lifecycle_status(
    *, causal_validity: str, continuity_posture: str, durability: str
) -> str:
    if durability == "LOCAL_REPLAY_FAILED":
        return "DURABILITY_FAILED"
    if durability == "NOT_EVALUATED":
        return "DURABILITY_NOT_EVALUATED"
    if causal_validity == "INVALID":
        return "CAUSAL_INVALID"
    if causal_validity in {"NOT_EVALUATED", "UNKNOWN"}:
        return "CAUSAL_NOT_EVALUATED"
    return {
        "CONTINUE_SIDE_EFFECT": "EXECUTABLE_CONTINUATION",
        "RETRY_SIDE_EFFECT": "EXECUTABLE_CONTINUATION",
        "REPORT_ONLY": "HISTORICAL_REPORT_ONLY",
        "REMEDIATE_RESPONSE": "RESPONSE_REMEDIATION",
        "REVALIDATE": "REVALIDATION_REQUIRED",
        "BLOCKED": "BLOCKED",
        "ALREADY_CONSUMED": "ALREADY_CONSUMED",
        "NOT_EVALUATED": "CONTINUITY_NOT_EVALUATED",
    }[continuity_posture]


def expected_resume_side_effects(
    *,
    authority: str,
    execution: str,
    response_integrity: str,
    causal_validity: str,
    continuity_posture: str,
    durability: str,
) -> int:
    if durability != "LOCAL_REPLAY_VERIFIED" or causal_validity != "VALID":
        return 0
    if authority != "VALID":
        return 0
    if continuity_posture == "CONTINUE_SIDE_EFFECT":
        return int(
            execution == "NOT_OBSERVED"
            and response_integrity in {"VERIFIED", "NOT_EVALUATED"}
        )
    if continuity_posture == "RETRY_SIDE_EFFECT":
        return int(
            execution == "OBSERVED_ERRORED"
            and response_integrity in {"VERIFIED", "NOT_EVALUATED"}
        )
    return 0


def verify_case(
    value: Any,
    *,
    index: int,
    implementations: dict[str, dict[str, Any]],
    base_cases: dict[str, dict[str, Any]],
) -> dict[str, Any]:
    label = f"cases[{index}]"
    if not isinstance(value, dict):
        raise LifecycleVerificationError(f"{label} must be an object")
    required = {
        "case_id",
        "base_case_id",
        "participants",
        "local_outcomes",
        "required_record_roles",
        "expected",
    }
    if set(value) != required:
        raise LifecycleVerificationError(
            f"{label} must contain exactly {sorted(required)}"
        )

    case_id = require_text(value["case_id"], label=f"{label}.case_id")
    base_case_id = require_text(
        value["base_case_id"], label=f"{case_id}.base_case_id"
    )
    if base_case_id not in base_cases:
        raise LifecycleVerificationError(
            f"{case_id} references unknown base case {base_case_id}"
        )
    base_case = base_cases[base_case_id]
    base_expected = base_case["expected"]

    participants = value["participants"]
    outcomes = value["local_outcomes"]
    expected = value["expected"]
    if not all(isinstance(item, dict) for item in (participants, outcomes, expected)):
        raise LifecycleVerificationError(
            f"{case_id} participants, local_outcomes, and expected must be objects"
        )
    if set(participants) != {"causal", "continuity", "durability"}:
        raise LifecycleVerificationError(f"{case_id} participant roles are invalid")
    if set(outcomes) != {
        "causal_validity",
        "continuity_posture",
        "durability_status",
    }:
        raise LifecycleVerificationError(f"{case_id} local outcome keys are invalid")

    causal = implementation_for(
        participants,
        "causal",
        CAUSAL_ROLE,
        implementations,
        case_id=case_id,
    )
    continuity = implementation_for(
        participants,
        "continuity",
        CONTINUITY_ROLE,
        implementations,
        case_id=case_id,
    )
    durability_impl = implementation_for(
        participants,
        "durability",
        DURABILITY_ROLE,
        implementations,
        case_id=case_id,
    )

    local_causal = require_text(
        outcomes["causal_validity"], label=f"{case_id}.causal_validity"
    )
    local_continuity = require_text(
        outcomes["continuity_posture"], label=f"{case_id}.continuity_posture"
    )
    local_durability = require_text(
        outcomes["durability_status"], label=f"{case_id}.durability_status"
    )

    causal_map = causal["causal_validity_map"]
    continuity_map = continuity["continuity_posture_map"]
    durability_map = durability_impl["durability_status_map"]
    if local_causal not in causal_map:
        raise LifecycleVerificationError(f"{case_id} unknown causal outcome")
    if local_continuity not in continuity_map:
        raise LifecycleVerificationError(f"{case_id} unknown continuity outcome")
    if local_durability not in durability_map:
        raise LifecycleVerificationError(f"{case_id} unknown durability outcome")

    dimensions = {
        "authority": base_expected["authority"],
        "execution": base_expected["execution"],
        "response_integrity": base_expected["response_integrity"],
        "causal_validity": causal_map[local_causal],
        "continuity_posture": continuity_map[local_continuity],
        "durability": durability_map[local_durability],
    }

    expected_keys = (
        BASE_DIMENSIONS
        | DOWNSTREAM_DIMENSIONS
        | {"expected_resume_side_effects", "full_lifecycle_status"}
    )
    if set(expected) != expected_keys:
        raise LifecycleVerificationError(
            f"{case_id}.expected keys must equal {sorted(expected_keys)}"
        )
    for dimension, actual in dimensions.items():
        if expected.get(dimension) != actual:
            raise LifecycleVerificationError(
                f"{case_id} expected {dimension} does not preserve mapped value"
            )

    base_roles = set(base_case["required_record_roles"])
    required_roles = set(
        require_string_list(
            value["required_record_roles"],
            label=f"{case_id}.required_record_roles",
        )
    )
    expected_roles = base_roles | DOWNSTREAM_RECORDS
    if required_roles != expected_roles:
        raise LifecycleVerificationError(
            f"{case_id} required records mismatch: expected {sorted(expected_roles)}"
        )

    if dimensions["causal_validity"] != "VALID" and dimensions[
        "continuity_posture"
    ] in EXECUTABLE_POSTURES:
        raise LifecycleVerificationError(
            f"{case_id} invalid/unknown causality cannot become executable continuity"
        )
    if dimensions["authority"] != "VALID" and dimensions[
        "continuity_posture"
    ] in EXECUTABLE_POSTURES:
        raise LifecycleVerificationError(
            f"{case_id} non-valid authority cannot become executable continuity"
        )
    if dimensions["execution"] == "OBSERVED_EXECUTED" and dimensions[
        "continuity_posture"
    ] in EXECUTABLE_POSTURES:
        raise LifecycleVerificationError(
            f"{case_id} executed work cannot be replayed as new continuation"
        )
    if (
        dimensions["authority"] == "VALID"
        and dimensions["response_integrity"] == "FAILED"
        and dimensions["causal_validity"] == "VALID"
        and dimensions["continuity_posture"] != "REMEDIATE_RESPONSE"
    ):
        raise LifecycleVerificationError(
            f"{case_id} valid-authority false response must route to remediation"
        )
    if (
        dimensions["response_integrity"] == "PARTIAL"
        and dimensions["authority"] == "VALID"
        and dimensions["continuity_posture"] != "REVALIDATE"
    ):
        raise LifecycleVerificationError(
            f"{case_id} partial response must require revalidation"
        )

    resume_effects = expected_resume_side_effects(**dimensions)
    if expected["expected_resume_side_effects"] != resume_effects:
        raise LifecycleVerificationError(
            f"{case_id} expected_resume_side_effects mismatch"
        )
    lifecycle_status = derive_full_lifecycle_status(
        causal_validity=dimensions["causal_validity"],
        continuity_posture=dimensions["continuity_posture"],
        durability=dimensions["durability"],
    )
    if expected["full_lifecycle_status"] != lifecycle_status:
        raise LifecycleVerificationError(
            f"{case_id} full_lifecycle_status mismatch"
        )

    return {
        "case_id": case_id,
        "base_case_id": base_case_id,
        "participants": dict(participants),
        "dimensions": dimensions,
        "required_record_roles": sorted(required_roles),
        "expected_resume_side_effects": resume_effects,
        "full_lifecycle_status": lifecycle_status,
    }


def verify_pack(
    fixture_path: Path,
    base_fixture_path: Path,
) -> dict[str, Any]:
    fixture = load_json(fixture_path)
    base_fixture = load_json(base_fixture_path)

    if fixture.get("schema_version") != 2:
        raise LifecycleVerificationError("schema_version must be 2")
    if fixture.get("profile") != PROFILE:
        raise LifecycleVerificationError("full-lifecycle profile mismatch")
    if fixture.get("base_profile") != BASE_PROFILE:
        raise LifecycleVerificationError("base_profile mismatch")
    declared_base = fixture.get("base_fixture")
    if declared_base != base_fixture_path.as_posix():
        raise LifecycleVerificationError(
            "base_fixture path must match the verifier input path"
        )
    if fixture.get("join_contract") != EXPECTED_JOIN_CONTRACT:
        raise LifecycleVerificationError("join_contract differs from v0.2 contract")

    implementations_raw = fixture.get("implementations")
    if not isinstance(implementations_raw, list) or len(implementations_raw) != 3:
        raise LifecycleVerificationError(
            "full-lifecycle pack must pin exactly three downstream implementations"
        )
    implementations: dict[str, dict[str, Any]] = {}
    for index, implementation_raw in enumerate(implementations_raw):
        implementation_id, implementation = verify_implementation(
            implementation_raw, index=index
        )
        if implementation_id in implementations:
            raise LifecycleVerificationError(
                f"duplicate implementation_id: {implementation_id}"
            )
        implementations[implementation_id] = implementation

    role_counts = Counter(
        role
        for implementation in implementations.values()
        for role in implementation["roles"]
    )
    if role_counts != Counter(
        {CAUSAL_ROLE: 1, CONTINUITY_ROLE: 1, DURABILITY_ROLE: 1}
    ):
        raise LifecycleVerificationError(
            "pack must contain exactly one causal, continuity, and durability owner"
        )

    base_cases = verify_base_fixture(base_fixture)
    cases_raw = fixture.get("cases")
    if not isinstance(cases_raw, list) or not cases_raw:
        raise LifecycleVerificationError("full-lifecycle fixture must contain cases")
    case_receipts: list[dict[str, Any]] = []
    seen_case_ids: set[str] = set()
    for index, case_raw in enumerate(cases_raw):
        receipt = verify_case(
            case_raw,
            index=index,
            implementations=implementations,
            base_cases=base_cases,
        )
        case_id = receipt["case_id"]
        if case_id in seen_case_ids:
            raise LifecycleVerificationError(f"duplicate case_id: {case_id}")
        seen_case_ids.add(case_id)
        case_receipts.append(receipt)

    statuses = Counter(receipt["full_lifecycle_status"] for receipt in case_receipts)
    durability_states = Counter(
        receipt["dimensions"]["durability"] for receipt in case_receipts
    )
    return {
        "profile": PROFILE,
        "base_profile": BASE_PROFILE,
        "fixture_digest": digest_json(fixture),
        "base_fixture_digest": digest_json(base_fixture),
        "implementation_pins": [
            {
                "implementation_id": implementation_id,
                "repository": implementation["repository"],
                "pull_request": implementation["pull_request"],
                "head_sha": implementation["head_sha"],
                "artifact_path": implementation["artifact_path"],
                "source_url": implementation["source_url"],
            }
            for implementation_id, implementation in sorted(implementations.items())
        ],
        "case_count": len(case_receipts),
        "status_counts": dict(sorted(statuses.items())),
        "durability_counts": dict(sorted(durability_states.items())),
        "cases": case_receipts,
        "verdict": "VERIFIED",
        "claim_boundary": (
            "This receipt verifies checked-in pins, mappings, composition rules, "
            "and independent dimensions. It does not fetch mutable repositories, "
            "attest signer identity, or replace repository-local runtime validation."
        ),
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--fixture",
        type=Path,
        default=Path(
            "fixtures/trustworthy-transition-full-lifecycle-compatibility-v0.2.json"
        ),
    )
    parser.add_argument(
        "--base-fixture",
        type=Path,
        default=Path(
            "fixtures/trustworthy-transition-ecosystem-compatibility-v0.1.json"
        ),
    )
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    receipt = verify_pack(args.fixture, args.base_fixture)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(
            json.dumps(receipt, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
    print(
        "OK: full-lifecycle ecosystem compatibility verified; "
        f"{receipt['case_count']} cases preserve six independent dimensions"
    )


if __name__ == "__main__":
    main()
