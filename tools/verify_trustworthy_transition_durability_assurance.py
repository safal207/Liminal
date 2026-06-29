#!/usr/bin/env python3
"""Verify trustworthy-transition checkpoint and platform assurance v0.3."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from collections import Counter
from pathlib import Path, PurePosixPath
from typing import Any

PROFILE = "org.liminal.trustworthy-transition.durability-assurance-compatibility.v0.3"
BASE_PROFILE = "org.liminal.trustworthy-transition.full-lifecycle-compatibility.v0.2"
RECEIPT_SCHEMA = "org.liminal.trustworthy-transition.durability-assurance-receipt.v0.3"
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
DIGEST_RE = re.compile(r"^sha256:[0-9a-f]{64}$")

CHECKPOINT_VALUES = {
    "NOT_EVALUATED",
    "LOCAL_SIGNATURE_ONLY",
    "EXTERNAL_ANCHOR_VERIFIED",
    "INVALID",
}
PROCESS_VALUES = {"NOT_EVALUATED", "PARTIAL", "VERIFIED", "FAILED"}
METADATA_STATES = {"AVAILABLE", "UNAVAILABLE"}
REQUIRED_PLATFORMS = {"ubuntu", "macos", "windows"}
LIFECYCLE_KEYS = {
    "authority",
    "execution",
    "response_integrity",
    "causal_validity",
    "continuity_posture",
    "durability",
    "full_lifecycle_status",
}
REQUIRED_CASES = {
    "external_anchor_and_three_platform_evidence",
    "local_signature_is_not_external_anchor",
    "durable_replay_without_checkpoint_evaluation",
    "invalid_checkpoint_preserves_valid_lifecycle",
    "external_anchor_rollback_is_rejected",
    "partial_platform_evidence",
    "windows_evidence_failure_is_not_partial_success",
    "strong_storage_assurance_does_not_upgrade_denied_authority",
    "strong_storage_assurance_does_not_repair_false_response",
    "storage_assurance_unavailable_preserves_lifecycle",
}
ALLOWED_CHECKPOINT_ERRORS = {
    "SIGNATURE_VERIFICATION_FAILED",
    "EXTERNAL_ANCHOR_ROLLBACK",
    "EXTERNAL_ANCHOR_FORK",
    "EXTERNAL_ANCHOR_NOT_IN_CHAIN",
    "KEY_REVOKED",
    "KEY_EXPIRED",
    "CHECKPOINT_EXPIRED",
    "PREVIOUS_CHECKPOINT_MISMATCH",
    "NON_MONOTONIC_CHECKPOINT",
}
EXPECTED_IMPLEMENTATIONS = {
    "lifecycle_profile": {
        "repository": "safal207/Liminal",
        "pull_request": 113,
        "head_sha": "b50ef4d90a76fbd5b5d2e835f85e348081f4110f",
        "artifact_path": "tools/verify_trustworthy_transition_lifecycle.py",
        "role": "BASE_LIFECYCLE_PROFILE",
    },
    "checkpoint_verifier": {
        "repository": "safal207/LiminalDB",
        "pull_request": 92,
        "head_sha": "2cac499c3b73c19d566ac763bf32816f4a4bcadc",
        "artifact_path": "liminal-db/crates/liminal-store/src/checkpoint.rs",
        "role": "CHECKPOINT_ASSURANCE_VERIFIER",
        "local_profile": "org.liminaldb.signed-checkpoint.v0.1",
    },
    "platform_evidence": {
        "repository": "safal207/LiminalDB",
        "pull_request": 93,
        "head_sha": "0cd06391837588817ad5ac55d19f47794ba157e6",
        "artifact_path": "tools/run_transition_durability_matrix.py",
        "role": "PROCESS_CRASH_EVIDENCE_PRODUCER",
        "local_profile": "org.liminaldb.transition-durability-evidence.v0.1",
    },
}


class AssuranceVerificationError(ValueError):
    """Raised when the v0.3 assurance pack is inconsistent."""


def load_json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise AssuranceVerificationError(f"cannot read {path}: {error}") from error
    except json.JSONDecodeError as error:
        raise AssuranceVerificationError(f"invalid JSON in {path}: {error.msg}") from error
    if not isinstance(value, dict):
        raise AssuranceVerificationError(f"{path} root must be an object")
    return value


def canonical_json(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def digest_json(value: Any) -> str:
    return "sha256:" + hashlib.sha256(canonical_json(value).encode("utf-8")).hexdigest()


def require_text(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise AssuranceVerificationError(f"{label} must be a non-empty string")
    return value.strip()


def require_relative_path(value: Any, label: str) -> str:
    path = require_text(value, label)
    parsed = PurePosixPath(path)
    if parsed.is_absolute() or ".." in parsed.parts or parsed.as_posix() != path:
        raise AssuranceVerificationError(f"{label} must be a normalized relative path")
    return path


def require_digest(value: Any, label: str) -> str:
    digest = require_text(value, label)
    if not DIGEST_RE.fullmatch(digest):
        raise AssuranceVerificationError(f"{label} must be a sha256 reference")
    return digest


def index_base_cases(base: dict[str, Any]) -> dict[str, dict[str, Any]]:
    if base.get("profile") != BASE_PROFILE:
        raise AssuranceVerificationError("base fixture profile mismatch")
    cases = base.get("cases")
    if not isinstance(cases, list) or not cases:
        raise AssuranceVerificationError("base fixture must contain cases")
    indexed: dict[str, dict[str, Any]] = {}
    for index, case in enumerate(cases):
        if not isinstance(case, dict):
            raise AssuranceVerificationError(f"base.cases[{index}] must be an object")
        case_id = require_text(case.get("case_id"), f"base.cases[{index}].case_id")
        expected = case.get("expected")
        if not isinstance(expected, dict) or not LIFECYCLE_KEYS <= set(expected):
            raise AssuranceVerificationError(f"base case {case_id} lacks lifecycle dimensions")
        if case_id in indexed:
            raise AssuranceVerificationError(f"duplicate base case_id: {case_id}")
        indexed[case_id] = case
    return indexed


def verify_implementations(value: Any) -> dict[str, str]:
    if not isinstance(value, dict) or set(value) != set(EXPECTED_IMPLEMENTATIONS):
        raise AssuranceVerificationError("implementations must contain the three pinned roles")
    heads: dict[str, str] = {}
    for implementation_id, expected in EXPECTED_IMPLEMENTATIONS.items():
        actual = value.get(implementation_id)
        if not isinstance(actual, dict):
            raise AssuranceVerificationError(f"{implementation_id} must be an object")
        for key, expected_value in expected.items():
            if actual.get(key) != expected_value:
                raise AssuranceVerificationError(
                    f"{implementation_id}.{key} mismatch: {actual.get(key)!r} != {expected_value!r}"
                )
        if not SHA_RE.fullmatch(actual["head_sha"]):
            raise AssuranceVerificationError(f"{implementation_id}.head_sha is invalid")
        require_relative_path(actual["artifact_path"], f"{implementation_id}.artifact_path")
        heads[implementation_id] = actual["head_sha"]

    artifacts = value["platform_evidence"].get("artifacts")
    if not isinstance(artifacts, dict) or set(artifacts) != REQUIRED_PLATFORMS:
        raise AssuranceVerificationError("platform evidence must pin all required artifacts")
    for platform, digest in artifacts.items():
        require_digest(digest, f"platform_evidence.artifacts.{platform}")
    return heads


def verify_contract(value: Any) -> None:
    if not isinstance(value, dict):
        raise AssuranceVerificationError("assurance_contract must be an object")
    if set(value.get("checkpoint_values", [])) != CHECKPOINT_VALUES:
        raise AssuranceVerificationError("checkpoint assurance vocabulary mismatch")
    if set(value.get("process_crash_values", [])) != PROCESS_VALUES:
        raise AssuranceVerificationError("process-crash vocabulary mismatch")
    if set(value.get("metadata_states", [])) != METADATA_STATES:
        raise AssuranceVerificationError("assurance metadata vocabulary mismatch")
    if set(value.get("required_platforms", [])) != REQUIRED_PLATFORMS:
        raise AssuranceVerificationError("required platform set mismatch")
    if set(value.get("platform_receipt_statuses", [])) != {"PASSED", "FAILED"}:
        raise AssuranceVerificationError("platform receipt status vocabulary mismatch")
    expected_dimensions = LIFECYCLE_KEYS - {"full_lifecycle_status"}
    expected_dimensions |= {"checkpoint_assurance", "process_crash_evidence"}
    if set(value.get("independent_dimensions", [])) != expected_dimensions:
        raise AssuranceVerificationError("independent assurance dimensions mismatch")
    boundary = value.get("claim_boundary")
    required_boundary = {
        "process_crash_only": True,
        "power_loss_verified": False,
        "controller_cache_loss_verified": False,
        "network_filesystem_verified": False,
        "external_anchor_provider_trusted_by_profile": False,
    }
    if boundary != required_boundary:
        raise AssuranceVerificationError("claim boundary must remain explicit and conservative")


def derive_platform_status(
    receipts: Any,
    artifact_map: dict[str, str],
    case_id: str,
) -> tuple[str, list[str]]:
    if not isinstance(receipts, list):
        raise AssuranceVerificationError(f"{case_id}.platform_receipts must be an array")
    seen: set[str] = set()
    passed: list[str] = []
    failed = False
    for index, receipt in enumerate(receipts):
        label = f"{case_id}.platform_receipts[{index}]"
        if not isinstance(receipt, dict) or set(receipt) != {
            "platform",
            "status",
            "artifact_digest",
        }:
            raise AssuranceVerificationError(f"{label} keys are invalid")
        platform = require_text(receipt["platform"], f"{label}.platform")
        if platform not in REQUIRED_PLATFORMS or platform in seen:
            raise AssuranceVerificationError(f"{label}.platform is unknown or duplicated")
        seen.add(platform)
        status = require_text(receipt["status"], f"{label}.status")
        if status not in {"PASSED", "FAILED"}:
            raise AssuranceVerificationError(f"{label}.status is invalid")
        digest = require_digest(receipt["artifact_digest"], f"{label}.artifact_digest")
        if digest != artifact_map[platform]:
            raise AssuranceVerificationError(f"{label}.artifact_digest does not match pinned evidence")
        if status == "PASSED":
            passed.append(platform)
        else:
            failed = True

    passed.sort()
    if failed:
        return "FAILED", passed
    if set(passed) == REQUIRED_PLATFORMS:
        return "VERIFIED", passed
    if passed:
        return "PARTIAL", passed
    return "NOT_EVALUATED", passed


def verify_case(
    case: Any,
    index: int,
    base_cases: dict[str, dict[str, Any]],
    artifact_map: dict[str, str],
) -> tuple[str, str, str, str]:
    if not isinstance(case, dict):
        raise AssuranceVerificationError(f"cases[{index}] must be an object")
    if set(case) != {"case_id", "base_case_id", "local_outcomes", "expected"}:
        raise AssuranceVerificationError(f"cases[{index}] keys are invalid")
    case_id = require_text(case["case_id"], f"cases[{index}].case_id")
    base_case_id = require_text(case["base_case_id"], f"{case_id}.base_case_id")
    if base_case_id not in base_cases:
        raise AssuranceVerificationError(f"{case_id} references unknown base case {base_case_id}")

    local = case["local_outcomes"]
    expected = case["expected"]
    if not isinstance(local, dict) or set(local) != {
        "assurance_metadata_state",
        "checkpoint_status",
        "checkpoint_error_code",
        "platform_receipts",
    }:
        raise AssuranceVerificationError(f"{case_id}.local_outcomes keys are invalid")
    if not isinstance(expected, dict) or set(expected) != {
        "checkpoint_assurance",
        "process_crash_evidence",
        "verified_platforms",
        "external_anti_rollback",
        "power_loss_verified",
        "assurance_status",
        "lifecycle",
    }:
        raise AssuranceVerificationError(f"{case_id}.expected keys are invalid")

    metadata_state = require_text(
        local["assurance_metadata_state"], f"{case_id}.assurance_metadata_state"
    )
    if metadata_state not in METADATA_STATES:
        raise AssuranceVerificationError(f"{case_id} has invalid assurance metadata state")

    if metadata_state == "UNAVAILABLE":
        if local["checkpoint_status"] is not None:
            raise AssuranceVerificationError(
                f"{case_id} unavailable metadata must not synthesize checkpoint status"
            )
        if local["checkpoint_error_code"] is not None:
            raise AssuranceVerificationError(
                f"{case_id} unavailable metadata must not synthesize checkpoint errors"
            )
        if local["platform_receipts"] is not None:
            raise AssuranceVerificationError(
                f"{case_id} unavailable metadata must not synthesize platform receipts"
            )
        checkpoint = "NOT_EVALUATED"
        process_status = "NOT_EVALUATED"
        passed_platforms: list[str] = []
    else:
        checkpoint = require_text(
            local["checkpoint_status"], f"{case_id}.checkpoint_status"
        )
        if checkpoint not in CHECKPOINT_VALUES:
            raise AssuranceVerificationError(f"{case_id} has invalid checkpoint status")
        error_code = local["checkpoint_error_code"]
        if checkpoint == "INVALID":
            error_code = require_text(error_code, f"{case_id}.checkpoint_error_code")
            if error_code not in ALLOWED_CHECKPOINT_ERRORS:
                raise AssuranceVerificationError(
                    f"{case_id} has unsupported checkpoint error code"
                )
        elif error_code is not None:
            raise AssuranceVerificationError(
                f"{case_id} non-invalid checkpoint must not carry an error"
            )
        process_status, passed_platforms = derive_platform_status(
            local["platform_receipts"], artifact_map, case_id
        )

    if expected["checkpoint_assurance"] != checkpoint:
        raise AssuranceVerificationError(f"{case_id} checkpoint normalization mismatch")
    if expected["process_crash_evidence"] != process_status:
        raise AssuranceVerificationError(f"{case_id} process-crash normalization mismatch")
    if expected["verified_platforms"] != passed_platforms:
        raise AssuranceVerificationError(f"{case_id} verified platform list mismatch")

    external = checkpoint == "EXTERNAL_ANCHOR_VERIFIED"
    if expected["external_anti_rollback"] is not external:
        raise AssuranceVerificationError(f"{case_id} external anti-rollback claim mismatch")
    if expected["power_loss_verified"] is not False:
        raise AssuranceVerificationError(f"{case_id} must not claim power-loss verification")
    assurance_status = require_text(
        expected["assurance_status"], f"{case_id}.assurance_status"
    )
    if metadata_state == "UNAVAILABLE" and assurance_status != "ASSURANCE_UNAVAILABLE":
        raise AssuranceVerificationError(
            f"{case_id} unavailable metadata must remain explicit in the normalized result"
        )

    lifecycle = expected["lifecycle"]
    if not isinstance(lifecycle, dict) or set(lifecycle) != LIFECYCLE_KEYS:
        raise AssuranceVerificationError(f"{case_id}.lifecycle dimensions are incomplete")
    base_expected = base_cases[base_case_id]["expected"]
    preserved = {key: base_expected[key] for key in LIFECYCLE_KEYS}
    if lifecycle != preserved:
        raise AssuranceVerificationError(
            f"{case_id} storage assurance mutated an independent lifecycle verdict"
        )

    error_code = local["checkpoint_error_code"]
    if error_code == "EXTERNAL_ANCHOR_ROLLBACK" and assurance_status != "ROLLBACK_REJECTED":
        raise AssuranceVerificationError(f"{case_id} rollback must be explicitly rejected")

    return case_id, checkpoint, process_status, metadata_state


def verify_fixture(fixture: dict[str, Any], base: dict[str, Any]) -> dict[str, Any]:
    if fixture.get("schema_version") != 3 or fixture.get("profile") != PROFILE:
        raise AssuranceVerificationError("fixture schema/profile mismatch")
    if fixture.get("base_profile") != BASE_PROFILE:
        raise AssuranceVerificationError("fixture base_profile mismatch")
    if fixture.get("base_fixture") != "fixtures/trustworthy-transition-full-lifecycle-compatibility-v0.2.json":
        raise AssuranceVerificationError("fixture base_fixture mismatch")

    heads = verify_implementations(fixture.get("implementations"))
    verify_contract(fixture.get("assurance_contract"))
    base_cases = index_base_cases(base)
    artifact_map = fixture["implementations"]["platform_evidence"]["artifacts"]

    cases = fixture.get("cases")
    if not isinstance(cases, list) or not cases:
        raise AssuranceVerificationError("fixture must contain cases")
    seen: set[str] = set()
    checkpoint_counts: Counter[str] = Counter()
    process_counts: Counter[str] = Counter()
    metadata_counts: Counter[str] = Counter()
    for index, case in enumerate(cases):
        case_id, checkpoint, process, metadata_state = verify_case(
            case, index, base_cases, artifact_map
        )
        if case_id in seen:
            raise AssuranceVerificationError(f"duplicate case_id: {case_id}")
        seen.add(case_id)
        checkpoint_counts[checkpoint] += 1
        process_counts[process] += 1
        metadata_counts[metadata_state] += 1

    if seen != REQUIRED_CASES:
        raise AssuranceVerificationError(
            f"required case coverage mismatch: missing={sorted(REQUIRED_CASES - seen)}, "
            f"unexpected={sorted(seen - REQUIRED_CASES)}"
        )
    if set(checkpoint_counts) != CHECKPOINT_VALUES:
        raise AssuranceVerificationError("fixture must cover every checkpoint assurance value")
    if set(process_counts) != PROCESS_VALUES:
        raise AssuranceVerificationError("fixture must cover every process-crash evidence value")
    if set(metadata_counts) != METADATA_STATES:
        raise AssuranceVerificationError("fixture must cover available and unavailable metadata")

    return {
        "schema": RECEIPT_SCHEMA,
        "profile": PROFILE,
        "verdict": "PASS",
        "fixture_digest": digest_json(fixture),
        "base_fixture_digest": digest_json(base),
        "case_count": len(cases),
        "implementation_heads": heads,
        "checkpoint_distribution": dict(sorted(checkpoint_counts.items())),
        "process_crash_distribution": dict(sorted(process_counts.items())),
        "metadata_distribution": dict(sorted(metadata_counts.items())),
        "verified_platforms": sorted(REQUIRED_PLATFORMS),
        "independent_dimensions": fixture["assurance_contract"]["independent_dimensions"],
        "claim_boundary": fixture["assurance_contract"]["claim_boundary"],
    }


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--fixture",
        type=Path,
        default=root / "fixtures/trustworthy-transition-durability-assurance-v0.3.json",
    )
    parser.add_argument(
        "--base-fixture",
        type=Path,
        default=root / "fixtures/trustworthy-transition-full-lifecycle-compatibility-v0.2.json",
    )
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    fixture = load_json(args.fixture)
    base = load_json(args.base_fixture)
    receipt = verify_fixture(fixture, base)
    rendered = json.dumps(receipt, ensure_ascii=False, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered, end="")


if __name__ == "__main__":
    try:
        main()
    except AssuranceVerificationError as error:
        raise SystemExit(f"durability assurance verification failed: {error}") from error
