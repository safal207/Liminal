#!/usr/bin/env python3
"""Verify the checked-in three-record trustworthy-transition fixture."""

from __future__ import annotations

import argparse
import copy
import json
from pathlib import Path
from typing import Any

from generate_trustworthy_transition_vector import (
    build_fixture,
    canonical_json,
    make_authorization,
    make_integrity,
    make_observation,
    sha256_uri_text,
)


class FixtureVerificationError(ValueError):
    """Raised when the fixture violates the published profile."""


def load_fixture(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise FixtureVerificationError(f"cannot read {path}: {error}") from error
    except json.JSONDecodeError as error:
        raise FixtureVerificationError(
            f"invalid JSON in {path}: {error.msg}"
        ) from error

    if not isinstance(value, dict):
        raise FixtureVerificationError("fixture root must be an object")
    return value


def semantic_view(generated: dict[str, Any]) -> dict[str, Any]:
    """Remove independently derived evidence from generator output."""
    view = copy.deepcopy(generated)
    view.pop("base_records", None)
    for case in view.get("cases", []):
        case.pop("derived", None)
    return view


def verify_wrapper(wrapper: dict[str, Any], *, label: str) -> None:
    if set(wrapper) != {"record", "canonical_bytes_utf8", "record_ref"}:
        raise FixtureVerificationError(
            f"{label} must contain exactly record, canonical_bytes_utf8, record_ref"
        )

    record = wrapper["record"]
    if not isinstance(record, dict):
        raise FixtureVerificationError(f"{label}.record must be an object")

    canonical = canonical_json(record)
    if wrapper["canonical_bytes_utf8"] != canonical:
        raise FixtureVerificationError(f"{label} canonical bytes mismatch")

    if wrapper["record_ref"] != sha256_uri_text(canonical):
        raise FixtureVerificationError(f"{label} record reference mismatch")


def verify_generated_evidence(generated: dict[str, Any]) -> None:
    base_records = generated.get("base_records")
    if not isinstance(base_records, dict):
        raise FixtureVerificationError("generator omitted base_records")

    base_authorization = base_records.get("authorization_record")
    base_observation = base_records.get("observation_record")
    if not isinstance(base_authorization, dict) or not isinstance(
        base_observation, dict
    ):
        raise FixtureVerificationError(
            "generator must emit base authorization and observation records"
        )

    verify_wrapper(base_authorization, label="base authorization_record")
    verify_wrapper(base_observation, label="base observation_record")

    if (
        base_observation["record"]["authorization_ref"]
        != base_authorization["record_ref"]
    ):
        raise FixtureVerificationError(
            "base observation does not join to base authorization"
        )

    result_digest = sha256_uri_text(
        canonical_json(base_observation["record"]["result"])
    )
    if base_observation["record"]["result_digest"] != result_digest:
        raise FixtureVerificationError("base observation result digest mismatch")

    seen: set[str] = set()
    for case in generated.get("cases", []):
        case_id = case.get("case_id")
        if not isinstance(case_id, str) or not case_id:
            raise FixtureVerificationError("case_id must be non-empty")
        if case_id in seen:
            raise FixtureVerificationError(f"duplicate case_id: {case_id}")
        seen.add(case_id)

        derived = case.get("derived")
        expected = case.get("expected")
        response = case.get("response")
        if not all(
            isinstance(value, dict)
            for value in (derived, expected, response)
        ):
            raise FixtureVerificationError(
                f"{case_id} missing derived, expected, or response object"
            )

        authorization = make_authorization(case)
        observation = make_observation(case, authorization["record_ref"])
        observation_ref = observation["record_ref"] if observation else None
        integrity = make_integrity(
            case,
            authorization["record_ref"],
            observation_ref,
        )

        verify_wrapper(
            authorization,
            label=f"{case_id}.authorization_record",
        )
        if observation is not None:
            verify_wrapper(
                observation,
                label=f"{case_id}.observation_record",
            )
            if (
                observation["record"]["authorization_ref"]
                != authorization["record_ref"]
            ):
                raise FixtureVerificationError(
                    f"{case_id} observation authorization join mismatch"
                )
            if (
                observation["record"]["action_identity_digest"]
                != authorization["record"]["action_identity_digest"]
                or observation["record"]["binding_digest"]
                != authorization["record"]["binding_digest"]
            ):
                raise FixtureVerificationError(
                    f"{case_id} observation action/binding mismatch"
                )

        verify_wrapper(
            integrity,
            label=f"{case_id}.response_integrity_record",
        )
        if (
            integrity["record"]["authorization_ref"]
            != authorization["record_ref"]
        ):
            raise FixtureVerificationError(
                f"{case_id} integrity authorization join mismatch"
            )

        expected_derived = {
            "authorization_ref": authorization["record_ref"],
            "observation_ref": observation_ref,
            "response_integrity_ref": integrity["record_ref"],
            "response_digest": integrity["record"]["response_digest"],
            "claim_digests": [
                claim["claim_digest"]
                for claim in integrity["record"]["claims"]
            ],
            "claim_observation_refs": [
                claim["observation_refs"]
                for claim in integrity["record"]["claims"]
            ],
        }
        if derived != expected_derived:
            raise FixtureVerificationError(
                f"{case_id} independently derived evidence mismatch"
            )

        claims = integrity["record"]["claims"]
        for claim in claims:
            if claim["verdict"] == "SUPPORTED" and not claim["observation_refs"]:
                raise FixtureVerificationError(
                    f"{case_id}.{claim['claim_id']} supported without observation"
                )

        authority = expected.get("authority")
        if not isinstance(authority, dict):
            raise FixtureVerificationError(
                f"{case_id}.expected.authority must be an object"
            )

        if observation is None:
            expected_execution = "NOT_OBSERVED"
        else:
            status = observation["record"]["execution_status"]
            if status == "EXECUTED":
                expected_execution = "OBSERVED_EXECUTED"
            elif status in {"BLOCKED", "ERRORED", "REFUSED"}:
                expected_execution = "OBSERVED_BLOCKED"
            else:
                raise FixtureVerificationError(
                    f"{case_id} unknown execution status: {status}"
                )
        if expected.get("execution") != expected_execution:
            raise FixtureVerificationError(
                f"{case_id} execution expectation mismatch"
            )

        authorization_record = authorization["record"]
        if authorization_record["decision"] == "DENY":
            expected_authority = {
                "verdict": "DENIED",
                "current_state": "DENIED",
            }
        elif authorization_record["decision"] == "ALLOW":
            if observation is None:
                raise FixtureVerificationError(
                    f"{case_id} allowed case must carry an observation"
                )
            observed_at = observation["record"]["observed_at"]
            if not (
                authorization_record["issued_at"]
                <= observed_at
                <= authorization_record["expires_at"]
            ):
                expected_authority = {
                    "verdict": "INVALID",
                    "current_state": "INVALID",
                }
            else:
                current_state = (
                    "EXPIRED_AT_REPORT"
                    if integrity["record"]["evaluated_at"]
                    > authorization_record["expires_at"]
                    else "ACTIVE"
                )
                expected_authority = {
                    "verdict": "VALID_AT_EXECUTION",
                    "current_state": current_state,
                }
        else:
            raise FixtureVerificationError(
                f"{case_id} unknown authority decision"
            )
        if authority != expected_authority:
            raise FixtureVerificationError(
                f"{case_id} authority expectation mismatch"
            )

        if authority.get("verdict") == "DENIED":
            if expected.get("expected_additional_side_effects") != 0:
                raise FixtureVerificationError(
                    f"{case_id} denied authority must expect zero side effects"
                )
            if observation is not None:
                raise FixtureVerificationError(
                    f"{case_id} denied case must not carry an observation"
                )

        if (
            integrity["record"]["overall_verdict"]
            != expected.get("integrity")
        ):
            raise FixtureVerificationError(
                f"{case_id} integrity expectation mismatch"
            )


def verify_fixture(path: Path) -> None:
    actual = load_fixture(path)
    generated = build_fixture()

    if actual != semantic_view(generated):
        raise FixtureVerificationError(
            "checked-in semantic fixture differs from the independent generator"
        )

    verify_generated_evidence(generated)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "fixture",
        nargs="?",
        type=Path,
        default=Path(
            "fixtures/"
            "trustworthy-transition-three-record-conformance-v0.1.json"
        ),
    )
    args = parser.parse_args()

    verify_fixture(args.fixture)
    print(
        "OK: semantic fixture matches the independent generator; "
        "record digests and joins are valid"
    )


if __name__ == "__main__":
    main()
