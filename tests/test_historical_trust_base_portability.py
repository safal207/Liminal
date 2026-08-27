from __future__ import annotations

import copy
from dataclasses import replace

import pytest

from liminal.historical_trust_base_portability import (
    HistoricalTrustPath,
    TerminalTrustControls,
    compare_historical_trust_bases,
    trust_state_digest,
)
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex


def _manifest(
    generation: int,
    previous: str | None,
    seed: str,
    *,
    terminal: bool = False,
) -> dict[str, object]:
    builder_sha = "a" * 40 if terminal else seed * 40
    verifier_sha = "b" * 40 if terminal else "0" * 40
    return {
        "schema_version": "liminal.recovery-trust-root-manifest.v0.1",
        "generation": generation,
        "previous_manifest_sha256": previous,
        "repository": "safal207/Liminal",
        "roots": {
            "builder": {
                "workflow_path": ".github/workflows/trusted-recovery-proof-builder.yml",
                "workflow_sha": builder_sha,
                "git_blob_sha": "c" * 40,
            },
            "verifier": {
                "workflow_path": ".github/workflows/trusted-recovery-proof-verifier.yml",
                "workflow_sha": verifier_sha,
                "git_blob_sha": "d" * 40,
            },
        },
        "policy_material": {
            "builder_environment_policy": {
                "path": "policies/trusted-recovery-proof-builder-v0.3.json",
                "sha256": "1" * 64,
                "source_sha": seed * 40,
            },
            "verifier_dependency_lock": {
                "path": "requirements/trusted-attestation-verifier.lock",
                "sha256": "2" * 64,
                "source_sha": seed * 40,
            },
        },
        "authorization_scope": {
            "repository_id": "1005410203",
            "source_ref": "refs/heads/agent/recovery-routing-v0-1",
            "deployment_environment": "live-provider-trace",
            "runner_environment": "github-hosted",
        },
    }


def _path(seed: str, authority: str, **control_changes: object) -> HistoricalTrustPath:
    genesis = _manifest(0, None, seed)
    genesis_digest = sha256_hex(canonical_json_bytes(genesis))
    terminal = _manifest(1, genesis_digest, seed, terminal=True)
    terminal_digest = sha256_hex(canonical_json_bytes(terminal))
    genesis_path = f"{seed}/genesis.json"
    terminal_path = f"{seed}/terminal.json"
    registry = {
        "schema_version": "liminal.recovery-trust-root-registry.v0.1",
        "active_generation": 1,
        "active_manifest_sha256": terminal_digest,
        "history": [
            {
                "generation": 0,
                "manifest_path": genesis_path,
                "manifest_sha256": genesis_digest,
            },
            {
                "generation": 1,
                "manifest_path": terminal_path,
                "manifest_sha256": terminal_digest,
            },
        ],
    }
    controls = TerminalTrustControls(
        trust_domain="liminal.trusted-recovery",
        authority_ids=("builder", "verifier"),
        threshold=2,
        rotation_contract_sha256="3" * 64,
        authorization_contract_sha256="4" * 64,
    )
    controls = replace(controls, **control_changes)
    return HistoricalTrustPath(
        verified=True,
        genesis_authority_id=authority,
        registry=registry,
        manifests={genesis_path: genesis, terminal_path: terminal},
        controls=controls,
    )


def _replace_path(path: HistoricalTrustPath, **changes: object) -> HistoricalTrustPath:
    values = {
        "verified": path.verified,
        "genesis_authority_id": path.genesis_authority_id,
        "registry": path.registry,
        "manifests": path.manifests,
        "controls": path.controls,
    }
    values.update(changes)
    return HistoricalTrustPath(**values)


def test_two_distinct_histories_converge_on_one_terminal_trust_state() -> None:
    primary = _path("e", "github-oidc-root")
    secondary = _path("f", "offline-ed25519-root")

    decision = compare_historical_trust_bases(primary, secondary)

    assert decision.verified is True
    assert decision.receipt is not None
    assert decision.receipt["genesis_a_manifest_sha256"] != decision.receipt[
        "genesis_b_manifest_sha256"
    ]
    assert decision.receipt["history_a_tip_sha256"] != decision.receipt[
        "history_b_tip_sha256"
    ]
    assert trust_state_digest(primary) == trust_state_digest(secondary)


@pytest.mark.parametrize(
    "control_changes",
    [
        {"authority_ids": ("builder", "other")},
        {"threshold": 1},
        {"authorization_contract_sha256": "5" * 64},
        {"rotation_contract_sha256": "6" * 64},
    ],
)
def test_valid_histories_with_terminal_semantic_drift_fail_closed(
    control_changes: dict[str, object],
) -> None:
    primary = _path("e", "github-oidc-root")
    secondary = _path("f", "offline-ed25519-root", **control_changes)

    decision = compare_historical_trust_bases(primary, secondary)

    assert decision.verified is False
    assert decision.reason == "terminal_trust_state_mismatch"


def test_same_genesis_authority_is_not_independent() -> None:
    decision = compare_historical_trust_bases(_path("e", "same-root"), _path("f", "same-root"))
    assert decision.reason == "genesis_authority_not_independent"


def test_failed_upstream_signature_verification_fails_closed() -> None:
    primary = _path("e", "github-oidc-root")
    secondary = _replace_path(_path("f", "offline-ed25519-root"), verified=False)
    assert compare_historical_trust_bases(primary, secondary).reason == "historical_path_unverified"


def test_copied_genesis_bytes_fail_closed_even_with_rebuilt_valid_history() -> None:
    primary = _path("e", "github-oidc-root")
    secondary = _path("f", "offline-ed25519-root")
    secondary_manifests = copy.deepcopy(dict(secondary.manifests))
    secondary_registry = copy.deepcopy(dict(secondary.registry))

    primary_genesis_path = primary.registry["history"][0]["manifest_path"]
    secondary_genesis_path = secondary_registry["history"][0]["manifest_path"]
    secondary_terminal_path = secondary_registry["history"][1]["manifest_path"]
    copied_genesis = copy.deepcopy(primary.manifests[primary_genesis_path])
    copied_genesis_digest = sha256_hex(canonical_json_bytes(copied_genesis))
    secondary_manifests[secondary_genesis_path] = copied_genesis

    terminal = copy.deepcopy(secondary_manifests[secondary_terminal_path])
    terminal["previous_manifest_sha256"] = copied_genesis_digest
    terminal_digest = sha256_hex(canonical_json_bytes(terminal))
    secondary_manifests[secondary_terminal_path] = terminal
    secondary_registry["history"][0]["manifest_sha256"] = copied_genesis_digest
    secondary_registry["history"][1]["manifest_sha256"] = terminal_digest
    secondary_registry["active_manifest_sha256"] = terminal_digest

    rebuilt_secondary = _replace_path(
        secondary,
        registry=secondary_registry,
        manifests=secondary_manifests,
    )
    assert (
        compare_historical_trust_bases(primary, rebuilt_secondary).reason
        == "genesis_manifest_not_independent"
    )


def test_hidden_cross_root_reference_in_unused_evidence_fails_closed() -> None:
    primary = _path("e", "github-oidc-root")
    secondary = _path("f", "offline-ed25519-root")
    secondary_manifests = copy.deepcopy(dict(secondary.manifests))
    primary_genesis_digest = primary.registry["history"][0]["manifest_sha256"]
    terminal_path = secondary.registry["history"][1]["manifest_path"]
    foreign = copy.deepcopy(secondary_manifests[terminal_path])
    foreign["metadata"] = {"alias": {"ancestor": primary_genesis_digest}}
    secondary_manifests["f/unused-cross-root-evidence.json"] = foreign

    contaminated_secondary = _replace_path(secondary, manifests=secondary_manifests)
    assert (
        compare_historical_trust_bases(primary, contaminated_secondary).reason
        == "cross_root_dependency"
    )


def test_corrupt_history_fails_before_semantic_comparison() -> None:
    primary = _path("e", "github-oidc-root")
    secondary = _path("f", "offline-ed25519-root")
    corrupt_registry = copy.deepcopy(dict(secondary.registry))
    corrupt_registry["active_manifest_sha256"] = "9" * 64
    corrupt_secondary = _replace_path(secondary, registry=corrupt_registry)

    assert (
        compare_historical_trust_bases(primary, corrupt_secondary).reason
        == "historical_path_invalid"
    )
