from __future__ import annotations

import copy
from pathlib import Path
from typing import Any

from liminal.historical_root_b_evidence import load_json, verify_ed25519_envelope
from liminal.historical_trust_base_portability import (
    HistoricalTrustPath,
    TerminalTrustControls,
    compare_historical_trust_bases,
)
from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    evaluate_registry_rotation,
    sha256_hex,
    validate_registry,
)

ROOT_B = Path("trust/experimental/historical-root-b-v0.1")
ROOT_A_AUTHORITY = (
    "github-oidc:safal207/Liminal:"
    "trusted-recovery-trust-root-rotation-drill@"
    "e2cb6a014236bc561d03c405f4986146026041fa"
)


def digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def test_signed_root_b_converges_without_shared_history() -> None:
    a0 = load_json(Path("policies/recovery-trust-root-manifest-v0.1.json"))
    ar0 = load_json(Path("policies/recovery-trust-root-registry-v0.1.json"))
    b0 = load_json(ROOT_B / "generation-0-manifest.json")
    b1 = load_json(ROOT_B / "generation-1-manifest.json")
    br1 = load_json(ROOT_B / "registry.json")
    envelope = load_json(ROOT_B / "signed-history-envelope.json")

    b_manifests: dict[str, Any] = {
        str(br1["history"][0]["manifest_path"]): b0,
        str(br1["history"][1]["manifest_path"]): b1,
    }
    assert validate_registry(br1, b_manifests)
    br0 = {
        "schema_version": br1["schema_version"],
        "active_generation": 0,
        "active_manifest_sha256": br1["history"][0]["manifest_sha256"],
        "history": br1["history"][:1],
    }
    rotation = evaluate_registry_rotation(br0, br1, b_manifests)
    assert rotation.authorized is True
    assert rotation.reason == "registry_rotation_authorized"

    a1 = copy.deepcopy(b1)
    a1["previous_manifest_sha256"] = ar0["active_manifest_sha256"]
    a1_sha = digest(a1)
    assert a1_sha == (
        "b9cb0b37da2d74ece6c1cf780b06b17fbbb96f02e073ac64fb26be49cae24277"
    )
    ar1 = {
        "schema_version": ar0["schema_version"],
        "active_generation": 1,
        "active_manifest_sha256": a1_sha,
        "history": [
            *ar0["history"],
            {
                "generation": 1,
                "manifest_path": "drill/generation-1-manifest.json",
                "manifest_sha256": a1_sha,
            },
        ],
    }
    assert digest(ar1) == (
        "5441072b0e550995a9ad0b27b4f3af7c7b5bf531f59e27c870ab1a8cf61789a1"
    )
    a_manifests = {
        str(ar0["history"][0]["manifest_path"]): a0,
        "drill/generation-1-manifest.json": a1,
    }
    assert validate_registry(ar1, a_manifests)

    rotation_contract = load_json(
        Path("policies/portable-rotation-producer-contract-v0.1.json")
    )
    authorization_contract = load_json(
        Path("policies/portable-rotation-authorization-contract-v0.1.json")
    )
    controls = TerminalTrustControls(
        trust_domain="liminal.trusted-recovery",
        authority_ids=(
            "liminal.trusted-recovery.builder",
            "liminal.trusted-recovery.verifier",
        ),
        threshold=2,
        rotation_contract_sha256=digest(rotation_contract),
        authorization_contract_sha256=digest(authorization_contract),
    )
    b_authority = verify_ed25519_envelope(
        ROOT_B / "root-b-public-key.pem",
        envelope,
    )
    decision = compare_historical_trust_bases(
        HistoricalTrustPath(
            True,
            ROOT_A_AUTHORITY,
            ar1,
            a_manifests,
            controls,
        ),
        HistoricalTrustPath(
            True,
            b_authority,
            br1,
            b_manifests,
            controls,
        ),
    )
    assert decision.verified is True
    assert decision.receipt is not None
    assert decision.receipt["genesis_a_manifest_sha256"] != (
        decision.receipt["genesis_b_manifest_sha256"]
    )
    assert decision.receipt["history_a_tip_sha256"] != (
        decision.receipt["history_b_tip_sha256"]
    )
    assert decision.receipt["trust_state_digest_a"] == (
        decision.receipt["trust_state_digest_b"]
    )
