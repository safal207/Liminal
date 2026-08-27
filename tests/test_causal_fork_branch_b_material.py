from __future__ import annotations

import hashlib
from pathlib import Path

from liminal.causal_fork_evidence import load_json, verify_ed25519_envelope


ROOT = Path(__file__).resolve().parents[1]
MATERIAL = ROOT / "trust/experimental/causal-fork-branch-b-v0.1"
SOURCE_SHA = "9ec014179132cb1bf5a6f21275583cd50425c96e"


def test_branch_b_signed_material_is_valid_and_source_bound() -> None:
    public_key = MATERIAL / "branch-b-public-key.pem"
    envelope_path = MATERIAL / "signed-branch-envelope.json"
    authority, claim = verify_ed25519_envelope(public_key, load_json(envelope_path))

    assert authority == claim["authority_id"]
    assert claim["schema"] == "liminal-causal-fork-branch-b-claim/v0.1"
    assert claim["provider_id"] == "offline-ed25519-fork-b"
    assert claim["evidence_identity"] == SOURCE_SHA
    assert claim["branch_observation"]["authority_id"] == authority
    assert claim["branch_observation"]["provider_id"] == claim["provider_id"]
    assert claim["reconciliation_vote"]["authority_id"] == authority
    assert claim["reconciliation_vote"]["provider_id"] == claim["provider_id"]
    assert claim["branch_semantic_state_sha256"] != claim[
        "sibling_branch_semantic_state_sha256"
    ]
    assert claim["reconciliation_target_semantic_state_sha256"] not in {
        claim["branch_semantic_state_sha256"],
        claim["sibling_branch_semantic_state_sha256"],
    }

    material = claim["source_material"]
    assert isinstance(material, list) and len(material) == 7
    for item in material:
        assert item["source_sha"] == SOURCE_SHA
        path = ROOT / item["path"]
        assert path.is_file()
        assert hashlib.sha256(path.read_bytes()).hexdigest() == item["sha256"]


def test_branch_b_private_key_is_not_committed() -> None:
    assert not (MATERIAL / "branch-b-private-key.pem").exists()
    assert not (MATERIAL / "claim.bin").exists()
    assert not (MATERIAL / "signature.bin").exists()
