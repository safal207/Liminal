from pathlib import Path

from liminal.causal_evolution_evidence import load_json, verify_evolution_envelope
from liminal.causal_evolution_proof_materials import controls, digest, registry_prefix, semantic
from liminal.recovery_trust_root_registry import validate_registry


def test_signed_root_b_causal_evolution_material_is_self_consistent() -> None:
    repo = Path(__file__).resolve().parents[1]
    anchor_dir = repo / "trust/experimental/historical-root-b-v0.1"
    evolution_dir = repo / "trust/experimental/causal-evolution-root-b-v0.1"
    b0 = load_json(anchor_dir / "generation-0-manifest.json")
    b1 = load_json(anchor_dir / "generation-1-manifest.json")
    b2, b3, b4, b5 = (load_json(evolution_dir / f"generation-{g}-manifest.json") for g in (2, 3, 4, 5))
    registry = load_json(evolution_dir / "registry.json")
    manifests = {str(registry["history"][i]["manifest_path"]): m for i, m in enumerate((b0, b1, b2, b3, b4, b5))}
    assert validate_registry(registry, manifests)

    signer, claim = verify_evolution_envelope(
        evolution_dir / "evolution-public-key.pem",
        load_json(evolution_dir / "signed-evolution-envelope.json"),
    )
    assert signer == "ed25519-sha256:51c016fefc63fce955d954bcd2b30e08eb40effd18f36a9646bdb5baa0fabfd8"
    assert claim["bootstrap_source_sha"] == "97b2c2f9b5b0e5ba250d97a8ceba070b07713792"
    assert digest(load_json(repo / "policies/portable-causal-evolution-state-step-1-v0.1.json")) == claim["policy_step_1_sha256"]
    assert digest(load_json(repo / "policies/portable-causal-evolution-state-step-2-v0.1.json")) == claim["policy_step_2_sha256"]
    assert digest(load_json(repo / "policies/portable-causal-evolution-transition-contract-v0.1.json")) == claim["transition_contract_sha256"]
    assert digest(load_json(repo / "policies/portable-causal-evolution-transition-authorization-contract-v0.1.json")) == claim["transition_authorization_contract_sha256"]
    assert claim["final_registry_sha256"] == digest(registry)
    assert claim["final_manifest_sha256"] == digest(b5)

    ctl = controls(
        load_json(repo / "policies/portable-rotation-producer-contract-v0.1.json"),
        load_json(repo / "policies/portable-rotation-authorization-contract-v0.1.json"),
    )
    genesis = str(claim["history_genesis_authority_id"])
    s0 = semantic(registry_prefix(registry, 1), manifests, ctl, genesis)
    s1 = semantic(registry_prefix(registry, 2), manifests, ctl, genesis)
    assert semantic(registry_prefix(registry, 3), manifests, ctl, genesis) == s1
    assert semantic(registry_prefix(registry, 4), manifests, ctl, genesis) == s1
    s2 = semantic(registry, manifests, ctl, genesis)
    assert (s0, s1, s2) == (
        "ceca17a68e8f469fdfb847ca7a72b80b6214507910c4e99670ec0f33efa1ef91",
        "5e098592e9a7cc96b3dc85da43de271209154504f8d1fd043690094f646927f8",
        "bd7a9d1eb813f9a817857f175f69d9f551c07d65a43705d9a85096a6c93d08f5",
    )
