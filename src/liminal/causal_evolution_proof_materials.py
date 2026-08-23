"""Deterministic reconstruction of causal-evolution proof material."""
from __future__ import annotations

from pathlib import Path
from typing import Any

from liminal.causal_evolution_evidence import (
    load_json,
    verify_evolution_envelope,
    verify_policy_source_material,
)
from liminal.causal_state_evolution import HistoricalTransitionObservation
from liminal.downstream_causal_state_portability import HistoricalStateObservation
from liminal.historical_trust_base_portability import HistoricalTrustPath, TerminalTrustControls, trust_state_digest
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex, validate_registry

ANCHOR_WORKFLOW_SHA = "65140882f172c53b6556ce9aa7a190f40bacc3bf"
BOOTSTRAP_SOURCE_SHA = "97b2c2f9b5b0e5ba250d97a8ceba070b07713792"
ROOT_A_PROVIDER = "github-oidc-root-a"
ROOT_B_PROVIDER = "offline-ed25519-root-b"
AUTHORITY_IDS = ("liminal.trusted-recovery.builder", "liminal.trusted-recovery.verifier")


def digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def registry_prefix(final_registry: dict[str, Any], generation: int) -> dict[str, Any]:
    history = final_registry.get("history")
    if not isinstance(history, list) or generation < 0 or generation >= len(history):
        raise ValueError("registry_prefix_generation_invalid")
    entry = history[generation]
    if not isinstance(entry, dict):
        raise ValueError("registry_prefix_entry_invalid")
    return {"schema_version": final_registry["schema_version"], "active_generation": generation,
            "active_manifest_sha256": entry["manifest_sha256"], "history": history[: generation + 1]}


def controls(rotation_contract: dict[str, Any], authorization_contract: dict[str, Any]) -> TerminalTrustControls:
    return TerminalTrustControls(
        trust_domain="liminal.trusted-recovery", authority_ids=AUTHORITY_IDS, threshold=2,
        rotation_contract_sha256=digest(rotation_contract), authorization_contract_sha256=digest(authorization_contract))


def semantic(registry: dict[str, Any], manifests: dict[str, dict[str, Any]], ctl: TerminalTrustControls,
             genesis_authority_id: str) -> str:
    if not validate_registry(registry, manifests):
        raise ValueError("evolution_registry_invalid")
    return trust_state_digest(HistoricalTrustPath(True, genesis_authority_id, registry, manifests, ctl))


def transition(value: object) -> HistoricalTransitionObservation:
    if not isinstance(value, dict):
        raise ValueError("transition_object_required")
    return HistoricalTransitionObservation(
        verified=value.get("verified") is True, provider_id=str(value.get("provider_id", "")),
        genesis_authority_id=str(value.get("genesis_authority_id", "")),
        from_history_generation=int(value.get("from_history_generation", -1)),
        to_history_generation=int(value.get("to_history_generation", -1)),
        from_registry_sha256=str(value.get("from_registry_sha256", "")),
        to_registry_sha256=str(value.get("to_registry_sha256", "")),
        from_manifest_sha256=str(value.get("from_manifest_sha256", "")),
        to_manifest_sha256=str(value.get("to_manifest_sha256", "")),
        transition_provenance_sha256=str(value.get("transition_provenance_sha256", "")),
        trust_domain=str(value.get("trust_domain", "")), logical_transition_id=str(value.get("logical_transition_id", "")),
        transition_contract_sha256=str(value.get("transition_contract_sha256", "")),
        authorization_contract_sha256=str(value.get("authorization_contract_sha256", "")),
        from_semantic_state_sha256=str(value.get("from_semantic_state_sha256", "")),
        to_semantic_state_sha256=str(value.get("to_semantic_state_sha256", "")))


def _provenance(provider: str, logical_id: str, fr: dict[str, Any], tr: dict[str, Any], fm: dict[str, Any], tm: dict[str, Any],
                *, signer: str | None = None) -> str:
    payload: dict[str, Any] = {
        "schema": "liminal-history-bound-causal-transition-provenance/v0.1", "provider_id": provider,
        "logical_transition_id": logical_id, "from_registry_sha256": digest(fr), "to_registry_sha256": digest(tr),
        "from_manifest_sha256": digest(fm), "to_manifest_sha256": digest(tm)}
    if provider == ROOT_A_PROVIDER:
        payload["producer_workflow_sha"] = BOOTSTRAP_SOURCE_SHA
    else:
        payload["evolution_signer_authority_id"] = signer
        payload["bootstrap_source_sha"] = BOOTSTRAP_SOURCE_SHA
    return digest(payload)


def _assert_transition(actual: HistoricalTransitionObservation, *, provider: str, genesis: str, fg: int, tg: int,
                       fr: dict[str, Any], tr: dict[str, Any], fm: dict[str, Any], tm: dict[str, Any], fs: str, ts: str,
                       logical_id: str, contract_sha: str, auth_sha: str, signer: str | None = None) -> None:
    expected = HistoricalTransitionObservation(
        True, provider, genesis, fg, tg, digest(fr), digest(tr), digest(fm), digest(tm),
        _provenance(provider, logical_id, fr, tr, fm, tm, signer=signer), "liminal.trusted-recovery",
        logical_id, contract_sha, auth_sha, fs, ts)
    if actual != expected:
        raise ValueError(f"transition_material_mismatch:{provider}:{logical_id}")


def verify_path_a(anchor_dir: Path, path_a_dir: Path, ctl: TerminalTrustControls, contract_sha: str, auth_sha: str
                  ) -> tuple[HistoricalStateObservation, tuple[HistoricalTransitionObservation, ...], tuple[str, str, str], dict[str, Any]]:
    historical = anchor_dir / "materials/historical-proof/materials/root-a"
    a0, a1 = load_json(historical / "generation-0-manifest.json"), load_json(historical / "generation-1-manifest.json")
    ar1 = load_json(historical / "generation-1-registry.json")
    result = load_json(path_a_dir / "path-a-causal-evolution-result.json")
    if result.get("verified") is not True or result.get("reason") != "root_a_causal_evolution_history_verified" or result.get("workflow_sha") != BOOTSTRAP_SOURCE_SHA:
        raise ValueError("path_a_result_invalid")
    a2, a3, a4 = (load_json(path_a_dir / f"materials/generation-{g}-manifest.json") for g in (2, 3, 4))
    ar2, ar3, ar4 = (load_json(path_a_dir / f"materials/generation-{g}-registry.json") for g in (2, 3, 4))
    manifests = {str(ar4["history"][i]["manifest_path"]): m for i, m in enumerate((a0, a1, a2, a3, a4))}
    genesis = str(result.get("genesis_authority_id", ""))
    s0, noop, s1, s2 = (semantic(r, manifests, ctl, genesis) for r in (ar1, ar2, ar3, ar4))
    if noop != s0 or len({s0, s1, s2}) != 3:
        raise ValueError("path_a_semantic_schedule_invalid")
    raw = result.get("transition_sequence")
    if not isinstance(raw, list) or len(raw) != 2 or digest(raw) != result.get("transition_sequence_sha256"):
        raise ValueError("path_a_transition_sequence_invalid")
    steps = tuple(transition(v) for v in raw)
    _assert_transition(steps[0], provider=ROOT_A_PROVIDER, genesis=genesis, fg=1, tg=3, fr=ar1, tr=ar3, fm=a1, tm=a3,
                       fs=s0, ts=s1, logical_id="authorization-policy-step-1", contract_sha=contract_sha, auth_sha=auth_sha)
    _assert_transition(steps[1], provider=ROOT_A_PROVIDER, genesis=genesis, fg=3, tg=4, fr=ar3, tr=ar4, fm=a3, tm=a4,
                       fs=s1, ts=s2, logical_id="authorization-policy-step-2", contract_sha=contract_sha, auth_sha=auth_sha)
    anchor = HistoricalStateObservation(True, ROOT_A_PROVIDER, genesis, 1, digest(ar1), digest(a1), s0, "liminal.trusted-recovery")
    return anchor, steps, (s0, s1, s2), result


def verify_path_b(anchor_dir: Path, path_b_dir: Path, ctl: TerminalTrustControls, contract_sha: str, auth_sha: str,
                  repository_root: Path) -> tuple[HistoricalStateObservation, tuple[HistoricalTransitionObservation, ...], tuple[str, str, str], str, dict[str, Any]]:
    historical = anchor_dir / "materials/historical-proof/materials/root-b"
    b0, b1 = load_json(historical / "generation-0-manifest.json"), load_json(historical / "generation-1-manifest.json")
    br1 = load_json(historical / "registry.json")
    b2, b3, b4, b5 = (load_json(path_b_dir / f"generation-{g}-manifest.json") for g in (2, 3, 4, 5))
    br5 = load_json(path_b_dir / "registry.json")
    br2, br3, br4 = (registry_prefix(br5, g) for g in (2, 3, 4))
    manifests = {str(br5["history"][i]["manifest_path"]): m for i, m in enumerate((b0, b1, b2, b3, b4, b5))}
    signer, claim = verify_evolution_envelope(path_b_dir / "evolution-public-key.pem", load_json(path_b_dir / "signed-evolution-envelope.json"))
    verify_policy_source_material(repository_root, claim)
    if claim.get("bootstrap_source_sha") != BOOTSTRAP_SOURCE_SHA:
        raise ValueError("path_b_bootstrap_source_mismatch")
    genesis = str(claim.get("history_genesis_authority_id", ""))
    s0, s1 = semantic(br1, manifests, ctl, genesis), semantic(br2, manifests, ctl, genesis)
    if semantic(br3, manifests, ctl, genesis) != s1 or semantic(br4, manifests, ctl, genesis) != s1:
        raise ValueError("path_b_noop_semantic_drift")
    s2 = semantic(br5, manifests, ctl, genesis)
    if claim.get("anchor_registry_sha256") != digest(br1) or claim.get("anchor_manifest_sha256") != digest(b1):
        raise ValueError("path_b_anchor_binding_mismatch")
    if claim.get("final_registry_sha256") != digest(br5) or claim.get("final_manifest_sha256") != digest(b5):
        raise ValueError("path_b_final_binding_mismatch")
    if claim.get("anchor_semantic_state_sha256") != s0 or claim.get("final_semantic_state_sha256") != s2:
        raise ValueError("path_b_semantic_binding_mismatch")
    if claim.get("transition_contract_sha256") != contract_sha or claim.get("transition_authorization_contract_sha256") != auth_sha:
        raise ValueError("path_b_contract_binding_mismatch")
    raw = claim.get("transition_sequence")
    if not isinstance(raw, list) or len(raw) != 2 or digest(raw) != claim.get("transition_sequence_sha256"):
        raise ValueError("path_b_transition_sequence_invalid")
    steps = tuple(transition(v) for v in raw)
    _assert_transition(steps[0], provider=ROOT_B_PROVIDER, genesis=genesis, fg=1, tg=2, fr=br1, tr=br2, fm=b1, tm=b2,
                       fs=s0, ts=s1, logical_id="authorization-policy-step-1", contract_sha=contract_sha, auth_sha=auth_sha, signer=signer)
    _assert_transition(steps[1], provider=ROOT_B_PROVIDER, genesis=genesis, fg=2, tg=5, fr=br2, tr=br5, fm=b2, tm=b5,
                       fs=s1, ts=s2, logical_id="authorization-policy-step-2", contract_sha=contract_sha, auth_sha=auth_sha, signer=signer)
    anchor = HistoricalStateObservation(True, ROOT_B_PROVIDER, genesis, 1, digest(br1), digest(b1), s0, "liminal.trusted-recovery")
    return anchor, steps, (s0, s1, s2), signer, claim
