#!/usr/bin/env python3
"""Produce an attested Root-A multi-epoch history evolution from the portable anchor."""

from __future__ import annotations

import argparse
import json
import shutil
from copy import deepcopy
from pathlib import Path
from typing import Any

from liminal.causal_state_evolution import (
    HistoricalTransitionObservation,
    validate_anchor_checkpoint,
    validate_anchor_witness,
)
from liminal.historical_trust_base_portability import (
    HistoricalTrustPath,
    TerminalTrustControls,
    trust_state_digest,
)
from liminal.recovery_trust_root_registry import (
    canonical_json_bytes,
    sha256_hex,
    validate_registry,
)

SCHEMA = "liminal-portable-causal-evolution-path-a-result/v0.1"
ROOT_A_PROVIDER = "github-oidc-root-a"
AUTHORITY_IDS = (
    "liminal.trusted-recovery.builder",
    "liminal.trusted-recovery.verifier",
)


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"object_required:{path}")
    return value


def digest(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def _extend_registry(
    registry: dict[str, Any],
    manifest: dict[str, Any],
    manifest_path: str,
) -> dict[str, Any]:
    generation = int(manifest["generation"])
    result = deepcopy(registry)
    result["active_generation"] = generation
    result["active_manifest_sha256"] = digest(manifest)
    result["history"] = [
        *registry["history"],
        {
            "generation": generation,
            "manifest_path": manifest_path,
            "manifest_sha256": digest(manifest),
        },
    ]
    return result


def _next_manifest(
    previous: dict[str, Any],
    *,
    generation: int,
    source_sha: str | None = None,
    builder_policy_path: str | None = None,
    builder_policy_sha256: str | None = None,
) -> dict[str, Any]:
    result = deepcopy(previous)
    result["generation"] = generation
    result["previous_manifest_sha256"] = digest(previous)
    if builder_policy_path is not None or builder_policy_sha256 is not None:
        if not builder_policy_path or not builder_policy_sha256 or not source_sha:
            raise ValueError("builder_policy_transition_incomplete")
        result["policy_material"]["builder_environment_policy"] = {
            "path": builder_policy_path,
            "sha256": builder_policy_sha256,
            "source_sha": source_sha,
        }
    return result


def _path(
    *,
    registry: dict[str, Any],
    manifests: dict[str, dict[str, Any]],
    controls: TerminalTrustControls,
    genesis_authority_id: str,
) -> HistoricalTrustPath:
    path = HistoricalTrustPath(
        verified=True,
        genesis_authority_id=genesis_authority_id,
        registry=registry,
        manifests=manifests,
        controls=controls,
    )
    if not validate_registry(registry, manifests):
        raise ValueError("root_a_evolution_registry_invalid")
    return path


def _transition(
    *,
    provider_id: str,
    genesis_authority_id: str,
    from_generation: int,
    to_generation: int,
    from_registry: dict[str, Any],
    to_registry: dict[str, Any],
    from_manifest: dict[str, Any],
    to_manifest: dict[str, Any],
    trust_domain: str,
    logical_transition_id: str,
    transition_contract_sha256: str,
    authorization_contract_sha256: str,
    from_semantic_state_sha256: str,
    to_semantic_state_sha256: str,
    workflow_sha: str,
) -> HistoricalTransitionObservation:
    provenance = {
        "schema": "liminal-history-bound-causal-transition-provenance/v0.1",
        "provider_id": provider_id,
        "logical_transition_id": logical_transition_id,
        "from_registry_sha256": digest(from_registry),
        "to_registry_sha256": digest(to_registry),
        "from_manifest_sha256": digest(from_manifest),
        "to_manifest_sha256": digest(to_manifest),
        "producer_workflow_sha": workflow_sha,
    }
    return HistoricalTransitionObservation(
        verified=True,
        provider_id=provider_id,
        genesis_authority_id=genesis_authority_id,
        from_history_generation=from_generation,
        to_history_generation=to_generation,
        from_registry_sha256=digest(from_registry),
        to_registry_sha256=digest(to_registry),
        from_manifest_sha256=digest(from_manifest),
        to_manifest_sha256=digest(to_manifest),
        transition_provenance_sha256=digest(provenance),
        trust_domain=trust_domain,
        logical_transition_id=logical_transition_id,
        transition_contract_sha256=transition_contract_sha256,
        authorization_contract_sha256=authorization_contract_sha256,
        from_semantic_state_sha256=from_semantic_state_sha256,
        to_semantic_state_sha256=to_semantic_state_sha256,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--downstream-proof-dir", required=True)
    parser.add_argument("--policy-step-1", required=True)
    parser.add_argument("--policy-step-2", required=True)
    parser.add_argument("--transition-contract", required=True)
    parser.add_argument("--transition-authorization-contract", required=True)
    parser.add_argument("--rotation-contract", required=True)
    parser.add_argument("--historical-authorization-contract", required=True)
    parser.add_argument("--workflow-sha", required=True)
    parser.add_argument("--output-dir", required=True)
    args = parser.parse_args()

    proof_dir = Path(args.downstream_proof_dir).resolve()
    output_dir = Path(args.output_dir).resolve()
    result = load_json(proof_dir / "downstream-causal-state-portability-result.json")
    if result.get("verified") is not True:
        raise ValueError("downstream_anchor_unverified")
    if result.get("reason") != "downstream_causal_state_portability_verified":
        raise ValueError("downstream_anchor_reason_invalid")
    if result.get("workflow_sha") != "65140882f172c53b6556ce9aa7a190f40bacc3bf":
        raise ValueError("downstream_anchor_workflow_invalid")
    checkpoint = result.get("checkpoint")
    witness = result.get("witness")
    if not isinstance(checkpoint, dict) or not isinstance(witness, dict):
        raise ValueError("downstream_anchor_objects_missing")
    if not validate_anchor_checkpoint(checkpoint) or not validate_anchor_witness(
        witness, checkpoint
    ):
        raise ValueError("downstream_anchor_invalid")

    root_a_dir = proof_dir / "materials/historical-proof/materials/root-a"
    gen0 = load_json(root_a_dir / "generation-0-manifest.json")
    gen1 = load_json(root_a_dir / "generation-1-manifest.json")
    reg1 = load_json(root_a_dir / "generation-1-registry.json")
    gen0_path = str(reg1["history"][0]["manifest_path"])
    gen1_path = str(reg1["history"][1]["manifest_path"])
    manifests: dict[str, dict[str, Any]] = {
        gen0_path: gen0,
        gen1_path: gen1,
    }
    if not validate_registry(reg1, manifests):
        raise ValueError("root_a_anchor_registry_invalid")

    primary = result.get("primary_provenance")
    if not isinstance(primary, dict):
        raise ValueError("root_a_anchor_provenance_missing")
    if primary.get("provider_id") != ROOT_A_PROVIDER:
        raise ValueError("root_a_provider_invalid")
    if primary.get("registry_sha256") != digest(reg1):
        raise ValueError("root_a_registry_digest_mismatch")
    if primary.get("manifest_sha256") != digest(gen1):
        raise ValueError("root_a_manifest_digest_mismatch")

    rotation_contract = load_json(Path(args.rotation_contract).resolve())
    historical_authorization = load_json(
        Path(args.historical_authorization_contract).resolve()
    )
    controls = TerminalTrustControls(
        trust_domain="liminal.trusted-recovery",
        authority_ids=AUTHORITY_IDS,
        threshold=2,
        rotation_contract_sha256=digest(rotation_contract),
        authorization_contract_sha256=digest(historical_authorization),
    )
    genesis_authority_id = str(primary["genesis_authority_id"])
    anchor_path = _path(
        registry=reg1,
        manifests=manifests,
        controls=controls,
        genesis_authority_id=genesis_authority_id,
    )
    anchor_semantic = trust_state_digest(anchor_path)
    if anchor_semantic != result.get("semantic_state_sha256"):
        raise ValueError("root_a_anchor_semantic_mismatch")

    step1_policy_path = Path(args.policy_step_1).resolve()
    step2_policy_path = Path(args.policy_step_2).resolve()
    step1_policy = load_json(step1_policy_path)
    step2_policy = load_json(step2_policy_path)
    transition_contract = load_json(Path(args.transition_contract).resolve())
    transition_authorization = load_json(
        Path(args.transition_authorization_contract).resolve()
    )
    transition_contract_sha256 = digest(transition_contract)
    transition_authorization_sha256 = digest(transition_authorization)

    # Path A intentionally inserts a semantic no-op generation before causal epoch 1.
    gen2 = _next_manifest(gen1, generation=2)
    gen2_path = "evolution/root-a/generation-2-manifest.json"
    manifests[gen2_path] = gen2
    reg2 = _extend_registry(reg1, gen2, gen2_path)
    no_op_path = _path(
        registry=reg2,
        manifests=manifests,
        controls=controls,
        genesis_authority_id=genesis_authority_id,
    )
    if trust_state_digest(no_op_path) != anchor_semantic:
        raise ValueError("root_a_noop_semantic_drift")

    gen3 = _next_manifest(
        gen2,
        generation=3,
        source_sha=args.workflow_sha,
        builder_policy_path="policies/portable-causal-evolution-state-step-1-v0.1.json",
        builder_policy_sha256=digest(step1_policy),
    )
    gen3_path = "evolution/root-a/generation-3-manifest.json"
    manifests[gen3_path] = gen3
    reg3 = _extend_registry(reg2, gen3, gen3_path)
    path3 = _path(
        registry=reg3,
        manifests=manifests,
        controls=controls,
        genesis_authority_id=genesis_authority_id,
    )
    semantic1 = trust_state_digest(path3)
    if semantic1 == anchor_semantic:
        raise ValueError("root_a_step1_semantic_not_advanced")

    gen4 = _next_manifest(
        gen3,
        generation=4,
        source_sha=args.workflow_sha,
        builder_policy_path="policies/portable-causal-evolution-state-step-2-v0.1.json",
        builder_policy_sha256=digest(step2_policy),
    )
    gen4_path = "evolution/root-a/generation-4-manifest.json"
    manifests[gen4_path] = gen4
    reg4 = _extend_registry(reg3, gen4, gen4_path)
    path4 = _path(
        registry=reg4,
        manifests=manifests,
        controls=controls,
        genesis_authority_id=genesis_authority_id,
    )
    semantic2 = trust_state_digest(path4)
    if semantic2 in {anchor_semantic, semantic1}:
        raise ValueError("root_a_step2_semantic_not_advanced")

    step1 = _transition(
        provider_id=ROOT_A_PROVIDER,
        genesis_authority_id=genesis_authority_id,
        from_generation=1,
        to_generation=3,
        from_registry=reg1,
        to_registry=reg3,
        from_manifest=gen1,
        to_manifest=gen3,
        trust_domain=controls.trust_domain,
        logical_transition_id="authorization-policy-step-1",
        transition_contract_sha256=transition_contract_sha256,
        authorization_contract_sha256=transition_authorization_sha256,
        from_semantic_state_sha256=anchor_semantic,
        to_semantic_state_sha256=semantic1,
        workflow_sha=args.workflow_sha,
    )
    step2 = _transition(
        provider_id=ROOT_A_PROVIDER,
        genesis_authority_id=genesis_authority_id,
        from_generation=3,
        to_generation=4,
        from_registry=reg3,
        to_registry=reg4,
        from_manifest=gen3,
        to_manifest=gen4,
        trust_domain=controls.trust_domain,
        logical_transition_id="authorization-policy-step-2",
        transition_contract_sha256=transition_contract_sha256,
        authorization_contract_sha256=transition_authorization_sha256,
        from_semantic_state_sha256=semantic1,
        to_semantic_state_sha256=semantic2,
        workflow_sha=args.workflow_sha,
    )
    transitions = [step1.__dict__, step2.__dict__]
    path_a_result = {
        "schema": SCHEMA,
        "verified": True,
        "reason": "root_a_causal_evolution_history_verified",
        "workflow_sha": args.workflow_sha,
        "provider_id": ROOT_A_PROVIDER,
        "genesis_authority_id": genesis_authority_id,
        "anchor_registry_sha256": digest(reg1),
        "anchor_manifest_sha256": digest(gen1),
        "anchor_semantic_state_sha256": anchor_semantic,
        "transition_sequence": transitions,
        "transition_sequence_sha256": digest(transitions),
        "final_registry_sha256": digest(reg4),
        "final_manifest_sha256": digest(gen4),
        "final_semantic_state_sha256": semantic2,
        "claim_boundary": {
            "registry_chain_validated": True,
            "semantic_noop_generation_present": True,
            "path_a_attestation_required": True,
        },
    }

    output_dir.mkdir(parents=True, exist_ok=True)
    materials_dir = output_dir / "materials"
    materials_dir.mkdir(parents=True, exist_ok=True)
    for generation, manifest in ((2, gen2), (3, gen3), (4, gen4)):
        (materials_dir / f"generation-{generation}-manifest.json").write_bytes(
            canonical_json_bytes(manifest)
        )
    for generation, registry in ((2, reg2), (3, reg3), (4, reg4)):
        (materials_dir / f"generation-{generation}-registry.json").write_bytes(
            canonical_json_bytes(registry)
        )
    shutil.copyfile(step1_policy_path, materials_dir / step1_policy_path.name)
    shutil.copyfile(step2_policy_path, materials_dir / step2_policy_path.name)
    (output_dir / "path-a-causal-evolution-result.json").write_bytes(
        canonical_json_bytes(path_a_result)
    )
    print(json.dumps(path_a_result, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
