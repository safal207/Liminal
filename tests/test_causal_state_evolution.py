from __future__ import annotations

from copy import deepcopy
from dataclasses import replace

import pytest

from liminal.causal_state_evolution import (
    HistoricalTransitionObservation,
    compare_multi_epoch_causal_evolution,
    validate_evolution_checkpoint_chain,
    validate_evolution_witness_chain,
)
from liminal.downstream_causal_state_portability import HistoricalStateObservation
from liminal.recovery_trust_root_registry import canonical_json_bytes, sha256_hex


def _sha(value: object) -> str:
    return sha256_hex(canonical_json_bytes(value))


def _anchor_observations() -> tuple[HistoricalStateObservation, HistoricalStateObservation]:
    semantic = _sha({"semantic": "anchor"})
    primary = HistoricalStateObservation(
        verified=True,
        provider_id="github-oidc-root-a",
        genesis_authority_id="root-a",
        history_generation=1,
        registry_sha256=_sha("a-registry-0"),
        manifest_sha256=_sha("a-manifest-0"),
        semantic_state_sha256=semantic,
        trust_domain="liminal.trusted-recovery",
    )
    secondary = HistoricalStateObservation(
        verified=True,
        provider_id="offline-ed25519-root-b",
        genesis_authority_id="root-b",
        history_generation=9,
        registry_sha256=_sha("b-registry-0"),
        manifest_sha256=_sha("b-manifest-0"),
        semantic_state_sha256=semantic,
        trust_domain="liminal.trusted-recovery",
    )
    return primary, secondary


def _anchor_objects(
    primary: HistoricalStateObservation,
) -> tuple[dict, dict]:
    checkpoint = {
        "schema_version": "liminal-causal-trust-checkpoint/v0.1",
        "state_ref": {
            "schema": "liminal-causal-trust-state-ref/v0.1",
            "trust_domain": primary.trust_domain,
            "logical_state_id": "liminal.trusted-recovery.authorization-state",
            "causal_epoch": 0,
            "semantic_state_sha256": primary.semantic_state_sha256,
        },
        "previous_checkpoint_sha256": None,
        "checkpoint_authority": {
            "schema": "liminal-causal-state-authority/v0.1",
            "role": "checkpoint-producer",
            "logical_authority_id": "liminal.trusted-recovery.causal-checkpoint",
            "producer_contract_sha256": _sha("checkpoint-producer"),
            "authorization_contract_sha256": _sha("checkpoint-authorization"),
        },
    }
    witness = {
        "schema_version": "liminal-causal-trust-witness/v0.1",
        "state_ref": checkpoint["state_ref"],
        "checkpoint_sha256": _sha(checkpoint),
        "previous_witness_sha256": None,
        "witness_authority": {
            "schema": "liminal-causal-state-authority/v0.1",
            "role": "witness-producer",
            "logical_authority_id": "liminal.trusted-recovery.causal-witness",
            "producer_contract_sha256": _sha("witness-producer"),
            "authorization_contract_sha256": _sha("witness-authorization"),
        },
    }
    return checkpoint, witness


def _path_transitions(
    anchor: HistoricalStateObservation,
    *,
    path: str,
    generations: tuple[int, int],
) -> tuple[HistoricalTransitionObservation, HistoricalTransitionObservation]:
    semantic_1 = _sha(
        {
            "schema": "test-semantic-state/v0.1",
            "previous": anchor.semantic_state_sha256,
            "logical_transition_id": "authorization-policy-step-1",
        }
    )
    semantic_2 = _sha(
        {
            "schema": "test-semantic-state/v0.1",
            "previous": semantic_1,
            "logical_transition_id": "authorization-policy-step-2",
        }
    )
    generation_1 = anchor.history_generation + generations[0]
    generation_2 = generation_1 + generations[1]
    registry_1 = _sha({"path": path, "registry": 1})
    manifest_1 = _sha({"path": path, "manifest": 1})
    registry_2 = _sha({"path": path, "registry": 2})
    manifest_2 = _sha({"path": path, "manifest": 2})
    step_1 = HistoricalTransitionObservation(
        verified=True,
        provider_id=anchor.provider_id,
        genesis_authority_id=anchor.genesis_authority_id,
        from_history_generation=anchor.history_generation,
        to_history_generation=generation_1,
        from_registry_sha256=anchor.registry_sha256,
        to_registry_sha256=registry_1,
        from_manifest_sha256=anchor.manifest_sha256,
        to_manifest_sha256=manifest_1,
        transition_provenance_sha256=_sha({"path": path, "proof": 1}),
        trust_domain=anchor.trust_domain,
        logical_transition_id="authorization-policy-step-1",
        transition_contract_sha256=_sha("transition-contract-step-1"),
        authorization_contract_sha256=_sha("transition-authorization-step-1"),
        from_semantic_state_sha256=anchor.semantic_state_sha256,
        to_semantic_state_sha256=semantic_1,
    )
    step_2 = HistoricalTransitionObservation(
        verified=True,
        provider_id=anchor.provider_id,
        genesis_authority_id=anchor.genesis_authority_id,
        from_history_generation=generation_1,
        to_history_generation=generation_2,
        from_registry_sha256=registry_1,
        to_registry_sha256=registry_2,
        from_manifest_sha256=manifest_1,
        to_manifest_sha256=manifest_2,
        transition_provenance_sha256=_sha({"path": path, "proof": 2}),
        trust_domain=anchor.trust_domain,
        logical_transition_id="authorization-policy-step-2",
        transition_contract_sha256=_sha("transition-contract-step-2"),
        authorization_contract_sha256=_sha("transition-authorization-step-2"),
        from_semantic_state_sha256=semantic_1,
        to_semantic_state_sha256=semantic_2,
    )
    return step_1, step_2


def _agreement():
    primary, secondary = _anchor_observations()
    checkpoint, witness = _anchor_objects(primary)
    primary_steps = _path_transitions(primary, path="a", generations=(2, 1))
    secondary_steps = _path_transitions(secondary, path="b", generations=(1, 3))
    agreement = compare_multi_epoch_causal_evolution(
        primary,
        secondary,
        anchor_checkpoint=checkpoint,
        anchor_witness=witness,
        primary_transitions=primary_steps,
        secondary_transitions=secondary_steps,
    )
    return agreement, primary, secondary, checkpoint, witness, primary_steps, secondary_steps


def test_two_history_paths_converge_across_epoch_one_and_two() -> None:
    agreement, _, _, checkpoint, witness, _, _ = _agreement()

    assert agreement.verified is True
    assert agreement.reason == "portable_causal_state_evolution_verified"
    assert len(agreement.checkpoints) == 2
    assert len(agreement.witnesses) == 2
    assert agreement.receipt is not None
    assert agreement.receipt["epochs_advanced"] == 2
    assert agreement.receipt["final_causal_epoch"] == 2
    assert agreement.receipt["raw_history_embedded"] is False
    assert agreement.receipt["equivalent_checkpoint_chain"] is True
    assert agreement.receipt["equivalent_witness_chain"] is True
    assert validate_evolution_checkpoint_chain(checkpoint, agreement.checkpoints)
    assert validate_evolution_witness_chain(
        witness,
        checkpoint,
        agreement.checkpoints,
        agreement.witnesses,
    )


def test_history_generation_is_not_causal_epoch() -> None:
    agreement, _, _, _, _, primary_steps, secondary_steps = _agreement()
    assert agreement.verified is True
    assert agreement.receipt is not None
    assert primary_steps[-1].to_history_generation != secondary_steps[-1].to_history_generation
    assert agreement.receipt["final_causal_epoch"] == 2


def test_raw_history_is_absent_from_portable_chains() -> None:
    agreement, primary, secondary, _, _, primary_steps, secondary_steps = _agreement()
    portable = repr((agreement.checkpoints, agreement.witnesses))
    forbidden = {
        primary.provider_id,
        secondary.provider_id,
        primary.genesis_authority_id,
        secondary.genesis_authority_id,
        primary.registry_sha256,
        secondary.registry_sha256,
        primary.manifest_sha256,
        secondary.manifest_sha256,
    }
    for transition in (*primary_steps, *secondary_steps):
        forbidden.update(
            {
                transition.from_registry_sha256,
                transition.to_registry_sha256,
                transition.from_manifest_sha256,
                transition.to_manifest_sha256,
                transition.transition_provenance_sha256,
            }
        )
    assert all(value not in portable for value in forbidden)


@pytest.mark.parametrize(
    ("mutator", "reason"),
    [
        (
            lambda p, s: (p, (replace(s[0], verified=False), s[1])),
            "historical_transition_invalid",
        ),
        (
            lambda p, s: (
                p,
                (
                    replace(s[0], transition_provenance_sha256=p[0].transition_provenance_sha256),
                    s[1],
                ),
            ),
            "transition_provenance_not_independent",
        ),
        (
            lambda p, s: (
                p,
                (replace(s[0], to_semantic_state_sha256=_sha("different")), s[1]),
            ),
            "transition_to_state_mismatch",
        ),
        (
            lambda p, s: (
                p,
                (replace(s[0], logical_transition_id="other-transition"), s[1]),
            ),
            "logical_transition_mismatch",
        ),
        (
            lambda p, s: (
                p,
                (replace(s[0], transition_contract_sha256=_sha("other-contract")), s[1]),
            ),
            "transition_contract_mismatch",
        ),
        (
            lambda p, s: (
                p,
                (
                    replace(
                        s[0],
                        authorization_contract_sha256=_sha("other-authorization"),
                    ),
                    s[1],
                ),
            ),
            "transition_authorization_mismatch",
        ),
    ],
)
def test_semantic_and_independence_failures_fail_closed(mutator, reason: str) -> None:
    primary, secondary = _anchor_observations()
    checkpoint, witness = _anchor_objects(primary)
    primary_steps = _path_transitions(primary, path="a", generations=(2, 1))
    secondary_steps = _path_transitions(secondary, path="b", generations=(1, 3))
    primary_steps, secondary_steps = mutator(primary_steps, secondary_steps)

    agreement = compare_multi_epoch_causal_evolution(
        primary,
        secondary,
        anchor_checkpoint=checkpoint,
        anchor_witness=witness,
        primary_transitions=primary_steps,
        secondary_transitions=secondary_steps,
    )
    assert agreement.verified is False
    assert agreement.reason == reason


def test_historical_prefix_break_fails_closed() -> None:
    primary, secondary = _anchor_observations()
    checkpoint, witness = _anchor_objects(primary)
    primary_steps = list(_path_transitions(primary, path="a", generations=(2, 1)))
    secondary_steps = _path_transitions(secondary, path="b", generations=(1, 3))
    primary_steps[1] = replace(
        primary_steps[1],
        from_registry_sha256=_sha("wrong-prefix"),
    )

    agreement = compare_multi_epoch_causal_evolution(
        primary,
        secondary,
        anchor_checkpoint=checkpoint,
        anchor_witness=witness,
        primary_transitions=primary_steps,
        secondary_transitions=secondary_steps,
    )
    assert agreement.verified is False
    assert agreement.reason == "historical_transition_prefix_mismatch"


def test_checkpoint_chain_rejects_tampered_epoch_two_prefix() -> None:
    agreement, _, _, checkpoint, _, _, _ = _agreement()
    chain = list(deepcopy(agreement.checkpoints))
    chain[1]["previous_checkpoint_sha256"] = _sha("wrong")
    assert validate_evolution_checkpoint_chain(checkpoint, chain) is False


def test_witness_chain_rejects_tampered_epoch_two_prefix() -> None:
    agreement, _, _, checkpoint, witness, _, _ = _agreement()
    witnesses = list(deepcopy(agreement.witnesses))
    witnesses[1]["previous_witness_sha256"] = _sha("wrong")
    assert (
        validate_evolution_witness_chain(
            witness,
            checkpoint,
            agreement.checkpoints,
            witnesses,
        )
        is False
    )


def test_sequence_length_mismatch_fails_closed() -> None:
    primary, secondary = _anchor_observations()
    checkpoint, witness = _anchor_objects(primary)
    primary_steps = _path_transitions(primary, path="a", generations=(2, 1))
    secondary_steps = _path_transitions(secondary, path="b", generations=(1, 3))

    agreement = compare_multi_epoch_causal_evolution(
        primary,
        secondary,
        anchor_checkpoint=checkpoint,
        anchor_witness=witness,
        primary_transitions=primary_steps,
        secondary_transitions=secondary_steps[:1],
    )
    assert agreement.verified is False
    assert agreement.reason == "transition_sequence_length_mismatch"


def test_raw_history_smuggled_as_transition_id_fails_closed() -> None:
    primary, secondary = _anchor_observations()
    checkpoint, witness = _anchor_objects(primary)
    primary_steps = list(_path_transitions(primary, path="a", generations=(2, 1)))
    secondary_steps = list(_path_transitions(secondary, path="b", generations=(1, 3)))
    leaked = primary_steps[0].to_registry_sha256
    primary_steps[0] = replace(primary_steps[0], logical_transition_id=leaked)
    secondary_steps[0] = replace(secondary_steps[0], logical_transition_id=leaked)

    agreement = compare_multi_epoch_causal_evolution(
        primary,
        secondary,
        anchor_checkpoint=checkpoint,
        anchor_witness=witness,
        primary_transitions=primary_steps,
        secondary_transitions=secondary_steps,
    )
    assert agreement.verified is False
    assert agreement.reason == "raw_history_dependency"
