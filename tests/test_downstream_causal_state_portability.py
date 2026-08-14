from __future__ import annotations

from copy import deepcopy
from dataclasses import replace

import pytest

from liminal.downstream_causal_state_portability import (
    CHECKPOINT_ROLE,
    WITNESS_ROLE,
    CausalAuthority,
    HistoricalStateObservation,
    compare_downstream_causal_states,
    validate_causal_checkpoint,
    validate_causal_witness,
)


def _sha(char: str) -> str:
    return char * 64


def _observations() -> tuple[HistoricalStateObservation, HistoricalStateObservation]:
    primary = HistoricalStateObservation(
        verified=True,
        provider_id="github-oidc-root-a",
        genesis_authority_id="root-a",
        history_generation=1,
        registry_sha256=_sha("b"),
        manifest_sha256=_sha("c"),
        semantic_state_sha256=_sha("a"),
        trust_domain="liminal.trusted-recovery",
    )
    secondary = HistoricalStateObservation(
        verified=True,
        provider_id="offline-ed25519-root-b",
        genesis_authority_id="root-b",
        history_generation=9,
        registry_sha256=_sha("d"),
        manifest_sha256=_sha("e"),
        semantic_state_sha256=_sha("a"),
        trust_domain="liminal.trusted-recovery",
    )
    return primary, secondary


def _authorities() -> tuple[CausalAuthority, CausalAuthority]:
    checkpoint = CausalAuthority(
        role=CHECKPOINT_ROLE,
        logical_authority_id="liminal.causal-checkpoint",
        producer_contract_sha256=_sha("1"),
        authorization_contract_sha256=_sha("2"),
    )
    witness = CausalAuthority(
        role=WITNESS_ROLE,
        logical_authority_id="liminal.causal-witness",
        producer_contract_sha256=_sha("3"),
        authorization_contract_sha256=_sha("4"),
    )
    return checkpoint, witness


def _agreement(
    primary: HistoricalStateObservation,
    secondary: HistoricalStateObservation,
):
    checkpoint_authority, witness_authority = _authorities()
    return compare_downstream_causal_states(
        primary,
        secondary,
        logical_state_id="liminal.trusted-recovery.authorization-state",
        causal_epoch=0,
        checkpoint_authority=checkpoint_authority,
        witness_authority=witness_authority,
    )


def test_distinct_histories_converge_on_identical_downstream_state() -> None:
    primary, secondary = _observations()
    agreement = _agreement(primary, secondary)

    assert agreement.verified is True
    assert agreement.reason == "downstream_causal_state_portability_verified"
    assert agreement.checkpoint is not None
    assert agreement.witness is not None
    assert agreement.receipt is not None
    assert agreement.receipt["primary_history_generation"] == 1
    assert agreement.receipt["secondary_history_generation"] == 9
    assert agreement.receipt["raw_history_embedded"] is False
    assert agreement.receipt["equivalent_downstream_checkpoint"] is True
    assert agreement.receipt["equivalent_downstream_witness"] is True
    assert validate_causal_checkpoint(agreement.checkpoint)
    assert validate_causal_witness(agreement.witness, agreement.checkpoint)


def test_raw_history_provenance_is_not_embedded_in_portable_objects() -> None:
    primary, secondary = _observations()
    agreement = _agreement(primary, secondary)
    assert agreement.verified is True
    portable = repr((agreement.checkpoint, agreement.witness))
    forbidden = (
        primary.provider_id,
        secondary.provider_id,
        primary.genesis_authority_id,
        secondary.genesis_authority_id,
        primary.registry_sha256,
        secondary.registry_sha256,
        primary.manifest_sha256,
        secondary.manifest_sha256,
    )
    assert all(value not in portable for value in forbidden)


@pytest.mark.parametrize(
    ("mutate", "reason"),
    [
        (lambda a, b: (a, replace(b, provider_id=a.provider_id)),
         "history_provider_not_independent"),
        (lambda a, b: (a, replace(b, genesis_authority_id=a.genesis_authority_id)),
         "genesis_authority_not_independent"),
        (lambda a, b: (a, replace(b, registry_sha256=a.registry_sha256)),
         "history_registry_not_independent"),
        (lambda a, b: (a, replace(b, manifest_sha256=a.manifest_sha256)),
         "history_manifest_not_independent"),
        (lambda a, b: (a, replace(b, verified=False)),
         "historical_observation_unverified"),
        (lambda a, b: (a, replace(b, semantic_state_sha256=_sha("f"))),
         "terminal_semantic_state_mismatch"),
        (lambda a, b: (a, replace(b, trust_domain="other-domain")),
         "trust_domain_mismatch"),
    ],
)
def test_independence_and_semantic_failures_fail_closed(mutate, reason: str) -> None:
    primary, secondary = _observations()
    primary, secondary = mutate(primary, secondary)
    agreement = _agreement(primary, secondary)
    assert agreement.verified is False
    assert agreement.reason == reason


def test_anchor_checkpoint_rejects_history_style_previous_digest() -> None:
    primary, secondary = _observations()
    agreement = _agreement(primary, secondary)
    assert agreement.checkpoint is not None
    tampered = deepcopy(agreement.checkpoint)
    tampered["previous_checkpoint_sha256"] = _sha("0")
    assert validate_causal_checkpoint(tampered) is False


def test_witness_rejects_checkpoint_digest_drift() -> None:
    primary, secondary = _observations()
    agreement = _agreement(primary, secondary)
    assert agreement.checkpoint is not None
    assert agreement.witness is not None
    tampered = deepcopy(agreement.witness)
    tampered["checkpoint_sha256"] = _sha("0")
    assert validate_causal_witness(tampered, agreement.checkpoint) is False
