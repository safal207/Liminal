from liminal.recovery_policy import (
    RecoveryMode,
    RecoveryPolicy,
    RecoverySignals,
    choose_recovery_mode,
)


def test_shallow_recovery_prefers_sequential() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=3,
            candidate_count=2,
            best_anchor_score=0.92,
            uncertainty=0.05,
        )
    )

    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.reason == "shallow_recovery_is_cheaper_or_simpler"


def test_deep_recovery_uses_focus_field_when_anchor_is_credible_and_cheaper() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=17,
            candidate_count=2,
            best_anchor_score=0.81,
            uncertainty=0.15,
        )
    )

    assert decision.mode is RecoveryMode.FOCUS_FIELD
    assert decision.estimated_savings_ratio > 0.85


def test_field_below_break_even_stays_sequential() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=10,
            candidate_count=8,
            best_anchor_score=0.80,
            uncertainty=0.10,
        )
    )

    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.reason == "field_savings_below_break_even"


def test_high_uncertainty_defers_instead_of_forcing_reanchor() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=20,
            candidate_count=3,
            best_anchor_score=0.78,
            uncertainty=0.72,
        )
    )

    assert decision.mode is RecoveryMode.DEFER
    assert decision.reason == "field_uncertainty_too_high"


def test_verified_requirement_defers_when_no_verified_anchor_exists() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=12,
            candidate_count=2,
            best_anchor_score=0.90,
            verified_candidate_available=False,
            require_verified=True,
        )
    )

    assert decision.mode is RecoveryMode.DEFER
    assert decision.reason == "verified_anchor_required"


def test_too_many_candidates_defers_to_keep_field_bounded() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=50,
            candidate_count=40,
            best_anchor_score=0.95,
            uncertainty=0.05,
        )
    )

    assert decision.mode is RecoveryMode.DEFER
    assert decision.reason == "field_candidate_bound_exceeded"


def test_custom_break_even_threshold_can_make_policy_more_conservative() -> None:
    policy = RecoveryPolicy(min_field_savings_ratio=0.75)
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=10,
            candidate_count=4,
            best_anchor_score=0.90,
            uncertainty=0.10,
        ),
        policy,
    )

    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.estimated_savings_ratio == 0.6


def test_low_observed_field_verification_rate_falls_back_to_sequential() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=12,
            candidate_count=3,
            best_anchor_score=0.82,
            uncertainty=0.10,
            field_verification_success_rate=1 / 3,
            field_completion_pressure=1 / 3,
            field_observation_count=3,
        )
    )

    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.reason == "field_observed_verification_rate_too_low"


def test_high_observed_completion_pressure_falls_back_to_sequential() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=12,
            candidate_count=3,
            best_anchor_score=0.82,
            uncertainty=0.10,
            field_verification_success_rate=0.90,
            field_completion_pressure=2 / 3,
            field_observation_count=3,
        )
    )

    assert decision.mode is RecoveryMode.SEQUENTIAL
    assert decision.reason == "field_completion_pressure_too_high"


def test_under_sampled_field_reliability_does_not_override_v01_route() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=12,
            candidate_count=3,
            best_anchor_score=0.82,
            uncertainty=0.10,
            field_verification_success_rate=0.0,
            field_completion_pressure=1.0,
            field_observation_count=2,
        )
    )

    assert decision.mode is RecoveryMode.FOCUS_FIELD
    assert decision.reason == "deep_recovery_with_credible_economic_reanchor"


def test_sufficient_healthy_field_evidence_keeps_focus_field_route() -> None:
    decision = choose_recovery_mode(
        RecoverySignals(
            replay_steps=12,
            candidate_count=3,
            best_anchor_score=0.82,
            uncertainty=0.10,
            field_verification_success_rate=0.90,
            field_completion_pressure=0.10,
            field_observation_count=10,
        )
    )

    assert decision.mode is RecoveryMode.FOCUS_FIELD


def test_invalid_field_observation_metric_is_rejected() -> None:
    try:
        choose_recovery_mode(
            RecoverySignals(
                replay_steps=12,
                candidate_count=3,
                best_anchor_score=0.82,
                field_verification_success_rate=1.2,
                field_observation_count=3,
            )
        )
    except ValueError as exc:
        assert str(exc) == "field_verification_success_rate_must_be_between_0_and_1"
    else:
        raise AssertionError("expected ValueError")
