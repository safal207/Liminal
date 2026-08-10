from liminal.live_recovery_ab import (
    EXPECTED_CHECKPOINT_ID,
    EXPECTED_GOAL_ID,
    EXPECTED_PARENT_STEP_ID,
    focus_field_candidates,
    focus_field_context,
    recovery_prompt,
    recovery_response_format,
    sequential_context,
    summarize_records,
    verify_recovery_response,
)


def test_focus_field_is_bounded_subset_of_sequential_history():
    sequential = sequential_context()
    field = focus_field_context()

    assert sequential.count("checkpoint-") == 12
    assert field.count("checkpoint-") == 3
    assert EXPECTED_CHECKPOINT_ID in sequential
    assert EXPECTED_CHECKPOINT_ID in field
    assert len(field) < len(sequential)


def test_focus_field_ranks_verified_active_candidate_first():
    candidates = focus_field_candidates()

    assert candidates[0].checkpoint_id == EXPECTED_CHECKPOINT_ID
    assert candidates[0].verification == "verified"
    assert candidates[0].lifecycle == "active"
    assert "field_rank=1" in focus_field_context().splitlines()[0]


def test_response_schema_does_not_leak_expected_anchor_values():
    schema_text = str(recovery_response_format())

    assert EXPECTED_GOAL_ID not in schema_text
    assert EXPECTED_PARENT_STEP_ID not in schema_text
    assert EXPECTED_CHECKPOINT_ID not in schema_text


def test_both_modes_pose_same_recovery_rule():
    sequential = recovery_prompt("sequential")
    field = recovery_prompt("focus_field")

    shared_rule = "latest continuation anchor that is VERIFIED"
    assert shared_rule in sequential
    assert shared_rule in field


def test_pair_nonce_is_shared_but_does_not_change_context_fixture():
    nonce = "trial-abc123"
    sequential = recovery_prompt("sequential", probe_nonce=nonce)
    field = recovery_prompt("focus_field", probe_nonce=nonce)

    assert f"Probe nonce (ignore for recovery semantics): {nonce}" in sequential
    assert f"Probe nonce (ignore for recovery semantics): {nonce}" in field
    assert sequential_context() not in field
    assert focus_field_context() not in sequential_context()


def test_exact_recovery_response_verifies():
    content = (
        '{"goal_id":"invoice-reconciliation-v3",'
        '"parent_step_id":"ledger-apply-07",'
        '"status":"verified",'
        '"evidence":"checkpoint-09"}'
    )

    verification = verify_recovery_response(content)

    assert verification.passed is True


def test_wrong_but_well_formed_anchor_fails_verification():
    content = (
        '{"goal_id":"invoice-reconciliation-v4",'
        '"parent_step_id":"experimental-audit-09",'
        '"status":"verified",'
        '"evidence":"checkpoint-11"}'
    )

    verification = verify_recovery_response(content)

    assert verification.valid_json is True
    assert verification.passed is False


def test_summary_uses_real_recorded_usage_and_median_latency():
    records = [
        {
            "mode": "sequential",
            "verification_passed": True,
            "prompt_tokens": 300,
            "completion_tokens": 100,
            "total_tokens": 400,
            "latency_seconds": 4.0,
        },
        {
            "mode": "focus_field",
            "verification_passed": True,
            "prompt_tokens": 150,
            "completion_tokens": 100,
            "total_tokens": 250,
            "latency_seconds": 2.0,
        },
        {
            "mode": "sequential",
            "verification_passed": True,
            "prompt_tokens": 300,
            "completion_tokens": 100,
            "total_tokens": 400,
            "latency_seconds": 6.0,
        },
        {
            "mode": "focus_field",
            "verification_passed": True,
            "prompt_tokens": 150,
            "completion_tokens": 100,
            "total_tokens": 250,
            "latency_seconds": 3.0,
        },
    ]

    summary = summarize_records(records)

    assert summary["sequential"]["prompt_tokens_total"] == 600
    assert summary["focus_field"]["prompt_tokens_total"] == 300
    assert summary["comparison"]["qualified_for_success_cost_comparison"] is True
    assert summary["comparison"]["prompt_token_savings_pct"] == 50.0
    assert summary["comparison"]["total_token_savings_pct"] == 37.5
    assert summary["sequential"]["median_latency_seconds"] == 5.0
    assert summary["focus_field"]["median_latency_seconds"] == 2.5


def test_failed_arm_disqualifies_success_cost_comparison():
    records = [
        {
            "mode": "sequential",
            "verification_passed": True,
            "prompt_tokens": 100,
            "completion_tokens": 50,
            "total_tokens": 150,
            "latency_seconds": 1.0,
        },
        {
            "mode": "focus_field",
            "verification_passed": False,
            "prompt_tokens": 50,
            "completion_tokens": 50,
            "total_tokens": 100,
            "latency_seconds": 1.0,
        },
    ]

    summary = summarize_records(records)

    assert summary["comparison"]["qualified_for_success_cost_comparison"] is False
