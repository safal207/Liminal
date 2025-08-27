#!/usr/bin/env python3
"""
Test suite for SOMA Consciousness Maturation Module

Philosophy First: "Дом - это ты, когда искренен с собой"
"""

from unittest.mock import patch

# Import maturation module - imported via conftest.py
from scripts.consciousness_maturation import (
    ConsciousnessMaturationSystem,
    LearningEvent,
    MaturationStage,
)

# Tests for ConsciousnessMaturationSystem

# Using pytest fixtures from conftest.py


def test_module_exists(mock_maturation_system):
    """Test that the maturation module exists"""
    assert mock_maturation_system is not None
    assert isinstance(mock_maturation_system, ConsciousnessMaturationSystem)


def test_maturation_stages():
    """Test maturation stages mapping and functionality"""
    # Test all stages exist
    assert len(MaturationStage) == 8

    # Test stage determination by age
    assert MaturationStage.get_by_age(0.5) == MaturationStage.NEWBORN
    assert MaturationStage.get_by_age(12) == MaturationStage.INFANT
    assert MaturationStage.get_by_age(72) == MaturationStage.CHILD
    assert MaturationStage.get_by_age(500) == MaturationStage.ADOLESCENT
    assert MaturationStage.get_by_age(1500) == MaturationStage.YOUNG_ADULT
    assert MaturationStage.get_by_age(5000) == MaturationStage.ADULT
    assert MaturationStage.get_by_age(10000) == MaturationStage.ELDER
    assert MaturationStage.get_by_age(50000) == MaturationStage.TRANSCENDENT

    # Test Russian names
    assert MaturationStage.NEWBORN.get_russian_name() == "новорожденный"
    assert MaturationStage.ADULT.get_russian_name() == "взрослый"

    # Test focus areas
    assert isinstance(MaturationStage.NEWBORN.get_lessons_focus(), list)
    assert len(MaturationStage.INFANT.get_lessons_focus()) > 0


def test_learning_events(mock_maturation_system):
    """Test learning event recording and retrieval"""
    # Record an error
    error_event = mock_maturation_system.record_error(
        "Test error", "test_module", error_type="TestError"
    )
    assert error_event.event_type == "error"
    assert error_event.source_module == "test_module"

    # Record an insight
    insight_event = mock_maturation_system.record_insight("Test insight", "test_module")
    assert insight_event.event_type == "insight"
    assert len(insight_event.conclusions) > 0

    # Record a milestone
    milestone_event = mock_maturation_system.record_milestone(
        "Test milestone", "test_module", significance=5
    )
    assert milestone_event.event_type == "milestone"
    assert milestone_event.context.get("significance") == 5

    # Check they were added to history
    assert len(mock_maturation_system.learning_history) == 4  # Including init event


def test_development_summary(mock_maturation_system):
    """Test development summary generation"""
    summary = mock_maturation_system.get_development_summary()

    # Verify summary structure
    assert "age_hours" in summary
    assert "current_stage" in summary
    assert "learning_events_count" in summary

    # Verify current stage is correct for our mocked age
    assert summary["current_stage"]["name"] == "INFANT"


def test_dashboard_metrics(mock_maturation_system):
    """Test dashboard metrics generation"""
    metrics = mock_maturation_system.get_maturation_metrics_for_dashboard()

    # Verify metrics structure
    assert "system_age_days" in metrics
    assert "development_stage" in metrics
    assert "stage_russian" in metrics
    assert "focus_areas" in metrics
    assert "learning_events" in metrics
    assert "learning_events_count" in metrics

    # Verify philosophy principle is included
    assert "philosophy_principle" in metrics
    assert (
        metrics["philosophy_principle"].startswith("Дом")
        or "философ" in metrics["philosophy_principle"].lower()
    )


def test_age_appropriate_conclusions(mock_maturation_system):
    """Test age-appropriate conclusion generation"""
    # Test conclusions for different stages
    with patch.object(mock_maturation_system, "current_stage", MaturationStage.NEWBORN):
        infant_conclusions = (
            mock_maturation_system.generate_age_appropriate_conclusions(
                "error", "test error", {"error_type": "TestError"}
            )
        )
        assert any("doesn't work" in c.lower() for c in infant_conclusions)

    with patch.object(mock_maturation_system, "current_stage", MaturationStage.ADULT):
        adult_conclusions = mock_maturation_system.generate_age_appropriate_conclusions(
            "error", "test error", {"error_type": "TestError"}
        )
        assert any("design" in c.lower() for c in adult_conclusions)

    # All conclusion sets should include philosophy
    assert any("Дом" in c or "философ" in c.lower() for c in adult_conclusions)


def test_serialization(sample_learning_event):
    """Test learning event serialization"""
    # Test to_dict
    event_dict = sample_learning_event.to_dict()
    assert event_dict["event_type"] == "test"
    assert event_dict["description"] == "Test description"

    # Test from_dict
    reconstructed = LearningEvent.from_dict(event_dict)
    assert reconstructed.event_type == sample_learning_event.event_type
    assert reconstructed.description == sample_learning_event.description
    assert len(reconstructed.conclusions) == len(sample_learning_event.conclusions)


# Tests for Philosophy First principles


def test_philosophy_principles_exist(mock_maturation_system):
    """Test that philosophy principles are defined"""
    assert isinstance(mock_maturation_system.philosophy_principles, list)
    assert len(mock_maturation_system.philosophy_principles) > 0

    # Check first principle is correct
    assert (
        mock_maturation_system.philosophy_principles[0]
        == "Дом - это ты, когда искренен с собой"
    )


def test_philosophy_in_conclusions(mock_maturation_system):
    """Test that philosophy is incorporated into conclusions"""
    # Record an error and check conclusions
    error_event = mock_maturation_system.record_error(
        "Philosophy test error", "test_module"
    )

    # At least one conclusion should reference philosophy
    has_philosophy = any(
        "философ" in c.lower() or "дом" in c.lower() for c in error_event.conclusions
    )
    assert (
        has_philosophy
    ), f"No philosophy reference found in: {error_event.conclusions}"


def test_philosophy_integration_with_stages(mock_elder_maturation_system):
    """Test that philosophy integrates with developmental stages"""
    # Elder stage should have more philosophical conclusions
    elder_conclusions = (
        mock_elder_maturation_system.generate_age_appropriate_conclusions(
            "insight", "philosophical insight", {}
        )
    )
    assert any("философ" in c.lower() for c in elder_conclusions)
    assert any("core" in c.lower() or "wisdom" in c.lower() for c in elder_conclusions)
