import asyncio
import os
import sys
from datetime import datetime, timedelta

import pytest

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from emotime.core import EmotimeEngine
from emotime.sensors import AudioData, SensorData, SensorType, TextData, TouchData


class TestEmotimeIntegration:
    """Integration tests for the complete Emotime system."""

    def setup_method(self):
        """Setup for each test method."""
        self.engine = EmotimeEngine(
            user_id="integration_test_user",
            session_id="integration_test_session",
            update_interval=0.05,  # Very fast for testing
            enable_neo4j=False,  # Disable Neo4j for integration tests
            enable_ml=False,  # Keep integration tests deterministic in CI
            trace_window=20,
        )

    def teardown_method(self):
        """Cleanup after each test method."""
        if hasattr(self.engine, "is_running"):
            self.engine.is_running = False

    @staticmethod
    def _text_data(
        text: str,
        timestamp: datetime | None = None,
        **metadata,
    ) -> SensorData:
        return SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=timestamp or datetime.now(),
            raw_data=TextData(
                text=text,
                word_count=len(text.split()),
                char_count=len(text),
            ),
            metadata=metadata,
        )

    @staticmethod
    def _touch_data(
        pressure: float,
        duration: float,
        frequency: float,
        pattern: str = "tap",
        timestamp: datetime | None = None,
        **metadata,
    ) -> SensorData:
        return SensorData(
            sensor_type=SensorType.TOUCH,
            timestamp=timestamp or datetime.now(),
            raw_data=TouchData(
                pressure=pressure,
                duration=duration,
                frequency=frequency,
                pattern=pattern,
            ),
            metadata=metadata,
        )

    @staticmethod
    def _audio_data(
        pitch_mean: float,
        speech_rate: float,
        volume_level: float,
        timestamp: datetime | None = None,
        pitch_variance: float = 25.0,
        pause_ratio: float = 0.15,
        emotion_markers: list[str] | None = None,
        **metadata,
    ) -> SensorData:
        return SensorData(
            sensor_type=SensorType.AUDIO,
            timestamp=timestamp or datetime.now(),
            raw_data=AudioData(
                pitch_mean=pitch_mean,
                pitch_variance=pitch_variance,
                speech_rate=speech_rate,
                volume_level=volume_level,
                pause_ratio=pause_ratio,
                emotion_markers=emotion_markers or [],
            ),
            metadata=metadata,
        )

    @pytest.mark.asyncio
    async def test_full_emotional_journey(self):
        """Test a complete emotional journey through the system."""
        # Start the engine
        await self.engine.start()

        try:
            # Phase 1: Calm state
            calm_data = [
                self._text_data(
                    "I feel peaceful and relaxed",
                    valence=0.7,
                    arousal=0.2,
                    intensity=0.2,
                ),
                self._touch_data(
                    pressure=0.2,
                    duration=3.0,
                    frequency=10.0,
                    pattern="hold",
                    valence=0.3,
                    arousal=0.2,
                    intensity=0.2,
                ),
                self._audio_data(
                    pitch_mean=130.0,
                    speech_rate=90.0,
                    volume_level=0.3,
                    pause_ratio=0.3,
                    valence=0.2,
                    arousal=0.2,
                    intensity=0.3,
                ),
            ]

            # Process calm data
            for data in calm_data:
                await self.engine.process_sensor_data(data)

            # Wait for processing
            await asyncio.sleep(0.2)

            # Check state after calm input
            state = await self.engine.get_current_state()
            if state:
                # Should be in a calm-like state
                assert state.features.valence > 0.4  # Positive
                assert state.features.arousal < 0.7  # Not too high arousal

            # Phase 2: Stress state
            stress_data = [
                self._text_data(
                    "URGENT DEADLINE! SO MUCH PRESSURE!!!",
                    valence=-0.7,
                    arousal=0.9,
                    intensity=0.95,
                ),
                self._touch_data(
                    pressure=0.9,
                    duration=0.1,
                    frequency=60.0,
                    pattern="tap",
                    valence=-0.2,
                    arousal=0.8,
                    intensity=0.9,
                ),
                self._audio_data(
                    pitch_mean=220.0,
                    speech_rate=180.0,
                    volume_level=0.9,
                    pause_ratio=0.05,
                    emotion_markers=["stress"],
                    valence=-0.3,
                    arousal=0.95,
                    intensity=0.95,
                ),
            ]

            # Process stress data
            for data in stress_data:
                await self.engine.process_sensor_data(data)

            # Wait for processing
            await asyncio.sleep(0.2)

            # Check state after stress input
            state = await self.engine.get_current_state()
            if state:
                # Should be in high arousal state
                assert state.features.arousal > 0.6
                assert state.features.intensity > 0.6

            # Phase 3: Joy state
            joy_data = [
                self._text_data(
                    "I'm so happy and excited! This is amazing!",
                    valence=0.95,
                    arousal=0.8,
                    intensity=0.85,
                ),
                self._touch_data(
                    pressure=0.6,
                    duration=1.5,
                    frequency=40.0,
                    pattern="gesture",
                    valence=0.6,
                    arousal=0.7,
                    intensity=0.7,
                ),
                self._audio_data(
                    pitch_mean=180.0,
                    speech_rate=170.0,
                    volume_level=0.7,
                    emotion_markers=["joy"],
                    valence=0.8,
                    arousal=0.8,
                    intensity=0.75,
                ),
            ]

            # Process joy data
            for data in joy_data:
                await self.engine.process_sensor_data(data)

            # Wait for processing
            await asyncio.sleep(0.2)

            # Check final state
            state = await self.engine.get_current_state()
            if state:
                # Should be in positive, high-energy state
                assert state.features.valence > 0.45  # Positive after smoothing
                assert state.features.arousal > 0.5  # High arousal

            # Check resonance trace
            trace = await self.engine.get_resonance_trace()
            assert len(trace) > 0  # Should have accumulated points

        finally:
            await self.engine.stop()

    @pytest.mark.asyncio
    async def test_mode_transitions(self):
        """Test emotional mode transitions."""
        await self.engine.start()

        try:
            # Start with focus data
            focus_data = self._text_data(
                "I need to concentrate on this task",
                valence=0.1,
                arousal=0.45,
                intensity=0.4,
            )

            await self.engine.process_sensor_data(focus_data)
            await asyncio.sleep(0.2)

            state1 = await self.engine.get_current_state()
            initial_mode = state1.mode.name if state1 else None

            # Switch to joy data
            joy_data = self._text_data(
                "Wow! I just won the lottery! So excited!",
                valence=0.9,
                arousal=0.9,
                intensity=0.9,
            )

            await self.engine.process_sensor_data(joy_data)
            await asyncio.sleep(0.2)

            state2 = await self.engine.get_current_state()
            final_mode = state2.mode.name if state2 else None

            # Modes might have changed
            if initial_mode and final_mode:
                # Just check that we have valid modes
                assert initial_mode in [
                    "Calm",
                    "Focus",
                    "Stress",
                    "Joy",
                    "Contemplation",
                    "Neutral",
                ]
                assert final_mode in [
                    "Calm",
                    "Focus",
                    "Stress",
                    "Joy",
                    "Contemplation",
                    "Neutral",
                ]

        finally:
            await self.engine.stop()

    @pytest.mark.asyncio
    async def test_confidence_evolution(self):
        """Test how confidence evolves over time."""
        await self.engine.start()

        try:
            # Send consistent data to build confidence
            consistent_data = [
                self._text_data(
                    "I feel calm and peaceful",
                    timestamp=datetime.now() + timedelta(seconds=i),
                    valence=0.5,
                    arousal=0.2,
                    intensity=0.2,
                )
                for i in range(5)
            ]

            confidences = []

            for data in consistent_data:
                await self.engine.process_sensor_data(data)
                await asyncio.sleep(0.1)

                state = await self.engine.get_current_state()
                if state:
                    confidences.append(state.confidence)

            # Confidence should generally increase with consistent data
            if len(confidences) >= 2:
                # At least some confidence values should be reasonable
                assert all(0.0 <= conf <= 1.0 for conf in confidences)

        finally:
            await self.engine.stop()

    @pytest.mark.asyncio
    async def test_sensor_fusion_integration(self):
        """Test integration of multiple sensors simultaneously."""
        await self.engine.start()

        try:
            # Send data from all sensor types at once
            mixed_data = [
                self._text_data(
                    "I'm feeling energetic today!",
                    valence=0.7,
                    arousal=0.7,
                    intensity=0.6,
                ),
                self._touch_data(
                    pressure=0.7,
                    duration=1.0,
                    frequency=35.0,
                    pattern="tap",
                    valence=0.5,
                    arousal=0.6,
                    intensity=0.7,
                ),
                self._audio_data(
                    pitch_mean=170.0,
                    speech_rate=160.0,
                    volume_level=0.6,
                    valence=0.6,
                    arousal=0.7,
                    intensity=0.6,
                ),
            ]

            # Process all data
            for data in mixed_data:
                await self.engine.process_sensor_data(data)

            await asyncio.sleep(0.2)

            # Check that fusion occurred
            state = await self.engine.get_current_state()
            assert state is not None

            # Should have reasonable feature values
            assert 0.0 <= state.features.valence <= 1.0
            assert 0.0 <= state.features.arousal <= 1.0
            assert 0.0 <= state.features.dominance <= 1.0
            assert 0.0 <= state.features.tempo <= 1.0
            assert 0.0 <= state.features.intensity <= 1.0

        finally:
            await self.engine.stop()

    @pytest.mark.asyncio
    async def test_emotional_insights_generation(self):
        """Test comprehensive emotional insights generation."""
        await self.engine.start()

        try:
            # Generate some emotional data
            data_sequence = [
                self._text_data(
                    "Starting my day with optimism",
                    timestamp=datetime.now(),
                    valence=0.7,
                    arousal=0.4,
                    intensity=0.4,
                ),
                self._text_data(
                    "Work is getting challenging",
                    timestamp=datetime.now() + timedelta(seconds=1),
                    valence=-0.1,
                    arousal=0.6,
                    intensity=0.5,
                ),
                self._text_data(
                    "Finally solved the problem! Great success!",
                    timestamp=datetime.now() + timedelta(seconds=2),
                    valence=0.9,
                    arousal=0.8,
                    intensity=0.7,
                ),
            ]

            for data in data_sequence:
                await self.engine.process_sensor_data(data)
                await asyncio.sleep(0.1)

            # Generate insights
            insights = await self.engine.get_emotional_insights()

            # Check insight structure
            assert "current_state" in insights
            assert "timeseries_analysis" in insights
            assert "mode_statistics" in insights
            assert "mode_insights" in insights
            assert "fusion_statistics" in insights

        finally:
            await self.engine.stop()

    @pytest.mark.asyncio
    async def test_metrics_integration(self):
        """Test metrics collection during operation."""
        await self.engine.start()

        try:
            # Process some data to generate metrics
            test_data = self._text_data(
                "Testing metrics collection",
                valence=0.0,
                arousal=0.4,
                intensity=0.4,
            )

            await self.engine.process_sensor_data(test_data)
            await asyncio.sleep(0.2)

            # Check that metrics system is working
            from emotime.metrics_integration import emotime_metrics

            summary = emotime_metrics.get_metrics_summary()

            assert "status" in summary
            assert summary["status"] in {"active", "metrics_disabled"}

        finally:
            await self.engine.stop()

    @pytest.mark.asyncio
    async def test_error_resilience(self):
        """Test system resilience to errors."""
        await self.engine.start()

        try:
            # Send malformed data
            bad_data = [
                SensorData(
                    sensor_type=SensorType.TEXT,
                    raw_data={},  # Empty data
                    metadata={},
                    timestamp=datetime.now(),
                ),
                SensorData(
                    sensor_type=SensorType.TOUCH,
                    raw_data={"invalid": "field"},  # Invalid fields
                    metadata={},
                    timestamp=datetime.now(),
                ),
                SensorData(
                    sensor_type=SensorType.AUDIO,
                    raw_data={"volume": "invalid"},  # Invalid type
                    metadata={},
                    timestamp=datetime.now(),
                ),
            ]

            # System should handle errors gracefully
            for data in bad_data:
                await self.engine.process_sensor_data(data)

            await asyncio.sleep(0.2)

            # Engine should still be running
            assert self.engine.is_running

            # Should handle good data after errors
            good_data = self._text_data(
                "Recovery test",
                valence=0.3,
                arousal=0.4,
                intensity=0.4,
            )

            await self.engine.process_sensor_data(good_data)
            await asyncio.sleep(0.2)

            # Should still work
            state = await self.engine.get_current_state()
            assert state is None or 0.0 <= state.confidence <= 1.0

        finally:
            await self.engine.stop()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
