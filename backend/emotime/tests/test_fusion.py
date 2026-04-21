"""
🌿✨ Emotime Feature Fusion Tests — testing multi-sensor data fusion
"""

import asyncio
import os
import sys
from datetime import datetime, timedelta

import pytest

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from emotime.fusion import EmotionalFeatures, FeatureFusion
from emotime.sensors import AudioData, SensorData, SensorType, TextData, TouchData


class TestEmotionalFeatures:
    """Test EmotionalFeatures dataclass."""

    def test_features_creation(self):
        """Test EmotionalFeatures creation."""
        features = EmotionalFeatures(
            valence=0.7,
            arousal=0.8,
            dominance=0.6,
            tempo=0.5,
            intensity=0.9,
            confidence=0.85,
        )

        assert features.valence == 0.7
        assert features.arousal == 0.8
        assert features.dominance == 0.6
        assert features.tempo == 0.5
        assert features.intensity == 0.9
        assert features.confidence == 0.85

    def test_features_validation(self):
        """Test dataclass stores provided feature values as-is."""
        features = EmotionalFeatures(
            valence=1.2, arousal=-0.1, dominance=0.5, tempo=0.5, intensity=0.5
        )

        assert features.valence == 1.2
        assert features.arousal == -0.1


class TestFeatureFusion:
    """Test FeatureFusion class."""

    def setup_method(self):
        """Setup for each test."""
        self.fusion = FeatureFusion(smoothing_alpha=0.3)

    @staticmethod
    def _make_text_sensor_data(
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
    def _make_touch_sensor_data(
        timestamp: datetime | None = None,
        **metadata,
    ) -> SensorData:
        return SensorData(
            sensor_type=SensorType.TOUCH,
            timestamp=timestamp or datetime.now(),
            raw_data=TouchData(
                pressure=0.6,
                duration=1.0,
                frequency=30.0,
                pattern="tap",
            ),
            metadata=metadata,
        )

    @staticmethod
    def _make_audio_sensor_data(
        timestamp: datetime | None = None,
        **metadata,
    ) -> SensorData:
        return SensorData(
            sensor_type=SensorType.AUDIO,
            timestamp=timestamp or datetime.now(),
            raw_data=AudioData(
                pitch_mean=160.0,
                pitch_variance=25.0,
                speech_rate=160.0,
                volume_level=0.7,
                pause_ratio=0.15,
                emotion_markers=["joy"],
            ),
            metadata=metadata,
        )

    @pytest.mark.asyncio
    async def test_process_single_sensor(self):
        """Test processing data from a single sensor."""
        sensor_data = [
            self._make_text_sensor_data(
                "I feel wonderful today!",
                valence=0.8,
                arousal=0.6,
                intensity=0.8,
            )
        ]

        result = await self.fusion.process_batch(sensor_data)

        assert result is not None
        assert result.valence == pytest.approx(0.8)
        assert result.arousal == pytest.approx(0.6)
        assert result.dominance == pytest.approx(0.5)
        assert result.tempo == pytest.approx(0.5)
        assert result.intensity == pytest.approx(0.8)
        assert result.confidence == pytest.approx(1 / 3)
        assert result.sources == ["text"]

    @pytest.mark.asyncio
    async def test_process_multiple_sensors(self):
        """Test processing data from multiple sensors."""
        sensor_data = [
            self._make_text_sensor_data(
                "I'm excited!",
                valence=0.9,
                arousal=0.8,
                intensity=0.9,
            ),
            self._make_touch_sensor_data(
                valence=0.7,
                arousal=0.6,
                intensity=0.7,
            ),
            self._make_audio_sensor_data(
                valence=0.8,
                arousal=0.9,
                intensity=0.8,
            ),
        ]

        result = await self.fusion.process_batch(sensor_data)

        assert result is not None
        assert result.valence == pytest.approx(0.81)
        assert result.arousal == pytest.approx(0.77)
        assert result.dominance == pytest.approx(0.56)
        assert result.tempo == pytest.approx(0.59)
        assert result.intensity == pytest.approx(0.81)
        assert result.confidence == pytest.approx(1.0)
        assert set(result.sources) == {"text", "touch", "audio"}

    @pytest.mark.asyncio
    async def test_process_empty_batch(self):
        """Test processing empty batch."""
        result = await self.fusion.process_batch([])
        assert result is None

    @pytest.mark.asyncio
    async def test_ewma_smoothing(self):
        """Test EWMA smoothing functionality."""
        # First batch
        sensor_data1 = [
            self._make_text_sensor_data(
                "Happy",
                timestamp=datetime.now(),
                valence=0.9,
                arousal=0.5,
                intensity=0.5,
            )
        ]

        # Second batch with different values
        sensor_data2 = [
            self._make_text_sensor_data(
                "Sad",
                timestamp=datetime.now() + timedelta(seconds=1),
                valence=0.1,
                arousal=0.5,
                intensity=0.5,
            )
        ]

        result1 = await self.fusion.process_batch(sensor_data1)
        assert result1.valence == pytest.approx(0.9)

        result2 = await self.fusion.process_batch(sensor_data2)
        assert result2.valence == pytest.approx(0.66)
        assert result2.arousal == pytest.approx(0.5)

    @pytest.mark.asyncio
    async def test_fuse_features_clamps_ranges(self):
        """Test weighted fusion clamps out-of-range values."""
        result = await self.fusion._fuse_features(
            {
                SensorType.TEXT: {
                    "valence": 2.0,
                    "arousal": -1.0,
                    "dominance": 1.5,
                    "tempo": 2.0,
                    "intensity": -0.5,
                }
            },
            ["text"],
        )

        assert result.valence == 1.0
        assert result.arousal == 0.0
        assert result.dominance == 1.0
        assert result.tempo == 1.0
        assert result.intensity == 0.0
        assert result.confidence == pytest.approx(1 / 3)

    def test_apply_ewma_smoothing(self):
        """Test EWMA smoothing application."""
        # Set up previous features
        self.fusion.last_features = EmotionalFeatures(
            valence=0.5,
            arousal=0.5,
            dominance=0.5,
            tempo=0.5,
            intensity=0.5,
            confidence=0.8,
        )

        # New features
        new_features = EmotionalFeatures(
            valence=1.0,
            arousal=1.0,
            dominance=1.0,
            tempo=1.0,
            intensity=1.0,
            confidence=0.9,
        )

        smoothed = asyncio.run(self.fusion._apply_smoothing(new_features))

        expected_valence = 0.5 * 0.7 + 1.0 * 0.3
        assert smoothed.valence == pytest.approx(expected_valence)
        assert smoothed.confidence == 0.9

    def test_feature_statistics(self):
        """Test feature statistics collection."""
        features1 = EmotionalFeatures(
            valence=0.8,
            arousal=0.6,
            dominance=0.7,
            tempo=0.5,
            intensity=0.8,
            confidence=0.9,
            sources=["text"],
        )
        features2 = EmotionalFeatures(
            valence=0.6,
            arousal=0.8,
            dominance=0.5,
            tempo=0.7,
            intensity=0.6,
            confidence=0.7,
            sources=["audio"],
        )

        self.fusion.feature_history.extend([features1, features2])

        stats = self.fusion.get_feature_statistics()

        assert stats["total_points"] == 2
        assert stats["valence"]["mean"] == pytest.approx(0.7)
        assert stats["valence"]["min"] == pytest.approx(0.6)
        assert stats["valence"]["max"] == pytest.approx(0.8)
        assert stats["arousal"]["mean"] == pytest.approx(0.7)
        assert stats["recent_confidence"] == pytest.approx(0.7)
        assert stats["active_sources"] == ["audio"]

    @pytest.mark.asyncio
    async def test_confidence_reflects_active_sensor_count(self):
        """Test confidence is derived from active sensor count."""
        single_sensor = await self.fusion.process_batch(
            [
                self._make_text_sensor_data(
                    "steady", valence=0.4, arousal=0.4, intensity=0.4
                )
            ]
        )
        all_sensors = await self.fusion.process_batch(
            [
                self._make_text_sensor_data(
                    "steady", valence=0.4, arousal=0.4, intensity=0.4
                ),
                self._make_touch_sensor_data(valence=0.4, arousal=0.4, intensity=0.4),
                self._make_audio_sensor_data(valence=0.4, arousal=0.4, intensity=0.4),
            ]
        )

        assert single_sensor.confidence == pytest.approx(1 / 3)
        assert all_sensors.confidence == pytest.approx(1.0)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
