"""
🌿✨ Emotime Sensors Tests — testing the sensory layer
"""

import os
import sys
from datetime import datetime

import pytest

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from emotime.sensors import (
    AudioData,
    AudioSensor,
    SensorData,
    SensorType,
    TextData,
    TextSensor,
    TouchData,
    TouchSensor,
)


class TestSensorData:
    """Test SensorData dataclasses."""

    def test_sensor_data_creation(self):
        """Test SensorData creation."""
        timestamp = datetime.now()
        text_data = TextData(text="Hello world", word_count=2, char_count=11)
        data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=timestamp,
            raw_data=text_data,
            metadata={},
        )

        assert data.sensor_type == SensorType.TEXT
        assert data.raw_data.text == "Hello world"
        assert data.timestamp == timestamp

    def test_sensor_data_with_metadata(self):
        """Test SensorData with metadata."""
        audio_data = AudioData(
            pitch_mean=180.0,
            pitch_variance=25.0,
            speech_rate=160.0,
            volume_level=0.8,
            pause_ratio=0.1,
            emotion_markers=["joy"],
        )
        data = SensorData(
            sensor_type=SensorType.AUDIO,
            timestamp=datetime.now(),
            raw_data=audio_data,
            metadata={"quality": "high", "duration": 30},
        )

        assert data.metadata["quality"] == "high"
        assert data.metadata["duration"] == 30


class TestTextSensor:
    """Test TextSensor class."""

    def setup_method(self):
        """Setup for each test."""
        self.sensor = TextSensor()

    @pytest.mark.asyncio
    async def test_process_positive_text(self):
        """Test processing positive text."""
        sensor_data = await self.sensor.process("I feel amazing and happy today!")

        assert sensor_data.sensor_type == SensorType.TEXT
        assert sensor_data.raw_data.text == "I feel amazing and happy today!"
        assert sensor_data.metadata["valence"] > 0.0
        assert sensor_data.metadata["arousal"] > 0.0

    @pytest.mark.asyncio
    async def test_process_negative_text(self):
        """Test processing negative text."""
        sensor_data = await self.sensor.process("I feel terrible and sad")

        assert sensor_data.metadata["valence"] < 0.0
        assert sensor_data.metadata["arousal"] > 0.0

    @pytest.mark.asyncio
    async def test_process_neutral_text(self):
        """Test processing neutral text."""
        sensor_data = await self.sensor.process("The weather is okay today.")

        assert -0.1 <= sensor_data.metadata["valence"] <= 0.1
        assert sensor_data.metadata["arousal"] >= 0.0
        assert sensor_data.metadata["intensity"] >= 0.0

    @pytest.mark.asyncio
    async def test_process_empty_text(self):
        """Test processing empty text."""
        sensor_data = await self.sensor.process("")

        assert sensor_data.metadata["valence"] == 0.0
        assert sensor_data.metadata["arousal"] == 0.0
        assert sensor_data.metadata["intensity"] == 0.0

    @pytest.mark.asyncio
    async def test_process_high_arousal_text(self):
        """Test processing high arousal text."""
        sensor_data = await self.sensor.process("URGENT! EMERGENCY! HELP NOW!!!")

        assert sensor_data.metadata["arousal"] > 0.7
        assert sensor_data.metadata["intensity"] > 0.6


class TestTouchSensor:
    """Test TouchSensor class."""

    def setup_method(self):
        """Setup for each test."""
        self.sensor = TouchSensor()

    @pytest.mark.asyncio
    async def test_process_gentle_touch(self):
        """Test processing gentle touch."""
        sensor_data = await self.sensor.process(
            pressure=0.3, duration=2.0, pattern="hold"
        )

        assert sensor_data.sensor_type == SensorType.TOUCH
        assert sensor_data.metadata["valence"] > 0.0
        assert sensor_data.metadata["arousal"] <= 0.1
        assert sensor_data.metadata["intensity"] < 0.5

    @pytest.mark.asyncio
    async def test_process_strong_touch(self):
        """Test processing strong touch."""
        sensor_data = await self.sensor.process(
            pressure=0.9, duration=0.5, pattern="tap"
        )

        assert sensor_data.metadata["valence"] <= 0.0
        assert sensor_data.metadata["arousal"] >= 0.3
        assert sensor_data.metadata["intensity"] >= 0.5

    def test_touch_emotion_analysis_with_frequency(self):
        """Test touch emotion analysis using explicit frequency data."""
        touch_data = TouchData(
            pressure=0.6,
            duration=0.2,
            frequency=30.0,
            pattern="tap",
        )

        features = self.sensor._analyze_touch_emotions(touch_data)

        assert features["arousal"] > 0.7
        assert features["intensity"] > 0.3

    @pytest.mark.asyncio
    async def test_process_returns_sensor_payload(self):
        """Test touch processing returns normalized sensor payload."""
        sensor_data = await self.sensor.process(
            pressure=0.5, duration=1.0, pattern="gesture"
        )

        assert sensor_data.raw_data.pressure == 0.5
        assert sensor_data.raw_data.duration == 1.0
        assert "valence" in sensor_data.metadata
        assert "arousal" in sensor_data.metadata
        assert "intensity" in sensor_data.metadata


class TestAudioSensor:
    """Test AudioSensor class."""

    def setup_method(self):
        """Setup for each test."""
        self.sensor = AudioSensor()

    @pytest.mark.asyncio
    async def test_process_calm_audio(self):
        """Test processing calm audio."""
        sensor_data = await self.sensor.process(
            pitch_mean=130.0,
            pitch_variance=10.0,
            speech_rate=100.0,
            volume_level=0.3,
            pause_ratio=0.4,
        )

        assert sensor_data.sensor_type == SensorType.AUDIO
        assert sensor_data.metadata["arousal"] <= 0.2
        assert sensor_data.metadata["intensity"] < 0.4

    @pytest.mark.asyncio
    async def test_process_energetic_audio(self):
        """Test processing energetic audio."""
        sensor_data = await self.sensor.process(
            pitch_mean=180.0,
            pitch_variance=30.0,
            speech_rate=220.0,
            volume_level=0.8,
            pause_ratio=0.05,
            emotion_markers=["joy"],
        )

        assert sensor_data.metadata["valence"] > 0.0
        assert sensor_data.metadata["arousal"] > 0.6
        assert sensor_data.metadata["intensity"] > 0.8

    @pytest.mark.asyncio
    async def test_analyze_speech_patterns(self):
        """Test speech-rate and pause analysis."""
        sensor_data = await self.sensor.process(
            pitch_mean=150.0,
            pitch_variance=20.0,
            speech_rate=210.0,
            volume_level=0.6,
            pause_ratio=0.05,
        )

        assert sensor_data.metadata["arousal"] > 0.6
        assert sensor_data.metadata["intensity"] > 0.6

    def test_audio_analysis_without_markers(self):
        """Test voice analysis remains bounded without emotion markers."""
        audio_data = AudioData(
            pitch_mean=150.0,
            pitch_variance=15.0,
            speech_rate=140.0,
            volume_level=0.5,
            pause_ratio=0.2,
            emotion_markers=[],
        )

        features = self.sensor._analyze_voice_emotions(audio_data)

        assert -1.0 <= features["valence"] <= 1.0
        assert 0.0 <= features["arousal"] <= 1.0
        assert 0.0 <= features["intensity"] <= 1.0


class TestSensorIntegration:
    """Test sensor integration scenarios."""

    @pytest.mark.asyncio
    async def test_multiple_sensor_processing(self):
        """Test processing data from multiple sensors."""
        text_sensor = TextSensor()
        touch_sensor = TouchSensor()
        audio_sensor = AudioSensor()

        text_data = await text_sensor.process("I'm feeling stressed")
        touch_data = await touch_sensor.process(
            pressure=0.7,
            duration=0.1,
            pattern="tap",
        )
        audio_data = await audio_sensor.process(
            pitch_mean=175.0,
            pitch_variance=25.0,
            speech_rate=200.0,
            volume_level=0.8,
            pause_ratio=0.05,
            emotion_markers=["stress"],
        )

        assert text_data.sensor_type == SensorType.TEXT
        assert touch_data.sensor_type == SensorType.TOUCH
        assert audio_data.sensor_type == SensorType.AUDIO
        assert text_data.metadata["arousal"] >= 0.0
        assert touch_data.metadata["intensity"] > 0.0
        assert audio_data.metadata["arousal"] > 0.6


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
