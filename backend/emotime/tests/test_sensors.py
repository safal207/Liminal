"""
🌿✨ Emotime Sensors Tests — testing the sensory layer
"""

import pytest
from datetime import datetime
from unittest.mock import patch, MagicMock

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from emotime.sensors import (
    SensorData, SensorType, 
    TextSensor, TouchSensor, AudioSensor
)


class TestSensorData:
    """Test SensorData class."""
    
    def test_sensor_data_creation(self):
        """Test SensorData creation."""
        from emotime.sensors import TextData
        timestamp = datetime.now()
        text_data = TextData(text="Hello world", word_count=2, char_count=11)
        data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=timestamp,
            raw_data=text_data,
            metadata={}
        )
        
        assert data.sensor_type == SensorType.TEXT
        assert data.raw_data.text == "Hello world"
        assert data.timestamp == timestamp
    
    def test_sensor_data_with_metadata(self):
        """Test SensorData with metadata."""
        from emotime.sensors import AudioData
        audio_data = AudioData(volume=0.8, pitch=0.6, tempo=0.7, duration=30.0)
        data = SensorData(
            sensor_type=SensorType.AUDIO,
            timestamp=datetime.now(),
            raw_data=audio_data,
            metadata={"quality": "high", "duration": 30}
        )
        
        assert data.metadata["quality"] == "high"
        assert data.metadata["duration"] == 30


class TestTextSensor:
    """Test TextSensor class."""
    
    def setup_method(self):
        """Setup for each test."""
        self.sensor = TextSensor()
    
    @pytest.mark.asyncio
    async def test_analyze_positive_text(self):
        """Test analysis of positive text."""
        from emotime.sensors import TextData
        text_data = TextData(
            text="I feel amazing and happy today!",
            word_count=6,
            char_count=32
        )
        sensor_data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=datetime.now(),
            raw_data=text_data,
            metadata={}
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Positive text should have high valence
        assert features.valence > 0.5
        assert features.confidence > 0.0
    
    @pytest.mark.asyncio
    async def test_analyze_negative_text(self):
        """Test analysis of negative text."""
        from emotime.sensors import TextData
        text_data = TextData(
            text="I feel terrible and sad",
            word_count=5,
            char_count=24
        )
        sensor_data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=datetime.now(),
            raw_data=text_data,
            metadata={}
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Negative text should have low valence
        assert features.valence < 0.5
        assert features.confidence > 0.0
    
    @pytest.mark.asyncio
    async def test_analyze_neutral_text(self):
        """Test analysis of neutral text."""
        from emotime.sensors import TextData
        text_data = TextData(
            text="The weather is okay today.",
            word_count=5,
            char_count=26
        )
        sensor_data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=datetime.now(),
            raw_data=text_data,
            metadata={}
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Neutral text should have moderate valence
        assert 0.4 <= features.valence <= 0.6
        assert features.arousal >= 0.0
        assert features.dominance >= 0.0
    
    @pytest.mark.asyncio
    async def test_analyze_empty_text(self):
        """Test analysis of empty text."""
        from emotime.sensors import TextData
        text_data = TextData(
            text="",
            word_count=0,
            char_count=0
        )
        sensor_data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=datetime.now(),
            raw_data=text_data,
            metadata={}
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Empty text should have neutral values
        assert features.valence == 0.5
        assert features.arousal == 0.5
        assert features.confidence == 0.0
    
    @pytest.mark.asyncio 
    async def test_analyze_high_arousal_text(self):
        """Test analysis of high arousal text."""
        sensor_data = SensorData(
            sensor_type=SensorType.TEXT,
            data={"text": "URGENT! EMERGENCY! HELP NOW!!!"},
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Urgent text should have high arousal
        assert features.arousal > 0.7
        assert features.intensity > 0.6


class TestTouchSensor:
    """Test TouchSensor class."""
    
    def setup_method(self):
        """Setup for each test."""
        self.sensor = TouchSensor()
    
    @pytest.mark.asyncio
    async def test_analyze_gentle_touch(self):
        """Test analysis of gentle touch."""
        sensor_data = SensorData(
            sensor_type=SensorType.TOUCH,
            data={
                "pressure": 0.3,
                "duration": 2.0,
                "pattern": "continuous"
            },
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Gentle touch should be calming
        assert features.valence > 0.5
        assert features.arousal < 0.5
        assert features.tempo < 0.5
    
    @pytest.mark.asyncio
    async def test_analyze_strong_touch(self):
        """Test analysis of strong touch."""
        sensor_data = SensorData(
            sensor_type=SensorType.TOUCH,
            data={
                "pressure": 0.9,
                "duration": 0.5,
                "pattern": "sharp"
            },
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Strong touch should be stimulating
        assert features.arousal > 0.6
        assert features.intensity > 0.7
        assert features.tempo > 0.5
    
    @pytest.mark.asyncio
    async def test_analyze_rapid_touches(self):
        """Test analysis of rapid touch pattern."""
        sensor_data = SensorData(
            sensor_type=SensorType.TOUCH,
            data={
                "pressure": 0.6,
                "duration": 0.2,
                "pattern": "rapid_taps",
                "frequency": 5.0  # 5 Hz
            },
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Rapid touches should have high tempo
        assert features.tempo > 0.7
        assert features.arousal > 0.5
    
    @pytest.mark.asyncio
    async def test_analyze_missing_touch_data(self):
        """Test analysis with missing touch data."""
        sensor_data = SensorData(
            sensor_type=SensorType.TOUCH,
            data={},  # Empty data
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Should return neutral values
        assert features.valence == 0.5
        assert features.arousal == 0.5
        assert features.confidence == 0.0


class TestAudioSensor:
    """Test AudioSensor class."""
    
    def setup_method(self):
        """Setup for each test."""
        self.sensor = AudioSensor()
    
    @pytest.mark.asyncio
    async def test_analyze_calm_audio(self):
        """Test analysis of calm audio."""
        sensor_data = SensorData(
            sensor_type=SensorType.AUDIO,
            data={
                "volume": 0.3,
                "pitch": 0.4,
                "tempo": 0.2,
                "duration": 5.0
            },
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Calm audio should be soothing
        assert features.valence > 0.4
        assert features.arousal < 0.6
        assert features.tempo < 0.5
    
    @pytest.mark.asyncio
    async def test_analyze_energetic_audio(self):
        """Test analysis of energetic audio."""
        sensor_data = SensorData(
            sensor_type=SensorType.AUDIO,
            data={
                "volume": 0.8,
                "pitch": 0.7,
                "tempo": 0.9,
                "duration": 3.0,
                "beats_per_minute": 140
            },
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Energetic audio should be stimulating
        assert features.arousal > 0.6
        assert features.tempo > 0.7
        assert features.intensity > 0.6
    
    @pytest.mark.asyncio
    async def test_analyze_speech_patterns(self):
        """Test analysis of speech patterns."""
        sensor_data = SensorData(
            sensor_type=SensorType.AUDIO,
            data={
                "volume": 0.6,
                "pitch": 0.5,
                "speech_rate": 0.8,  # Fast speech
                "pauses": 0.2,       # Few pauses
                "duration": 10.0
            },
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Fast speech should indicate high arousal
        assert features.arousal > 0.6
        assert features.tempo > 0.6
    
    @pytest.mark.asyncio
    async def test_analyze_missing_audio_data(self):
        """Test analysis with missing audio data."""
        sensor_data = SensorData(
            sensor_type=SensorType.AUDIO,
            data={"volume": 0.5},  # Only partial data
            timestamp=datetime.now()
        )
        
        features = await self.sensor.analyze(sensor_data)
        
        # Should still provide reasonable analysis
        assert 0.0 <= features.valence <= 1.0
        assert 0.0 <= features.arousal <= 1.0
        assert features.confidence >= 0.0


class TestSensorIntegration:
    """Test sensor integration scenarios."""
    
    @pytest.mark.asyncio
    async def test_multiple_sensor_analysis(self):
        """Test analyzing data from multiple sensors."""
        text_sensor = TextSensor()
        touch_sensor = TouchSensor()
        audio_sensor = AudioSensor()
        
        # Text data
        text_data = SensorData(
            sensor_type=SensorType.TEXT,
            data={"text": "I'm feeling stressed"},
            timestamp=datetime.now()
        )
        
        # Touch data (rapid tapping)
        touch_data = SensorData(
            sensor_type=SensorType.TOUCH,
            data={
                "pressure": 0.7,
                "duration": 0.1,
                "pattern": "rapid_taps"
            },
            timestamp=datetime.now()
        )
        
        # Audio data (high volume, fast tempo)
        audio_data = SensorData(
            sensor_type=SensorType.AUDIO,
            data={
                "volume": 0.8,
                "tempo": 0.9,
                "pitch": 0.7
            },
            timestamp=datetime.now()
        )
        
        # Analyze each
        text_features = await text_sensor.analyze(text_data)
        touch_features = await touch_sensor.analyze(touch_data)
        audio_features = await audio_sensor.analyze(audio_data)
        
        # All should indicate high arousal/stress
        assert text_features.arousal > 0.5
        assert touch_features.arousal > 0.5
        assert audio_features.arousal > 0.5


if __name__ == "__main__":
    pytest.main([__file__, "-v"])