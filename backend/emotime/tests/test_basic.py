"""
🌿✨ Emotime Basic Tests — minimal smoke tests for core functionality
"""

import pytest
import asyncio
from datetime import datetime

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from emotime.core import EmotimeEngine
from emotime.fusion import EmotionalFeatures
from emotime.modes import ModeType, EmotionalModes
from emotime.sensors import SensorType, SensorData, TextData, TouchData, AudioData
from emotime.timeseries import EmotionalPoint


class TestEmotimeBasics:
    """Basic smoke tests for Emotime."""
    
    def test_imports(self):
        """Test that all modules import correctly."""
        # If we get here, imports worked
        assert True
    
    def test_engine_creation(self):
        """Test engine can be created."""
        engine = EmotimeEngine(
            user_id="test_user",
            enable_neo4j=False
        )
        assert engine.user_id == "test_user"
        assert not engine.is_running
    
    def test_emotional_features_creation(self):
        """Test emotional features creation."""
        features = EmotionalFeatures(
            valence=0.7,
            arousal=0.6,
            dominance=0.8,
            tempo=0.5,
            intensity=0.9,
            confidence=0.85
        )
        assert features.valence == 0.7
        assert features.arousal == 0.6
    
    def test_sensor_data_creation(self):
        """Test sensor data creation."""
        text_data = TextData(
            text="Hello world",
            word_count=2,
            char_count=11
        )
        
        sensor_data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=datetime.now(),
            raw_data=text_data,
            metadata={}
        )
        
        assert sensor_data.sensor_type == SensorType.TEXT
        assert sensor_data.raw_data.text == "Hello world"
    
    def test_emotional_point_creation(self):
        """Test emotional point creation."""
        point = EmotionalPoint(
            timestamp=datetime.now(),
            valence=0.8,
            arousal=0.6,
            dominance=0.7,
            tempo=0.5,
            intensity=0.9
        )
        
        assert 0.0 <= point.valence <= 1.0
        assert 0.0 <= point.arousal <= 1.0
    
    def test_emotional_modes_exists(self):
        """Test emotional modes system exists."""
        modes = EmotionalModes()
        assert modes is not None
        
        # Test mode definitions exist
        assert ModeType.CALM in modes.MODE_DEFINITIONS
        assert ModeType.JOY in modes.MODE_DEFINITIONS
        assert ModeType.STRESS in modes.MODE_DEFINITIONS
    
    @pytest.mark.asyncio
    async def test_engine_start_stop(self):
        """Test engine can start and stop."""
        engine = EmotimeEngine(
            user_id="test_user",
            enable_neo4j=False,
            update_interval=0.1
        )
        
        # Start
        await engine.start()
        assert engine.is_running
        
        # Stop
        await engine.stop()
        assert not engine.is_running
    
    def test_touch_data_creation(self):
        """Test touch data creation."""
        touch_data = TouchData(
            pressure=0.7,
            duration=2.0,
            frequency=3.0,
            pattern="tap"
        )
        
        assert touch_data.pressure == 0.7
        assert touch_data.duration == 2.0
        assert touch_data.pattern == "tap"
    
    def test_audio_data_creation(self):
        """Test audio data creation."""
        audio_data = AudioData(
            pitch_mean=0.6,
            pitch_variance=0.2,
            speech_rate=120.0,
            volume_level=0.8,
            pause_ratio=0.15,
            emotion_markers=["calm"]
        )
        
        assert audio_data.pitch_mean == 0.6
        assert audio_data.volume_level == 0.8
        assert "calm" in audio_data.emotion_markers
    
    def test_mode_types(self):
        """Test mode type enumeration."""
        assert ModeType.CALM.value == "calm"
        assert ModeType.JOY.value == "joy"
        assert ModeType.STRESS.value == "stress"
        assert ModeType.FOCUS.value == "focus"
        assert ModeType.CONTEMPLATION.value == "contemplation"
        assert ModeType.NEUTRAL.value == "neutral"


class TestEmotimeConfigurable:
    """Test configurable aspects of Emotime."""
    
    def test_engine_configuration(self):
        """Test engine with different configurations."""
        # Minimal config
        engine1 = EmotimeEngine()
        assert engine1.user_id == "default_user"
        
        # Custom config
        engine2 = EmotimeEngine(
            user_id="custom_user",
            session_id="custom_session",
            trace_window=50,
            update_interval=2.0,
            enable_neo4j=False
        )
        assert engine2.user_id == "custom_user"
        assert engine2.session_id == "custom_session"
        assert engine2.trace_window == 50
        assert engine2.update_interval == 2.0
    
    def test_serialization(self):
        """Test basic serialization."""
        engine = EmotimeEngine(enable_neo4j=False)
        result = engine.to_dict()
        
        assert "status" in result
        assert result["status"] == "no_data"  # Initially no data


if __name__ == "__main__":
    pytest.main([__file__, "-v"])