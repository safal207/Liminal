"""
🌿✨ Emotime Core Engine Tests — testing the heartbeat of emotional time
"""

import pytest
import asyncio
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, AsyncMock

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from emotime.core import EmotimeEngine, EmotimeState
from emotime.sensors import SensorData, SensorType
from emotime.fusion import EmotionalFeatures
from emotime.modes import EmotionalMode, ModeType


class TestEmotimeEngine:
    """Test suite for EmotimeEngine."""
    
    def setup_method(self):
        """Setup for each test method."""
        self.engine = EmotimeEngine(
            user_id="test_user",
            session_id="test_session",
            update_interval=0.1,  # Fast updates for testing
            enable_neo4j=False  # Disable Neo4j for unit tests
        )
    
    def teardown_method(self):
        """Cleanup after each test method."""
        if hasattr(self.engine, 'is_running'):
            self.engine.is_running = False
    
    def test_engine_initialization(self):
        """Test engine initialization."""
        assert self.engine.user_id == "test_user"
        assert self.engine.session_id == "test_session"
        assert self.engine.update_interval == 0.1
        assert not self.engine.is_running
        assert self.engine.current_state is None
        assert len(self.engine._sensor_buffer) == 0
    
    @pytest.mark.asyncio
    async def test_start_stop(self):
        """Test engine start and stop functionality."""
        # Start the engine
        await self.engine.start()
        assert self.engine.is_running
        
        # Stop the engine
        await self.engine.stop()
        assert not self.engine.is_running
    
    @pytest.mark.asyncio
    async def test_process_sensor_data(self):
        """Test sensor data processing."""
        # Create test sensor data
        from emotime.sensors import TextData
        sensor_data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=datetime.now(),
            raw_data=TextData(
                text="I feel great today!",
                word_count=4,
                char_count=20
            ),
            metadata={}
        )
        
        # Process the data
        await self.engine.process_sensor_data(sensor_data)
        
        # Check that data was added to buffer
        assert len(self.engine._sensor_buffer) == 1
        assert self.engine._sensor_buffer[0].data["text"] == "I feel great today!"
    
    @pytest.mark.asyncio 
    async def test_get_current_state(self):
        """Test getting current emotional state."""
        # Initially should be None
        state = await self.engine.get_current_state()
        assert state is None
        
        # Create a mock state
        features = EmotionalFeatures(
            valence=0.8, arousal=0.6, dominance=0.7,
            tempo=0.5, intensity=0.9, confidence=0.8
        )
        mode = EmotionalMode(
            name="Joy", type=ModeType.JOY,
            intensity=0.9, confidence=0.8, duration=10,
            description="State of happiness and inspiration"
        )
        
        self.engine.current_state = EmotimeState(
            timestamp=datetime.now(),
            features=features,
            mode=mode,
            resonance_trace=[],
            confidence=0.8
        )
        
        # Check state retrieval
        state = await self.engine.get_current_state()
        assert state is not None
        assert state.mode.name == "Joy"
        assert state.features.valence == 0.8
    
    @pytest.mark.asyncio
    async def test_get_resonance_trace(self):
        """Test resonance trace retrieval."""
        # Initially should be empty
        trace = await self.engine.get_resonance_trace()
        assert len(trace) == 0
        
        # Mock a state with resonance trace
        from emotime.timeseries import EmotionalPoint
        points = [
            EmotionalPoint(
                timestamp=datetime.now() - timedelta(seconds=i),
                valence=0.5 + i*0.1,
                arousal=0.5,
                dominance=0.5,
                tempo=0.5,
                intensity=0.5
            ) for i in range(5)
        ]
        
        features = EmotionalFeatures(
            valence=0.8, arousal=0.6, dominance=0.7,
            tempo=0.5, intensity=0.9, confidence=0.8
        )
        mode = EmotionalMode(
            name="Calm", type=ModeType.CALM,
            intensity=0.7, confidence=0.9, duration=15,
            description="State of peace and balance"
        )
        
        self.engine.current_state = EmotimeState(
            timestamp=datetime.now(),
            features=features,
            mode=mode,
            resonance_trace=points,
            confidence=0.9
        )
        
        # Test full trace
        trace = await self.engine.get_resonance_trace()
        assert len(trace) == 5
        
        # Test limited trace
        trace_limited = await self.engine.get_resonance_trace(limit=3)
        assert len(trace_limited) == 3
    
    @pytest.mark.asyncio
    async def test_heartbeat_processing(self):
        """Test heartbeat processing with mocked components."""
        # Mock the fusion component
        mock_features = EmotionalFeatures(
            valence=0.7, arousal=0.5, dominance=0.6,
            tempo=0.4, intensity=0.8, confidence=0.9
        )
        
        with patch.object(self.engine.fusion, 'process_batch', return_value=mock_features) as mock_fusion:
            with patch.object(self.engine.modes, 'classify_mode') as mock_classify:
                mock_mode = EmotionalMode(
                    name="Focus", type=ModeType.FOCUS,
                    intensity=0.8, confidence=0.9, duration=20,
                    description="State of deep concentration"
                )
                mock_classify.return_value = mock_mode
                
                # Add sensor data to buffer
                from emotime.sensors import TextData
                sensor_data = SensorData(
                    sensor_type=SensorType.TEXT,
                    timestamp=datetime.now(),
                    raw_data=TextData(
                        text="I'm concentrating hard",
                        word_count=3,
                        char_count=22
                    ),
                    metadata={}
                )
                await self.engine.process_sensor_data(sensor_data)
                
                # Process heartbeat
                await self.engine._process_heartbeat()
                
                # Check that fusion was called
                mock_fusion.assert_called_once()
                
                # Check that mode classification was called
                mock_classify.assert_called_once()
                
                # Check that state was updated
                assert self.engine.current_state is not None
                assert self.engine.current_state.mode.name == "Focus"
                assert self.engine.current_state.features.valence == 0.7
    
    @pytest.mark.asyncio
    async def test_confidence_calculation(self):
        """Test confidence calculation."""
        from emotime.timeseries import EmotionalPoint
        
        # Create test points
        points = [
            EmotionalPoint(
                timestamp=datetime.now() - timedelta(seconds=i),
                valence=0.8,  # Consistent high valence
                arousal=0.6,
                dominance=0.7,
                tempo=0.5,
                intensity=0.9
            ) for i in range(10)
        ]
        
        # Mock mode
        mode = EmotionalMode(
            name="Joy", type=ModeType.JOY,
            intensity=0.9, confidence=0.8, duration=15,
            description="State of happiness and inspiration"
        )
        
        # Mock classify_mode to return consistent results
        with patch.object(self.engine.modes, 'classify_mode', return_value=mode):
            confidence = await self.engine._calculate_confidence(mode, points)
            
            # Should be high confidence due to consistency
            assert confidence > 0.8
    
    def test_to_dict(self):
        """Test serialization to dictionary."""
        # Test with no current state
        result = self.engine.to_dict()
        assert result["status"] == "no_data"
        
        # Test with current state
        features = EmotionalFeatures(
            valence=0.6, arousal=0.7, dominance=0.5,
            tempo=0.8, intensity=0.9, confidence=0.85
        )
        mode = EmotionalMode(
            name="Stress", type=ModeType.STRESS,
            intensity=0.8, confidence=0.75, duration=30,
            description="State of stress and anxiety"
        )
        
        self.engine.current_state = EmotimeState(
            timestamp=datetime.now(),
            features=features,
            mode=mode,
            resonance_trace=[],
            confidence=0.8
        )
        
        result = self.engine.to_dict()
        assert result["status"] == "active"
        assert result["user_id"] == "test_user"
        assert result["mode"]["name"] == "Stress"
        assert result["features"]["valence"] == 0.6
        assert result["confidence"] == 0.8
    
    @pytest.mark.asyncio
    async def test_emotional_insights(self):
        """Test emotional insights generation."""
        # Mock various methods
        with patch.object(self.engine.timeseries, 'to_dict', return_value={"points": 10}):
            with patch.object(self.engine.modes, 'get_mode_statistics', return_value={"calm": 0.3}):
                with patch.object(self.engine.modes, 'get_mode_insights', return_value={"dominant_mode": "calm"}):
                    with patch.object(self.engine.fusion, 'get_feature_statistics', return_value={"avg_valence": 0.6}):
                        
                        insights = await self.engine.get_emotional_insights()
                        
                        assert "current_state" in insights
                        assert "timeseries_analysis" in insights
                        assert "mode_statistics" in insights
                        assert "mode_insights" in insights
                        assert "fusion_statistics" in insights


if __name__ == "__main__":
    pytest.main([__file__, "-v"])