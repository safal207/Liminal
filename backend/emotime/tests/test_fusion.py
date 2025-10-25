"""
🌿✨ Emotime Feature Fusion Tests — testing multi-sensor data fusion
"""

import pytest
from datetime import datetime, timedelta
from unittest.mock import patch, MagicMock

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from emotime.fusion import FeatureFusion, EmotionalFeatures
from emotime.sensors import SensorData, SensorType


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
            confidence=0.85
        )
        
        assert features.valence == 0.7
        assert features.arousal == 0.8
        assert features.dominance == 0.6
        assert features.tempo == 0.5
        assert features.intensity == 0.9
        assert features.confidence == 0.85
    
    def test_features_validation(self):
        """Test features are in valid range."""
        features = EmotionalFeatures(
            valence=1.2,  # Will be clamped
            arousal=-0.1,  # Will be clamped
            dominance=0.5,
            tempo=0.5,
            intensity=0.5
        )
        
        # Values should be clamped to [0, 1]
        assert 0.0 <= features.valence <= 1.0
        assert 0.0 <= features.arousal <= 1.0


class TestFeatureFusion:
    """Test FeatureFusion class."""
    
    def setup_method(self):
        """Setup for each test."""
        self.fusion = FeatureFusion(
            smoothing_factor=0.3,  # More responsive for testing
            confidence_threshold=0.1
        )
    
    @pytest.mark.asyncio
    async def test_process_single_sensor(self):
        """Test processing data from a single sensor."""
        sensor_data = [
            SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "I feel wonderful today!"},
                timestamp=datetime.now()
            )
        ]
        
        # Mock the sensor analysis
        with patch('emotime.sensors.TextSensor.analyze') as mock_analyze:
            mock_analyze.return_value = EmotionalFeatures(
                valence=0.8, arousal=0.6, dominance=0.7,
                tempo=0.5, intensity=0.8, confidence=0.9
            )
            
            result = await self.fusion.process_batch(sensor_data)
            
            assert result is not None
            assert result.valence == 0.8
            assert result.confidence == 0.9
    
    @pytest.mark.asyncio
    async def test_process_multiple_sensors(self):
        """Test processing data from multiple sensors."""
        sensor_data = [
            SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "I'm excited!"},
                timestamp=datetime.now()
            ),
            SensorData(
                sensor_type=SensorType.TOUCH,
                data={"pressure": 0.6, "duration": 1.0},
                timestamp=datetime.now()
            ),
            SensorData(
                sensor_type=SensorType.AUDIO,
                data={"volume": 0.7, "tempo": 0.8},
                timestamp=datetime.now()
            )
        ]
        
        # Mock sensor analyses
        with patch('emotime.sensors.TextSensor.analyze') as mock_text:
            with patch('emotime.sensors.TouchSensor.analyze') as mock_touch:
                with patch('emotime.sensors.AudioSensor.analyze') as mock_audio:
                    
                    mock_text.return_value = EmotionalFeatures(
                        valence=0.9, arousal=0.8, dominance=0.7,
                        tempo=0.6, intensity=0.9, confidence=0.8
                    )
                    mock_touch.return_value = EmotionalFeatures(
                        valence=0.7, arousal=0.6, dominance=0.8,
                        tempo=0.5, intensity=0.7, confidence=0.7
                    )
                    mock_audio.return_value = EmotionalFeatures(
                        valence=0.8, arousal=0.9, dominance=0.6,
                        tempo=0.8, intensity=0.8, confidence=0.9
                    )
                    
                    result = await self.fusion.process_batch(sensor_data)
                    
                    assert result is not None
                    # Result should be a weighted combination
                    assert 0.7 <= result.valence <= 0.9
                    assert 0.6 <= result.arousal <= 0.9
                    assert result.confidence > 0.0
    
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
            SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "Happy"},
                timestamp=datetime.now()
            )
        ]
        
        # Second batch with different values
        sensor_data2 = [
            SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "Sad"},
                timestamp=datetime.now() + timedelta(seconds=1)
            )
        ]
        
        with patch('emotime.sensors.TextSensor.analyze') as mock_analyze:
            # First analysis - high valence
            mock_analyze.return_value = EmotionalFeatures(
                valence=0.9, arousal=0.5, dominance=0.5,
                tempo=0.5, intensity=0.5, confidence=0.8
            )
            
            result1 = await self.fusion.process_batch(sensor_data1)
            assert result1.valence == 0.9
            
            # Second analysis - low valence
            mock_analyze.return_value = EmotionalFeatures(
                valence=0.1, arousal=0.5, dominance=0.5,
                tempo=0.5, intensity=0.5, confidence=0.8
            )
            
            result2 = await self.fusion.process_batch(sensor_data2)
            
            # Result should be smoothed (not exactly 0.1)
            assert 0.1 < result2.valence < 0.9
    
    def test_calculate_sensor_weights(self):
        """Test sensor weight calculation."""
        features_list = [
            EmotionalFeatures(
                valence=0.8, arousal=0.6, dominance=0.7,
                tempo=0.5, intensity=0.8, confidence=0.9  # High confidence
            ),
            EmotionalFeatures(
                valence=0.5, arousal=0.5, dominance=0.5,
                tempo=0.5, intensity=0.5, confidence=0.3  # Low confidence
            ),
            EmotionalFeatures(
                valence=0.7, arousal=0.7, dominance=0.6,
                tempo=0.6, intensity=0.7, confidence=0.8  # High confidence
            )
        ]
        
        weights = self.fusion._calculate_sensor_weights(features_list)
        
        assert len(weights) == 3
        assert sum(weights) == pytest.approx(1.0, rel=1e-6)
        
        # Higher confidence features should have higher weights
        assert weights[0] > weights[1]  # High confidence > low confidence
        assert weights[2] > weights[1]  # High confidence > low confidence
    
    def test_weighted_fusion(self):
        """Test weighted fusion of features."""
        features_list = [
            EmotionalFeatures(
                valence=1.0, arousal=0.0, dominance=0.5,
                tempo=0.5, intensity=0.5, confidence=0.8
            ),
            EmotionalFeatures(
                valence=0.0, arousal=1.0, dominance=0.5,
                tempo=0.5, intensity=0.5, confidence=0.8
            )
        ]
        weights = [0.6, 0.4]  # First feature weighted more
        
        result = self.fusion._weighted_fusion(features_list, weights)
        
        # Should be weighted average
        expected_valence = 1.0 * 0.6 + 0.0 * 0.4
        expected_arousal = 0.0 * 0.6 + 1.0 * 0.4
        
        assert result.valence == pytest.approx(expected_valence)
        assert result.arousal == pytest.approx(expected_arousal)
    
    def test_apply_ewma_smoothing(self):
        """Test EWMA smoothing application."""
        # Set up previous features
        self.fusion.previous_features = EmotionalFeatures(
            valence=0.5, arousal=0.5, dominance=0.5,
            tempo=0.5, intensity=0.5, confidence=0.8
        )
        
        # New features
        new_features = EmotionalFeatures(
            valence=1.0, arousal=1.0, dominance=1.0,
            tempo=1.0, intensity=1.0, confidence=0.9
        )
        
        # Apply smoothing with alpha=0.3
        smoothed = self.fusion._apply_ewma_smoothing(new_features)
        
        # Should be: old_value * (1 - alpha) + new_value * alpha
        expected_valence = 0.5 * 0.7 + 1.0 * 0.3
        assert smoothed.valence == pytest.approx(expected_valence)
    
    def test_feature_statistics(self):
        """Test feature statistics collection."""
        # Process some batches to build statistics
        features1 = EmotionalFeatures(
            valence=0.8, arousal=0.6, dominance=0.7,
            tempo=0.5, intensity=0.8, confidence=0.9
        )
        features2 = EmotionalFeatures(
            valence=0.6, arousal=0.8, dominance=0.5,
            tempo=0.7, intensity=0.6, confidence=0.7
        )
        
        # Add to history
        self.fusion.feature_history.extend([features1, features2])
        
        stats = self.fusion.get_feature_statistics()
        
        assert "total_fusions" in stats
        assert "avg_confidence" in stats
        assert "feature_ranges" in stats
        
        # Check averages
        expected_avg_valence = (0.8 + 0.6) / 2
        assert stats["feature_ranges"]["valence"]["avg"] == pytest.approx(expected_avg_valence)
    
    @pytest.mark.asyncio
    async def test_low_confidence_filtering(self):
        """Test filtering of low confidence results."""
        # Create fusion with high confidence threshold
        fusion = FeatureFusion(confidence_threshold=0.8)
        
        sensor_data = [
            SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "unclear..."},
                timestamp=datetime.now()
            )
        ]
        
        with patch('emotime.sensors.TextSensor.analyze') as mock_analyze:
            # Return low confidence result
            mock_analyze.return_value = EmotionalFeatures(
                valence=0.5, arousal=0.5, dominance=0.5,
                tempo=0.5, intensity=0.5, confidence=0.3  # Below threshold
            )
            
            result = await fusion.process_batch(sensor_data)
            
            # Should return None due to low confidence
            assert result is None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])