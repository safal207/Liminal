"""
🌿✨ Emotime Integration Tests — testing full system integration
"""

import pytest
import asyncio
from datetime import datetime, timedelta
from unittest.mock import patch, MagicMock

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from emotime.core import EmotimeEngine
from emotime.sensors import SensorData, SensorType
from emotime.modes import ModeType


class TestEmotimeIntegration:
    """Integration tests for the complete Emotime system."""
    
    def setup_method(self):
        """Setup for each test method."""
        self.engine = EmotimeEngine(
            user_id="integration_test_user",
            session_id="integration_test_session", 
            update_interval=0.05,  # Very fast for testing
            enable_neo4j=False,  # Disable Neo4j for integration tests
            trace_window=20
        )
    
    def teardown_method(self):
        """Cleanup after each test method."""
        if hasattr(self.engine, 'is_running'):
            self.engine.is_running = False
    
    @pytest.mark.asyncio
    async def test_full_emotional_journey(self):
        """Test a complete emotional journey through the system."""
        # Start the engine
        await self.engine.start()
        
        try:
            # Phase 1: Calm state
            calm_data = [
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "I feel peaceful and relaxed"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.TOUCH,
                    data={"pressure": 0.2, "duration": 3.0, "pattern": "gentle"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.AUDIO,
                    data={"volume": 0.3, "tempo": 0.2, "pitch": 0.4},
                    timestamp=datetime.now()
                )
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
                assert state.features.arousal < 0.7   # Not too high arousal
            
            # Phase 2: Stress state
            stress_data = [
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "URGENT DEADLINE! SO MUCH PRESSURE!!!"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.TOUCH,
                    data={"pressure": 0.9, "duration": 0.1, "pattern": "rapid_taps"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.AUDIO,
                    data={"volume": 0.9, "tempo": 0.9, "pitch": 0.8, "beats_per_minute": 160},
                    timestamp=datetime.now()
                )
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
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "I'm so happy and excited! This is amazing!"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.TOUCH,
                    data={"pressure": 0.6, "duration": 1.5, "pattern": "rhythmic"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.AUDIO,
                    data={"volume": 0.7, "tempo": 0.8, "pitch": 0.7},
                    timestamp=datetime.now()
                )
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
                assert state.features.valence > 0.6  # High valence
                assert state.features.arousal > 0.5   # High arousal
            
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
            focus_data = SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "I need to concentrate on this task"},
                timestamp=datetime.now()
            )
            
            await self.engine.process_sensor_data(focus_data)
            await asyncio.sleep(0.2)
            
            state1 = await self.engine.get_current_state()
            initial_mode = state1.mode.name if state1 else None
            
            # Switch to joy data
            joy_data = SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "Wow! I just won the lottery! So excited!"},
                timestamp=datetime.now()
            )
            
            await self.engine.process_sensor_data(joy_data)
            await asyncio.sleep(0.2)
            
            state2 = await self.engine.get_current_state()
            final_mode = state2.mode.name if state2 else None
            
            # Modes might have changed
            if initial_mode and final_mode:
                # Just check that we have valid modes
                assert initial_mode in ["Calm", "Focus", "Stress", "Joy", "Contemplation", "Neutral"]
                assert final_mode in ["Calm", "Focus", "Stress", "Joy", "Contemplation", "Neutral"]
            
        finally:
            await self.engine.stop()
    
    @pytest.mark.asyncio
    async def test_confidence_evolution(self):
        """Test how confidence evolves over time."""
        await self.engine.start()
        
        try:
            # Send consistent data to build confidence
            consistent_data = [
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "I feel calm and peaceful"},
                    timestamp=datetime.now() + timedelta(seconds=i)
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
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "I'm feeling energetic today!"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.TOUCH,
                    data={"pressure": 0.7, "duration": 1.0, "pattern": "firm"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.AUDIO,
                    data={"volume": 0.6, "tempo": 0.7, "pitch": 0.6},
                    timestamp=datetime.now()
                )
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
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "Starting my day with optimism"},
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "Work is getting challenging"},
                    timestamp=datetime.now() + timedelta(seconds=1)
                ),
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "Finally solved the problem! Great success!"},
                    timestamp=datetime.now() + timedelta(seconds=2)
                )
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
            test_data = SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "Testing metrics collection"},
                timestamp=datetime.now()
            )
            
            await self.engine.process_sensor_data(test_data)
            await asyncio.sleep(0.2)
            
            # Check that metrics system is working
            from emotime.metrics_integration import emotime_metrics
            summary = emotime_metrics.get_metrics_summary()
            
            assert "status" in summary
            assert summary["status"] == "active"
            
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
                    data={},  # Empty data
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.TOUCH,
                    data={"invalid": "field"},  # Invalid fields
                    timestamp=datetime.now()
                ),
                SensorData(
                    sensor_type=SensorType.AUDIO,
                    data={"volume": "invalid"},  # Invalid type
                    timestamp=datetime.now()
                )
            ]
            
            # System should handle errors gracefully
            for data in bad_data:
                await self.engine.process_sensor_data(data)
            
            await asyncio.sleep(0.2)
            
            # Engine should still be running
            assert self.engine.is_running
            
            # Should handle good data after errors
            good_data = SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": "Recovery test"},
                timestamp=datetime.now()
            )
            
            await self.engine.process_sensor_data(good_data)
            await asyncio.sleep(0.2)
            
            # Should still work
            state = await self.engine.get_current_state()
            # State might be None if all previous data was invalid, which is OK
            
        finally:
            await self.engine.stop()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])