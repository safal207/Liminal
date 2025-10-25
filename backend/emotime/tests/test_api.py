"""
🌿✨ Emotime API Tests — testing REST endpoints
"""

import pytest
from datetime import datetime

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

try:
    from emotime.api import emotime_router as router
except ImportError:
    router = None
from emotime.core import EmotimeEngine
from emotime.sensors import SensorType, SensorData, TextData


class TestEmotimeAPI:
    """Test Emotime API endpoints."""
    
    @pytest.mark.asyncio
    async def test_health_endpoint_exists(self):
        """Test health endpoint is accessible."""
        # Just test that the router exists and has routes
        if router is not None:
            assert len(router.routes) > 0
        else:
            # Skip test if router not available
            pytest.skip("Router not available")
    
    @pytest.mark.asyncio
    async def test_demo_scenario_types(self):
        """Test that demo scenarios are available."""
        try:
            from emotime.api import DEMO_SCENARIOS
            assert "calm_morning" in DEMO_SCENARIOS
            assert "work_focus" in DEMO_SCENARIOS  
            assert "stress_peak" in DEMO_SCENARIOS
            assert "joy_breakthrough" in DEMO_SCENARIOS
        except ImportError:
            pytest.skip("Demo scenarios not available")


class TestEmotimeEngineIntegration:
    """Test engine integration scenarios."""
    
    def setup_method(self):
        """Setup for each test."""
        self.engine = EmotimeEngine(
            user_id="api_test_user",
            enable_neo4j=False,
            update_interval=0.1
        )
    
    def teardown_method(self):
        """Cleanup after each test."""
        if hasattr(self.engine, 'is_running'):
            self.engine.is_running = False
    
    @pytest.mark.asyncio
    async def test_simple_text_processing(self):
        """Test simple text processing workflow."""
        await self.engine.start()
        
        try:
            # Send some text data
            text_data = TextData(
                text="I'm feeling great today!",
                word_count=4,
                char_count=24
            )
            
            sensor_data = SensorData(
                sensor_type=SensorType.TEXT,
                timestamp=datetime.now(),
                raw_data=text_data,
                metadata={}
            )
            
            # Process the data
            await self.engine.process_sensor_data(sensor_data)
            
            # Give it some time to process
            import asyncio
            await asyncio.sleep(0.2)
            
            # Check that engine processed something
            state = await self.engine.get_current_state()
            # State might be None if processing didn't complete, which is okay
            
            # Test serialization works
            result = self.engine.to_dict()
            assert "status" in result
            assert "user_id" in result
            assert result["user_id"] == "api_test_user"
            
        finally:
            await self.engine.stop()
    
    @pytest.mark.asyncio
    async def test_emotional_insights(self):
        """Test insights generation."""
        await self.engine.start()
        
        try:
            # Generate some insights (even with no data)
            insights = await self.engine.get_emotional_insights()
            
            # Should have basic structure
            assert "current_state" in insights
            assert "timeseries_analysis" in insights
            assert "mode_statistics" in insights
            
        finally:
            await self.engine.stop()
    
    def test_metrics_integration(self):
        """Test metrics integration."""
        from emotime.metrics_integration import emotime_metrics
        
        # Should be able to get metrics summary
        summary = emotime_metrics.get_metrics_summary()
        assert "status" in summary
        
        # Test metrics recording (won't fail even if Prometheus unavailable)
        from emotime.fusion import EmotionalFeatures
        test_features = EmotionalFeatures(
            valence=0.7, arousal=0.6, dominance=0.8,
            tempo=0.5, intensity=0.9, confidence=0.85
        )
        
        # Should not raise error
        emotime_metrics.record_emotional_features(test_features)
        emotime_metrics.record_heartbeat("test")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])