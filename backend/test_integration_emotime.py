"""
LIMINAL Integration Tests - Emotime System
Tests full integration between Emotime, WebSocket, ML, and metrics systems.
"""

import asyncio
import json
import requests
import time
from datetime import datetime, timedelta
from unittest.mock import AsyncMock, MagicMock
import sys
import os

# Add emotime to path
sys.path.append(os.path.join(os.path.dirname(__file__), 'emotime'))

from emotime.core import EmotimeEngine
from emotime.sensors import SensorData, SensorType
from emotime.fusion import EmotionalFeatures
from emotime.modes import EmotionalMode, ModeType


class IntegrationTestSuite:
    """Complete integration test suite for LIMINAL."""
    
    def __init__(self):
        self.base_url = "http://localhost:8000"
        self.test_results = {
            "emotime_api": False,
            "emotime_engine": False,
            "metrics_integration": False,
            "websocket_integration": False,
            "ml_integration": False
        }
    
    async def run_all_tests(self):
        """Run complete integration test suite."""
        print("LIMINAL INTEGRATION TESTS - EMOTIME SYSTEM")
        print("=" * 60)
        print(f"Start time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print()
        
        # Test 1: Emotime API endpoints
        await self.test_emotime_api()
        
        # Test 2: Emotime engine functionality
        await self.test_emotime_engine()
        
        # Test 3: Metrics integration
        await self.test_metrics_integration()
        
        # Test 4: WebSocket integration (if available)
        await self.test_websocket_integration()
        
        # Test 5: ML system integration
        await self.test_ml_integration()
        
        # Summary
        self.print_summary()
        
        return all(self.test_results.values())
    
    async def test_emotime_api(self):
        """Test Emotime API endpoints."""
        print("1. EMOTIME API ENDPOINTS")
        print("-" * 40)
        
        try:
            # Test status endpoint
            response = requests.get(f"{self.base_url}/emotime/status", timeout=5)
            if response.status_code == 200:
                print("   + Status endpoint: OK")
            else:
                print(f"   - Status endpoint: ERROR {response.status_code}")
                return
            
            # Test text processing endpoint
            text_payload = {
                "text": "I feel absolutely wonderful and energetic today!",
                "user_id": "test_user",
                "session_id": "test_session"
            }
            
            response = requests.post(
                f"{self.base_url}/emotime/text", 
                json=text_payload, 
                timeout=10
            )
            
            if response.status_code == 200:
                data = response.json()
                print("   ✓ Text processing: OK")
                print(f"     - Detected mood: {data.get('emotional_features', {}).get('valence', 'unknown')}")
                print(f"     - Confidence: {data.get('confidence', 'unknown')}")
            else:
                print(f"   ✗ Text processing: ERROR {response.status_code}")
                return
            
            # Test touch processing endpoint
            touch_payload = {
                "pressure": 0.7,
                "duration": 2.5,
                "pattern": "gentle",
                "user_id": "test_user",
                "session_id": "test_session"
            }
            
            response = requests.post(
                f"{self.base_url}/emotime/touch",
                json=touch_payload,
                timeout=10
            )
            
            if response.status_code == 200:
                print("   ✓ Touch processing: OK")
            else:
                print(f"   ✗ Touch processing: ERROR {response.status_code}")
                return
            
            # Test insights endpoint
            response = requests.get(
                f"{self.base_url}/emotime/insights?user_id=test_user&session_id=test_session",
                timeout=10
            )
            
            if response.status_code == 200:
                insights = response.json()
                print("   ✓ Insights endpoint: OK")
                print(f"     - Current state: {insights.get('current_state', {}).get('status', 'unknown')}")
            else:
                print(f"   ✗ Insights endpoint: ERROR {response.status_code}")
                return
            
            self.test_results["emotime_api"] = True
            print("   → Emotime API tests: PASSED")
            
        except Exception as e:
            print(f"   ✗ Emotime API tests failed: {e}")
        
        print()
    
    async def test_emotime_engine(self):
        """Test Emotime engine core functionality."""
        print("2. EMOTIME ENGINE CORE")
        print("-" * 40)
        
        try:
            # Create engine instance
            engine = EmotimeEngine(
                user_id="integration_test_user",
                session_id="integration_test_session",
                update_interval=0.5,  # Fast updates for testing
                enable_neo4j=False    # Disable Neo4j for tests
            )
            
            # Start engine
            await engine.start()
            print("   ✓ Engine start: OK")
            
            # Process various sensor data
            test_data = [
                # Happy text
                SensorData(
                    sensor_type=SensorType.TEXT,
                    data={"text": "I'm so happy and excited about today!"},
                    timestamp=datetime.now()
                ),
                # Gentle touch
                SensorData(
                    sensor_type=SensorType.TOUCH,
                    data={"pressure": 0.3, "duration": 3.0, "pattern": "continuous"},
                    timestamp=datetime.now()
                ),
                # Calm audio
                SensorData(
                    sensor_type=SensorType.AUDIO,
                    data={"volume": 0.4, "pitch": 0.3, "tempo": 0.2},
                    timestamp=datetime.now()
                )
            ]
            
            # Process all data
            for i, data in enumerate(test_data):
                await engine.process_sensor_data(data)
                print(f"   ✓ Sensor data {i+1} processed: {data.sensor_type.value}")
            
            # Wait for heartbeat processing
            await asyncio.sleep(1.0)
            
            # Check current state
            state = await engine.get_current_state()
            if state:
                print(f"   ✓ Current state: {state.mode.name}")
                print(f"     - Valence: {state.features.valence:.2f}")
                print(f"     - Arousal: {state.features.arousal:.2f}")
                print(f"     - Confidence: {state.confidence:.2f}")
            else:
                print("   ⚠ No current state available")
            
            # Get insights
            insights = await engine.get_emotional_insights()
            if insights:
                print("   ✓ Insights generated: OK")
                print(f"     - Timeseries points: {insights.get('timeseries_analysis', {}).get('total_points', 0)}")
            
            # Get resonance trace
            trace = await engine.get_resonance_trace(limit=5)
            print(f"   ✓ Resonance trace: {len(trace)} points")
            
            # Stop engine
            await engine.stop()
            print("   ✓ Engine stop: OK")
            
            self.test_results["emotime_engine"] = True
            print("   → Emotime Engine tests: PASSED")
            
        except Exception as e:
            print(f"   ✗ Emotime Engine tests failed: {e}")
            import traceback
            traceback.print_exc()
        
        print()
    
    async def test_metrics_integration(self):
        """Test Prometheus metrics integration."""
        print("3. METRICS INTEGRATION")
        print("-" * 40)
        
        try:
            # Get Prometheus metrics
            response = requests.get(f"{self.base_url}/metrics", timeout=5)
            
            if response.status_code != 200:
                print(f"   ✗ Metrics endpoint: ERROR {response.status_code}")
                return
            
            metrics_text = response.text
            
            # Check for Emotime metrics
            emotime_metrics = [
                "emotime_sensor_data_total",
                "emotime_emotional_features",
                "emotime_mode_duration_seconds",
                "emotime_mode_transitions_total",
                "emotime_fusion_confidence",
                "emotime_heartbeat_total",
                "emotime_timeseries_points",
                "emotime_peak_detection_total"
            ]
            
            found_metrics = []
            for metric in emotime_metrics:
                if metric in metrics_text:
                    found_metrics.append(metric)
                    print(f"   ✓ Metric found: {metric}")
            
            if len(found_metrics) >= 5:  # At least 5 metrics should be present
                print(f"   ✓ Emotime metrics: {len(found_metrics)}/{len(emotime_metrics)} found")
                self.test_results["metrics_integration"] = True
                print("   → Metrics integration: PASSED")
            else:
                print(f"   ⚠ Only {len(found_metrics)}/{len(emotime_metrics)} metrics found")
            
        except Exception as e:
            print(f"   ✗ Metrics integration failed: {e}")
        
        print()
    
    async def test_websocket_integration(self):
        """Test WebSocket integration (basic connectivity check)."""
        print("4. WEBSOCKET INTEGRATION")
        print("-" * 40)
        
        try:
            # Test WebSocket health via HTTP
            response = requests.get(f"{self.base_url}/health", timeout=5)
            
            if response.status_code == 200:
                health = response.json()
                print(f"   ✓ WebSocket health: {health.get('status', 'unknown')}")
            
            # Check WebSocket metrics
            response = requests.get(f"{self.base_url}/metrics", timeout=5)
            if response.status_code == 200:
                metrics_text = response.text
                if "websocket_connections" in metrics_text:
                    print("   ✓ WebSocket metrics: Available")
                    self.test_results["websocket_integration"] = True
                    print("   → WebSocket integration: PASSED")
            
        except Exception as e:
            print(f"   ✗ WebSocket integration failed: {e}")
        
        print()
    
    async def test_ml_integration(self):
        """Test ML system integration."""
        print("5. ML SYSTEM INTEGRATION")
        print("-" * 40)
        
        try:
            # Test ML health endpoint
            response = requests.get(f"{self.base_url}/ml/health", timeout=5)
            
            if response.status_code == 200:
                ml_health = response.json()
                print(f"   ✓ ML system health: {ml_health.get('status', 'unknown')}")
            
            # Test ML metrics endpoint
            response = requests.get(f"{self.base_url}/ml/metrics", timeout=5)
            
            if response.status_code == 200:
                ml_metrics = response.json()
                print("   ✓ ML metrics: Available")
                print(f"     - Feature extractor: {ml_metrics.get('feature_extractor', {}).get('status', 'unknown')}")
                print(f"     - Anomaly detector: {ml_metrics.get('anomaly_detector', {}).get('status', 'unknown')}")
            
            # Check for ML-related Prometheus metrics
            response = requests.get(f"{self.base_url}/metrics", timeout=5)
            if response.status_code == 200:
                metrics_text = response.text
                ml_metric_keywords = [
                    "ml_requests_total",
                    "ml_processing_time",
                    "anomaly_detection"
                ]
                
                found_ml_metrics = sum(1 for keyword in ml_metric_keywords if keyword in metrics_text)
                print(f"   ✓ ML Prometheus metrics: {found_ml_metrics} found")
            
            self.test_results["ml_integration"] = True
            print("   → ML integration: PASSED")
            
        except Exception as e:
            print(f"   ✗ ML integration failed: {e}")
        
        print()
    
    def print_summary(self):
        """Print test summary."""
        print("INTEGRATION TEST SUMMARY")
        print("=" * 60)
        
        passed = sum(1 for result in self.test_results.values() if result)
        total = len(self.test_results)
        success_rate = (passed / total) * 100
        
        for test_name, result in self.test_results.items():
            status = "PASSED" if result else "FAILED"
            indicator = "✓" if result else "✗"
            print(f"   {indicator} {test_name.replace('_', ' ').title()}: {status}")
        
        print(f"\nOverall: {passed}/{total} tests passed ({success_rate:.0f}%)")
        
        if success_rate >= 80:
            print("\n🎯 INTEGRATION TESTS: SUCCESS")
        elif success_rate >= 60:
            print("\n⚠️  INTEGRATION TESTS: PARTIAL SUCCESS")
        else:
            print("\n❌ INTEGRATION TESTS: FAILED")
        
        print(f"End time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")


# Standalone performance test
async def performance_test():
    """Performance test for Emotime system."""
    print("\nPERFORMANCE TEST - EMOTIME")
    print("-" * 40)
    
    try:
        engine = EmotimeEngine(
            user_id="perf_test_user",
            session_id="perf_test_session",
            update_interval=0.1,
            enable_neo4j=False
        )
        
        await engine.start()
        
        # Generate test data
        start_time = time.time()
        num_messages = 50
        
        for i in range(num_messages):
            sensor_data = SensorData(
                sensor_type=SensorType.TEXT,
                data={"text": f"Test message {i} with varying emotional content"},
                timestamp=datetime.now()
            )
            await engine.process_sensor_data(sensor_data)
            
            if i % 10 == 0:
                print(f"   Processed {i+1}/{num_messages} messages")
        
        # Wait for processing
        await asyncio.sleep(2.0)
        
        end_time = time.time()
        processing_time = end_time - start_time
        rate = num_messages / processing_time
        
        print(f"   Processing time: {processing_time:.2f} seconds")
        print(f"   Processing rate: {rate:.1f} messages/second")
        
        # Check final state
        state = await engine.get_current_state()
        if state:
            print(f"   Final state: {state.mode.name} (confidence: {state.confidence:.2f})")
        
        await engine.stop()
        print("   → Performance test: COMPLETED")
        
    except Exception as e:
        print(f"   ✗ Performance test failed: {e}")


async def main():
    """Main test runner."""
    print("Starting LIMINAL Integration Tests...")
    print("Make sure the LIMINAL server is running on localhost:8000")
    print()
    
    # Wait a moment for user to read
    await asyncio.sleep(2)
    
    # Run integration tests
    test_suite = IntegrationTestSuite()
    success = await test_suite.run_all_tests()
    
    # Run performance test
    await performance_test()
    
    return success


if __name__ == "__main__":
    try:
        result = asyncio.run(main())
        exit(0 if result else 1)
    except KeyboardInterrupt:
        print("\nTests interrupted by user")
        exit(130)
    except Exception as e:
        print(f"Integration tests error: {e}")
        import traceback
        traceback.print_exc()
        exit(1)