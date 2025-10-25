"""
Simple Integration Test for LIMINAL - Emotime System (ASCII only)
"""

import asyncio
import json
import requests
import time
from datetime import datetime


class SimpleLIMINALTest:
    """Simple integration test without Unicode characters."""
    
    def __init__(self):
        self.base_url = "http://localhost:8000"
        self.test_results = {}
    
    async def test_emotime_api(self):
        """Test Emotime API endpoints."""
        print("1. EMOTIME API TEST")
        print("-" * 30)
        
        try:
            # Test status endpoint
            response = requests.get(f"{self.base_url}/emotime/status", timeout=5)
            if response.status_code == 200:
                print("   Status endpoint: PASSED")
                status_data = response.json()
                print(f"   Current status: {status_data.get('status', 'unknown')}")
            else:
                print(f"   Status endpoint: FAILED ({response.status_code})")
                return False
            
            # Test text processing
            text_payload = {
                "text": "I feel wonderful and energetic today!",
                "user_id": "test_user",
                "session_id": "test_session"
            }
            
            response = requests.post(
                f"{self.base_url}/emotime/text",
                json=text_payload,
                timeout=10
            )
            
            if response.status_code == 200:
                print("   Text processing: PASSED")
                data = response.json()
                features = data.get('emotional_features', {})
                print(f"   Valence: {features.get('valence', 'unknown')}")
                print(f"   Arousal: {features.get('arousal', 'unknown')}")
            else:
                print(f"   Text processing: FAILED ({response.status_code})")
                return False
            
            # Test insights
            response = requests.get(
                f"{self.base_url}/emotime/insights",
                params={"user_id": "test_user", "session_id": "test_session"},
                timeout=10
            )
            
            if response.status_code == 200:
                print("   Insights: PASSED")
                insights = response.json()
                current_state = insights.get('current_state', {})
                print(f"   State status: {current_state.get('status', 'unknown')}")
            else:
                print(f"   Insights: FAILED ({response.status_code})")
                return False
            
            return True
            
        except Exception as e:
            print(f"   API Test ERROR: {e}")
            return False
    
    async def test_metrics(self):
        """Test Prometheus metrics."""
        print("\n2. METRICS TEST")
        print("-" * 30)
        
        try:
            response = requests.get(f"{self.base_url}/metrics", timeout=5)
            
            if response.status_code == 200:
                metrics_text = response.text
                
                # Check for key metrics
                emotime_metrics = [
                    "emotime_sensor_data_total",
                    "emotime_heartbeat_total",
                    "emotime_mode_duration_seconds"
                ]
                
                found_count = 0
                for metric in emotime_metrics:
                    if metric in metrics_text:
                        found_count += 1
                        print(f"   Found metric: {metric}")
                
                print(f"   Emotime metrics: {found_count}/{len(emotime_metrics)} found")
                
                if found_count >= 2:
                    print("   Metrics test: PASSED")
                    return True
                else:
                    print("   Metrics test: PARTIAL")
                    return False
            else:
                print(f"   Metrics endpoint: FAILED ({response.status_code})")
                return False
                
        except Exception as e:
            print(f"   Metrics test ERROR: {e}")
            return False
    
    async def test_health_checks(self):
        """Test system health endpoints."""
        print("\n3. HEALTH CHECKS")
        print("-" * 30)
        
        try:
            # Main health endpoint
            response = requests.get(f"{self.base_url}/health", timeout=5)
            if response.status_code == 200:
                print("   Main health: PASSED")
            else:
                print("   Main health: FAILED")
                return False
            
            # Emotime health
            response = requests.get(f"{self.base_url}/emotime/health", timeout=5)
            if response.status_code == 200:
                health_data = response.json()
                print("   Emotime health: PASSED")
                print(f"   Status: {health_data.get('status', 'unknown')}")
            else:
                print("   Emotime health: FAILED")
                return False
            
            # ML health (if available)
            try:
                response = requests.get(f"{self.base_url}/ml/health", timeout=5)
                if response.status_code == 200:
                    print("   ML health: PASSED")
                else:
                    print("   ML health: FAILED")
            except:
                print("   ML health: NOT AVAILABLE")
            
            return True
            
        except Exception as e:
            print(f"   Health checks ERROR: {e}")
            return False
    
    async def test_emotime_workflow(self):
        """Test complete Emotime processing workflow."""
        print("\n4. EMOTIME WORKFLOW TEST")
        print("-" * 30)
        
        try:
            user_id = f"workflow_test_{int(time.time())}"
            session_id = f"session_{int(time.time())}"
            
            # Start session
            start_payload = {"user_id": user_id, "session_id": session_id}
            response = requests.post(
                f"{self.base_url}/emotime/session/start",
                json=start_payload,
                timeout=10
            )
            
            if response.status_code == 200:
                print("   Session start: PASSED")
            else:
                print(f"   Session start: FAILED ({response.status_code})")
                return False
            
            # Process different emotional states
            test_messages = [
                "I am incredibly happy and joyful today!",
                "This situation is making me quite anxious",
                "I feel peaceful and completely at ease",
                "What an exciting and thrilling experience!"
            ]
            
            for i, message in enumerate(test_messages):
                payload = {
                    "text": message,
                    "user_id": user_id,
                    "session_id": session_id
                }
                
                response = requests.post(
                    f"{self.base_url}/emotime/text",
                    json=payload,
                    timeout=10
                )
                
                if response.status_code == 200:
                    print(f"   Message {i+1}: PROCESSED")
                else:
                    print(f"   Message {i+1}: FAILED")
                
                # Small delay between messages
                await asyncio.sleep(0.5)
            
            # Get timeline
            response = requests.get(
                f"{self.base_url}/emotime/timeline",
                params={"user_id": user_id, "session_id": session_id},
                timeout=10
            )
            
            if response.status_code == 200:
                timeline_data = response.json()
                timeline_points = len(timeline_data.get('timeline', []))
                print(f"   Timeline: {timeline_points} points")
                print("   Workflow test: PASSED")
                return True
            else:
                print("   Timeline: FAILED")
                return False
            
        except Exception as e:
            print(f"   Workflow test ERROR: {e}")
            return False
    
    async def run_all_tests(self):
        """Run all integration tests."""
        print("LIMINAL SIMPLE INTEGRATION TESTS")
        print("=" * 50)
        print(f"Start: {datetime.now().strftime('%H:%M:%S')}")
        print()
        
        # Run tests
        test_functions = [
            ("emotime_api", self.test_emotime_api),
            ("metrics", self.test_metrics),
            ("health_checks", self.test_health_checks),
            ("workflow", self.test_emotime_workflow)
        ]
        
        results = {}
        for test_name, test_func in test_functions:
            try:
                result = await test_func()
                results[test_name] = result
            except Exception as e:
                print(f"   {test_name} test CRASHED: {e}")
                results[test_name] = False
        
        # Summary
        print("\n" + "=" * 50)
        print("TEST SUMMARY")
        
        passed = 0
        total = len(results)
        
        for test_name, result in results.items():
            status = "PASSED" if result else "FAILED"
            print(f"   {test_name.upper()}: {status}")
            if result:
                passed += 1
        
        success_rate = (passed / total) * 100
        print(f"\nResults: {passed}/{total} tests passed ({success_rate:.0f}%)")
        
        if success_rate >= 75:
            print("OVERALL: SUCCESS")
            return True
        else:
            print("OVERALL: FAILED")
            return False


async def main():
    """Main test runner."""
    print("Starting LIMINAL Integration Tests...")
    print("Ensure server is running on localhost:8000")
    print()
    
    await asyncio.sleep(1)
    
    test_suite = SimpleLIMINALTest()
    success = await test_suite.run_all_tests()
    
    return success


if __name__ == "__main__":
    try:
        result = asyncio.run(main())
        exit(0 if result else 1)
    except Exception as e:
        print(f"Test error: {e}")
        exit(1)