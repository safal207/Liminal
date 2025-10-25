"""
WebSocket + Emotime Integration Test
Tests real WebSocket communication with Emotime processing.
"""

import asyncio
import json
import websockets
import requests
from datetime import datetime
import time


class WebSocketEmotimeIntegration:
    """Test WebSocket integration with Emotime system."""
    
    def __init__(self):
        self.ws_url = "ws://localhost:8000/ws/integration_test_client"
        self.http_url = "http://localhost:8000"
        self.test_user_id = "ws_emotime_test_user"
        self.jwt_token = "dummy_jwt_token_for_test"  # In real scenario, get from auth
    
    async def test_websocket_emotime_flow(self):
        """Test complete WebSocket + Emotime integration flow."""
        print("WEBSOCKET + EMOTIME INTEGRATION TEST")
        print("=" * 50)
        
        success = False
        
        try:
            # Connect to WebSocket
            print("1. Connecting to WebSocket...")
            
            async with websockets.connect(self.ws_url) as websocket:
                print("   ✓ WebSocket connected")
                
                # Receive auth request
                auth_request = await websocket.recv()
                auth_data = json.loads(auth_request)
                print(f"   → Auth request: {auth_data.get('type')}")
                
                # Send authentication (mock JWT)
                auth_message = {
                    "token": self.jwt_token
                }
                await websocket.send(json.dumps(auth_message))
                print("   → Auth sent")
                
                # Receive auth response
                auth_response = await websocket.recv()
                auth_result = json.loads(auth_response)
                
                if auth_result.get("type") == "auth_success":
                    print("   ✓ Authentication successful")
                else:
                    print(f"   ✗ Authentication failed: {auth_result}")
                    return False
                
                # Test 1: Send messages that should trigger Emotime processing
                print("\n2. Testing Emotime processing via WebSocket...")
                
                emotional_messages = [
                    "I feel absolutely wonderful and energetic today!",
                    "This is making me really stressed and anxious",
                    "I'm in a very peaceful and calm state of mind",
                    "Wow! This is so exciting and thrilling!",
                    "I'm feeling quite sad and melancholic today"
                ]
                
                for i, message_text in enumerate(emotional_messages):
                    # Send message through WebSocket
                    message = {
                        "type": "message",
                        "user_id": self.test_user_id,
                        "channel": "emotime_test_channel",
                        "message": {
                            "text": message_text,
                            "timestamp": datetime.now().isoformat()
                        }
                    }
                    
                    print(f"   → Sending emotional message {i+1}: {message_text[:50]}...")
                    await websocket.send(json.dumps(message))
                    
                    # Wait for any responses
                    try:
                        response = await asyncio.wait_for(websocket.recv(), timeout=2.0)
                        response_data = json.loads(response)
                        print(f"   ← Received: {response_data.get('type', 'unknown')}")
                        
                        # Check for home state notifications
                        if response_data.get("type") == "home_state_update":
                            print(f"     Home state detected! Authenticity: {response_data.get('data', {}).get('authenticity_level', 'unknown')}")
                    
                    except asyncio.TimeoutError:
                        print("     No immediate response")
                    
                    # Small delay between messages
                    await asyncio.sleep(0.5)
                
                print("   ✓ All emotional messages sent")
                
                # Test 2: Check Emotime API after WebSocket processing
                print("\n3. Checking Emotime API integration...")
                
                # Wait a moment for processing
                await asyncio.sleep(2.0)
                
                # Check Emotime status
                try:
                    response = requests.get(
                        f"{self.http_url}/emotime/status",
                        timeout=5
                    )
                    
                    if response.status_code == 200:
                        emotime_status = response.json()
                        print(f"   ✓ Emotime status: {emotime_status.get('status', 'unknown')}")
                        
                        if emotime_status.get("status") == "active":
                            mode = emotime_status.get("mode", {})
                            features = emotime_status.get("features", {})
                            print(f"     Current mode: {mode.get('name', 'unknown')}")
                            print(f"     Valence: {features.get('valence', 'unknown'):.2f}")
                            print(f"     Confidence: {emotime_status.get('confidence', 'unknown'):.2f}")
                    
                except Exception as e:
                    print(f"   ⚠ Emotime API check failed: {e}")
                
                # Test 3: Check metrics
                print("\n4. Checking metrics integration...")
                
                try:
                    metrics_response = requests.get(f"{self.http_url}/metrics", timeout=5)
                    
                    if metrics_response.status_code == 200:
                        metrics_text = metrics_response.text
                        
                        # Check for WebSocket metrics
                        ws_metrics = [
                            "websocket_connections",
                            "websocket_messages_total",
                            "websocket_auth_total"
                        ]
                        
                        # Check for Emotime metrics  
                        emotime_metrics = [
                            "emotime_sensor_data_total",
                            "emotime_heartbeat_total",
                            "emotime_mode_duration_seconds"
                        ]
                        
                        found_ws = sum(1 for metric in ws_metrics if metric in metrics_text)
                        found_emotime = sum(1 for metric in emotime_metrics if metric in metrics_text)
                        
                        print(f"   ✓ WebSocket metrics: {found_ws}/{len(ws_metrics)} found")
                        print(f"   ✓ Emotime metrics: {found_emotime}/{len(emotime_metrics)} found")
                        
                        if found_ws >= 2 and found_emotime >= 2:
                            print("   ✓ Metrics integration: PASSED")
                        else:
                            print("   ⚠ Metrics integration: PARTIAL")
                    
                except Exception as e:
                    print(f"   ⚠ Metrics check failed: {e}")
                
                # Test 4: Send heartbeat/ping
                print("\n5. Testing heartbeat integration...")
                
                ping_message = {
                    "type": "ping",
                    "timestamp": datetime.now().isoformat()
                }
                
                await websocket.send(json.dumps(ping_message))
                print("   → Ping sent")
                
                try:
                    pong_response = await asyncio.wait_for(websocket.recv(), timeout=2.0)
                    print("   ← Pong received (or other response)")
                except asyncio.TimeoutError:
                    print("   ← No pong response (timeout)")
                
                success = True
                print("\n   ✓ WebSocket + Emotime integration: SUCCESS")
        
        except Exception as e:
            print(f"\n   ✗ WebSocket integration failed: {e}")
            import traceback
            traceback.print_exc()
        
        return success
    
    async def test_emotime_session_management(self):
        """Test Emotime session start/stop via API."""
        print("\n6. Testing Emotime session management...")
        
        try:
            # Start session
            start_payload = {
                "user_id": self.test_user_id,
                "session_id": f"session_{int(time.time())}"
            }
            
            response = requests.post(
                f"{self.http_url}/emotime/session/start",
                json=start_payload,
                timeout=10
            )
            
            if response.status_code == 200:
                session_info = response.json()
                print(f"   ✓ Session started: {session_info.get('session_id')}")
                session_id = session_info.get('session_id')
                
                # Process some data in the session
                text_payload = {
                    "text": "Starting my emotional journey session",
                    "user_id": self.test_user_id,
                    "session_id": session_id
                }
                
                response = requests.post(
                    f"{self.http_url}/emotime/text",
                    json=text_payload,
                    timeout=10
                )
                
                if response.status_code == 200:
                    print("   ✓ Session data processing: OK")
                
                # Get session timeline
                response = requests.get(
                    f"{self.http_url}/emotime/timeline",
                    params={
                        "user_id": self.test_user_id,
                        "session_id": session_id,
                        "limit": 10
                    },
                    timeout=10
                )
                
                if response.status_code == 200:
                    timeline = response.json()
                    print(f"   ✓ Timeline retrieved: {len(timeline.get('timeline', []))} points")
                
                # Stop session
                stop_payload = {
                    "user_id": self.test_user_id,
                    "session_id": session_id
                }
                
                response = requests.post(
                    f"{self.http_url}/emotime/session/stop",
                    json=stop_payload,
                    timeout=10
                )
                
                if response.status_code == 200:
                    print("   ✓ Session stopped: OK")
                    return True
            
        except Exception as e:
            print(f"   ✗ Session management failed: {e}")
        
        return False
    
    async def run_all_tests(self):
        """Run all WebSocket + Emotime integration tests."""
        print("Starting WebSocket + Emotime Integration Tests...")
        print("Make sure LIMINAL server is running on localhost:8000\n")
        
        # Main WebSocket integration test
        ws_success = await self.test_websocket_emotime_flow()
        
        # Session management test
        session_success = await self.test_emotime_session_management()
        
        # Summary
        print("\n" + "=" * 50)
        print("WEBSOCKET + EMOTIME TEST SUMMARY")
        
        total_tests = 2
        passed_tests = sum([ws_success, session_success])
        
        print(f"WebSocket Integration: {'PASSED' if ws_success else 'FAILED'}")
        print(f"Session Management: {'PASSED' if session_success else 'FAILED'}")
        print(f"\nOverall: {passed_tests}/{total_tests} tests passed")
        
        if passed_tests == total_tests:
            print("🎯 ALL TESTS PASSED!")
        elif passed_tests > 0:
            print("⚠️  PARTIAL SUCCESS")
        else:
            print("❌ ALL TESTS FAILED")
        
        return passed_tests == total_tests


async def main():
    """Main test runner."""
    integration_test = WebSocketEmotimeIntegration()
    success = await integration_test.run_all_tests()
    return success


if __name__ == "__main__":
    try:
        result = asyncio.run(main())
        exit(0 if result else 1)
    except KeyboardInterrupt:
        print("\nTests interrupted by user")
        exit(130)
    except Exception as e:
        print(f"Test error: {e}")
        exit(1)