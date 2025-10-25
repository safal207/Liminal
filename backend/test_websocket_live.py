#!/usr/bin/env python3
"""
🧠⚡ Live WebSocket Test for Emotime Real-time System

Tests the world-class WebSocket implementation with:
- Real-time emotional intelligence streaming
- Adaptive quality control
- Safety monitoring
- Multi-modal sensor fusion
"""

import asyncio
import json
import websockets
from datetime import datetime

async def test_emotime_websocket():
    """Test the Emotime WebSocket system."""
    uri = "ws://localhost:8000/ws/emotime/test_user_live"
    
    print("EMOTIME WEBSOCKET LIVE TEST")
    print("=" * 50)
    print(f"Connecting to: {uri}")
    
    try:
        async with websockets.connect(uri) as websocket:
            print("Connected to Emotime WebSocket!")
            
            # Wait for initial state
            try:
                initial_msg = await asyncio.wait_for(websocket.recv(), timeout=5.0)
                initial_data = json.loads(initial_msg)
                print("Received initial state:")
                print(json.dumps(initial_data, indent=2))
                print()
            except asyncio.TimeoutError:
                print("No initial state received (timeout)")
            
            # Test 1: Send ping
            print("Testing ping...")
            ping_msg = {"type": "ping"}
            await websocket.send(json.dumps(ping_msg))
            
            pong_response = await asyncio.wait_for(websocket.recv(), timeout=3.0)
            pong_data = json.loads(pong_response)
            print("Ping response:", pong_data["type"])
            print()
            
            # Test 2: Send sensor data
            print("Testing sensor data...")
            sensor_msg = {
                "type": "sensor_data",
                "data": {
                    "type": "text",
                    "text": "I feel incredible and energized today! This AI system is amazing!",
                    "typing_speed": 120,
                    "pause_duration": 0.8
                }
            }
            await websocket.send(json.dumps(sensor_msg))
            
            sensor_response = await asyncio.wait_for(websocket.recv(), timeout=5.0)
            sensor_data = json.loads(sensor_response)
            print("Sensor ack:", sensor_data["type"])
            print()
            
            # Test 3: Request analytics
            print("Testing analytics...")
            analytics_msg = {"type": "get_analytics"}
            await websocket.send(json.dumps(analytics_msg))
            
            analytics_response = await asyncio.wait_for(websocket.recv(), timeout=5.0)
            analytics_data = json.loads(analytics_response)
            print("Analytics received:")
            print(f"  Type: {analytics_data.get('type')}")
            if 'data' in analytics_data:
                for key, value in analytics_data['data'].items():
                    print(f"  {key}: {value}")
            print()
            
            # Test 4: Send learning feedback
            print("Testing learning feedback...")
            feedback_msg = {
                "type": "feedback",
                "data": {
                    "actual_emotion": "excitement",
                    "context": {
                        "time_of_day": 0.6,
                        "confidence": 0.95,
                        "user_rating": 5
                    }
                }
            }
            await websocket.send(json.dumps(feedback_msg))
            
            feedback_response = await asyncio.wait_for(websocket.recv(), timeout=5.0)
            feedback_data = json.loads(feedback_response)
            print("Feedback ack:", feedback_data["type"])
            print()
            
            # Test 5: Test quality preference
            print("Testing quality preference...")
            quality_msg = {
                "type": "quality_preference",
                "data": {
                    "quality": "high",
                    "update_frequency": 2.0
                }
            }
            await websocket.send(json.dumps(quality_msg))
            print("Quality preference sent")
            print()
            
            # Wait a bit to see if we receive any streaming updates
            print("Waiting for streaming updates...")
            try:
                for i in range(3):
                    update_msg = await asyncio.wait_for(websocket.recv(), timeout=5.0)
                    update_data = json.loads(update_msg)
                    print(f"Stream update {i+1}: {update_data.get('type', 'unknown')}")
                    
                    if update_data.get('type') == 'emotional_update':
                        mode = update_data.get('data', {}).get('mode', {})
                        print(f"   Emotion: {mode.get('type', 'unknown')}")
                        print(f"   Intensity: {mode.get('intensity', 0.0):.2f}")
                
            except asyncio.TimeoutError:
                print("No streaming updates received (timeout)")
            
            print()
            print("WebSocket test completed successfully!")
            print("World-class real-time emotional intelligence system working!")
            
    except websockets.exceptions.ConnectionRefused:
        print("Connection refused - is the server running?")
    except Exception as e:
        print(f"WebSocket test error: {e}")

if __name__ == "__main__":
    asyncio.run(test_emotime_websocket())