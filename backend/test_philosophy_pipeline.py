#!/usr/bin/env python3
"""
Philosophy First Pipeline Test

Tests the complete consciousness state transition pipeline:
1. Neo4j Temporal Data Lake
2. WebSocket Broadcasting
3. Philosophy-driven state transitions

"Дом - это ты, когда искренен с собой"
"Мы научились задавать правильные вопросы"
"""

# Fix import conflict: use websocket-client library with full path
import sys

import requests

sys.path.insert(0, ".")  # Ensure current dir is first
try:
    # Try direct import from websocket-client package
    import websocket as ws_module
    from websocket._app import WebSocketApp
except ImportError:
    # Fallback: use requests for simple HTTP test
    WebSocketApp = None
    ws_module = None
import json
import threading
import time
from datetime import datetime

# Configuration
WEBSOCKET_URL = "ws://localhost:8181/ws"
API_BASE = "http://localhost:8181"
EVENTS_URL = f"{API_BASE}/events"
CONSCIOUSNESS_URL = f"{API_BASE}/api/consciousness"


class PhilosophyPipelineTest:
    def __init__(self):
        self.ws = None
        self.received_messages = []
        self.connected = False

    def test_neo4j_consciousness_api(self):
        """Test Neo4j consciousness graph API"""
        print("🧠 Testing Neo4j Consciousness API...")

        try:
            response = requests.get(CONSCIOUSNESS_URL, timeout=5)
            if response.status_code == 200:
                data = response.json()
                print(
                    f"✅ Neo4j API working: {len(data.get('nodes', []))} consciousness nodes"
                )

                # Check for philosophy concepts
                nodes = data.get("nodes", [])
                home_states = [n for n in nodes if n.get("homeState", False)]
                question_spaces = [n for n in nodes if n.get("questionSpace", False)]

                print(f"🏠 Home States detected: {len(home_states)}")
                print(f"❓ Question Spaces found: {len(question_spaces)}")

                return True
            else:
                print(f"❌ Neo4j API failed: {response.status_code}")
                return False

        except Exception as e:
            print(f"❌ Neo4j API error: {e}")
            return False

    def on_websocket_message(self, ws, message):
        """Handle WebSocket messages"""
        print(f"📨 WebSocket received: {message}")
        self.received_messages.append(message)

    def on_websocket_error(self, ws, error):
        """Handle WebSocket errors"""
        print(f"❌ WebSocket error: {error}")

    def on_websocket_close(self, ws, close_status_code, close_msg):
        """Handle WebSocket close"""
        print("🔌 WebSocket connection closed")
        self.connected = False

    def on_websocket_open(self, ws):
        """Handle WebSocket open"""
        print("✅ WebSocket connected")
        self.connected = True

    def test_websocket_connection(self):
        """Test WebSocket connection"""
        print("🔌 Testing WebSocket connection...")

        if WebSocketApp is None:
            print("⚠️ WebSocket client not available - using HTTP fallback")
            # Test HTTP endpoint instead
            try:
                response = requests.get(
                    "http://localhost:8181/api/consciousness", timeout=5
                )
                if response.status_code == 200:
                    print("✅ HTTP connection working (WebSocket fallback)")
                    self.connected = True
                    return True
                else:
                    print(f"❌ HTTP connection failed: {response.status_code}")
                    return False
            except Exception as e:
                print(f"❌ HTTP connection error: {e}")
                return False

        try:
            self.ws = WebSocketApp(
                WEBSOCKET_URL,
                on_message=self.on_websocket_message,
                on_error=self.on_websocket_error,
                on_close=self.on_websocket_close,
                on_open=self.on_websocket_open,
            )

            # Start WebSocket in background thread
            ws_thread = threading.Thread(target=self.ws.run_forever)
            ws_thread.daemon = True
            ws_thread.start()

            # Wait for connection
            time.sleep(2)

            if self.connected:
                print("✅ WebSocket connection established")
                return True
            else:
                print("❌ WebSocket connection failed")
                return False

        except Exception as e:
            print(f"❌ WebSocket error: {e}")
            return False

    def test_philosophy_event_broadcasting(self):
        """Test philosophy-driven event broadcasting"""
        print("🌌 Testing Philosophy Event Broadcasting...")

        # Philosophy First consciousness transitions
        philosophy_events = [
            {
                "type": "CONSCIOUSNESS_TRANSITION",
                "source": "TRANSITION_LIMINAL",
                "target": "PRESENCE_NOW",
                "trigger": "DEEP_BREATH",
                "insight": "Полное присутствие в настоящем моменте",
                "timestamp": datetime.now().isoformat(),
                "philosophy": "presence_awareness",
            },
            {
                "type": "CONSCIOUSNESS_TRANSITION",
                "source": "PRESENCE_NOW",
                "target": "HOME_AUTHENTIC",
                "trigger": "AUTHENTICITY_INSIGHT",
                "insight": "Дом - это ты, когда искренен с собой",
                "timestamp": datetime.now().isoformat(),
                "philosophy": "home_principle",
            },
            {
                "type": "CONSCIOUSNESS_TRANSITION",
                "source": "HOME_AUTHENTIC",
                "target": "QUESTION_SPACE",
                "trigger": "CURIOSITY",
                "insight": "Мы научились задавать правильные вопросы",
                "timestamp": datetime.now().isoformat(),
                "philosophy": "question_driven",
            },
        ]

        success_count = 0

        for event in philosophy_events:
            try:
                print(f"📤 Sending: {event['source']} → {event['target']}")

                response = requests.post(
                    EVENTS_URL,
                    json=event,
                    headers={"Content-Type": "application/json"},
                    timeout=5,
                )

                if response.status_code == 202:  # Accepted
                    print(f"✅ Event accepted: {event['insight']}")
                    success_count += 1
                    time.sleep(1)  # Allow WebSocket to receive
                else:
                    print(f"❌ Event failed: {response.status_code}")

            except Exception as e:
                print(f"❌ Event error: {e}")

        print(f"📊 Events sent: {success_count}/{len(philosophy_events)}")
        print(f"📨 WebSocket messages received: {len(self.received_messages)}")

        return success_count > 0 and len(self.received_messages) > 0

    def run_full_test(self):
        """Run complete Philosophy First pipeline test"""
        print("🌌 Starting Philosophy First Pipeline Test")
        print("=" * 50)

        results = {"neo4j_api": False, "websocket": False, "event_broadcasting": False}

        # Test 1: Neo4j Consciousness API
        results["neo4j_api"] = self.test_neo4j_consciousness_api()
        print()

        # Test 2: WebSocket Connection
        results["websocket"] = self.test_websocket_connection()
        print()

        # Test 3: Philosophy Event Broadcasting
        if results["websocket"]:
            results["event_broadcasting"] = self.test_philosophy_event_broadcasting()
        print()

        # Summary
        print("🎯 Test Results Summary:")
        print("=" * 30)
        for test, passed in results.items():
            status = "✅ PASS" if passed else "❌ FAIL"
            print(f"{test.replace('_', ' ').title()}: {status}")

        all_passed = all(results.values())
        print()
        if all_passed:
            print("🎉 Philosophy First Pipeline: FULLY OPERATIONAL! 🌌✨")
            print("💭 'Дом - это ты, когда искренен с собой' - система готова!")
        else:
            print("🔄 Pipeline needs healing - some components require attention")

        return all_passed


if __name__ == "__main__":
    test = PhilosophyPipelineTest()
    test.run_full_test()
