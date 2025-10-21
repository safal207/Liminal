#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Unit Tests for Philosophy WebSocket Bridge
==========================================

Tests the functionality of the PhilosophyWebSocketBridge component that connects
Neo4j Temporal Data Lake with the Go WebSocket relay.
"""

import datetime
import json
import unittest
from unittest.mock import ANY, MagicMock, patch

import requests

# Import the module to test
try:
    from backend.philosophy_websocket_bridge import (
        CONSCIOUSNESS_STATES,
        TRANSITION_TRIGGERS,
        PhilosophyWebSocketBridge,
    )

    BRIDGE_MODULE_PATH = "backend.philosophy_websocket_bridge"
except ModuleNotFoundError:  # pragma: no cover - fallback for legacy setups
    from philosophy_websocket_bridge import (  # type: ignore[no-redef]
        CONSCIOUSNESS_STATES,
        TRANSITION_TRIGGERS,
        PhilosophyWebSocketBridge,
    )

    BRIDGE_MODULE_PATH = "philosophy_websocket_bridge"

from neo4j import GraphDatabase


class TestPhilosophyWebSocketBridge(unittest.TestCase):
    """Test cases for PhilosophyWebSocketBridge"""

    def setUp(self):
        """Setup test fixtures"""
        self.mock_driver = MagicMock()
        self.mock_session = MagicMock()
        self.mock_driver.session.return_value.__enter__.return_value = self.mock_session

        # Create bridge with test values
        self.bridge = PhilosophyWebSocketBridge(
            neo4j_uri="bolt://test:7687",
            neo4j_user="test_user",
            neo4j_password="test_password",
            websocket_url="http://test:8181/events",
            poll_interval=1,
        )

        # Inject mock driver
        self.bridge.driver = self.mock_driver

    def test_connect_success(self):
        """Test successful Neo4j connection"""
        # Mock Neo4j driver and session
        with patch(f"{BRIDGE_MODULE_PATH}.GraphDatabase") as mock_db:
            mock_db.driver.return_value = self.mock_driver
            self.mock_session.run.return_value.single.return_value = {"count": 42}

            # Call connect method
            result = self.bridge.connect()

            # Verify connection was successful
            self.assertTrue(result)
            mock_db.driver.assert_called_once_with("bolt://test:7687", auth=ANY)

    def test_connect_failure(self):
        """Test Neo4j connection failure"""
        with patch(f"{BRIDGE_MODULE_PATH}.GraphDatabase") as mock_db:
            mock_db.driver.side_effect = Exception("Connection refused")

            # Call connect method
            result = self.bridge.connect()

            # Verify connection failed
            self.assertFalse(result)

    def test_check_websocket_relay_success(self):
        """Test successful WebSocket relay check"""
        with patch(f"{BRIDGE_MODULE_PATH}.requests.get") as mock_get:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_get.return_value = mock_response

            # Call check method
            result = self.bridge.check_websocket_relay()

            # Verify check was successful
            self.assertTrue(result)
            mock_get.assert_called_once_with("http://test:8181/health", timeout=5)

    def test_check_websocket_relay_failure(self):
        """Test WebSocket relay check failure"""
        with patch(f"{BRIDGE_MODULE_PATH}.requests.get") as mock_get:
            # Test with HTTP error status
            mock_response = MagicMock()
            mock_response.status_code = 500
            mock_get.return_value = mock_response

            result = self.bridge.check_websocket_relay()
            self.assertFalse(result)

            # Test with connection error
            mock_get.side_effect = requests.exceptions.ConnectionError()

            result = self.bridge.check_websocket_relay()
            self.assertFalse(result)

    def test_get_new_transitions_empty(self):
        """Test getting transitions when there are none"""
        self.mock_session.run.return_value = []

        transitions = self.bridge.get_new_transitions()

        self.assertEqual(len(transitions), 0)
        self.mock_session.run.assert_called_once()

    def test_get_new_transitions_with_data(self):
        """Test getting transitions with sample data"""
        # Create mock record
        mock_record = MagicMock()
        mock_record["source"] = {"user_id": "philosopher_1", "state": "PRESENCE_NOW"}
        mock_record["target"] = {"user_id": "philosopher_1", "state": "HARMONY_BALANCE"}
        mock_record["t"] = {
            "timestamp": "2025-08-06T22:04:27.561798000+00:00",
            "trigger": "MEDITATION",
            "philosophical_significance": "Achieving harmony through meditation",
        }

        # Set up mock result
        self.mock_session.run.return_value = [mock_record]

        # Call method
        transitions = self.bridge.get_new_transitions()

        # Verify result
        self.assertEqual(len(transitions), 1)
        self.assertEqual(transitions[0]["user_id"], "philosopher_1")
        self.assertEqual(transitions[0]["source_state"], "PRESENCE_NOW")
        self.assertEqual(transitions[0]["target_state"], "HARMONY_BALANCE")
        self.assertEqual(transitions[0]["trigger"], "MEDITATION")
        self.assertEqual(
            transitions[0]["meta"]["description"],
            "Achieving harmony through meditation",
        )

    def test_send_event_to_websocket_success(self):
        """Test successful event sending to WebSocket relay"""
        with patch(f"{BRIDGE_MODULE_PATH}.requests.post") as mock_post:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_post.return_value = mock_response

            event = {
                "id": "test-id",
                "event_type": "consciousness_transition",
                "timestamp": "2025-08-06T22:17:24+00:00",
                "user_id": "philosopher_1",
                "source_state": "TRANSITION_LIMINAL",
                "target_state": "PRESENCE_NOW",
                "trigger": "PRESENCE_PRACTICE",
            }

            result = self.bridge.send_event_to_websocket(event)

            self.assertTrue(result)
            mock_post.assert_called_once_with(
                "http://test:8181/events",
                json=event,
                headers={"Content-Type": "application/json"},
                timeout=5,
            )

    def test_send_event_to_websocket_failure(self):
        """Test failed event sending to WebSocket relay"""
        with patch(f"{BRIDGE_MODULE_PATH}.requests.post") as mock_post:
            # Test with HTTP error status
            mock_response = MagicMock()
            mock_response.status_code = 500
            mock_response.text = "Internal server error"
            mock_post.return_value = mock_response

            event = {
                "id": "test-id",
                "user_id": "philosopher_1",
                "source_state": "TRANSITION_LIMINAL",
                "target_state": "PRESENCE_NOW",
            }

            result = self.bridge.send_event_to_websocket(event)
            self.assertFalse(result)

            # Test with connection error
            mock_post.side_effect = requests.exceptions.ConnectionError()

            result = self.bridge.send_event_to_websocket(event)
            self.assertFalse(result)

    def test_generate_simulated_event(self):
        """Test generation of simulated events"""
        event = self.bridge.generate_simulated_event()

        # Verify event structure
        self.assertIn("id", event)
        self.assertEqual(event["event_type"], "consciousness_transition")
        self.assertIn("timestamp", event)
        self.assertIn(
            event["user_id"],
            [
                "philosopher_1",
                "philosopher_2",
                "consciousness_explorer",
                "authentic_self",
            ],
        )
        self.assertIn(event["source_state"], CONSCIOUSNESS_STATES)
        self.assertIn(event["target_state"], CONSCIOUSNESS_STATES)
        self.assertIn(event["trigger"], TRANSITION_TRIGGERS)

        # Verify meta structure
        self.assertIn("meta", event)
        self.assertIn("description", event["meta"])
        self.assertIn("presence_delta", event["meta"])
        self.assertIn("harmony_delta", event["meta"])
        self.assertIn("authenticity_delta", event["meta"])
        self.assertIn("resonance_delta", event["meta"])
        self.assertTrue(event["meta"]["simulated"])

        # Verify values are in expected ranges
        self.assertGreaterEqual(event["meta"]["presence_delta"], 0)
        self.assertLessEqual(event["meta"]["presence_delta"], 0.5)

        # Source and target should be different
        self.assertNotEqual(event["source_state"], event["target_state"])

    def test_run_neo4j_monitor_keyboard_interrupt(self):
        """Test Neo4j monitoring loop can be interrupted"""
        self.bridge.get_new_transitions = MagicMock(side_effect=KeyboardInterrupt())

        # This should not raise an exception
        self.bridge.run_neo4j_monitor()

        self.bridge.get_new_transitions.assert_called_once()

    def test_run_simulation_count_limit(self):
        """Test simulation respects count limit"""
        self.bridge.generate_simulated_event = MagicMock(return_value={"id": "test"})
        self.bridge.send_event_to_websocket = MagicMock(return_value=True)

        # Set interval to 0 for faster test
        self.bridge.run_simulation(interval=0, count=5)

        self.assertEqual(self.bridge.generate_simulated_event.call_count, 5)
        self.assertEqual(self.bridge.send_event_to_websocket.call_count, 5)

    def test_close(self):
        """Test closing connection"""
        self.bridge.close()
        self.mock_driver.close.assert_called_once()


if __name__ == "__main__":
    unittest.main()
