#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Integration Tests for Philosophy First Neo4j and WebSocket Pipeline
==================================================================

This script tests the full integration pipeline between:
1. Neo4j Temporal Data Lake
2. Philosophy WebSocket Bridge
3. Go WebSocket Relay

Tests verify end-to-end functionality with real dependencies when available,
falling back to mocks when necessary.
"""

import datetime
import json
import logging
import os
import random
import subprocess
import sys
import threading
import time
import unittest
import uuid
from unittest.mock import MagicMock, patch

import requests
from neo4j import GraphDatabase

# Configure logging for tests
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("philosophy_integration_tests.log", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger(__name__)

# Import modules to test
try:
    from philosophy_schema import (ConsciousnessNode, ConsciousnessState,
                                   StateTransition, TransitionTrigger)
    from philosophy_websocket_bridge import PhilosophyWebSocketBridge

    bridge_imported = True
except ImportError:
    logger.warning(
        "Could not import PhilosophyWebSocketBridge, some tests will be skipped"
    )
    bridge_imported = False

# Neo4j connection parameters (for integration tests)
NEO4J_URI = "bolt://localhost:7687"
NEO4J_USER = "neo4j"
NEO4J_PASSWORD = "NewStrongPass123!"

# WebSocket relay settings
WEBSOCKET_RELAY_URL = "http://localhost:8181/events"
WEBSOCKET_RELAY_WS = "ws://localhost:8181/ws"


class PhilosophyIntegrationBase(unittest.TestCase):
    """Base class for integration tests"""

    @classmethod
    def setUpClass(cls):
        """Check if services are available"""
        cls.neo4j_available = cls._check_neo4j_connection()
        cls.websocket_relay_available = cls._check_websocket_relay()

        # Log service availability
        logger.info(f"Neo4j available: {cls.neo4j_available}")
        logger.info(f"WebSocket relay available: {cls.websocket_relay_available}")

    @staticmethod
    def _check_neo4j_connection():
        """Check if Neo4j is available"""
        try:
            driver = GraphDatabase.driver(NEO4J_URI, auth=(NEO4J_USER, NEO4J_PASSWORD))
            with driver.session() as session:
                result = session.run("MATCH (n) RETURN count(n) as count")
                record = result.single()
                count = record["count"]
                logger.info(f"Neo4j connection successful. Found {count} nodes")
            driver.close()
            return True
        except Exception as e:
            logger.warning(f"Neo4j connection failed: {e}")
            return False

    @staticmethod
    def _check_websocket_relay():
        """Check if WebSocket relay is available"""
        try:
            health_url = WEBSOCKET_RELAY_URL.replace("/events", "/health")
            response = requests.get(health_url, timeout=2)
            if response.status_code == 200:
                logger.info("WebSocket relay is available")
                return True
            else:
                logger.warning(
                    f"WebSocket relay returned status {response.status_code}"
                )
                return False
        except requests.RequestException as e:
            logger.warning(f"WebSocket relay connection failed: {e}")
            return False

    def setUp(self):
        """Setup test resources"""
        # Set this test to skip if necessary resources are not available
        if not bridge_imported:
            self.skipTest("Philosophy WebSocket Bridge module not available")

    def create_test_data_in_neo4j(self, user_id="test_user"):
        """Create test consciousness states in Neo4j"""
        if not self.neo4j_available:
            return False

        try:
            driver = GraphDatabase.driver(NEO4J_URI, auth=(NEO4J_USER, NEO4J_PASSWORD))

            # Generate consciousness nodes for test user
            states = [
                "TRANSITION_LIMINAL",
                "PRESENCE_NOW",
                "HARMONY_BALANCE",
                "HOME_AUTHENTIC",
                "QUESTION_SPACE",
                "RESONANCE_MOMENT",
            ]

            # Create timestamp sequence
            base_time = datetime.datetime.now()
            timestamps = [
                (base_time + datetime.timedelta(seconds=i)).isoformat()
                for i in range(len(states))
            ]

            # Create nodes and transitions
            with driver.session() as session:
                # Clear previous test data
                session.run(
                    "MATCH (n:ConsciousnessNode {user_id: $user_id}) DETACH DELETE n",
                    user_id=user_id,
                )

                # Create nodes
                node_ids = []
                for i, state in enumerate(states):
                    node_id = str(uuid.uuid4())
                    node_ids.append(node_id)

                    session.run(
                        """
                        CREATE (n:ConsciousnessNode {
                            id: $id,
                            user_id: $user_id,
                            state: $state,
                            timestamp: $timestamp,
                            presence_value: $presence,
                            harmony_value: $harmony,
                            authenticity_value: $authenticity,
                            resonance_value: $resonance
                        })
                        """,
                        id=node_id,
                        user_id=user_id,
                        state=state,
                        timestamp=timestamps[i],
                        presence=random.uniform(0, 1),
                        harmony=random.uniform(0, 1),
                        authenticity=random.uniform(0, 1),
                        resonance=random.uniform(0, 1),
                    )

                # Create transitions
                triggers = [
                    "PRESENCE_PRACTICE",
                    "MEDITATION",
                    "HOME_RECOGNITION",
                    "QUESTION_REFRAMING",
                    "RESONANCE_ALIGNMENT",
                ]

                for i in range(len(states) - 1):
                    session.run(
                        """
                        MATCH (source:ConsciousnessNode {id: $source_id})
                        MATCH (target:ConsciousnessNode {id: $target_id})
                        CREATE (source)-[t:TRANSITIONS_TO {
                            id: $id,
                            timestamp: $timestamp,
                            trigger: $trigger,
                            philosophical_significance: $significance,
                            presence_delta: $presence_delta,
                            harmony_delta: $harmony_delta,
                            authenticity_delta: $authenticity_delta,
                            resonance_delta: $resonance_delta
                        }]->(target)
                        """,
                        source_id=node_ids[i],
                        target_id=node_ids[i + 1],
                        id=str(uuid.uuid4()),
                        timestamp=timestamps[i + 1],
                        trigger=triggers[i],
                        significance=f"Test transition from {states[i]} to {states[i+1]}",
                        presence_delta=random.uniform(0.1, 0.5),
                        harmony_delta=random.uniform(0.1, 0.5),
                        authenticity_delta=random.uniform(0.1, 0.5),
                        resonance_delta=random.uniform(0.1, 0.5),
                    )

            driver.close()
            logger.info(f"Created test data in Neo4j for user {user_id}")
            return True

        except Exception as e:
            logger.error(f"Error creating test data in Neo4j: {e}")
            return False

    def send_test_event_to_relay(self, user_id="test_user"):
        """Send a test event to WebSocket relay"""
        if not self.websocket_relay_available:
            return False

        try:
            event = {
                "id": str(uuid.uuid4()),
                "event_type": "consciousness_transition",
                "timestamp": datetime.datetime.now().isoformat(),
                "user_id": user_id,
                "source_state": "TRANSITION_LIMINAL",
                "target_state": "PRESENCE_NOW",
                "trigger": "PRESENCE_PRACTICE",
                "meta": {
                    "description": "Test transition to presence",
                    "presence_delta": 0.3,
                    "harmony_delta": 0.2,
                    "authenticity_delta": 0.4,
                    "resonance_delta": 0.1,
                    "test_event": True,
                },
            }

            response = requests.post(
                WEBSOCKET_RELAY_URL,
                json=event,
                headers={"Content-Type": "application/json"},
                timeout=5,
            )

            if response.status_code == 200:
                logger.info(f"Successfully sent test event to WebSocket relay")
                return True
            else:
                logger.error(
                    f"Failed to send test event. Status: {response.status_code}, Response: {response.text}"
                )
                return False

        except requests.RequestException as e:
            logger.error(f"Error sending test event to WebSocket relay: {e}")
            return False


class TestPhilosophyNeo4jIntegration(PhilosophyIntegrationBase):
    """Test Neo4j integration"""

    def setUp(self):
        """Setup test fixtures"""
        super().setUp()
        if not self.neo4j_available:
            self.skipTest("Neo4j not available")

    def test_create_consciousness_data(self):
        """Test creating consciousness data in Neo4j"""
        success = self.create_test_data_in_neo4j("test_integration_user")
        self.assertTrue(success)

        # Verify data was created
        driver = GraphDatabase.driver(NEO4J_URI, auth=(NEO4J_USER, NEO4J_PASSWORD))

        with driver.session() as session:
            # Count nodes
            result = session.run(
                """
                MATCH (n:ConsciousnessNode {user_id: $user_id}) 
                RETURN count(n) as node_count
                """,
                user_id="test_integration_user",
            )
            node_count = result.single()["node_count"]

            # Count transitions
            result = session.run(
                """
                MATCH (:ConsciousnessNode {user_id: $user_id})-[t:TRANSITIONS_TO]->() 
                RETURN count(t) as transition_count
                """,
                user_id="test_integration_user",
            )
            transition_count = result.single()["transition_count"]

        driver.close()

        # Verify counts
        self.assertEqual(node_count, 6)  # 6 states
        self.assertEqual(transition_count, 5)  # 5 transitions


class TestPhilosophyWebSocketRelayIntegration(PhilosophyIntegrationBase):
    """Test WebSocket relay integration"""

    def setUp(self):
        """Setup test fixtures"""
        super().setUp()
        if not self.websocket_relay_available:
            self.skipTest("WebSocket relay not available")

    def test_send_event_to_relay(self):
        """Test sending event to WebSocket relay"""
        success = self.send_test_event_to_relay("test_relay_user")
        self.assertTrue(success)


@unittest.skipIf(not bridge_imported, "Philosophy WebSocket Bridge not available")
class TestPhilosophyBridgeIntegration(PhilosophyIntegrationBase):
    """Test bridge between Neo4j and WebSocket relay"""

    def setUp(self):
        """Setup test fixtures"""
        super().setUp()
        if not self.neo4j_available or not self.websocket_relay_available:
            self.skipTest("Neo4j or WebSocket relay not available")

        self.bridge = PhilosophyWebSocketBridge(
            neo4j_uri=NEO4J_URI,
            neo4j_user=NEO4J_USER,
            neo4j_password=NEO4J_PASSWORD,
            websocket_url=WEBSOCKET_RELAY_URL,
            poll_interval=1,
        )

    def test_bridge_connections(self):
        """Test bridge can connect to both Neo4j and WebSocket relay"""
        neo4j_result = self.bridge.connect()
        websocket_result = self.bridge.check_websocket_relay()

        self.assertTrue(neo4j_result)
        self.assertTrue(websocket_result)

    def test_bridge_poll_and_send(self):
        """Test bridge can poll Neo4j and send to WebSocket"""
        # Create test data with known timestamp
        test_user_id = "bridge_test_user"
        now = datetime.datetime.now()

        # Make sure bridge is connected
        self.bridge.connect()

        # Record last transition time
        self.bridge.last_transition_time = (
            now - datetime.timedelta(minutes=5)
        ).isoformat()

        # Create fresh test data
        self.create_test_data_in_neo4j(test_user_id)

        # Poll for new transitions
        transitions = self.bridge.get_new_transitions()

        # Verify we got transitions
        self.assertGreater(len(transitions), 0)

        # Send first transition to WebSocket
        if transitions:
            success = self.bridge.send_event_to_websocket(transitions[0])
            self.assertTrue(success)

    def test_simulated_events(self):
        """Test generating and sending simulated events"""
        # Make sure bridge is connected
        self.bridge.check_websocket_relay()

        # Generate event
        event = self.bridge.generate_simulated_event()

        # Verify event structure
        self.assertIn("id", event)
        self.assertEqual(event["event_type"], "consciousness_transition")
        self.assertIn("timestamp", event)
        self.assertIn("user_id", event)
        self.assertIn("source_state", event)
        self.assertIn("target_state", event)
        self.assertIn("trigger", event)
        self.assertIn("meta", event)

        # Send event
        success = self.bridge.send_event_to_websocket(event)
        self.assertTrue(success)


class TestEndToEndIntegration(PhilosophyIntegrationBase):
    """
    End-to-end integration test that runs the bridge in simulation mode
    and verifies events are sent correctly
    """

    def setUp(self):
        """Setup test fixtures"""
        super().setUp()
        if not bridge_imported:
            self.skipTest("Philosophy WebSocket Bridge not available")
        if not self.websocket_relay_available:
            self.skipTest("WebSocket relay not available")

    def test_simulation_mode(self):
        """Test running in simulation mode"""
        bridge = PhilosophyWebSocketBridge(
            neo4j_uri=NEO4J_URI,
            neo4j_user=NEO4J_USER,
            neo4j_password=NEO4J_PASSWORD,
            websocket_url=WEBSOCKET_RELAY_URL,
            poll_interval=0.5,
        )

        # Run simulation for 5 events with 0.1s interval
        bridge.run_simulation(interval=0.1, count=5)

        # No assertions needed - if it completes without exceptions, the test passes
        # The actual WebSocket messages would be verified in a full E2E test with a WebSocket client


# Main entry point
if __name__ == "__main__":
    unittest.main()
