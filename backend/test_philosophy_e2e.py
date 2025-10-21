#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
End-to-End Testing for Philosophy First
======================================

Comprehensive end-to-end tests for the complete Philosophy First pipeline:
1. Neo4j Temporal Data Lake
2. Philosophy WebSocket Bridge
3. Go WebSocket Relay
4. HTML Visualization Client

This script verifies the full philosophy-first approach with real philosophical
consciousness transitions and resonance moments.
"""

import asyncio
import atexit
import datetime
import json
import logging
import os
import random
import signal
import subprocess
import sys
import tempfile
import threading
import time
import unittest
import uuid
import webbrowser
from urllib.parse import urlparse

import requests

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("philosophy_e2e_tests.log", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger(__name__)

# Service configurations
NEO4J_CONFIG = {
    "uri": "bolt://localhost:7687",
    "user": "neo4j",
    "password": "NewStrongPass123!",
}

WEBSOCKET_CONFIG = {
    "host": "localhost",
    "port": 8181,
    "events_url": "http://localhost:8181/events",
    "ws_url": "ws://localhost:8181/ws",
    "health_url": "http://localhost:8181/health",
}

VISUALIZATION_PATH = os.path.abspath(
    os.path.join(
        os.path.dirname(__file__), "..", "frontend", "consciousness_visualizer.html"
    )
)

# Philosophical constants
CONSCIOUSNESS_STATES = [
    "TRANSITION_LIMINAL",
    "PRESENCE_NOW",
    "HARMONY_BALANCE",
    "HOME_AUTHENTIC",
    "QUESTION_SPACE",
    "RESONANCE_MOMENT",
    "REFLECTION_SELF",
    "INTEGRATION_WISDOM",
]

TRANSITION_TRIGGERS = [
    "PRESENCE_PRACTICE",
    "MEDITATION",
    "HOME_RECOGNITION",
    "QUESTION_REFRAMING",
    "RESONANCE_ALIGNMENT",
    "PHILOSOPHICAL_INSIGHT",
    "SYNCHRONICITY",
]

PHILOSOPHICAL_SIGNIFICANCE = {
    "TRANSITION_LIMINAL": "The threshold between mundane and conscious awareness",
    "PRESENCE_NOW": "Transition to full presence in the moment",
    "HARMONY_BALANCE": "Achieving harmony through meditation",
    "HOME_AUTHENTIC": "Home is where you're authentic with yourself",
    "QUESTION_SPACE": "We've learned to ask the right questions",
    "RESONANCE_MOMENT": "Moment of consciousness resonance",
    "REFLECTION_SELF": "Self-reflection after resonance moment",
    "INTEGRATION_WISDOM": "Integration of acquired wisdom",
}


class ProcessManager:
    """Manages external processes for E2E testing"""

    def __init__(self):
        """Initialize process manager"""
        self.processes = {}
        atexit.register(self.kill_all)
        signal.signal(signal.SIGINT, self.signal_handler)

    def start_process(self, name, cmd, cwd=None, env=None, shell=False):
        """Start a process"""
        logger.info(f"Starting process: {name}")
        try:
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                universal_newlines=True,
                cwd=cwd,
                env={**os.environ, **(env or {})},
                shell=shell,
            )
            self.processes[name] = process
            logger.info(f"Process {name} started with PID {process.pid}")
            return True
        except Exception as e:
            logger.error(f"Failed to start process {name}: {e}")
            return False

    def kill_process(self, name):
        """Kill a specific process"""
        if name in self.processes:
            logger.info(f"Killing process: {name}")
            try:
                process = self.processes[name]
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                logger.info(f"Process {name} terminated")
                del self.processes[name]
                return True
            except Exception as e:
                logger.error(f"Failed to kill process {name}: {e}")
                return False
        return False

    def kill_all(self):
        """Kill all managed processes"""
        for name in list(self.processes.keys()):
            self.kill_process(name)

    def signal_handler(self, sig, frame):
        """Handle signal interrupts"""
        logger.info("Received interrupt signal, terminating processes")
        self.kill_all()
        sys.exit(0)


class PhilosophyE2ETest(unittest.TestCase):
    """End-to-end test for Philosophy First"""

    @classmethod
    def setUpClass(cls):
        """Set up class fixtures"""
        cls.process_manager = ProcessManager()

        # Check Neo4j availability first
        cls.neo4j_available = cls._check_neo4j()

        # Check/start WebSocket relay if needed
        cls.websocket_relay_available = cls._check_websocket_relay()
        if not cls.websocket_relay_available:
            cls.websocket_relay_available = cls._start_websocket_relay()

        # Check visualization HTML exists
        cls.visualization_available = cls._check_visualization()

        # Set up server processes here (will be implemented later)

    @classmethod
    def tearDownClass(cls):
        """Tear down class fixtures"""
        # Kill any remaining processes
        cls.process_manager.kill_all()

    @staticmethod
    def _check_neo4j():
        """Check if Neo4j is available"""
        try:
            import neo4j

            driver = neo4j.GraphDatabase.driver(
                NEO4J_CONFIG["uri"],
                auth=(NEO4J_CONFIG["user"], NEO4J_CONFIG["password"]),
            )
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
            response = requests.get(WEBSOCKET_CONFIG["health_url"], timeout=2)
            if response.status_code == 200:
                logger.info("WebSocket relay is already running")
                return True
            else:
                logger.warning(
                    f"WebSocket relay health check failed: {response.status_code}"
                )
                return False
        except requests.RequestException:
            logger.info("WebSocket relay not running or health check not available")
            return False

    @classmethod
    def _start_websocket_relay(cls):
        """Start WebSocket relay server"""
        # Get Go WebSocket relay path
        relay_dir = os.path.abspath(
            os.path.join(os.path.dirname(__file__), "..", "go_ws_relay")
        )

        if not os.path.exists(os.path.join(relay_dir, "main.go")):
            logger.error(f"WebSocket relay directory not found: {relay_dir}")
            return False

        # Start relay process
        if sys.platform == "win32":
            cmd = ["go", "run", "main.go"]
        else:
            cmd = ["go", "run", "main.go"]

        success = cls.process_manager.start_process(
            "websocket_relay", cmd, cwd=relay_dir
        )

        if success:
            # Wait for relay to start
            max_attempts = 10
            for i in range(max_attempts):
                try:
                    time.sleep(1)
                    response = requests.get(WEBSOCKET_CONFIG["health_url"], timeout=1)
                    if response.status_code == 200:
                        logger.info("WebSocket relay started successfully")
                        return True
                except requests.RequestException:
                    pass

                logger.info(
                    f"Waiting for WebSocket relay to start (attempt {i+1}/{max_attempts})"
                )

            logger.error("WebSocket relay failed to start in time")
            return False

        return False

    @staticmethod
    def _check_visualization():
        """Check if visualization HTML exists"""
        if os.path.exists(VISUALIZATION_PATH):
            logger.info(f"Visualization HTML found at {VISUALIZATION_PATH}")
            return True
        else:
            logger.warning(f"Visualization HTML not found at {VISUALIZATION_PATH}")
            return False

    def setUp(self):
        """Set up test fixtures"""
        if not self.neo4j_available:
            self.skipTest("Neo4j not available")
        if not self.websocket_relay_available:
            self.skipTest("WebSocket relay not available")
        if not self.visualization_available:
            self.skipTest("Visualization HTML not available")

    def _generate_philosophical_event(self, user_id="e2e_test_user"):
        """Generate a philosophical event for testing"""
        # Generate philosophical transition following the sequence
        transitions = [
            ("TRANSITION_LIMINAL", "PRESENCE_NOW", "PRESENCE_PRACTICE"),
            ("PRESENCE_NOW", "HARMONY_BALANCE", "MEDITATION"),
            ("HARMONY_BALANCE", "HOME_AUTHENTIC", "HOME_RECOGNITION"),
            ("HOME_AUTHENTIC", "QUESTION_SPACE", "QUESTION_REFRAMING"),
            ("QUESTION_SPACE", "RESONANCE_MOMENT", "RESONANCE_ALIGNMENT"),
            ("RESONANCE_MOMENT", "REFLECTION_SELF", "PHILOSOPHICAL_INSIGHT"),
            ("REFLECTION_SELF", "INTEGRATION_WISDOM", "SYNCHRONICITY"),
        ]

        source_state, target_state, trigger = random.choice(transitions)

        # Create event
        event = {
            "id": str(uuid.uuid4()),
            "event_type": "consciousness_transition",
            "timestamp": datetime.datetime.now().isoformat(),
            "user_id": user_id,
            "source_state": source_state,
            "target_state": target_state,
            "trigger": trigger,
            "meta": {
                "description": PHILOSOPHICAL_SIGNIFICANCE.get(target_state, ""),
                "presence_delta": random.uniform(0.1, 0.5),
                "harmony_delta": random.uniform(0.1, 0.5),
                "authenticity_delta": random.uniform(0.1, 0.5),
                "resonance_delta": (
                    random.uniform(0.1, 0.5)
                    if target_state == "RESONANCE_MOMENT"
                    else 0
                ),
                "test_event": True,
            },
        }

        return event

    def _send_event_to_relay(self, event):
        """Send event to WebSocket relay"""
        try:
            response = requests.post(
                WEBSOCKET_CONFIG["events_url"],
                json=event,
                headers={"Content-Type": "application/json"},
                timeout=5,
            )

            if response.status_code == 200:
                logger.info(
                    f"Successfully sent event: {event['source_state']} → {event['target_state']}"
                )
                return True
            else:
                logger.error(
                    f"Failed to send event. Status: {response.status_code}, Response: {response.text}"
                )
                return False

        except requests.RequestException as e:
            logger.error(f"Error sending event to WebSocket relay: {e}")
            return False

    def test_send_multiple_events(self):
        """Test sending multiple philosophical events"""
        for i in range(5):
            # Generate event for different users to create resonance potential
            user_id = f"philosopher_{i % 2 + 1}"
            event = self._generate_philosophical_event(user_id)

            # Send event
            success = self._send_event_to_relay(event)
            self.assertTrue(success)

            # Small delay between events
            time.sleep(1)

    def test_resonance_events(self):
        """Test creating resonance events between users"""
        # Create resonance by sending the same state for different users
        timestamp = datetime.datetime.now().isoformat()

        # First user event
        event1 = {
            "id": str(uuid.uuid4()),
            "event_type": "consciousness_transition",
            "timestamp": timestamp,
            "user_id": "resonance_user_1",
            "source_state": "QUESTION_SPACE",
            "target_state": "RESONANCE_MOMENT",
            "trigger": "RESONANCE_ALIGNMENT",
            "meta": {
                "description": "Moment of consciousness resonance",
                "presence_delta": 0.4,
                "harmony_delta": 0.3,
                "authenticity_delta": 0.5,
                "resonance_delta": 0.8,
                "test_event": True,
            },
        }

        # Second user event (same state, slightly different time)
        event2 = {
            "id": str(uuid.uuid4()),
            "event_type": "consciousness_transition",
            "timestamp": (
                datetime.datetime.now() + datetime.timedelta(milliseconds=250)
            ).isoformat(),
            "user_id": "resonance_user_2",
            "source_state": "QUESTION_SPACE",
            "target_state": "RESONANCE_MOMENT",
            "trigger": "RESONANCE_ALIGNMENT",
            "meta": {
                "description": "Moment of consciousness resonance",
                "presence_delta": 0.3,
                "harmony_delta": 0.4,
                "authenticity_delta": 0.6,
                "resonance_delta": 0.7,
                "test_event": True,
            },
        }

        # Send events
        success1 = self._send_event_to_relay(event1)
        time.sleep(0.25)  # Small delay to simulate real-world conditions
        success2 = self._send_event_to_relay(event2)

        self.assertTrue(success1 and success2)

    def test_complete_philosophical_sequence(self):
        """Test sending a complete philosophical sequence for one user"""
        # Create sequence of states following the philosophy first approach
        transitions = [
            (
                "TRANSITION_LIMINAL",
                "PRESENCE_NOW",
                "PRESENCE_PRACTICE",
                "Transition to full presence in the moment",
            ),
            (
                "PRESENCE_NOW",
                "HARMONY_BALANCE",
                "MEDITATION",
                "Achieving harmony through meditation",
            ),
            (
                "HARMONY_BALANCE",
                "HOME_AUTHENTIC",
                "HOME_RECOGNITION",
                "Home is where you're authentic with yourself",
            ),
            (
                "HOME_AUTHENTIC",
                "QUESTION_SPACE",
                "QUESTION_REFRAMING",
                "We've learned to ask the right questions",
            ),
            (
                "QUESTION_SPACE",
                "RESONANCE_MOMENT",
                "RESONANCE_ALIGNMENT",
                "Moment of consciousness resonance",
            ),
            (
                "RESONANCE_MOMENT",
                "REFLECTION_SELF",
                "PHILOSOPHICAL_INSIGHT",
                "Self-reflection after resonance moment",
            ),
            (
                "REFLECTION_SELF",
                "INTEGRATION_WISDOM",
                "SYNCHRONICITY",
                "Integration of acquired wisdom",
            ),
        ]

        # Send each transition in sequence
        user_id = "philosophy_sequence_test"

        for source, target, trigger, description in transitions:
            # Create event
            event = {
                "id": str(uuid.uuid4()),
                "event_type": "consciousness_transition",
                "timestamp": datetime.datetime.now().isoformat(),
                "user_id": user_id,
                "source_state": source,
                "target_state": target,
                "trigger": trigger,
                "meta": {
                    "description": description,
                    "presence_delta": random.uniform(0.1, 0.5),
                    "harmony_delta": random.uniform(0.1, 0.5),
                    "authenticity_delta": random.uniform(0.1, 0.5),
                    "resonance_delta": (
                        random.uniform(0.1, 0.5) if target == "RESONANCE_MOMENT" else 0
                    ),
                },
            }

            # Send event
            success = self._send_event_to_relay(event)
            self.assertTrue(success)

            # Small delay between events
            time.sleep(1)

    @unittest.skip("Manual test that opens browser window")
    def test_visualization(self):
        """Test the visualization by opening it in browser"""
        # This test is designed to be run manually if needed

        # First send some test events
        for i in range(3):
            event = self._generate_philosophical_event()
            self._send_event_to_relay(event)
            time.sleep(0.5)

        # Open visualization in browser
        if os.path.exists(VISUALIZATION_PATH):
            url = f"file://{VISUALIZATION_PATH}"
            logger.info(f"Opening visualization at {url}")
            webbrowser.open(url)

            # Give user time to interact with visualization
            time.sleep(10)
        else:
            self.fail(f"Visualization not found at {VISUALIZATION_PATH}")


class TestPhilosophyPipeline(unittest.TestCase):
    """Test the full Philosophy First pipeline"""

    @classmethod
    def setUpClass(cls):
        """Set up test fixtures"""
        # Check prerequisites
        cls.websocket_relay_available = PhilosophyE2ETest._check_websocket_relay()
        cls.bridge_module_available = cls._check_bridge_module()

        if cls.bridge_module_available and cls.websocket_relay_available:
            # Start bridge in simulation mode
            cls.bridge_process = cls._start_bridge_simulation()
        else:
            cls.bridge_process = None

    @classmethod
    def tearDownClass(cls):
        """Clean up test fixtures"""
        # Terminate bridge process
        if cls.bridge_process:
            try:
                cls.bridge_process.terminate()
                cls.bridge_process.wait(timeout=5)
            except:
                if cls.bridge_process.poll() is None:
                    cls.bridge_process.kill()

    @staticmethod
    def _check_bridge_module():
        """Check if the bridge module is available"""
        try:
            from backend.philosophy_websocket_bridge import (
                PhilosophyWebSocketBridge,
            )

            return True
        except ImportError:
            try:
                from philosophy_websocket_bridge import PhilosophyWebSocketBridge  # type: ignore[no-redef]

                return True
            except ImportError:
                logger.warning("Philosophy WebSocket Bridge module not available")
                return False

    @classmethod
    def _start_bridge_simulation(cls):
        """Start bridge in simulation mode"""
        try:
            # Construct the command to run bridge in simulation mode
            cmd = [
                sys.executable,
                os.path.join(
                    os.path.dirname(__file__), "philosophy_websocket_bridge.py"
                ),
                "--mode",
                "simulate",
                "--interval",
                "3",
                "--count",
                "10",
            ]

            # Start process
            logger.info(f"Starting bridge simulation: {' '.join(cmd)}")
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                universal_newlines=True,
            )

            # Wait briefly to see if process starts correctly
            time.sleep(2)

            # Check if process is still running
            if process.poll() is not None:
                _, stderr = process.communicate()
                logger.error(f"Bridge simulation failed to start: {stderr}")
                return None

            logger.info("Bridge simulation started successfully")
            return process

        except Exception as e:
            logger.error(f"Failed to start bridge simulation: {e}")
            return None

    def setUp(self):
        """Set up test fixtures"""
        if not self.websocket_relay_available:
            self.skipTest("WebSocket relay not available")
        if not self.bridge_module_available:
            self.skipTest("Philosophy WebSocket Bridge module not available")
        if not self.bridge_process:
            self.skipTest("Bridge simulation process not started")

    def test_pipeline_running(self):
        """Test that the pipeline is running"""
        # Just check that bridge process is still running
        self.assertIsNone(self.bridge_process.poll())

        # Allow time for events to be generated and sent
        logger.info("Waiting for bridge to generate events...")
        time.sleep(10)

        # Bridge should still be running
        self.assertIsNone(self.bridge_process.poll())

        # If we get here without exceptions, test passes


# Main entry point
if __name__ == "__main__":
    unittest.main()
