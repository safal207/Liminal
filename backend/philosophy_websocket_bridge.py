#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Philosophy First - Neo4j WebSocket Relay Bridge
==============================================

This script serves as a bridge between the Neo4j Temporal Data Lake and
the Go WebSocket relay server. It:

1. Monitors Neo4j for new consciousness state transitions
2. Formats them as WebSocket events
3. Sends them to the Go WebSocket relay via HTTP POST
4. Can also generate simulated philosophical transitions

Philosophy First concept integration:
- Home State: "дом это ты, когда искренен с собой"
- Resonance Broadcasting: синхронизация состояний между пользователями
- Question Space: "мы научились задавать правильные вопросы"
- Presence Now: определение "здесь и сейчас"
"""

import argparse
import datetime
import json
import logging
import os
import random
import sys
import time
import traceback
import uuid
from typing import Any, Dict, List, Optional, Tuple

import requests
from neo4j import GraphDatabase, basic_auth

import redis

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("philosophy_websocket_bridge.log", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger(__name__)

# Neo4j connection parameters
NEO4J_URI = "bolt://localhost:7687"
NEO4J_USER = "neo4j"
NEO4J_PASSWORD = "NewStrongPass123!"

# WebSocket relay settings
WEBSOCKET_RELAY_URL = "http://localhost:8181/events"

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

PHILOSOPHICAL_USERS = [
    "philosopher_1",
    "philosopher_2",
    "consciousness_explorer",
    "authentic_self",
]


class PhilosophyWebSocketBridge:
    """Bridge between Neo4j Temporal Data Lake and WebSocket relay"""

    def __init__(
        self,
        neo4j_uri: str,
        neo4j_user: str,
        neo4j_password: str,
        websocket_url: str,
        poll_interval: int = 5,
    ):
        """
        Initialize the bridge

        Args:
            neo4j_uri: Neo4j connection URI
            neo4j_user: Neo4j username
            neo4j_password: Neo4j password
            websocket_url: WebSocket relay events endpoint
            poll_interval: How often to poll Neo4j for new transitions (seconds)
        """
        self.neo4j_uri = neo4j_uri
        self.neo4j_user = neo4j_user
        self.neo4j_password = neo4j_password
        self.websocket_url = websocket_url
        self.poll_interval = poll_interval
        self.driver = None
        # Optional Redis settings
        redis_host = os.getenv("REDIS_HOST")
        redis_port = int(os.getenv("REDIS_PORT", "6379"))
        redis_password = os.getenv("REDIS_PASSWORD")
        if redis_host:
            try:
                self.redis_client = redis.Redis(
                    host=redis_host,
                    port=redis_port,
                    password=redis_password,
                    decode_responses=True,
                )
                # Test connection
                self.redis_client.ping()
                logging.info(
                    "PhilosophyWebSocketBridge connected to Redis at %s:%s",
                    redis_host,
                    redis_port,
                )
            except Exception as re:
                logging.warning(
                    "Could not connect to Redis: %s — falling back to HTTP POST", re
                )
                self.redis_client = None
        else:
            self.redis_client = None
        self.last_transition_time = None

    def connect(self) -> bool:
        """Establish connection to Neo4j"""
        try:
            self.driver = GraphDatabase.driver(
                self.neo4j_uri, auth=basic_auth(self.neo4j_user, self.neo4j_password)
            )
            with self.driver.session() as session:
                result = session.run("MATCH (n) RETURN count(n) as count")
                record = result.single()
                count = record["count"]
                logger.info(f"Successfully connected to Neo4j. Found {count} nodes.")
            return True
        except Exception as e:
            logger.error(f"Failed to connect to Neo4j: {e}")
            traceback.print_exc()
            return False

    def check_websocket_relay(self) -> bool:
        """Check if WebSocket relay is available"""
        try:
            response = requests.get(
                self.websocket_url.replace("/events", "/health"), timeout=5
            )
            if response.status_code == 200:
                logger.info("WebSocket relay is available")
                return True
            else:
                logger.error(f"WebSocket relay returned status {response.status_code}")
                return False
        except requests.RequestException as e:
            logger.error(f"Failed to connect to WebSocket relay: {e}")
            return False

    def get_new_transitions(self) -> List[Dict[str, Any]]:
        """
        Get new consciousness transitions from Neo4j

        Returns:
            List of transitions as dictionaries
        """
        if not self.driver:
            logger.error("Not connected to Neo4j")
            return []

        query = """
        MATCH (source:ConsciousnessNode)-[t:TRANSITIONS_TO]->(target:ConsciousnessNode)
        WHERE t.timestamp > $last_time
        RETURN source, t, target
        ORDER BY t.timestamp ASC
        LIMIT 100
        """

        try:
            with self.driver.session() as session:
                result = session.run(query, last_time=self.last_transition_time or 0)

                transitions = []
                latest_timestamp = self.last_transition_time

                for record in result:
                    source = record["source"]
                    transition = record["t"]
                    target = record["target"]

                    # Track latest timestamp
                    if transition["timestamp"] > (latest_timestamp or 0):
                        latest_timestamp = transition["timestamp"]

                    # Format as event
                    event = {
                        "id": str(uuid.uuid4()),
                        "event_type": "consciousness_transition",
                        "timestamp": transition["timestamp"],
                        "user_id": source["user_id"],
                        "source_state": source["state"],
                        "target_state": target["state"],
                        "trigger": transition["trigger"],
                        "meta": {
                            "description": transition.get(
                                "philosophical_significance", ""
                            ),
                            "presence_delta": transition.get("presence_delta", 0),
                            "harmony_delta": transition.get("harmony_delta", 0),
                            "authenticity_delta": transition.get(
                                "authenticity_delta", 0
                            ),
                            "resonance_delta": transition.get("resonance_delta", 0),
                        },
                    }
                    transitions.append(event)

                # Update last transition time for next poll
                if latest_timestamp:
                    self.last_transition_time = latest_timestamp

                return transitions

        except Exception as e:
            logger.error(f"Error getting transitions from Neo4j: {e}")
            traceback.print_exc()
            return []

    def send_event(self, event: Dict[str, Any]) -> bool:
        """
        Send event to WebSocket relay

        Args:
            event: Event dictionary to send

        Returns:
            True if successful, False otherwise
        """
        if self.redis_client:
            try:
                self.redis_client.xadd(
                    "consciousness_stream", {"data": json.dumps(event)}, maxlen=10000
                )
                logger.info("Event published to Redis stream")
                return True
            except Exception as re:
                logger.error("Redis publish failed: %s", re)
        # fallback to HTTP POST
        try:
            response = requests.post(
                self.websocket_url,
                json=event,
                headers={"Content-Type": "application/json"},
                timeout=5,
            )

            if response.status_code == 200:
                logger.info(
                    f"Successfully sent event: {event['user_id']} → {event['target_state']}"
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

    def generate_simulated_event(self) -> Dict[str, Any]:
        """
        Generate a simulated philosophical transition event

        Returns:
            Event dictionary
        """
        user_id = random.choice(PHILOSOPHICAL_USERS)

        # Generate consistent transition pattern
        if random.random() < 0.7:  # 70% chance of following the philosophical sequence
            # Select sequential transitions more often
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
        else:
            # Completely random transition
            source_state = random.choice(CONSCIOUSNESS_STATES)
            # Avoid transitioning to same state
            remaining_states = [s for s in CONSCIOUSNESS_STATES if s != source_state]
            target_state = random.choice(remaining_states)
            trigger = random.choice(TRANSITION_TRIGGERS)

        # Generate philosophical metrics
        presence_delta = random.uniform(0.1, 0.5) if random.random() > 0.3 else 0
        harmony_delta = random.uniform(0.1, 0.5) if random.random() > 0.3 else 0
        authenticity_delta = random.uniform(0.1, 0.5) if random.random() > 0.3 else 0
        resonance_delta = (
            random.uniform(0.1, 0.5) if target_state == "RESONANCE_MOMENT" else 0
        )

        # Small chance of resonance moment for other states
        if target_state != "RESONANCE_MOMENT" and random.random() < 0.1:
            resonance_delta = random.uniform(0.3, 0.7)

        # Build event
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
                "presence_delta": presence_delta,
                "harmony_delta": harmony_delta,
                "authenticity_delta": authenticity_delta,
                "resonance_delta": resonance_delta,
                "simulated": True,
            },
        }

        return event

    def run_neo4j_monitor(self) -> None:
        """
        Main monitoring loop for Neo4j transitions

        Continuously polls Neo4j for new transitions and sends them to WebSocket
        """
        logger.info("Starting Neo4j transition monitor")
        try:
            while True:
                # Get new transitions
                transitions = self.get_new_transitions()
                logger.info(f"Found {len(transitions)} new transitions")

                # Send each transition to WebSocket relay
                for transition in transitions:
                    success = self.send_event(transition)
                    if not success:
                        logger.warning(
                            "Failed to send transition, will retry on next poll"
                        )

                # Wait for next poll
                time.sleep(self.poll_interval)

        except KeyboardInterrupt:
            logger.info("Stopping Neo4j monitor")
        except Exception as e:
            logger.error(f"Error in Neo4j monitor: {e}")
            traceback.print_exc()

    def run_simulation(self, interval: float, count: int = None) -> None:
        """
        Run a simulation generating philosophical events

        Args:
            interval: Time between events (seconds)
            count: Number of events to generate (None for infinite)
        """
        logger.info(
            f"Starting simulation, interval={interval}s, count={count or 'infinite'}"
        )

        try:
            generated = 0
            while count is None or generated < count:
                # Generate simulated event
                event = self.generate_simulated_event()

                # Send to WebSocket relay
                success = self.send_event(event)
                if success:
                    generated += 1
                    logger.info(
                        f"Generated event {generated}/{count or '∞'}: "
                        f"{event['user_id']} {event['source_state']} → {event['target_state']}"
                    )
                else:
                    logger.warning("Failed to send simulated event")

                # Check if we're done
                if count is not None and generated >= count:
                    break

                # Wait for next event
                time.sleep(interval)

        except KeyboardInterrupt:
            logger.info("Simulation stopped by user")
        except Exception as e:
            logger.error(f"Error in simulation: {e}")
            traceback.print_exc()
        finally:
            logger.info(f"Simulation complete. Generated {generated} events.")

    def close(self) -> None:
        """Clean up resources"""
        if self.driver:
            self.driver.close()


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Philosophy First - Neo4j WebSocket Bridge"
    )
    parser.add_argument(
        "--mode",
        choices=["monitor", "simulate"],
        default="monitor",
        help="Operation mode: monitor Neo4j for transitions or simulate events",
    )
    parser.add_argument(
        "--interval",
        type=float,
        default=2.0,
        help="Polling/simulation interval in seconds",
    )
    parser.add_argument(
        "--count",
        type=int,
        default=None,
        help="Number of simulated events (simulation mode only)",
    )
    parser.add_argument(
        "--relay-url", default=WEBSOCKET_RELAY_URL, help="WebSocket relay events URL"
    )
    args = parser.parse_args()

    # Create bridge
    bridge = PhilosophyWebSocketBridge(
        neo4j_uri=NEO4J_URI,
        neo4j_user=NEO4J_USER,
        neo4j_password=NEO4J_PASSWORD,
        websocket_url=args.relay_url,
        poll_interval=args.interval,
    )

    try:
        # Check connections
        if args.mode == "monitor":
            logger.info("Connecting to Neo4j...")
            if not bridge.connect():
                logger.error("Failed to connect to Neo4j. Exiting.")
                return 1

        logger.info("Checking WebSocket relay...")
        if not bridge.check_websocket_relay():
            logger.warning(
                "WebSocket relay not available or not responding to health checks"
            )
            response = input("Continue anyway? (y/n): ")
            if response.lower() != "y":
                logger.info("Exiting.")
                return 1

        # Run selected mode
        if args.mode == "monitor":
            bridge.run_neo4j_monitor()
        else:  # simulate
            bridge.run_simulation(interval=args.interval, count=args.count)

        return 0

    finally:
        bridge.close()


if __name__ == "__main__":
    sys.exit(main())
