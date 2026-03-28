"""
Locust load testing configuration for LIMINAL API.

This file configures the load test scenarios and settings.
"""

import logging
import os
from typing import Any, Dict, List

from locust import FastHttpUser, between, events, task
from locust.env import Environment
from prometheus_client import start_http_server

# Import scenarios
from scenarios.websocket_scenario import WebSocketScenario

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize environment
env = Environment(user_classes=[])

# Register scenarios
WebSocketScenario.register(env)

# Get the registered user classes
user_classes = env.user_classes

# Start Prometheus metrics server if enabled
if os.getenv("ENABLE_PROMETHEUS", "false").lower() == "true":
    prometheus_port = int(os.getenv("PROMETHEUS_PORT", "9090"))
    start_http_server(prometheus_port)
    logger.info(f"Prometheus metrics available on port {prometheus_port}")

# Add test run ID to environment
if not hasattr(env.parsed_options, "test_run_id"):
    import uuid

    env.parsed_options.test_run_id = str(uuid.uuid4())[:8]


# Add event handlers
@events.test_start.add_listener
def on_test_start(environment, **kwargs):
    """Handle test start event."""
    logger.info("=" * 80)
    logger.info("Starting load test")
    logger.info(f"Test run ID: {environment.parsed_options.test_run_id}")
    logger.info(f"Target host: {environment.host}")
    logger.info("=" * 80)


@events.test_stop.add_listener
def on_test_stop(environment, **kwargs):
    """Handle test stop event."""
    logger.info("=" * 80)
    logger.info("Load test completed")
    logger.info(f"Test run ID: {environment.parsed_options.test_run_id}")
    logger.info("=" * 80)


# Add test data collection for master node
@events.test_start.add_listener
def on_test_start(environment, **kwargs):
    if not isinstance(environment.runner, MasterRunner):
        return

    logger.info("Test started")


@events.test_stop.add_listener
def on_test_stop(environment, **kwargs):
    if not isinstance(environment.runner, WorkerRunner):
        logger.info("Test finished")
