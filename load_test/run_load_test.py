#!/usr/bin/env python3
"""
Advanced load test runner for LIMINAL API with support for different test profiles.
"""
import argparse
import asyncio
import json
import logging
import os
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# Default settings
DEFAULT_HOST = "http://localhost:8000"
DEFAULT_OUTPUT_DIR = "reports"


class LoadTestRunner:
    """Run load tests with different profiles and collect metrics."""

    def __init__(self, host: str = DEFAULT_HOST, output_dir: str = DEFAULT_OUTPUT_DIR):
        """Initialize the load test runner."""
        self.host = host.rstrip("/")
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.test_start_time: Optional[datetime] = None

        # Configure environment
        os.environ["TARGET_HOST"] = self.host
        os.environ["PYTHONPATH"] = str(Path(__file__).parent.absolute())

    async def run_test(
        self,
        users: int,
        spawn_rate: int,
        duration: str,
        headless: bool = True,
        profile: str = "load",
    ) -> bool:
        """Run a load test with the given parameters."""
        test_id = f"{profile}_{int(time.time())}"
        logger.info(f"Starting {test_id} with {users} users...")

        # Prepare output directory
        test_dir = self.output_dir / test_id
        test_dir.mkdir(exist_ok=True)

        # Build command
        cmd = [
            "locust",
            "-f",
            "locustfile.py",
            "--host",
            self.host,
            "--users",
            str(users),
            "--spawn-rate",
            str(spawn_rate),
            "--run-time",
            duration,
            "--csv",
            str(test_dir / "results"),
            "--html",
            str(test_dir / "report.html"),
        ]

        if headless:
            cmd.extend(["--headless", "--only-summary"])

        # Run test
        try:
            self.test_start_time = datetime.now()
            process = await asyncio.create_subprocess_exec(
                *cmd, stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.PIPE
            )

            # Stream output
            async def stream_output(stream, is_error=False):
                while not stream.at_eof():
                    line = await stream.readline()
                    if line:
                        line = line.decode().strip()
                        if is_error:
                            logger.error(line)
                        else:
                            logger.info(line)

            await asyncio.gather(
                stream_output(process.stdout),
                stream_output(process.stderr, is_error=True),
            )

            await process.wait()

            if process.returncode == 0:
                logger.info(f"Test {test_id} completed successfully")
                return True
            else:
                logger.error(f"Test {test_id} failed with code {process.returncode}")
                return False

        except Exception as e:
            logger.error(f"Error running test: {str(e)}", exc_info=True)
            return False

    async def run_smoke_test(self, users: int = 5) -> bool:
        """Run a quick smoke test."""
        return await self.run_test(
            users=users, spawn_rate=1, duration="1m", profile="smoke"
        )

    async def run_load_test(self, users: int = 100) -> bool:
        """Run a standard load test."""
        return await self.run_test(
            users=users, spawn_rate=10, duration="5m", profile="load"
        )

    async def run_stress_test(self, users: int = 1000) -> bool:
        """Run a stress test with high load."""
        return await self.run_test(
            users=users, spawn_rate=50, duration="10m", profile="stress"
        )

    async def run_soak_test(self, users: int = 50) -> bool:
        """Run a long-running soak test."""
        return await self.run_test(
            users=users, spawn_rate=5, duration="1h", profile="soak"
        )


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description="Run load tests for LIMINAL API")

    # Global options
    parser.add_argument(
        "--host",
        type=str,
        default=DEFAULT_HOST,
        help=f"Target host (default: {DEFAULT_HOST})",
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default=DEFAULT_OUTPUT_DIR,
        help=f"Output directory for reports (default: {DEFAULT_OUTPUT_DIR})",
    )

    # Subcommands
    subparsers = parser.add_subparsers(dest="command", required=True, help="Test type")

    # Smoke test
    smoke = subparsers.add_parser("smoke", help="Run smoke test")
    smoke.add_argument("-u", "--users", type=int, default=5, help="Number of users")

    # Load test
    load = subparsers.add_parser("load", help="Run load test")
    load.add_argument("-u", "--users", type=int, default=100, help="Number of users")

    # Stress test
    stress = subparsers.add_parser("stress", help="Run stress test")
    stress.add_argument("-u", "--users", type=int, default=1000, help="Number of users")

    # Soak test
    soak = subparsers.add_parser("soak", help="Run soak test")
    soak.add_argument("-u", "--users", type=int, default=50, help="Number of users")

    # Custom test
    custom = subparsers.add_parser("custom", help="Run custom test")
    custom.add_argument(
        "-u", "--users", type=int, required=True, help="Number of users"
    )
    custom.add_argument(
        "-r", "--rate", type=int, required=True, help="Spawn rate (users/sec)"
    )
    custom.add_argument(
        "-d", "--duration", required=True, help="Test duration (e.g., 5m, 1h)"
    )
    custom.add_argument("--ui", action="store_true", help="Show web UI")

    return parser.parse_args()


async def main():
    """Main entry point."""
    args = parse_args()
    runner = LoadTestRunner(host=args.host, output_dir=args.output_dir)

    try:
        if args.command == "smoke":
            success = await runner.run_smoke_test(users=args.users)
        elif args.command == "load":
            success = await runner.run_load_test(users=args.users)
        elif args.command == "stress":
            success = await runner.run_stress_test(users=args.users)
        elif args.command == "soak":
            success = await runner.run_soak_test(users=args.users)
        elif args.command == "custom":
            success = await runner.run_test(
                users=args.users,
                spawn_rate=args.rate,
                duration=args.duration,
                headless=not args.ui,
                profile="custom",
            )
        else:
            logger.error(f"Unknown command: {args.command}")
            return 1

        return 0 if success else 1

    except KeyboardInterrupt:
        logger.info("Test interrupted by user")
        return 1
    except Exception as e:
        logger.error(f"Error: {str(e)}", exc_info=True)
        return 1


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
