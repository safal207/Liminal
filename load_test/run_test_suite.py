#!/usr/bin/env python3
"""
Performance Test Suite Runner for LIMINAL API.
"""
import argparse
import json
import logging
import os
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List

import yaml

# Setup logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)


class TestSuiteRunner:
    """Manages execution of performance test scenarios."""

    def __init__(self, config_file: str = "performance_config.yaml"):
        self.config = self._load_config(config_file)
        self.suite_id = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.output_dir = (
            Path(self.config["global"]["output_dir"]) / f"suite_{self.suite_id}"
        )
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def _load_config(self, config_file: str) -> Dict:
        with open(config_file, "r") as f:
            return yaml.safe_load(f)

    def run(self, scenarios: List[str] = None):
        """Run test scenarios."""
        scenarios = scenarios or list(self.config["test_scenarios"].keys())
        results = []

        for scenario in scenarios:
            if scenario not in self.config["test_scenarios"]:
                logger.warning(f"Unknown scenario: {scenario}")
                continue

            logger.info(f"\n🚀 Running scenario: {scenario}")
            result = self._run_scenario(scenario)
            results.append(result)

        self._generate_report(results)
        logger.info(f"\n✅ Test suite completed. Report: {self.output_dir}/index.html")

    def _run_scenario(self, name: str) -> Dict:
        """Execute a single test scenario."""
        cfg = self.config["test_scenarios"][name]
        cmd = [
            sys.executable,
            "run_performance_analysis.py",
            "--test-type",
            "custom",
            "--users",
            str(cfg["users"]),
            "--duration",
            cfg["duration"],
            "--output-dir",
            str(self.output_dir / name),
        ]

        try:
            start = datetime.now()
            subprocess.run(cmd, check=True)
            return {
                "name": name,
                "status": "PASSED",
                "duration": str(datetime.now() - start),
                "config": cfg,
            }
        except subprocess.CalledProcessError as e:
            return {"name": name, "status": "FAILED", "error": str(e), "config": cfg}

    def _generate_report(self, results: List[Dict]):
        """Generate HTML report."""
        html = f"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>Test Suite Report - {self.suite_id}</title>
            <style>
                body {{ font-family: Arial, sans-serif; margin: 20px; }}
                .scenario {{ margin: 10px 0; padding: 10px; border-left: 5px solid #ccc; }}
                .PASSED {{ border-color: #2ecc71; }}
                .FAILED {{ border-color: #e74c3c; }}
                .status {{ font-weight: bold; }}
                .PASSED .status {{ color: #2ecc71; }}
                .FAILED .status {{ color: #e74c3c; }}
            </style>
        </head>
        <body>
            <h1>Test Suite Report - {self.suite_id}</h1>
            <p>Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
            <h2>Results</h2>
        """

        for r in results:
            html += f"""
            <div class="scenario {r['status']}">
                <h3>{r['name']} <span class="status">{r['status']}</span></h3>
                <p><strong>Duration:</strong> {r.get('duration', 'N/A')}</p>
                <p><strong>Users:</strong> {r['config']['users']}</p>
                <p><strong>Duration:</strong> {r['config']['duration']}</p>
                <p><strong>Spawn Rate:</strong> {r['config']['spawn_rate']}/s</p>
                {"<p><strong>Error:</strong> " + r['error'] + "</p>" if 'error' in r else ''}
            </div>
            """

        html += "</body></html>"

        with open(self.output_dir / "index.html", "w") as f:
            f.write(html)


def main():
    parser = argparse.ArgumentParser(description="Run performance test suite")
    parser.add_argument("scenarios", nargs="*", help="Scenarios to run (default: all)")
    parser.add_argument(
        "--config", default="performance_config.yaml", help="Configuration file"
    )

    args = parser.parse_args()

    try:
        runner = TestSuiteRunner(args.config)
        runner.run(args.scenarios or None)
    except Exception as e:
        logger.error(f"Test suite failed: {e}", exc_info=True)
        sys.exit(1)


if __name__ == "__main__":
    main()
