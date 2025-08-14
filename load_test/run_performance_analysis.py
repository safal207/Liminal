#!/usr/bin/env python3
"""
Automated performance analysis for LIMINAL API load tests.
Runs tests, collects metrics, and generates a performance report.
"""
import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from prometheus_api_client import PrometheusConnect

# Configure logging and plotting
plt.style.use("seaborn")
sns.set_palette("husl")


class PerformanceAnalyzer:
    """Automated performance testing and analysis."""

    def __init__(self, output_dir: str = "performance_reports"):
        """Initialize with output directory for reports."""
        self.output_dir = Path(output_dir)
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.report_dir = self.output_dir / f"report_{self.timestamp}"
        self.metrics_dir = self.report_dir / "metrics"
        self.plots_dir = self.report_dir / "plots"
        self._setup_directories()

        # Prometheus connection
        self.prometheus = self._connect_prometheus()

    def _setup_directories(self):
        """Create necessary directories for reports."""
        self.report_dir.mkdir(parents=True, exist_ok=True)
        self.metrics_dir.mkdir(exist_ok=True)
        self.plots_dir.mkdir(exist_ok=True)

    def _connect_prometheus(self):
        """Connect to Prometheus server."""
        try:
            return PrometheusConnect(url="http://localhost:9090", disable_ssl=True)
        except Exception as e:
            print(f"Warning: Could not connect to Prometheus: {e}")
            return None

    def run_load_test(
        self, test_type: str = "load", users: int = 100, duration: str = "5m"
    ):
        """Run a load test with the specified parameters."""
        print(f"🚀 Running {test_type} test with {users} users for {duration}...")

        cmd = [
            "python",
            "run_load_test.py",
            test_type,
            "--users",
            str(users),
            "--output-dir",
            str(self.report_dir / "test_results"),
        ]

        if test_type == "custom":
            cmd.extend(["--duration", duration])

        try:
            subprocess.run(cmd, check=True)
            print("✅ Load test completed successfully")
            return True
        except subprocess.CalledProcessError as e:
            print(f"❌ Load test failed: {e}")
            return False

    def collect_metrics(self):
        """Collect metrics from Prometheus."""
        if not self.prometheus:
            print("⚠️  Prometheus not available, skipping metrics collection")
            return

        print("📊 Collecting metrics from Prometheus...")

        # Define the time range (last 10 minutes)
        end_time = datetime.now()
        start_time = end_time - pd.Timedelta(minutes=10)

        # Query metrics
        metrics = {
            "request_rate": "sum(rate(http_requests_total[1m]))",
            "response_time_p95": "histogram_quantile(0.95, sum(rate(http_request_duration_seconds_bucket[1m])) by (le))",
            "error_rate": 'sum(rate(http_requests_total{status=~"5.."}[1m])) / sum(rate(http_requests_total[1m]))',
            "active_users": "sum(active_users)",
        }

        results = {}
        for name, query in metrics.items():
            try:
                metric_data = self.prometheus.custom_query_range(
                    query=query,
                    start_time=start_time,
                    end_time=end_time,
                    step=15,  # 15-second intervals
                )
                results[name] = metric_data

                # Save raw data
                with open(self.metrics_dir / f"{name}.json", "w") as f:
                    json.dump(metric_data, f, indent=2)

            except Exception as e:
                print(f"⚠️  Failed to collect {name}: {e}")

        print("✅ Metrics collection completed")
        return results

    def generate_plots(self, metrics: dict):
        """Generate plots from collected metrics."""
        if not metrics:
            return

        print("📈 Generating performance plots...")

        # Plot request rate
        self._plot_metric(
            metrics.get("request_rate", []),
            title="Request Rate Over Time",
            ylabel="Requests per second",
            filename="request_rate.png",
        )

        # Plot response time
        self._plot_metric(
            metrics.get("response_time_p95", []),
            title="95th Percentile Response Time",
            ylabel="Response Time (seconds)",
            filename="response_time.png",
        )

        # Plot error rate
        self._plot_metric(
            metrics.get("error_rate", []),
            title="Error Rate Over Time",
            ylabel="Error Rate (%)",
            filename="error_rate.png",
        )

        # Plot active users
        self._plot_metric(
            metrics.get("active_users", []),
            title="Active Users Over Time",
            ylabel="Active Users",
            filename="active_users.png",
        )

    def _plot_metric(self, metric_data: list, title: str, ylabel: str, filename: str):
        """Helper method to plot a single metric."""
        if not metric_data:
            return

        plt.figure(figsize=(12, 6))

        for series in metric_data:
            # Skip if no data points
            if "values" not in series or not series["values"]:
                continue

            # Extract timestamps and values
            timestamps = [pd.to_datetime(x[0], unit="s") for x in series["values"]]
            values = [float(x[1]) for x in series["values"]]

            # Plot the data
            label = series.get("metric", {}).get("instance", "all")
            plt.plot(timestamps, values, label=label if label != "all" else None)

        plt.title(title)
        plt.xlabel("Time")
        plt.ylabel(ylabel)
        plt.grid(True)

        # Add legend if we have multiple series
        if len(metric_data) > 1:
            plt.legend()

        # Save the plot
        plt.tight_layout()
        plt.savefig(self.plots_dir / filename)
        plt.close()

    def generate_report(self):
        """Generate an HTML performance report."""
        print("📝 Generating performance report...")

        # Get list of plots
        plots = [f for f in self.plots_dir.glob("*.png") if f.is_file()]

        # Create HTML report
        html = f"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>Performance Test Report - {self.timestamp}</title>
            <style>
                body {{ font-family: Arial, sans-serif; margin: 20px; }}
                h1 {{ color: #2c3e50; }}
                .plot {{ margin: 20px 0; }}
                .plot img {{ max-width: 100%; height: auto; border: 1px solid #ddd; }}
                .summary {{ background: #f8f9fa; padding: 15px; border-radius: 5px; }}
            </style>
        </head>
        <body>
            <h1>Performance Test Report</h1>
            <div class="summary">
                <p><strong>Test Timestamp:</strong> {self.timestamp}</p>
                <p><strong>Report Generated:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
            </div>
        """

        # Add plots to the report
        for plot in plots:
            html += f"""
            <div class="plot">
                <h2>{plot.stem.replace('_', ' ').title()}</h2>
                <img src="{plot.relative_to(self.report_dir)}" alt="{plot.stem}">
            </div>
            """

        html += """
            <footer>
                <p>Generated by LIMINAL Performance Analysis Tool</p>
            </footer>
        </body>
        </html>
        """

        # Save the report
        report_file = self.report_dir / "index.html"
        with open(report_file, "w") as f:
            f.write(html)

        print(f"✅ Report generated: {report_file.absolute()}")
        return report_file


def main():
    """Main entry point for performance analysis."""
    parser = argparse.ArgumentParser(
        description="Run performance analysis for LIMINAL API"
    )
    parser.add_argument(
        "--test-type",
        type=str,
        default="load",
        choices=["smoke", "load", "stress", "soak", "custom"],
        help="Type of test to run (default: load)",
    )
    parser.add_argument(
        "--users",
        type=int,
        default=100,
        help="Number of users for the test (default: 100)",
    )
    parser.add_argument(
        "--duration",
        type=str,
        default="5m",
        help="Test duration (e.g., 5m, 1h) (default: 5m)",
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default="performance_reports",
        help="Output directory for reports (default: performance_reports)",
    )

    args = parser.parse_args()

    # Initialize analyzer
    analyzer = PerformanceAnalyzer(output_dir=args.output_dir)

    # Run the test
    success = analyzer.run_load_test(
        test_type=args.test_type, users=args.users, duration=args.duration
    )

    if not success:
        print("❌ Test failed, skipping analysis")
        return 1

    # Collect metrics and generate report
    metrics = analyzer.collect_metrics()
    analyzer.generate_plots(metrics)
    report_file = analyzer.generate_report()

    print(f"\n🎉 Performance analysis completed!")
    print(f"📊 View the report at: file://{report_file.absolute()}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
