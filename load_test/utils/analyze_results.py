#!/usr/bin/env python3
"""
Utility script to analyze and visualize load test results.
"""
import argparse
import json
import os
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from tabulate import tabulate

# Configure matplotlib for better looking plots
plt.style.use("seaborn")
sns.set_palette("husl")


class ResultAnalyzer:
    """Analyze and visualize load test results."""

    def __init__(self, results_dir: str = "reports"):
        """Initialize with the directory containing test results."""
        self.results_dir = Path(results_dir)
        self.results = []

    def load_results(self) -> List[Dict]:
        """Load all test results from the results directory."""
        if not self.results_dir.exists():
            print(f"Error: Directory '{self.results_dir}' not found.")
            return []

        test_dirs = [d for d in self.results_dir.iterdir() if d.is_dir()]

        for test_dir in test_dirs:
            stats_file = test_dir / "results_stats.csv"
            if not stats_file.exists():
                continue

            try:
                df = pd.read_csv(stats_file)
                last_row = df.iloc[-1].to_dict()

                # Extract test metadata from directory name
                parts = test_dir.name.split("_")
                test_type = parts[0] if parts else "unknown"
                timestamp = "_".join(parts[1:]) if len(parts) > 1 else "unknown"

                result = {
                    "test_id": test_dir.name,
                    "test_type": test_type,
                    "timestamp": timestamp,
                    "total_requests": int(last_row.get("Total Request Count", 0)),
                    "failures": int(last_row.get("Failure Count", 0)),
                    "rps": float(last_row.get("Requests/s", 0)),
                    "avg_response_time": float(
                        last_row.get("Average Response Time", 0)
                    ),
                    "p50": float(last_row.get("50%", 0)),
                    "p90": float(last_row.get("90%", 0)),
                    "p95": float(last_row.get("95%", 0)),
                    "p99": float(last_row.get("99%", 0)),
                    "max_response_time": float(last_row.get("Max Response Time", 0)),
                    "min_response_time": float(last_row.get("Min Response Time", 0)),
                    "data_transferred": float(
                        last_row.get("Total Data Transferred", 0)
                    ),
                    "data_rate": float(last_row.get("Data Transfer Rate/s", 0)),
                }

                # Calculate failure rate
                if result["total_requests"] > 0:
                    result["failure_rate"] = (
                        result["failures"] / result["total_requests"]
                    ) * 100
                else:
                    result["failure_rate"] = 0.0

                self.results.append(result)

            except Exception as e:
                print(f"Error processing {test_dir}: {str(e)}")
                continue

        return self.results

    def print_summary(self):
        """Print a summary of all test results."""
        if not self.results:
            print("No test results found.")
            return

        # Sort by test type and timestamp
        sorted_results = sorted(
            self.results, key=lambda x: (x["test_type"], x["timestamp"]), reverse=True
        )

        # Prepare data for tabulate
        table_data = []
        for result in sorted_results:
            table_data.append(
                [
                    result["test_id"],
                    f"{result['rps']:.1f}",
                    f"{result['avg_response_time']:.1f}ms",
                    f"{result['p95']:.1f}ms",
                    f"{result['failure_rate']:.2f}%",
                    result["total_requests"],
                ]
            )

        headers = ["Test ID", "RPS", "Avg RT", "p95 RT", "Failure %", "Total Reqs"]
        print("\nTest Results Summary:")
        print(tabulate(table_data, headers=headers, tablefmt="grid"))

    def plot_trends(self, output_file: Optional[str] = None):
        """Plot trends across test runs."""
        if not self.results:
            print("No test results to plot.")
            return

        df = pd.DataFrame(self.results)

        # Group by test type and get the latest run for each type
        latest_tests = (
            df.sort_values("timestamp").groupby("test_type").last().reset_index()
        )

        # Create a 2x2 grid of subplots
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        fig.suptitle("Load Test Results Analysis", fontsize=16)

        # Plot 1: RPS by Test Type
        sns.barplot(x="test_type", y="rps", data=latest_tests, ax=axes[0, 0])
        axes[0, 0].set_title("Requests Per Second (RPS)")
        axes[0, 0].set_xlabel("Test Type")
        axes[0, 0].set_ylabel("RPS")

        # Plot 2: Response Time Percentiles
        percentiles = ["avg_response_time", "p50", "p90", "p95", "p99"]
        percentile_data = latest_tests.melt(
            id_vars=["test_type"],
            value_vars=percentiles,
            var_name="percentile",
            value_name="response_time",
        )
        sns.barplot(
            x="test_type",
            y="response_time",
            hue="percentile",
            data=percentile_data,
            ax=axes[0, 1],
        )
        axes[0, 1].set_title("Response Time Percentiles")
        axes[0, 1].set_xlabel("Test Type")
        axes[0, 1].set_ylabel("Response Time (ms)")
        axes[0, 1].legend(title="Percentile")

        # Plot 3: Failure Rate
        sns.barplot(x="test_type", y="failure_rate", data=latest_tests, ax=axes[1, 0])
        axes[1, 0].set_title("Failure Rate")
        axes[1, 0].set_xlabel("Test Type")
        axes[1, 0].set_ylabel("Failure Rate (%)")

        # Plot 4: Data Transfer Rate
        sns.barplot(x="test_type", y="data_rate", data=latest_tests, ax=axes[1, 1])
        axes[1, 1].set_title("Data Transfer Rate")
        axes[1, 1].set_xlabel("Test Type")
        axes[1, 1].set_ylabel("Data Rate (bytes/s)")

        plt.tight_layout(rect=[0, 0.03, 1, 0.95])

        if output_file:
            plt.savefig(output_file)
            print(f"Plot saved to {output_file}")
        else:
            plt.show()


def main():
    """Main entry point for the result analyzer."""
    parser = argparse.ArgumentParser(description="Analyze load test results")
    parser.add_argument(
        "--dir",
        type=str,
        default="reports",
        help="Directory containing test results (default: reports)",
    )
    parser.add_argument(
        "--plot", type=str, help="Save plot to file (e.g., results_plot.png)"
    )

    args = parser.parse_args()

    analyzer = ResultAnalyzer(args.dir)
    analyzer.load_results()
    analyzer.print_summary()

    if args.plot or args.plot == "":
        output_file = args.plot if args.plot else "load_test_analysis.png"
        analyzer.plot_trends(output_file)


if __name__ == "__main__":
    main()
