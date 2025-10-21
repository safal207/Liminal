#!/usr/bin/env python3
"""Smoke-test наличия ключевых метрик в Prometheus-реестре."""

from __future__ import annotations

import importlib
import sys
from pathlib import Path
from typing import Iterable, List

from prometheus_client import generate_latest


ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))


MODULES: Iterable[str] = (
    "backend.metrics.collectors",
    "backend.ml.observability",
)

REQUIRED_METRICS: List[str] = [
    "memory_timeline_events_total",
    "memory_timeline_processing_seconds",
    "memory_timeline_backlog_size",
    "memory_timeline_subscribers",
    "ml_pipeline_runs_total",
    "ml_pipeline_run_duration_seconds",
    "ml_pipeline_queue_depth",
    "ml_pipeline_last_success_timestamp",
    "neo4j_operations_total",
    "neo4j_operation_duration_seconds",
    "neo4j_active_sessions",
    "neo4j_saturation_ratio",
]


def main() -> int:
    for module_name in MODULES:
        importlib.import_module(module_name)

    metrics_text = generate_latest().decode("utf-8", errors="ignore")
    missing = [
        metric
        for metric in REQUIRED_METRICS
        if f"# HELP {metric}" not in metrics_text and f"# TYPE {metric}" not in metrics_text
    ]

    if missing:
        sys.stderr.write(
            "Missing required Prometheus metrics: " + ", ".join(sorted(missing)) + "\n"
        )
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
