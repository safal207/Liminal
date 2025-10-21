"""Prometheus metrics and helpers for ML pipeline observability."""

from __future__ import annotations

import time
from typing import Optional

from prometheus_client import Counter, Gauge, Histogram


ml_pipeline_runs_total = Counter(
    "ml_pipeline_runs_total",
    "Количество запусков ML-пайплайнов по стадиям и статусам",
    ["pipeline", "stage", "status"],
)

ml_pipeline_run_duration_seconds = Histogram(
    "ml_pipeline_run_duration_seconds",
    "Длительность выполнения стадий ML-пайплайна",
    ["pipeline", "stage"],
    buckets=(
        0.1,
        0.5,
        1,
        2.5,
        5,
        10,
        30,
        60,
        120,
        300,
        600,
    ),
)

ml_pipeline_queue_depth = Gauge(
    "ml_pipeline_queue_depth",
    "Размер очередей или буферов данных для ML-пайплайнов",
    ["pipeline", "stage"],
)

ml_pipeline_last_success_timestamp = Gauge(
    "ml_pipeline_last_success_timestamp",
    "Unix-время последнего успешного запуска ML-пайплайна",
    ["pipeline", "stage"],
)


def set_queue_depth(pipeline: str, stage: str, depth: int) -> None:
    """Обновить глубину очереди для указанного пайплайна."""

    ml_pipeline_queue_depth.labels(pipeline=pipeline, stage=stage).set(depth)


def mark_success(pipeline: str, stage: str, timestamp: Optional[float] = None) -> None:
    """Зафиксировать успешное завершение стадии пайплайна."""

    ml_pipeline_last_success_timestamp.labels(pipeline=pipeline, stage=stage).set(
        timestamp if timestamp is not None else time.time()
    )
