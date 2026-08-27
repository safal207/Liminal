"""
ML модуль для Resonance Liminal.
Интеграция с AutoML for Embedded (Kenning framework) для интеллектуальной оптимизации.

The public ML symbols stay available from ``backend.ml``, but heavy model
modules are imported lazily. This keeps lightweight provider/instrumentation
paths from pulling the full ML dependency graph just by importing a submodule.
"""

from __future__ import annotations

from importlib import import_module
from typing import Any


_LAZY_EXPORTS = {
    "FeatureExtractor": (".feature_extractor", "FeatureExtractor"),
    "ModelManager": (".model_manager", "ModelManager"),
    "AnomalyDetector": (".anomaly_detector", "AnomalyDetector"),
    "MLInferenceClient": (".client", "MLInferenceClient"),
}

__all__ = ["FeatureExtractor", "ModelManager", "AnomalyDetector", "MLInferenceClient"]


def __getattr__(name: str) -> Any:
    """Load legacy public ML exports only when a caller actually requests them."""

    target = _LAZY_EXPORTS.get(name)
    if target is None:
        raise AttributeError(f"module {__name__!r} has no attribute {name!r}")

    module_name, attribute_name = target
    value = getattr(import_module(module_name, __name__), attribute_name)
    globals()[name] = value
    return value


def __dir__() -> list[str]:
    return sorted(set(globals()) | set(__all__))
