"""
ML модуль для Resonance Liminal.
Интеграция с AutoML for Embedded (Kenning framework) для интеллектуальной оптимизации.
"""

from .anomaly_detector import AnomalyDetector
from .client import MLInferenceClient
from .feature_extractor import FeatureExtractor
from .model_manager import ModelManager

__all__ = ["FeatureExtractor", "ModelManager", "AnomalyDetector", "MLInferenceClient"]
