"""
🧠✨ Emotime ML Module — MIT-level Machine Learning для эмоций

Advanced emotional intelligence with:
- Adaptive learning algorithms
- Multi-modal sensor fusion with deep learning
- Real-time model optimization
- Personalized emotional patterns recognition

"Intelligence is not about static rules, but adaptive learning" — MIT AI Lab
"""

from .calibration import AdaptiveCalibrator, get_calibrator

try:
    from .adaptive_engine import AdaptiveEmotionalEngine, get_adaptive_engine
    ADAPTIVE_ENGINE_AVAILABLE = True
except ImportError:
    ADAPTIVE_ENGINE_AVAILABLE = False
    AdaptiveEmotionalEngine = None
    get_adaptive_engine = None

try:
    from .feature_learning import DeepFeatureLearner, get_feature_learner
    FEATURE_LEARNING_AVAILABLE = True
except ImportError:
    FEATURE_LEARNING_AVAILABLE = False
    DeepFeatureLearner = None
    get_feature_learner = None

__all__ = [
    'AdaptiveCalibrator',
    'get_calibrator',
    'AdaptiveEmotionalEngine',
    'get_adaptive_engine',
    'DeepFeatureLearner',
    'get_feature_learner',
    'ADAPTIVE_ENGINE_AVAILABLE',
    'FEATURE_LEARNING_AVAILABLE'
]