import subprocess
import sys


def test_importing_backend_ml_does_not_eagerly_load_heavy_model_modules():
    code = """
import sys
import backend.ml
heavy = {
    'backend.ml.anomaly_detector',
    'backend.ml.feature_extractor',
    'backend.ml.model_manager',
    'backend.ml.client',
}
loaded = sorted(name for name in heavy if name in sys.modules)
if loaded:
    raise SystemExit('eager imports: ' + ','.join(loaded))
assert backend.ml.__all__ == [
    'FeatureExtractor',
    'ModelManager',
    'AnomalyDetector',
    'MLInferenceClient',
]
"""
    completed = subprocess.run(
        [sys.executable, "-c", code],
        check=False,
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0, completed.stderr or completed.stdout
