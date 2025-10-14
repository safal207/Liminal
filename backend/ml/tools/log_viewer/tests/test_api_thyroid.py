import importlib
import sys
from pathlib import Path


# Ensure the repository root (which contains the ``backend`` package) is available on
# the Python path when pytest sets ``rootdir`` to ``backend/``. Without this adjustment
# the import below fails with ``ModuleNotFoundError('backend')`` during test collection
# in isolated CI environments.
REPO_ROOT = Path(__file__).resolve().parents[5]
if str(REPO_ROOT) not in sys.path:
    sys.path.append(str(REPO_ROOT))

app_module = importlib.import_module("backend.ml.tools.log_viewer.app")
app = app_module.app


def test_thyroid_status_endpoint():
    client = app.test_client()
    res = client.get("/api/thyroid_status")
    assert res.status_code == 200
    data = res.get_json()
    assert "charge" in data and "threshold" in data and "ready" in data
