import importlib
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parents[2]))
app_module = importlib.import_module("backend.ml.tools.log_viewer.app")
app = app_module.app


def test_thyroid_status_endpoint():
    client = app.test_client()
    res = client.get("/api/thyroid_status")
    assert res.status_code == 200
    data = res.get_json()
    assert "charge" in data
    assert "threshold" in data
    assert "ready" in data
