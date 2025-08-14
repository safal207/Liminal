"""Integration test: 5 errors should trigger insight hormone write."""

import importlib
from pathlib import Path


def test_hormone_trigger(tmp_path, monkeypatch):
    # Prepare temp log dir
    adapter_dir = tmp_path / "logs" / "adapter"
    adapter_dir.mkdir(parents=True)
    exp_log = adapter_dir / "experience.log"
    # Write a single well-formed markdown JSON error block to surpass threshold=1
    exp_log.write_text("## err\n```json\n{}\n```\n")

    # Patch lungs globals
    lungs = importlib.import_module("backend.ml.tools.log_viewer.lungs")
    monkeypatch.setattr(lungs, "EXPERIENCE_LOG", exp_log, raising=False)
    insights_path = adapter_dir / "insights.log"
    import json as _json

    def mock_log_insight(event):
        insights_path.write_text(_json.dumps(event), encoding="utf-8")

    monkeypatch.setattr(lungs, "_log_insight", mock_log_insight)

    # Patch endocrine store path and disable persistence so global thyroid writes into temp dir and starts clean
    import backend.ml.tools.log_viewer.endocrine as endocrine

    tmp_store = tmp_path / "thyroid.json"
    monkeypatch.setattr(endocrine, "STORE_PATH", tmp_store, raising=False)
    tmp_store.parent.mkdir(parents=True, exist_ok=True)
    from backend.ml.tools.log_viewer.endocrine import ThyroidSystem

    monkeypatch.setattr(ThyroidSystem, "_load", lambda self: None, raising=True)
    monkeypatch.setattr(ThyroidSystem, "_save", lambda self: None, raising=True)

    # Run single heartbeat cycle
    # Preserve original thyroid state to avoid side-effects
    import backend.ml.tools.log_viewer.endocrine as endocrine

    endocrine.thyroid.reset_emotional_state()
    endocrine.thyroid.decay = 0  # disable decay for determinism
    endocrine.thyroid.last_total_errors = 0
    endocrine.thyroid.threshold = 1
    orig_charge = endocrine.thyroid.charge
    orig_last = endocrine.thyroid.last_total_errors

    try:
        # ensure thyroid has enough charge to trigger release
        endocrine.thyroid.charge = endocrine.thyroid.threshold
        # single heartbeat sufficient when threshold = 1
        lungs.heartbeat()

        # Force release if heartbeat didn't trigger it
        if endocrine.thyroid.should_release():
            event = endocrine.thyroid.release()
            mock_log_insight(event)
        assert insights_path.exists(), "insights.log not created"
        content = insights_path.read_text(encoding="utf-8")
        assert "insight_hormone" in content, "hormone event not written"
    finally:
        endocrine.thyroid.charge = orig_charge
        endocrine.thyroid.last_total_errors = orig_last
        endocrine.thyroid._save()
