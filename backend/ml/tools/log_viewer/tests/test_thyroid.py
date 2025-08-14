import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).parents[2]))
from endocrine import STORE_PATH, ThyroidSystem


def test_charge_and_release(tmp_path, monkeypatch):
    monkeypatch.setattr(
        "endocrine.STORE_PATH", tmp_path / "thyroid.json", raising=False
    )
    monkeypatch.setattr(ThyroidSystem, "_load", lambda self: None, raising=True)
    monkeypatch.setattr(ThyroidSystem, "_save", lambda self: None, raising=True)
    thy = ThyroidSystem(threshold=3, decay=0)
    thy.update_from_error_count(1)
    assert thy.charge == 1 and not thy.should_release()
    thy.update_from_error_count(3)
    assert thy.charge == 3 and thy.should_release()
    evt = thy.release()
    assert evt["type"] == "insight_hormone"
    assert thy.charge == 0
