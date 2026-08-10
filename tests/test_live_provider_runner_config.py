import importlib.util
from pathlib import Path

import pytest


SCRIPT_PATH = Path(__file__).resolve().parents[1] / "scripts" / "run-live-provider-trace.py"


def _load_runner_module():
    spec = importlib.util.spec_from_file_location("liminal_live_provider_runner", SCRIPT_PATH)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_gonka_provider_settings_use_broker_env(monkeypatch):
    module = _load_runner_module()
    monkeypatch.setenv("GONKA_BROKER_API_KEY", "test-key")
    monkeypatch.setenv("GONKA_BROKER_URL", "https://broker.example/v1/")

    api_key, base_url = module._provider_settings("gonka")

    assert api_key == "test-key"
    assert base_url == "https://broker.example/v1"


def test_gonka_provider_settings_support_legacy_aliases(monkeypatch):
    module = _load_runner_module()
    monkeypatch.delenv("GONKA_BROKER_API_KEY", raising=False)
    monkeypatch.delenv("GONKA_BROKER_URL", raising=False)
    monkeypatch.setenv("GONKA_API_KEY", "legacy-key")
    monkeypatch.setenv("GONKA_BASE_URL", "https://legacy.example/v1")

    api_key, base_url = module._provider_settings("gonka")

    assert api_key == "legacy-key"
    assert base_url == "https://legacy.example/v1"


def test_gonka_provider_settings_fail_closed_without_key(monkeypatch):
    module = _load_runner_module()
    monkeypatch.delenv("GONKA_BROKER_API_KEY", raising=False)
    monkeypatch.delenv("GONKA_API_KEY", raising=False)
    monkeypatch.setenv("GONKA_BROKER_URL", "https://broker.example/v1")

    with pytest.raises(RuntimeError, match="GONKA_BROKER_API_KEY"):
        module._provider_settings("gonka")


def test_gonka_provider_settings_fail_closed_without_url(monkeypatch):
    module = _load_runner_module()
    monkeypatch.setenv("GONKA_BROKER_API_KEY", "test-key")
    monkeypatch.delenv("GONKA_BROKER_URL", raising=False)
    monkeypatch.delenv("GONKA_BASE_URL", raising=False)

    with pytest.raises(RuntimeError, match="GONKA_BROKER_URL"):
        module._provider_settings("gonka")
