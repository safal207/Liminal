import os
import pytest
from unittest.mock import patch, MagicMock

from backend.storage.datomic_adapter import connect, _NoDatomic

def test_connect_raises_error_without_pydatomic():
    """
    Tests that connect() raises _NoDatomic if pydatomic is not installed.
    """
    with patch.dict('sys.modules', {'pydatomic': None}):
        with pytest.raises(_NoDatomic, match="Datomic client not installed"):
            connect()

@patch('backend.storage.datomic_adapter.Datomic')
def test_connect_uses_env_vars(mock_datomic):
    """
    Tests that connect() uses the DATOMIC_BASE_URL and DATOMIC_DB_ALIAS
    environment variables.
    """
    base_url = "http://fake-datomic:1234"
    db_alias = "test/db"

    with patch.dict(os.environ, {'DATOMIC_BASE_URL': base_url, 'DATOMIC_DB_ALIAS': db_alias}):
        connect()

    mock_datomic.assert_called_once_with(base_url, db_alias)

@patch('backend.storage.datomic_adapter.Datomic')
def test_connect_uses_default_values(mock_datomic):
    """
    Tests that connect() uses default values if env vars are not set.
    """
    # Ensure env vars are not set
    with patch.dict(os.environ, {}, clear=True):
        connect()

    mock_datomic.assert_called_once_with(
        "http://localhost:3000/", "mem/test"
    )

def test_adapter_init_does_not_crash():
    """
    Tests that initializing the adapter does not crash the process,
    even if pydatomic is not installed.
    """
    try:
        with patch.dict('sys.modules', {'pydatomic': None}):
            from backend.datomic_client import DatomicClient
            # Initialization should not raise an exception, just print a warning
            DatomicClient()
    except Exception as e:
        pytest.fail(f"DatomicClient initialization crashed: {e}")
