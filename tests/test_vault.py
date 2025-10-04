"""Тесты для Vault клиента."""
import sys
import types

import pytest
from unittest.mock import MagicMock, patch

if "hvac" not in sys.modules:  # pragma: no cover - ветка зависит от окружения
    try:
        import hvac  # type: ignore
    except ModuleNotFoundError:  # pragma: no cover - среда без hvac
        hvac_stub = types.ModuleType("hvac")

        class _VaultError(Exception):
            """Заглушка для исключения hvac."""

            pass

        hvac_stub.Client = object()
        hvac_stub.exceptions = types.SimpleNamespace(VaultError=_VaultError)
        sys.modules["hvac"] = hvac_stub

from backend.vault_client import VaultClient, VaultConfig, get_vault_client

@pytest.fixture
def mock_hvac_client():
    """Мок для hvac клиента."""
    with patch('hvac.Client') as mock:
        yield mock

@pytest.fixture
def vault_client(mock_hvac_client):
    """Тестовый клиент Vault."""
    config = VaultConfig(
        url="http://test-vault:8200",
        token="test-token",
        mount_point="secret",
        path_prefix="test"
    )
    return VaultClient(config)

def test_read_secret(vault_client, mock_hvac_client):
    """Тест чтения секрета."""
    # Подготовка мока
    mock_response = {
        "data": {
            "data": {
                "test_key": "test_value"
            }
        }
    }
    mock_hvac_client.return_value.secrets.kv.v2.read_secret_version.return_value = mock_response
    
    # Тест
    result = vault_client.read_secret("test/path")
    assert result == {"test_key": "test_value"}
    
    # Проверка вызова
    mock_hvac_client.return_value.secrets.kv.v2.read_secret_version.assert_called_once_with(
        path="test/test/path",
        mount_point="secret"
    )

def test_write_secret(vault_client, mock_hvac_client):
    """Тест записи секрета."""
    # Тестовые данные
    test_data = {"test_key": "test_value"}
    
    # Тест
    vault_client.write_secret("test/path", test_data)
    
    # Проверка вызова
    mock_hvac_client.return_value.secrets.kv.v2.create_or_update_secret.assert_called_once_with(
        path="test/test/path",
        secret=test_data,
        mount_point="secret"
    )

def test_delete_secret(vault_client, mock_hvac_client):
    """Тест удаления секрета."""
    # Тест
    vault_client.delete_secret("test/path")
    
    # Проверка вызова
    mock_hvac_client.return_value.secrets.kv.v2.delete_metadata_and_all_versions.assert_called_once_with(
        path="test/test/path",
        mount_point="secret"
    )

def test_vault_client_error_handling(vault_client, mock_hvac_client):
    """Тест обработки ошибок."""
    # Настройка мока для генерации ошибки
    mock_hvac_client.return_value.secrets.kv.v2.read_secret_version.side_effect = Exception("Test error")
    
    # Проверка вызова исключения
    with pytest.raises(ValueError) as exc_info:
        vault_client.read_secret("test/path")
    assert "Ошибка при чтении секрета" in str(exc_info.value)

@pytest.mark.asyncio
async def test_initialize_vault(vault_client):
    """Тест инициализации Vault."""
    from backend.vault_client import initialize_vault

    # Мокаем метод write_secret
    vault_client.write_secret = MagicMock()

    # Тест
    with patch("backend.vault_client.get_vault_client", return_value=vault_client):
        await initialize_vault()
    
    # Проверяем, что все необходимые секреты были созданы
    calls = vault_client.write_secret.call_args_list
    assert len(calls) == 3  # Проверяем количество вызовов
    
    # Проверяем вызовы для каждого типа секретов
    paths = [call[0][0] for call in calls]
    assert "backend/neo4j" in paths
    assert "backend/redis" in paths
    assert "shared/api-keys" in paths
