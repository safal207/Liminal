"""Модуль для работы с HashiCorp Vault."""
import importlib
import os
from functools import lru_cache
from typing import Dict, Optional

try:  # pragma: no cover - тонкая проверка импорта
    import hvac  # type: ignore
except ModuleNotFoundError:  # pragma: no cover - зависит от окружения
    hvac = None  # type: ignore

from pydantic import BaseModel


class VaultUnavailableError(RuntimeError):
    """Исключение при отсутствии клиента Vault."""

    def __init__(self) -> None:
        super().__init__(
            "hvac library is required to interact with Vault. "
            "Install the optional dependency or override get_vault_client() "
            "with a test double."
        )


class _FallbackVaultError(Exception):
    """Запасной тип исключения при отсутствии hvac."""


if hvac is not None:  # pragma: no branch - выбираем тип исключения
    VaultError = hvac.exceptions.VaultError  # type: ignore[attr-defined]
else:  # pragma: no cover - используется только без hvac
    VaultError = _FallbackVaultError


def _ensure_hvac_available():
    """Гарантирует наличие клиента hvac, подгружая его при необходимости."""

    global hvac, VaultError
    if hvac is None:  # pragma: no branch - проверяем только при отсутствии
        hvac = importlib.import_module("hvac")  # type: ignore[assignment]
        VaultError = getattr(  # type: ignore[assignment]
            hvac.exceptions, "VaultError", _FallbackVaultError  # type: ignore[attr-defined]
        )


class VaultConfig(BaseModel):
    """Конфигурация подключения к Vault."""
    url: str = "http://vault:8200"
    token: str = os.getenv("VAULT_TOKEN", "dev-token-only")
    mount_point: str = "secret"
    path_prefix: str = "liminal"


class VaultClient:
    """Клиент для работы с Vault."""

    def __init__(self, config: Optional[VaultConfig] = None):
        """Инициализация клиента."""
        self.config = config or VaultConfig()
        _ensure_hvac_available()
        if hvac is None:  # pragma: no cover - защита на случай неудачного импорта
            raise VaultUnavailableError()
        self.client = hvac.Client(
            url=self.config.url,
            token=self.config.token
        )
        
    def _get_full_path(self, path: str) -> str:
        """Получить полный путь в Vault."""
        return f"{self.config.path_prefix}/{path}"

    def read_secret(self, path: str) -> Dict:
        """Чтение секрета."""
        try:
            secret = self.client.secrets.kv.v2.read_secret_version(
                path=self._get_full_path(path),
                mount_point=self.config.mount_point
            )
            return secret["data"]["data"]
        except VaultError as e:
            raise ValueError(f"Ошибка при чтении секрета: {e}")
        except Exception as e:  # pragma: no cover - защита для тестовых заглушек
            raise ValueError(f"Ошибка при чтении секрета: {e}") from e

    def write_secret(self, path: str, data: Dict) -> None:
        """Запись секрета."""
        try:
            self.client.secrets.kv.v2.create_or_update_secret(
                path=self._get_full_path(path),
                secret=data,
                mount_point=self.config.mount_point
            )
        except VaultError as e:
            raise ValueError(f"Ошибка при записи секрета: {e}")
        except Exception as e:  # pragma: no cover - защита для тестовых заглушек
            raise ValueError(f"Ошибка при записи секрета: {e}") from e

    def delete_secret(self, path: str) -> None:
        """Удаление секрета."""
        try:
            self.client.secrets.kv.v2.delete_metadata_and_all_versions(
                path=self._get_full_path(path),
                mount_point=self.config.mount_point
            )
        except VaultError as e:
            raise ValueError(f"Ошибка при удалении секрета: {e}")
        except Exception as e:  # pragma: no cover - защита для тестовых заглушек
            raise ValueError(f"Ошибка при удалении секрета: {e}") from e


@lru_cache()
def get_vault_client() -> VaultClient:
    """Получить экземпляр клиента Vault."""
    return VaultClient()


# Примеры использования
async def initialize_vault():
    """Инициализация начальных секретов."""
    client = get_vault_client()
    
    # Сохранение секретов для Neo4j
    client.write_secret("backend/neo4j", {
        "uri": "bolt://neo4j:7687",
        "user": "neo4j",
        "password": "secure-password-here"
    })
    
    # Сохранение секретов для Redis
    client.write_secret("backend/redis", {
        "host": "redis",
        "port": 6379,
        "password": "secure-redis-password"
    })
    
    # API ключи
    client.write_secret("shared/api-keys", {
        "openai": "sk-...",
        "anthropic": "sk-..."
    })
