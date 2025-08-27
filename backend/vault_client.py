"""Модуль для работы с HashiCorp Vault."""
import os
from functools import lru_cache
from typing import Dict, Optional

import hvac
from pydantic import BaseModel


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
        except hvac.exceptions.VaultError as e:
            raise ValueError(f"Ошибка при чтении секрета: {e}")

    def write_secret(self, path: str, data: Dict) -> None:
        """Запись секрета."""
        try:
            self.client.secrets.kv.v2.create_or_update_secret(
                path=self._get_full_path(path),
                secret=data,
                mount_point=self.config.mount_point
            )
        except hvac.exceptions.VaultError as e:
            raise ValueError(f"Ошибка при записи секрета: {e}")

    def delete_secret(self, path: str) -> None:
        """Удаление секрета."""
        try:
            self.client.secrets.kv.v2.delete_metadata_and_all_versions(
                path=self._get_full_path(path),
                mount_point=self.config.mount_point
            )
        except hvac.exceptions.VaultError as e:
            raise ValueError(f"Ошибка при удалении секрета: {e}")


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
