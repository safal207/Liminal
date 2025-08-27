"""Модуль для нагрузочного тестирования безопасности."""
import asyncio
import random
import string
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
from typing import List, Optional

import aiohttp
import pytest
from locust import HttpUser, TaskSet, between, task

# Тестовые данные
SQL_INJECTION_PAYLOADS = [
    "' OR '1'='1",
    "'; DROP TABLE users; --",
    "' UNION SELECT * FROM users --",
]

XSS_PAYLOADS = [
    "<script>alert('xss')</script>",
    "javascript:alert('xss')",
    "<img src=x onerror=alert('xss')>",
]

PATH_TRAVERSAL_PAYLOADS = [
    "../../../etc/passwd",
    "..\\..\\..\\windows\\system32\\config\\sam",
    "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",
]

class SecurityLoadTest(HttpUser):
    """Класс для нагрузочного тестирования безопасности."""
    
    wait_time = between(1, 3)
    
    def on_start(self):
        """Инициализация перед началом теста."""
        self.valid_token = self.get_valid_token()
    
    def get_valid_token(self) -> str:
        """Получение валидного токена."""
        response = self.client.post("/token", {
            "username": "test_user",
            "password": "test_password"
        })
        return response.json()["access_token"]
    
    @task(1)
    def test_sql_injection(self):
        """Тест на SQL инъекции."""
        payload = random.choice(SQL_INJECTION_PAYLOADS)
        self.client.get(f"/api/users?id={payload}")
    
    @task(1)
    def test_xss(self):
        """Тест на XSS."""
        payload = random.choice(XSS_PAYLOADS)
        self.client.post("/api/comments", json={
            "content": payload
        })
    
    @task(1)
    def test_path_traversal(self):
        """Тест на Path Traversal."""
        payload = random.choice(PATH_TRAVERSAL_PAYLOADS)
        self.client.get(f"/api/files?path={payload}")
    
    @task(2)
    def test_brute_force(self):
        """Тест на брутфорс."""
        password = ''.join(random.choices(string.ascii_letters + string.digits, k=8))
        self.client.post("/token", {
            "username": "admin",
            "password": password
        })
    
    @task(2)
    def test_rate_limiting(self):
        """Тест на rate limiting."""
        for _ in range(100):
            self.client.get("/api/users", headers={
                "Authorization": f"Bearer {self.valid_token}"
            })

class AsyncSecurityTest:
    """Класс для асинхронного тестирования безопасности."""
    
    def __init__(self, base_url: str, num_users: int = 100):
        """Инициализация тестера."""
        self.base_url = base_url
        self.num_users = num_users
        self.session = aiohttp.ClientSession()
    
    async def close(self):
        """Закрытие сессии."""
        await self.session.close()
    
    async def test_ddos(self):
        """Тест на DDoS атаки."""
        tasks = []
        for _ in range(self.num_users):
            tasks.append(self.session.get(f"{self.base_url}/api/health"))
        
        await asyncio.gather(*tasks)
    
    async def test_credential_stuffing(self, credentials: List[tuple]):
        """Тест на credential stuffing."""
        tasks = []
        for username, password in credentials:
            tasks.append(
                self.session.post(
                    f"{self.base_url}/token",
                    json={"username": username, "password": password}
                )
            )
        
        await asyncio.gather(*tasks)
    
    async def test_concurrent_auth(self):
        """Тест на конкурентную аутентификацию."""
        tasks = []
        for _ in range(self.num_users):
            tasks.append(
                self.session.post(
                    f"{self.base_url}/token",
                    json={
                        "username": "test_user",
                        "password": "test_password"
                    }
                )
            )
        
        await asyncio.gather(*tasks)

@pytest.mark.asyncio
async def test_security_load():
    """Запуск нагрузочного тестирования безопасности."""
    base_url = "http://localhost:8000"
    tester = AsyncSecurityTest(base_url)
    
    try:
        # DDoS тест
        await tester.test_ddos()
        
        # Credential stuffing тест
        credentials = [
            ("user1", "pass1"),
            ("user2", "pass2"),
            ("admin", "admin123")
        ]
        await tester.test_credential_stuffing(credentials)
        
        # Тест конкурентной аутентификации
        await tester.test_concurrent_auth()
        
    finally:
        await tester.close()

if __name__ == "__main__":
    # Запуск через locust
    # locust -f security_load_test.py --host=http://localhost:8000
    pass
