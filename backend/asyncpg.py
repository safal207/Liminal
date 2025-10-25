"""
Simple fallback implementation of asyncpg for testing.
This provides mock PostgreSQL functionality when asyncpg is not available.
"""

import asyncio
from typing import Any, Dict, List, Optional

class MockConnection:
    """Mock PostgreSQL connection."""
    
    def __init__(self):
        self.closed = False
    
    async def execute(self, query: str, *args):
        """Mock execute method."""
        await asyncio.sleep(0.01)  # Simulate network delay
        return "MOCK_EXECUTE"
    
    async def fetch(self, query: str, *args) -> List[Dict[str, Any]]:
        """Mock fetch method."""
        await asyncio.sleep(0.01)
        return [{"id": 1, "data": "mock_data"}]
    
    async def fetchrow(self, query: str, *args) -> Optional[Dict[str, Any]]:
        """Mock fetchrow method."""
        await asyncio.sleep(0.01)
        return {"id": 1, "data": "mock_data"}
    
    async def fetchval(self, query: str, *args) -> Any:
        """Mock fetchval method."""
        await asyncio.sleep(0.01)
        return "mock_value"
    
    async def close(self):
        """Mock close method."""
        self.closed = True
    
    def __await__(self):
        """Make connection awaitable."""
        return self
    
    async def __aenter__(self):
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.close()

class MockPool:
    """Mock PostgreSQL connection pool."""
    
    def __init__(self, dsn: str, **kwargs):
        self.dsn = dsn
        self.min_size = kwargs.get('min_size', 1)
        self.max_size = kwargs.get('max_size', 10)
        self.closed = False
    
    async def acquire(self) -> MockConnection:
        """Acquire a connection from the pool."""
        if self.closed:
            raise Exception("Pool is closed")
        return MockConnection()
    
    async def release(self, connection: MockConnection):
        """Release a connection back to the pool."""
        await connection.close()
    
    async def close(self):
        """Close the pool."""
        self.closed = True
    
    async def __aenter__(self):
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.close()

async def connect(dsn: str = None, **kwargs) -> MockConnection:
    """Create a mock PostgreSQL connection."""
    await asyncio.sleep(0.01)  # Simulate connection time
    return MockConnection()

async def create_pool(dsn: str = None, **kwargs) -> MockPool:
    """Create a mock PostgreSQL connection pool."""
    await asyncio.sleep(0.01)  # Simulate pool creation time
    return MockPool(dsn, **kwargs)

# Exception classes for compatibility
class PostgresError(Exception):
    """Mock PostgreSQL error."""
    pass

class ConnectionDoesNotExistError(PostgresError):
    """Mock connection error."""
    pass