"""
Production-ready Datomic client for LIMINAL.
Provides connection management, error handling, and data operations.
"""

import asyncio
import json
import logging
import os
import time
from datetime import datetime
from typing import Any, Dict, List, Optional, Union
from urllib.parse import urljoin

import httpx
from config import get_database_settings

logger = logging.getLogger(__name__)


class DatomicConnectionError(Exception):
    """Raised when Datomic connection fails."""
    pass


class DatomicQueryError(Exception):
    """Raised when Datomic query fails."""
    pass


class DatomicTransactionError(Exception):
    """Raised when Datomic transaction fails."""
    pass


class DatomicClient:
    """
    Production-ready Datomic client using REST API.
    
    Provides async operations, connection pooling, error handling,
    and automatic retry mechanisms for reliable data access.
    """

    def __init__(
        self,
        uri: Optional[str] = None,
        db_name: Optional[str] = None,
        storage_type: Optional[str] = None,
        connection_timeout: int = 30,
        max_retries: int = 3,
        retry_delay: float = 1.0,
    ):
        """
        Initialize Datomic client with configuration.

        Args:
            uri: Datomic peer server URI
            db_name: Database name
            storage_type: Storage type (dev, pro, etc.)
            connection_timeout: Connection timeout in seconds
            max_retries: Maximum retry attempts
            retry_delay: Delay between retries in seconds
        """
        settings = get_database_settings()
        
        self.uri = uri or settings.datomic_uri
        self.db_name = db_name or settings.datomic_db_name
        self.storage_type = storage_type or settings.datomic_storage_type
        self.connection_timeout = connection_timeout
        self.max_retries = max_retries
        self.retry_delay = retry_delay
        
        # HTTP client for REST API calls
        self.http_client: Optional[httpx.AsyncClient] = None
        self.connected = False
        
        # Connection state
        self._connection_attempts = 0
        self._last_connection_time: Optional[float] = None
        
        logger.info(
            "Datomic client initialized",
            extra={
                "uri": self.uri,
                "db_name": self.db_name,
                "storage_type": self.storage_type
            }
        )

    async def connect(self) -> bool:
        """
        Establish connection to Datomic peer server.
        
        Returns:
            True if connection successful, False otherwise
        """
        try:
            self._connection_attempts += 1
            
            # Create HTTP client with timeout and retry configuration
            self.http_client = httpx.AsyncClient(
                timeout=httpx.Timeout(self.connection_timeout),
                limits=httpx.Limits(max_connections=10, max_keepalive_connections=5)
            )
            
            # Test connection with a simple query
            test_url = urljoin(self.uri, "/")
            response = await self.http_client.get(test_url)
            response.raise_for_status()
            
            # Create database if it doesn't exist
            await self._ensure_database_exists()
            
            self.connected = True
            self._last_connection_time = time.time()
            
            logger.info(
                "Successfully connected to Datomic",
                extra={
                    "uri": self.uri,
                    "db_name": self.db_name,
                    "attempt": self._connection_attempts
                }
            )
            return True
            
        except Exception as e:
            logger.error(
                "Failed to connect to Datomic",
                extra={
                    "error": str(e),
                    "uri": self.uri,
                    "attempt": self._connection_attempts
                }
            )
            self.connected = False
            if self.http_client:
                await self.http_client.aclose()
                self.http_client = None
            raise DatomicConnectionError(f"Connection failed: {e}")

    async def _ensure_database_exists(self) -> bool:
        """
        Ensure the database exists, create if necessary.
        
        Returns:
            True if database exists or was created successfully
        """
        try:
            # Try to list databases to check if ours exists
            db_list_url = urljoin(self.uri, "/data/")
            response = await self.http_client.get(db_list_url)
            
            if response.status_code == 200:
                databases = response.json()
                if self.db_name not in databases:
                    # Create database
                    create_url = urljoin(self.uri, f"/data/{self.db_name}/")
                    create_response = await self.http_client.put(
                        create_url,
                        json={"db-name": self.db_name}
                    )
                    create_response.raise_for_status()
                    
                    logger.info(
                        "Created new Datomic database",
                        extra={"db_name": self.db_name}
                    )
                return True
            else:
                logger.warning(
                    "Could not verify database existence",
                    extra={"status_code": response.status_code}
                )
                return False
                
        except Exception as e:
            logger.error(
                "Error ensuring database exists",
                extra={"error": str(e), "db_name": self.db_name}
            )
            # Don't fail connection if we can't verify database
            return True

    async def _execute_with_retry(
        self, 
        operation: str, 
        func, 
        *args, 
        **kwargs
    ) -> Any:
        """
        Execute operation with retry logic.
        
        Args:
            operation: Operation name for logging
            func: Function to execute
            *args: Function arguments
            **kwargs: Function keyword arguments
            
        Returns:
            Operation result
        """
        last_exception = None
        
        for attempt in range(self.max_retries):
            try:
                if not self.connected:
                    await self.connect()
                
                return await func(*args, **kwargs)
                
            except (httpx.ConnectError, httpx.TimeoutException) as e:
                last_exception = e
                logger.warning(
                    f"Retry {attempt + 1}/{self.max_retries} for {operation}",
                    extra={"error": str(e)}
                )
                
                if attempt < self.max_retries - 1:
                    await asyncio.sleep(self.retry_delay * (2 ** attempt))  # Exponential backoff
                    
            except Exception as e:
                logger.error(
                    f"Non-retryable error in {operation}",
                    extra={"error": str(e)}
                )
                raise
        
        raise DatomicConnectionError(
            f"Operation {operation} failed after {self.max_retries} attempts: {last_exception}"
        )

    async def transact(self, data: List[Dict]) -> Dict:
        """
        Execute transaction with retry logic.

        Args:
            data: List of transaction data

        Returns:
            Transaction result
        """
        async def _transact():
            url = urljoin(self.uri, f"/data/{self.db_name}/")
            payload = {
                "tx-data": data
            }
            
            response = await self.http_client.post(url, json=payload)
            response.raise_for_status()
            
            result = response.json()
            logger.debug(
                "Transaction completed",
                extra={
                    "tx_id": result.get("tx"),
                    "data_count": len(data)
                }
            )
            return result
        
        return await self._execute_with_retry("transact", _transact)

    async def query(self, query: str, params: Optional[Dict] = None) -> List[Dict]:
        """
        Execute query with retry logic.

        Args:
            query: Datalog query string
            params: Query parameters

        Returns:
            Query results
        """
        async def _query():
            url = urljoin(self.uri, f"/api/query")
            payload = {
                "query": query,
                "database": {
                    "database/alias": self.db_name
                }
            }
            
            if params:
                payload["args"] = params
            
            response = await self.http_client.post(url, json=payload)
            response.raise_for_status()
            
            result = response.json()
            logger.debug(
                "Query executed",
                extra={
                    "query_hash": hash(query),
                    "result_count": len(result) if isinstance(result, list) else 1
                }
            )
            return result
        
        return await self._execute_with_retry("query", _query)

    async def add_emotion_entry(
        self,
        user_id: str,
        emotion: str,
        intensity: float,
        timestamp: Optional[datetime] = None,
        metadata: Optional[Dict] = None,
    ) -> Dict:
        """
        Add emotion entry with enhanced metadata.

        Args:
            user_id: User identifier
            emotion: Emotion name
            intensity: Emotion intensity (0.0 to 1.0)
            timestamp: Entry timestamp
            metadata: Additional metadata

        Returns:
            Transaction result
        """
        if not timestamp:
            timestamp = datetime.utcnow()
        
        # Validate intensity
        if not 0.0 <= intensity <= 1.0:
            raise ValueError("Intensity must be between 0.0 and 1.0")
        
        entry_data = {
            "db/id": f"emotion-{int(time.time() * 1000)}",
            "entry/user": user_id,
            "entry/emotion": emotion,
            "entry/intensity": intensity,
            "entry/timestamp": timestamp.isoformat(),
            "entry/type": "emotion"
        }
        
        # Add metadata if provided
        if metadata:
            entry_data["entry/metadata"] = json.dumps(metadata)
        
        data = [entry_data]
        
        try:
            result = await self.transact(data)
            logger.info(
                "Emotion entry added",
                extra={
                    "user_id": user_id,
                    "emotion": emotion,
                    "intensity": intensity
                }
            )
            return result
        except Exception as e:
            logger.error(
                "Failed to add emotion entry",
                extra={
                    "user_id": user_id,
                    "emotion": emotion,
                    "error": str(e)
                }
            )
            raise DatomicTransactionError(f"Failed to add emotion entry: {e}")

    async def get_emotion_history(
        self, 
        user_id: str, 
        limit: int = 100,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        emotion_filter: Optional[str] = None
    ) -> List[Dict]:
        """
        Get emotion history with filtering options.

        Args:
            user_id: User identifier
            limit: Maximum number of entries
            start_time: Filter start time
            end_time: Filter end time
            emotion_filter: Filter by specific emotion

        Returns:
            List of emotion entries
        """
        # Build query with filters
        query_parts = [
            "[:find ?e ?emotion ?intensity ?timestamp ?metadata",
            " :in $ ?user",
            " :where",
            " [?e :entry/user ?user]",
            " [?e :entry/emotion ?emotion]",
            " [?e :entry/intensity ?intensity]",
            " [?e :entry/timestamp ?timestamp]",
            " [?e :entry/type \"emotion\"]",
            " [(get-else $ ?e :entry/metadata \"\") ?metadata]"
        ]
        
        params = {"?user": user_id}
        
        # Add time filters
        if start_time:
            query_parts.append(" [(>= ?timestamp ?start_time)]")
            params["?start_time"] = start_time.isoformat()
        
        if end_time:
            query_parts.append(" [(<= ?timestamp ?end_time)]")
            params["?end_time"] = end_time.isoformat()
        
        if emotion_filter:
            query_parts.append(" [(= ?emotion ?emotion_filter)]")
            params["?emotion_filter"] = emotion_filter
        
        query_parts.append("]")
        query = "".join(query_parts)
        
        try:
            results = await self.query(query, params)
            
            # Process results
            processed_results = []
            for result in results[:limit]:  # Apply limit
                entry = {
                    "id": str(result[0]),
                    "emotion": result[1],
                    "intensity": float(result[2]),
                    "timestamp": result[3],
                }
                
                # Parse metadata if present
                if result[4]:
                    try:
                        entry["metadata"] = json.loads(result[4])
                    except json.JSONDecodeError:
                        entry["metadata"] = {}
                else:
                    entry["metadata"] = {}
                
                processed_results.append(entry)
            
            logger.debug(
                "Emotion history retrieved",
                extra={
                    "user_id": user_id,
                    "count": len(processed_results),
                    "limit": limit
                }
            )
            
            return processed_results
            
        except Exception as e:
            logger.error(
                "Failed to get emotion history",
                extra={
                    "user_id": user_id,
                    "error": str(e)
                }
            )
            raise DatomicQueryError(f"Failed to get emotion history: {e}")

    async def health_check(self) -> Dict[str, Any]:
        """
        Perform health check on Datomic connection.
        
        Returns:
            Health status information
        """
        try:
            if not self.connected or not self.http_client:
                return {
                    "status": "disconnected",
                    "connected": False,
                    "last_connection": self._last_connection_time
                }
            
            # Simple test query
            start_time = time.time()
            await self.query("[:find ?e :where [?e :db/ident]]")
            response_time = time.time() - start_time
            
            return {
                "status": "healthy",
                "connected": True,
                "response_time_ms": round(response_time * 1000, 2),
                "last_connection": self._last_connection_time,
                "connection_attempts": self._connection_attempts
            }
            
        except Exception as e:
            return {
                "status": "unhealthy",
                "connected": False,
                "error": str(e),
                "last_connection": self._last_connection_time,
                "connection_attempts": self._connection_attempts
            }

    async def close(self):
        """
        Close Datomic connection and cleanup resources.
        """
        if self.http_client:
            await self.http_client.aclose()
            self.http_client = None
        
        self.connected = False
        logger.info("Datomic connection closed")

    async def __aenter__(self):
        """Async context manager entry."""
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit."""
        await self.close()


# Factory function for dependency injection
async def get_datomic_client() -> DatomicClient:
    """
    Factory function to create and configure Datomic client.
    
    Returns:
        Configured DatomicClient instance
    """
    return DatomicClient()


# Example usage and testing
if __name__ == "__main__":
    import asyncio
    
    async def example_usage():
        """Example of how to use the DatomicClient."""
        
        # Create client with async context manager
        async with DatomicClient() as client:
            try:
                # Health check
                health = await client.health_check()
                print(f"🏥 Health status: {health}")
                
                # Add some test data
                result = await client.add_emotion_entry(
                    user_id="user-123",
                    emotion="joy",
                    intensity=0.8,
                    metadata={"source": "test", "context": "example"}
                )
                print(f"📝 Added emotion entry: {result}")
                
                # Query emotion history
                history = await client.get_emotion_history(
                    "user-123", 
                    limit=10
                )
                print(f"📚 Emotion history: {len(history)} entries")
                
                # Print first entry details
                if history:
                    print(f"🎭 Latest emotion: {history[0]}")
                
            except Exception as e:
                print(f"❌ Error during example: {e}")
    
    # Run example
    asyncio.run(example_usage())
