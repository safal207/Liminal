"""
Redis Client module for Resonance Liminal backend
Provides Redis connection, PubSub, and caching functionality
"""

import os

from loguru import logger

import redis


class RedisClient:
    def __init__(self, host="redis", port=6379, db=0, password=None):
        """Initialize Redis connection for various backend services"""
        # Try to get from environment or use default
        redis_host = os.environ.get("REDIS_HOST", host)
        redis_port = int(os.environ.get("REDIS_PORT", port))
        redis_db = int(os.environ.get("REDIS_DB", db))
        redis_password = os.environ.get("REDIS_PASSWORD", password)

        # For local test mode, use localhost
        if redis_host == "redis" and os.environ.get("TEST_MODE", "false").lower() == "true":
            redis_host = "localhost"

        try:
            self.client = redis.Redis(
                host=redis_host,
                port=redis_port,
                db=redis_db,
                password=redis_password,
                decode_responses=True,
                socket_timeout=5,
                socket_connect_timeout=5,
            )
            # Test connection
            self.client.ping()
            logger.info(f"Connected to Redis at {redis_host}:{redis_port}, db={redis_db}")
        except redis.ConnectionError as e:
            logger.warning(f"Redis connection failed: {e} - Using dummy mode")
            self.client = DummyRedis()

    def get(self, key):
        """Get value from Redis"""
        return self.client.get(key)

    def set(self, key, value, expiry=None):
        """Set value in Redis with optional expiry time in seconds"""
        return self.client.set(key, value, ex=expiry)

    def delete(self, key):
        """Delete a key from Redis"""
        return self.client.delete(key)

    def publish(self, channel, message):
        """Publish message to channel"""
        return self.client.publish(channel, message)

    def subscribe(self, channel):
        """Create subscription to channel"""
        pubsub = self.client.pubsub()
        pubsub.subscribe(channel)
        return pubsub

    def exists(self, key):
        """Check if key exists"""
        return bool(self.client.exists(key))

    def ttl(self, key):
        """Get key time-to-live in seconds"""
        return self.client.ttl(key)

    def close(self):
        """Close Redis connection"""
        if hasattr(self, "client") and hasattr(self.client, "close"):
            self.client.close()


class DummyRedis:
    """Dummy Redis client for when Redis is unavailable"""

    def __init__(self):
        self.data = {}
        self.logger = logger
        self.logger.warning("Using DummyRedis - data will not persist!")

    def ping(self):
        return True

    def get(self, key):
        return self.data.get(key)

    def set(self, key, value, ex=None):
        self.data[key] = value
        return True

    def delete(self, key):
        if key in self.data:
            del self.data[key]
            return 1
        return 0

    def publish(self, channel, message):
        self.logger.debug(f"DummyRedis: Would publish '{message}' to '{channel}'")
        return 0

    def pubsub(self):
        return DummyPubSub(self.logger)

    def exists(self, key):
        return key in self.data

    def ttl(self, key):
        return -1  # No expiration in dummy mode

    def close(self):
        pass


class DummyPubSub:
    """Dummy PubSub client for when Redis is unavailable"""

    def __init__(self, logger):
        self.channels = {}
        self.logger = logger

    def subscribe(self, channel):
        self.channels[channel] = True
        self.logger.debug(f"DummyPubSub: Subscribed to '{channel}'")

    def get_message(self, timeout=None):
        return None
