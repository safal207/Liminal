"""Rate limiting middleware for LIMINAL API."""
from collections import defaultdict
from datetime import datetime, timedelta
from typing import Dict, Tuple
import asyncio

from fastapi import HTTPException, Request, status
from fastapi.responses import JSONResponse


class RateLimiter:
    """Simple in-memory rate limiter.

    For production, use Redis-based rate limiting.
    """

    def __init__(self):
        # Store: {ip_address: [(timestamp, endpoint), ...]}
        self._requests: Dict[str, list] = defaultdict(list)
        self._lock = asyncio.Lock()

        # Rate limits per endpoint (requests per minute)
        self.limits = {
            "/auth/login": 5,  # Max 5 login attempts per minute
            "/token": 5,
            "/auth/register": 3,  # Max 3 registrations per minute
            "default": 60,  # Default: 60 requests per minute
        }

    async def is_rate_limited(
        self, client_ip: str, endpoint: str
    ) -> Tuple[bool, int, int]:
        """Check if request should be rate limited.

        Returns:
            (is_limited, requests_made, limit)
        """
        async with self._lock:
            now = datetime.utcnow()
            one_minute_ago = now - timedelta(minutes=1)

            # Get limit for this endpoint
            limit = self.limits.get(endpoint, self.limits["default"])

            # Clean old requests
            if client_ip in self._requests:
                self._requests[client_ip] = [
                    (ts, ep)
                    for ts, ep in self._requests[client_ip]
                    if ts > one_minute_ago
                ]

            # Count requests to this endpoint in last minute
            requests_made = sum(
                1
                for ts, ep in self._requests[client_ip]
                if ep == endpoint and ts > one_minute_ago
            )

            # Check if over limit
            is_limited = requests_made >= limit

            if not is_limited:
                # Record this request
                self._requests[client_ip].append((now, endpoint))

            return is_limited, requests_made, limit

    async def cleanup_old_requests(self):
        """Periodic cleanup of old request records."""
        while True:
            await asyncio.sleep(300)  # Cleanup every 5 minutes
            async with self._lock:
                now = datetime.utcnow()
                one_hour_ago = now - timedelta(hours=1)

                # Remove all requests older than 1 hour
                for ip in list(self._requests.keys()):
                    self._requests[ip] = [
                        (ts, ep) for ts, ep in self._requests[ip] if ts > one_hour_ago
                    ]
                    # Remove empty entries
                    if not self._requests[ip]:
                        del self._requests[ip]


# Global rate limiter instance
rate_limiter = RateLimiter()


async def rate_limit_middleware(request: Request, call_next):
    """Middleware to enforce rate limiting."""

    # Get client IP
    client_ip = request.client.host if request.client else "unknown"

    # Get endpoint path
    endpoint = request.url.path

    # Check if rate limited
    is_limited, requests_made, limit = await rate_limiter.is_rate_limited(
        client_ip, endpoint
    )

    if is_limited:
        return JSONResponse(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            content={
                "error": "Rate limit exceeded",
                "message": f"Too many requests to {endpoint}. "
                f"Limit: {limit} requests per minute. "
                f"Try again in 60 seconds.",
                "requests_made": requests_made,
                "limit": limit,
                "retry_after": 60,
            },
            headers={"Retry-After": "60"},
        )

    # Add rate limit headers to response
    response = await call_next(request)
    response.headers["X-RateLimit-Limit"] = str(limit)
    response.headers["X-RateLimit-Remaining"] = str(max(0, limit - requests_made - 1))
    response.headers["X-RateLimit-Reset"] = str(60)

    return response
