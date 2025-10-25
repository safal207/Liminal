#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Production-ready resilience and error handling patterns for LIMINAL.

Implements circuit breakers, retry mechanisms, bulkheads, and graceful degradation
to ensure system stability and fault tolerance.
"""

import asyncio
import functools
import logging
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Type, Union
from datetime import datetime, timedelta

from config import get_monitoring_settings

logger = logging.getLogger(__name__)


class CircuitState(Enum):
    """Circuit breaker states."""
    CLOSED = "closed"      # Normal operation
    OPEN = "open"          # Failing, rejecting requests
    HALF_OPEN = "half_open"  # Testing if service recovered


@dataclass
class CircuitMetrics:
    """Circuit breaker metrics tracking."""
    total_requests: int = 0
    failed_requests: int = 0
    success_requests: int = 0
    last_failure_time: Optional[float] = None
    last_success_time: Optional[float] = None
    consecutive_failures: int = 0
    consecutive_successes: int = 0


class LiminalException(Exception):
    """Base exception for LIMINAL-specific errors."""
    
    def __init__(self, message: str, error_code: str = None, context: Dict = None):
        super().__init__(message)
        self.message = message
        self.error_code = error_code or self.__class__.__name__
        self.context = context or {}
        self.timestamp = datetime.utcnow()


class DatabaseConnectionError(LiminalException):
    """Database connection related errors."""
    pass


class MLServiceError(LiminalException):
    """ML service related errors."""
    pass


class AuthenticationError(LiminalException):
    """Authentication related errors."""
    pass


class WebSocketError(LiminalException):
    """WebSocket related errors."""
    pass


class CircuitBreakerError(LiminalException):
    """Circuit breaker is open - service unavailable."""
    pass


class CircuitBreaker:
    """
    Circuit breaker implementation for fault tolerance.
    
    Protects services from cascading failures by monitoring
    error rates and temporarily stopping requests when thresholds are exceeded.
    """
    
    def __init__(
        self,
        name: str,
        failure_threshold: int = 5,
        recovery_timeout: float = 60.0,
        expected_exception: Type[Exception] = Exception,
        success_threshold: int = 3,
    ):
        """
        Initialize circuit breaker.
        
        Args:
            name: Circuit breaker identifier
            failure_threshold: Number of failures before opening circuit
            recovery_timeout: Time to wait before attempting recovery
            expected_exception: Exception types that trigger the circuit
            success_threshold: Successes needed in half-open to close circuit
        """
        self.name = name
        self.failure_threshold = failure_threshold
        self.recovery_timeout = recovery_timeout
        self.expected_exception = expected_exception
        self.success_threshold = success_threshold
        
        self.state = CircuitState.CLOSED
        self.metrics = CircuitMetrics()
        self._lock = asyncio.Lock()
        
        logger.info(
            "Circuit breaker initialized",
            extra={
                "circuit_name": name,
                "failure_threshold": failure_threshold,
                "recovery_timeout": recovery_timeout
            }
        )
    
    async def call(self, func: Callable, *args, **kwargs) -> Any:
        """
        Execute function with circuit breaker protection.
        
        Args:
            func: Function to execute
            *args: Function arguments
            **kwargs: Function keyword arguments
            
        Returns:
            Function result
            
        Raises:
            CircuitBreakerError: When circuit is open
        """
        async with self._lock:
            if self.state == CircuitState.OPEN:
                if self._should_attempt_reset():
                    self.state = CircuitState.HALF_OPEN
                    logger.info(f"Circuit {self.name} moved to HALF_OPEN")
                else:
                    raise CircuitBreakerError(
                        f"Circuit breaker {self.name} is OPEN",
                        context={"metrics": self.metrics.__dict__}
                    )
        
        try:
            start_time = time.time()
            
            if asyncio.iscoroutinefunction(func):
                result = await func(*args, **kwargs)
            else:
                result = func(*args, **kwargs)
            
            execution_time = time.time() - start_time
            await self._on_success(execution_time)
            return result
            
        except self.expected_exception as e:
            await self._on_failure(e)
            raise
        except Exception as e:
            # Unexpected exceptions don't trigger circuit breaker
            logger.error(
                f"Unexpected error in circuit {self.name}",
                extra={"error": str(e), "type": type(e).__name__}
            )
            raise
    
    async def _on_success(self, execution_time: float):
        """Handle successful execution."""
        async with self._lock:
            self.metrics.total_requests += 1
            self.metrics.success_requests += 1
            self.metrics.consecutive_successes += 1
            self.metrics.consecutive_failures = 0
            self.metrics.last_success_time = time.time()
            
            if self.state == CircuitState.HALF_OPEN:
                if self.metrics.consecutive_successes >= self.success_threshold:
                    self.state = CircuitState.CLOSED
                    logger.info(f"Circuit {self.name} moved to CLOSED")
    
    async def _on_failure(self, exception: Exception):
        """Handle failed execution."""
        async with self._lock:
            self.metrics.total_requests += 1
            self.metrics.failed_requests += 1
            self.metrics.consecutive_failures += 1
            self.metrics.consecutive_successes = 0
            self.metrics.last_failure_time = time.time()
            
            if (self.state == CircuitState.CLOSED and 
                self.metrics.consecutive_failures >= self.failure_threshold):
                self.state = CircuitState.OPEN
                logger.warning(
                    f"Circuit {self.name} moved to OPEN",
                    extra={
                        "consecutive_failures": self.metrics.consecutive_failures,
                        "failure_threshold": self.failure_threshold,
                        "error": str(exception)
                    }
                )
            elif self.state == CircuitState.HALF_OPEN:
                self.state = CircuitState.OPEN
                logger.warning(f"Circuit {self.name} failed in HALF_OPEN, back to OPEN")
    
    def _should_attempt_reset(self) -> bool:
        """Check if enough time has passed to attempt reset."""
        if self.metrics.last_failure_time is None:
            return False
        return (time.time() - self.metrics.last_failure_time) >= self.recovery_timeout
    
    def get_state(self) -> Dict[str, Any]:
        """Get current circuit breaker state."""
        return {
            "name": self.name,
            "state": self.state.value,
            "metrics": self.metrics.__dict__,
            "failure_rate": (
                self.metrics.failed_requests / max(self.metrics.total_requests, 1)
            ),
        }


class RetryPolicy:
    """Configurable retry policy with different strategies."""
    
    def __init__(
        self,
        max_attempts: int = 3,
        base_delay: float = 1.0,
        max_delay: float = 60.0,
        exponential_base: float = 2.0,
        jitter: bool = True,
        retryable_exceptions: tuple = (Exception,)
    ):
        """
        Initialize retry policy.
        
        Args:
            max_attempts: Maximum retry attempts
            base_delay: Base delay between retries
            max_delay: Maximum delay between retries
            exponential_base: Base for exponential backoff
            jitter: Add random jitter to delays
            retryable_exceptions: Exception types that trigger retries
        """
        self.max_attempts = max_attempts
        self.base_delay = base_delay
        self.max_delay = max_delay
        self.exponential_base = exponential_base
        self.jitter = jitter
        self.retryable_exceptions = retryable_exceptions
    
    def calculate_delay(self, attempt: int) -> float:
        """Calculate delay for given attempt number."""
        delay = self.base_delay * (self.exponential_base ** attempt)
        delay = min(delay, self.max_delay)
        
        if self.jitter:
            import random
            delay = delay * (0.5 + random.random() * 0.5)  # 50-100% of calculated delay
        
        return delay


async def with_retry(
    func: Callable,
    retry_policy: RetryPolicy,
    *args,
    **kwargs
) -> Any:
    """
    Execute function with retry logic.
    
    Args:
        func: Function to execute
        retry_policy: Retry configuration
        *args: Function arguments
        **kwargs: Function keyword arguments
        
    Returns:
        Function result
        
    Raises:
        Last exception if all retries fail
    """
    last_exception = None
    
    for attempt in range(retry_policy.max_attempts):
        try:
            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            else:
                return func(*args, **kwargs)
                
        except retry_policy.retryable_exceptions as e:
            last_exception = e
            
            if attempt == retry_policy.max_attempts - 1:
                # Last attempt failed
                logger.error(
                    "All retry attempts failed",
                    extra={
                        "function": func.__name__,
                        "attempts": retry_policy.max_attempts,
                        "final_error": str(e)
                    }
                )
                break
            
            delay = retry_policy.calculate_delay(attempt)
            logger.warning(
                f"Retry attempt {attempt + 1}/{retry_policy.max_attempts}",
                extra={
                    "function": func.__name__,
                    "error": str(e),
                    "delay": delay
                }
            )
            
            await asyncio.sleep(delay)
        
        except Exception as e:
            # Non-retryable exception
            logger.error(
                "Non-retryable exception occurred",
                extra={
                    "function": func.__name__,
                    "error": str(e),
                    "type": type(e).__name__
                }
            )
            raise
    
    raise last_exception


class Bulkhead:
    """
    Resource isolation pattern to prevent resource exhaustion.
    
    Limits concurrent operations to prevent one failing component
    from exhausting shared resources.
    """
    
    def __init__(self, name: str, max_concurrent: int = 10):
        """
        Initialize bulkhead.
        
        Args:
            name: Bulkhead identifier
            max_concurrent: Maximum concurrent operations
        """
        self.name = name
        self.max_concurrent = max_concurrent
        self.semaphore = asyncio.Semaphore(max_concurrent)
        self.active_count = 0
        self.total_requests = 0
        self.rejected_requests = 0
        
        logger.info(
            f"Bulkhead {name} initialized",
            extra={"max_concurrent": max_concurrent}
        )
    
    async def execute(self, func: Callable, *args, **kwargs) -> Any:
        """
        Execute function with resource isolation.
        
        Args:
            func: Function to execute
            *args: Function arguments
            **kwargs: Function keyword arguments
            
        Returns:
            Function result
            
        Raises:
            BulkheadFullError: When bulkhead is at capacity
        """
        self.total_requests += 1
        
        try:
            # Try to acquire semaphore without blocking
            acquired = self.semaphore.acquire_nowait()
            if not acquired:
                self.rejected_requests += 1
                raise LiminalException(
                    f"Bulkhead {self.name} is at capacity",
                    error_code="BULKHEAD_FULL",
                    context={
                        "max_concurrent": self.max_concurrent,
                        "active_count": self.active_count
                    }
                )
        except asyncio.InvalidStateError:
            self.rejected_requests += 1
            raise LiminalException(
                f"Bulkhead {self.name} is at capacity",
                error_code="BULKHEAD_FULL"
            )
        
        try:
            self.active_count += 1
            
            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            else:
                return func(*args, **kwargs)
        finally:
            self.active_count -= 1
            self.semaphore.release()
    
    def get_stats(self) -> Dict[str, Any]:
        """Get bulkhead statistics."""
        return {
            "name": self.name,
            "max_concurrent": self.max_concurrent,
            "active_count": self.active_count,
            "total_requests": self.total_requests,
            "rejected_requests": self.rejected_requests,
            "rejection_rate": (
                self.rejected_requests / max(self.total_requests, 1)
            )
        }


class GracefulDegradation:
    """
    Graceful degradation handler for maintaining service availability
    when dependencies fail.
    """
    
    def __init__(self):
        self.fallback_handlers: Dict[str, Callable] = {}
        self.degraded_services: Dict[str, datetime] = {}
    
    def register_fallback(self, service: str, handler: Callable):
        """Register fallback handler for a service."""
        self.fallback_handlers[service] = handler
        logger.info(f"Registered fallback handler for {service}")
    
    async def execute_with_fallback(
        self,
        service: str,
        primary_func: Callable,
        *args,
        **kwargs
    ) -> Any:
        """
        Execute function with fallback on failure.
        
        Args:
            service: Service identifier
            primary_func: Primary function to execute
            *args: Function arguments
            **kwargs: Function keyword arguments
            
        Returns:
            Result from primary or fallback function
        """
        try:
            if asyncio.iscoroutinefunction(primary_func):
                result = await primary_func(*args, **kwargs)
            else:
                result = primary_func(*args, **kwargs)
            
            # Mark service as recovered if it was degraded
            if service in self.degraded_services:
                del self.degraded_services[service]
                logger.info(f"Service {service} recovered from degradation")
            
            return result
            
        except Exception as e:
            logger.warning(
                f"Primary function failed for {service}, attempting fallback",
                extra={"error": str(e)}
            )
            
            # Mark service as degraded
            self.degraded_services[service] = datetime.utcnow()
            
            if service in self.fallback_handlers:
                try:
                    fallback_func = self.fallback_handlers[service]
                    if asyncio.iscoroutinefunction(fallback_func):
                        return await fallback_func(*args, **kwargs)
                    else:
                        return fallback_func(*args, **kwargs)
                except Exception as fallback_error:
                    logger.error(
                        f"Fallback also failed for {service}",
                        extra={"fallback_error": str(fallback_error)}
                    )
                    raise
            else:
                logger.error(f"No fallback handler registered for {service}")
                raise
    
    def get_degraded_services(self) -> Dict[str, datetime]:
        """Get currently degraded services."""
        return self.degraded_services.copy()


# Global resilience manager
class ResilienceManager:
    """Central manager for all resilience patterns."""
    
    def __init__(self):
        self.circuit_breakers: Dict[str, CircuitBreaker] = {}
        self.bulkheads: Dict[str, Bulkhead] = {}
        self.degradation = GracefulDegradation()
        self.default_retry_policy = RetryPolicy()
    
    def get_circuit_breaker(
        self,
        name: str,
        failure_threshold: int = 5,
        recovery_timeout: float = 60.0,
        expected_exception: Type[Exception] = Exception
    ) -> CircuitBreaker:
        """Get or create circuit breaker."""
        if name not in self.circuit_breakers:
            self.circuit_breakers[name] = CircuitBreaker(
                name=name,
                failure_threshold=failure_threshold,
                recovery_timeout=recovery_timeout,
                expected_exception=expected_exception
            )
        return self.circuit_breakers[name]
    
    def get_bulkhead(self, name: str, max_concurrent: int = 10) -> Bulkhead:
        """Get or create bulkhead."""
        if name not in self.bulkheads:
            self.bulkheads[name] = Bulkhead(name, max_concurrent)
        return self.bulkheads[name]
    
    def get_health_status(self) -> Dict[str, Any]:
        """Get overall health status of resilience components."""
        return {
            "circuit_breakers": {
                name: cb.get_state() 
                for name, cb in self.circuit_breakers.items()
            },
            "bulkheads": {
                name: bh.get_stats() 
                for name, bh in self.bulkheads.items()
            },
            "degraded_services": list(self.degradation.get_degraded_services().keys())
        }


# Global instance
resilience_manager = ResilienceManager()


# Decorators for easy usage
def circuit_breaker(
    name: str = None,
    failure_threshold: int = 5,
    recovery_timeout: float = 60.0,
    expected_exception: Type[Exception] = Exception
):
    """Decorator to add circuit breaker protection to a function."""
    def decorator(func: Callable) -> Callable:
        cb_name = name or f"{func.__module__}.{func.__name__}"
        cb = resilience_manager.get_circuit_breaker(
            cb_name, failure_threshold, recovery_timeout, expected_exception
        )
        
        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            return await cb.call(func, *args, **kwargs)
        
        return wrapper
    return decorator


def with_bulkhead(name: str = None, max_concurrent: int = 10):
    """Decorator to add bulkhead protection to a function."""
    def decorator(func: Callable) -> Callable:
        bh_name = name or f"{func.__module__}.{func.__name__}"
        bh = resilience_manager.get_bulkhead(bh_name, max_concurrent)
        
        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            return await bh.execute(func, *args, **kwargs)
        
        return wrapper
    return decorator


def with_retry_policy(retry_policy: RetryPolicy = None):
    """Decorator to add retry logic to a function."""
    if retry_policy is None:
        retry_policy = resilience_manager.default_retry_policy
    
    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            return await with_retry(func, retry_policy, *args, **kwargs)
        
        return wrapper
    return decorator


# Example usage and testing
if __name__ == "__main__":
    import random
    
    async def example_unreliable_service():
        """Simulate an unreliable service for testing."""
        if random.random() < 0.3:  # 30% failure rate
            raise DatabaseConnectionError("Simulated database failure")
        return {"status": "success", "data": "test_data"}
    
    @circuit_breaker(name="test_service", failure_threshold=3)
    @with_bulkhead(name="test_bulkhead", max_concurrent=5)
    @with_retry_policy(RetryPolicy(max_attempts=3, base_delay=0.1))
    async def protected_service():
        """Service with all resilience patterns applied."""
        return await example_unreliable_service()
    
    async def test_resilience():
        """Test resilience patterns."""
        print("🔧 Testing resilience patterns...")
        
        for i in range(10):
            try:
                result = await protected_service()
                print(f"✅ Call {i+1}: {result}")
            except Exception as e:
                print(f"❌ Call {i+1}: {type(e).__name__}: {e}")
            
            await asyncio.sleep(0.1)
        
        # Print health status
        health = resilience_manager.get_health_status()
        print(f"\n📊 Health Status: {health}")
    
    # Run test
    asyncio.run(test_resilience())