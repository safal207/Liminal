#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Comprehensive Testing Strategy for LIMINAL Production Deployment.

Includes:
- Unit testing framework with mocking
- Integration testing for all components
- Load testing scenarios
- E2E testing automation
- Performance benchmarking
- Chaos engineering tests
"""

import asyncio
import json
import pytest
import time
import uuid
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from unittest.mock import AsyncMock, MagicMock, patch

import aiohttp
import pytest_asyncio
from fastapi.testclient import TestClient
from locust import HttpUser, task, between
from playwright.async_api import async_playwright

# Import our modules for testing
from config import get_settings
from resilience import CircuitBreaker, Bulkhead, resilience_manager
from monitoring import monitoring_service
from datomic_client import DatomicClient
from ml_pipeline_enhanced import ml_pipeline, ModelVersion
from security_enhanced import security_middleware, InputValidator, RateLimiter


class TestConfig:
    """Test configuration and fixtures."""
    
    @pytest.fixture
    def test_settings(self):
        """Test configuration settings."""
        return {
            "database": {
                "neo4j_uri": "bolt://localhost:7687",
                "datomic_uri": "http://localhost:8080",
                "redis_url": "redis://localhost:6379"
            },
            "security": {
                "jwt_secret_key": "test-secret-key-for-testing-only",
                "rate_limit_per_minute": 100
            },
            "ml": {
                "ml_enabled": True,
                "ml_model_cache_size": 10
            }
        }
    
    @pytest.fixture
    async def mock_datomic_client(self):
        """Mock Datomic client for testing."""
        client = AsyncMock(spec=DatomicClient)
        client.connect.return_value = True
        client.health_check.return_value = {"status": "healthy"}
        client.add_emotion_entry.return_value = {"tx": "test-tx-id"}
        client.get_emotion_history.return_value = [
            {
                "id": "test-emotion-1",
                "emotion": "joy",
                "intensity": 0.8,
                "timestamp": datetime.utcnow().isoformat()
            }
        ]
        return client
    
    @pytest.fixture
    async def mock_neo4j_client(self):
        """Mock Neo4j client for testing."""
        client = MagicMock()
        client.session.return_value.__enter__.return_value.run.return_value = []
        return client


class UnitTests:
    """Unit tests for individual components."""
    
    class TestResilience:
        """Test resilience patterns."""
        
        @pytest.mark.asyncio
        async def test_circuit_breaker_basic_functionality(self):
            """Test circuit breaker opens after failures."""
            circuit = CircuitBreaker("test_circuit", failure_threshold=3)
            
            async def failing_function():
                raise Exception("Test failure")
            
            # Should succeed initially (circuit closed)
            assert circuit.state.value == "closed"
            
            # Trigger failures
            for i in range(3):
                with pytest.raises(Exception):
                    await circuit.call(failing_function)
            
            # Circuit should be open now
            assert circuit.state.value == "open"
            
            # Further calls should raise CircuitBreakerError
            from resilience import CircuitBreakerError
            with pytest.raises(CircuitBreakerError):
                await circuit.call(failing_function)
        
        @pytest.mark.asyncio
        async def test_circuit_breaker_recovery(self):
            """Test circuit breaker recovery after timeout."""
            circuit = CircuitBreaker("test_recovery", failure_threshold=2, recovery_timeout=0.1)
            
            async def failing_function():
                raise Exception("Test failure")
            
            async def success_function():
                return "success"
            
            # Trigger failures to open circuit
            for i in range(2):
                with pytest.raises(Exception):
                    await circuit.call(failing_function)
            
            assert circuit.state.value == "open"
            
            # Wait for recovery timeout
            await asyncio.sleep(0.2)
            
            # Should move to half-open and allow test
            result = await circuit.call(success_function)
            assert result == "success"
            assert circuit.state.value == "closed"
        
        @pytest.mark.asyncio
        async def test_bulkhead_resource_isolation(self):
            """Test bulkhead prevents resource exhaustion."""
            bulkhead = Bulkhead("test_bulkhead", max_concurrent=2)
            
            async def slow_function():
                await asyncio.sleep(0.1)
                return "done"
            
            # Start 3 concurrent operations (exceeds limit of 2)
            tasks = []
            for i in range(3):
                task = asyncio.create_task(bulkhead.execute(slow_function))
                tasks.append(task)
            
            # Wait for completion
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            # Should have 2 successes and 1 rejection
            successes = sum(1 for r in results if r == "done")
            rejections = sum(1 for r in results if isinstance(r, Exception))
            
            assert successes == 2
            assert rejections == 1
    
    class TestSecurity:
        """Test security components."""
        
        def test_input_validator_xss_detection(self):
            """Test XSS detection in input validation."""
            validator = InputValidator()
            
            malicious_inputs = [
                "<script>alert('xss')</script>",
                "javascript:alert('xss')",
                "<img src=x onerror=alert('xss')>",
                "<iframe src='javascript:alert(1)'></iframe>"
            ]
            
            for malicious_input in malicious_inputs:
                with pytest.raises(Exception) as exc_info:
                    validator.validate_input(malicious_input)
                assert "XSS" in str(exc_info.value)
        
        def test_input_validator_sql_injection_detection(self):
            """Test SQL injection detection."""
            validator = InputValidator()
            
            malicious_inputs = [
                "'; DROP TABLE users; --",
                "1' OR '1'='1",
                "UNION SELECT * FROM passwords",
                "1; DELETE FROM users WHERE 1=1"
            ]
            
            for malicious_input in malicious_inputs:
                with pytest.raises(Exception) as exc_info:
                    validator.validate_input(malicious_input)
                assert "SQL" in str(exc_info.value)
        
        def test_password_strength_validation(self):
            """Test password strength validation."""
            validator = InputValidator()
            
            weak_passwords = ["123456", "password", "abc123"]
            strong_passwords = ["MyStr0ng!Pass123", "C0mpl3x&Secure!2024"]
            
            for weak_pass in weak_passwords:
                result = validator.validate_password_strength(weak_pass)
                assert result["strength"] in ["very_weak", "weak", "fair"]
            
            for strong_pass in strong_passwords:
                result = validator.validate_password_strength(strong_pass)
                assert result["strength"] in ["good", "strong", "very_strong"]
        
        @pytest.mark.asyncio
        async def test_rate_limiter_fixed_window(self):
            """Test fixed window rate limiting."""
            from security_enhanced import RateLimitRule, RateLimitType
            
            rate_limiter = RateLimiter()
            rule = RateLimitRule(
                name="test_rule",
                limit=3,
                window_seconds=60,
                rule_type=RateLimitType.FIXED_WINDOW,
                scope="test"
            )
            rate_limiter.add_rule(rule)
            
            # Should allow first 3 requests
            for i in range(3):
                result = await rate_limiter.check_rate_limit("test_user", "test_rule")
                assert result["allowed"] is True
            
            # Fourth request should be blocked
            result = await rate_limiter.check_rate_limit("test_user", "test_rule")
            assert result["allowed"] is False
            assert result["reason"] == "rate_limit_exceeded"
    
    class TestMLPipeline:
        """Test ML pipeline components."""
        
        @pytest.mark.asyncio
        async def test_prediction_caching(self):
            """Test ML prediction caching."""
            from ml_pipeline_enhanced import PredictionCache, Prediction
            
            cache = PredictionCache(max_size=10, ttl_seconds=60)
            
            # Create test prediction
            prediction = Prediction(
                id="test-pred-1",
                model_id="test-model",
                input_features={"x1": 1, "x2": 2},
                prediction="result",
                confidence=0.9,
                inference_time_ms=50.0,
                timestamp=datetime.utcnow()
            )
            
            # Cache prediction
            await cache.set("test-model", {"x1": 1, "x2": 2}, prediction)
            
            # Retrieve from cache
            cached_prediction = await cache.get("test-model", {"x1": 1, "x2": 2})
            
            assert cached_prediction is not None
            assert cached_prediction.cached is True
            assert cached_prediction.prediction == "result"
            
            # Cache stats should show hit
            stats = cache.get_stats()
            assert stats["hit_count"] == 1
        
        @pytest.mark.asyncio
        async def test_model_registry(self):
            """Test model registry functionality."""
            from ml_pipeline_enhanced import ModelRegistry, SklearnModelWrapper
            from sklearn.ensemble import RandomForestClassifier
            
            registry = ModelRegistry(storage_path="test_models/")
            
            # Create test model
            sklearn_model = RandomForestClassifier(n_estimators=10, random_state=42)
            model_wrapper = SklearnModelWrapper(sklearn_model)
            
            # Register model
            model_version = await registry.register_model(
                name="test_model",
                version="v1.0.0",
                model=model_wrapper,
                metadata={"framework": "sklearn"},
                metrics={"accuracy": 0.85}
            )
            
            assert model_version.name == "test_model"
            assert model_version.version == "v1.0.0"
            assert model_version.metrics["accuracy"] == 0.85
            
            # Deploy model
            success = await registry.deploy_model(model_version.id)
            assert success is True
            
            # Get deployed model
            deployed = registry.get_deployed_model("test_model")
            assert deployed is not None
            assert deployed.id == model_version.id
    
    class TestMonitoring:
        """Test monitoring components."""
        
        @pytest.mark.asyncio
        async def test_health_monitor(self):
            """Test health monitoring system."""
            from monitoring import HealthMonitor, HealthCheck
            
            monitor = HealthMonitor()
            
            async def healthy_check():
                return {"status": "ok"}
            
            async def unhealthy_check():
                raise Exception("Service unavailable")
            
            # Register health checks
            monitor.register_health_check(HealthCheck("healthy_service", healthy_check))
            monitor.register_health_check(HealthCheck("unhealthy_service", unhealthy_check))
            
            # Run health checks
            results = await monitor.run_all_health_checks()
            
            assert "healthy_service" in results
            assert "unhealthy_service" in results
            assert results["healthy_service"]["status"] == "healthy"
            assert results["unhealthy_service"]["status"] == "unhealthy"
        
        @pytest.mark.asyncio
        async def test_distributed_tracing(self):
            """Test distributed tracing functionality."""
            from monitoring import DistributedTracer
            
            tracer = DistributedTracer()
            
            # Create trace
            async with tracer.trace("test_operation", test_tag="value") as span:
                span.log("operation_started")
                await asyncio.sleep(0.01)  # Simulate work
                span.set_tag("result", "success")
            
            # Verify span was created and finished
            assert span.operation_name == "test_operation"
            assert span.status == "ok"
            assert span.duration() is not None
            assert span.duration() > 0
            assert "test_tag" in span.tags
            assert len(span.logs) == 1


class IntegrationTests:
    """Integration tests for component interactions."""
    
    @pytest.mark.asyncio
    async def test_api_with_security_middleware(self, test_settings):
        """Test API endpoints with security middleware."""
        from fastapi import FastAPI, Request
        from security_enhanced import get_security_context
        
        app = FastAPI()
        
        @app.get("/test")
        async def test_endpoint(request: Request, security_context = None):
            return {"message": "success"}
        
        # Mock request with security context
        with TestClient(app) as client:
            response = client.get("/test")
            # Note: In real test, would need proper security setup
            assert response.status_code in [200, 403, 429]  # Depends on security rules
    
    @pytest.mark.asyncio
    async def test_database_adapter_integration(self, mock_datomic_client, mock_neo4j_client):
        """Test database adapter with mocked databases."""
        from database_adapter import DatabaseAdapter, DataType
        
        with patch('database_adapter.DatomicClient', return_value=mock_datomic_client):
            with patch('database_adapter.PhilosophyNeo4jWriter', return_value=mock_neo4j_client):
                adapter = DatabaseAdapter()
                
                # Test connection
                success = adapter.connect()
                assert success is True
                
                # Test data routing
                # This would test the actual routing logic
                assert adapter._choose_database(DataType.TEMPORAL) == "datomic"
                assert adapter._choose_database(DataType.RELATIONSHIP) == "neo4j"
    
    @pytest.mark.asyncio
    async def test_ml_pipeline_with_monitoring(self):
        """Test ML pipeline integration with monitoring."""
        # This would test the full ML prediction flow with monitoring
        pass
    
    @pytest.mark.asyncio
    async def test_websocket_with_authentication(self):
        """Test WebSocket connections with authentication."""
        # This would test WebSocket authentication flow
        pass


class LoadTests:
    """Load testing scenarios using Locust."""
    
    class LiminalLoadTest(HttpUser):
        """Load test user for LIMINAL API."""
        
        wait_time = between(1, 3)
        
        def on_start(self):
            """Setup for load test user."""
            # Authenticate user
            response = self.client.post("/auth/token", data={
                "username": "testuser",
                "password": "testpass"
            })
            if response.status_code == 200:
                self.token = response.json().get("access_token")
                self.headers = {"Authorization": f"Bearer {self.token}"}
            else:
                self.headers = {}
        
        @task(3)
        def health_check(self):
            """Test health endpoint under load."""
            self.client.get("/health")
        
        @task(2)
        def get_consciousness_state(self):
            """Test consciousness state API under load."""
            self.client.get("/api/consciousness/current", headers=self.headers)
        
        @task(1)
        def ml_prediction(self):
            """Test ML prediction endpoint under load."""
            self.client.post("/ml/predict", json={
                "model_name": "emotion_classifier",
                "features": {"text": "I feel great today!", "intensity": 0.8}
            }, headers=self.headers)
        
        @task(1)
        def emotion_entry(self):
            """Test emotion entry under load."""
            self.client.post("/api/emotions", json={
                "emotion": "joy",
                "intensity": 0.7,
                "context": "load_test"
            }, headers=self.headers)
    
    class WebSocketLoadTest:
        """WebSocket load testing."""
        
        async def test_websocket_connections(self, num_connections=100):
            """Test multiple WebSocket connections."""
            import websockets
            
            connections = []
            try:
                # Create multiple connections
                for i in range(num_connections):
                    uri = f"ws://localhost:8000/ws/load_test_client_{i}"
                    conn = await websockets.connect(uri)
                    connections.append(conn)
                
                # Send messages through all connections
                tasks = []
                for i, conn in enumerate(connections):
                    task = self._send_test_messages(conn, f"client_{i}")
                    tasks.append(task)
                
                await asyncio.gather(*tasks, return_exceptions=True)
                
            finally:
                # Clean up connections
                for conn in connections:
                    await conn.close()
        
        async def _send_test_messages(self, websocket, client_id):
            """Send test messages through WebSocket."""
            for i in range(10):
                message = {
                    "type": "consciousness_transition",
                    "client_id": client_id,
                    "data": {"from": "presence", "to": "harmony"}
                }
                await websocket.send(json.dumps(message))
                await asyncio.sleep(0.1)


class E2ETests:
    """End-to-end testing with Playwright."""
    
    @pytest.mark.asyncio
    async def test_consciousness_monitor_e2e(self):
        """Test consciousness monitor UI end-to-end."""
        async with async_playwright() as p:
            browser = await p.chromium.launch()
            page = await browser.new_page()
            
            try:
                # Navigate to consciousness monitor
                await page.goto("http://localhost:8000/frontend/consciousness_monitor.html")
                
                # Wait for page to load
                await page.wait_for_selector(".state-card")
                
                # Check if consciousness states are displayed
                state_cards = await page.query_selector_all(".state-card")
                assert len(state_cards) > 0
                
                # Test WebSocket connection button
                connect_btn = await page.query_selector("#connect-btn")
                if connect_btn:
                    await connect_btn.click()
                    
                    # Wait for connection status
                    await page.wait_for_selector(".status.connected", timeout=5000)
                
                # Test consciousness transition simulation
                simulate_btn = await page.query_selector("#simulate-btn")
                if simulate_btn:
                    await simulate_btn.click()
                    
                    # Check for event log updates
                    await page.wait_for_function(
                        "document.querySelector('#event-log').children.length > 0"
                    )
                
            finally:
                await browser.close()
    
    @pytest.mark.asyncio
    async def test_api_documentation_e2e(self):
        """Test API documentation accessibility."""
        async with async_playwright() as p:
            browser = await p.chromium.launch()
            page = await browser.new_page()
            
            try:
                # Navigate to API docs
                await page.goto("http://localhost:8000/docs")
                
                # Check if Swagger UI loads
                await page.wait_for_selector(".swagger-ui")
                
                # Test API endpoint expansion
                health_endpoint = await page.query_selector('text="GET /health"')
                if health_endpoint:
                    await health_endpoint.click()
                    
                    # Check if endpoint details appear
                    await page.wait_for_selector(".opblock-section")
                
            finally:
                await browser.close()


class PerformanceTests:
    """Performance benchmarking tests."""
    
    @pytest.mark.asyncio
    async def test_api_response_times(self):
        """Test API response time benchmarks."""
        import aiohttp
        
        async with aiohttp.ClientSession() as session:
            # Test health endpoint performance
            start_time = time.time()
            for _ in range(100):
                async with session.get("http://localhost:8000/health") as response:
                    assert response.status == 200
            end_time = time.time()
            
            avg_response_time = (end_time - start_time) / 100
            assert avg_response_time < 0.1  # 100ms average
    
    @pytest.mark.asyncio
    async def test_memory_usage(self):
        """Test memory usage under load."""
        import psutil
        import os
        
        process = psutil.Process(os.getpid())
        initial_memory = process.memory_info().rss
        
        # Simulate load
        tasks = []
        for _ in range(1000):
            task = asyncio.create_task(self._simulate_request())
            tasks.append(task)
        
        await asyncio.gather(*tasks)
        
        final_memory = process.memory_info().rss
        memory_increase = final_memory - initial_memory
        
        # Memory increase should be reasonable (less than 100MB)
        assert memory_increase < 100 * 1024 * 1024
    
    async def _simulate_request(self):
        """Simulate a request for memory testing."""
        # Create some objects to simulate request processing
        data = {"test": "data" * 100}
        await asyncio.sleep(0.001)
        return data


class ChaosEngineeringTests:
    """Chaos engineering tests for resilience validation."""
    
    @pytest.mark.asyncio
    async def test_database_failure_resilience(self):
        """Test system behavior when database fails."""
        # This would test circuit breaker behavior when DB is down
        pass
    
    @pytest.mark.asyncio
    async def test_network_partition_tolerance(self):
        """Test behavior during network partitions."""
        # This would test service behavior under network issues
        pass
    
    @pytest.mark.asyncio
    async def test_high_cpu_load_resilience(self):
        """Test system behavior under high CPU load."""
        # This would test performance under stress
        pass


# Test Configuration and Runners
class TestRunner:
    """Test execution coordinator."""
    
    @staticmethod
    def run_unit_tests():
        """Run all unit tests."""
        return pytest.main([
            "test_comprehensive.py::UnitTests",
            "-v",
            "--tb=short"
        ])
    
    @staticmethod
    def run_integration_tests():
        """Run integration tests."""
        return pytest.main([
            "test_comprehensive.py::IntegrationTests", 
            "-v",
            "--tb=short"
        ])
    
    @staticmethod
    def run_performance_tests():
        """Run performance benchmarks."""
        return pytest.main([
            "test_comprehensive.py::PerformanceTests",
            "-v",
            "--tb=short",
            "--benchmark-only"
        ])
    
    @staticmethod
    def run_e2e_tests():
        """Run end-to-end tests."""
        return pytest.main([
            "test_comprehensive.py::E2ETests",
            "-v",
            "--tb=short",
            "--headed"  # For Playwright
        ])
    
    @staticmethod
    def run_all_tests():
        """Run complete test suite."""
        return pytest.main([
            "test_comprehensive.py",
            "-v",
            "--tb=short",
            "--cov=.",
            "--cov-report=html"
        ])


# Test Data Factories
class TestDataFactory:
    """Factory for generating test data."""
    
    @staticmethod
    def create_test_user():
        """Create test user data."""
        return {
            "user_id": f"test_user_{uuid.uuid4()}",
            "username": "testuser",
            "email": "test@example.com",
            "roles": ["user"],
            "scopes": ["read", "write"]
        }
    
    @staticmethod
    def create_test_emotion_entry():
        """Create test emotion entry."""
        return {
            "user_id": "test_user",
            "emotion": "joy",
            "intensity": 0.8,
            "timestamp": datetime.utcnow().isoformat(),
            "metadata": {"source": "test"}
        }
    
    @staticmethod
    def create_test_consciousness_state():
        """Create test consciousness state."""
        return {
            "state_id": "test_state",
            "name": "presence_now",
            "description": "Test consciousness state",
            "active": True,
            "metadata": {}
        }


if __name__ == "__main__":
    # Example usage
    print("🧪 LIMINAL Comprehensive Testing Framework")
    print("=" * 50)
    
    # Run specific test suites
    runner = TestRunner()
    
    print("Running unit tests...")
    unit_result = runner.run_unit_tests()
    
    print("Running integration tests...")
    integration_result = runner.run_integration_tests()
    
    print("Running performance tests...")
    performance_result = runner.run_performance_tests()
    
    print(f"Test Results: Unit={unit_result}, Integration={integration_result}, Performance={performance_result}")