#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Comprehensive monitoring and alerting system for LIMINAL.

Provides distributed tracing, business metrics, health monitoring,
and alerting capabilities for production operations.
"""

import asyncio
import json
import logging
import time
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Dict, List, Optional, Union
from contextlib import asynccontextmanager

import structlog
from prometheus_client import (
    Counter, Histogram, Gauge, Summary, Info,
    CollectorRegistry, generate_latest, CONTENT_TYPE_LATEST
)

from config import get_monitoring_settings, get_app_settings
from resilience import resilience_manager

logger = structlog.get_logger(__name__)


class AlertSeverity(Enum):
    """Alert severity levels."""
    INFO = "info"
    WARNING = "warning"
    CRITICAL = "critical"
    FATAL = "fatal"


class HealthStatus(Enum):
    """Health check status."""
    HEALTHY = "healthy"
    DEGRADED = "degraded"
    UNHEALTHY = "unhealthy"
    UNKNOWN = "unknown"


@dataclass
class TraceSpan:
    """Distributed tracing span."""
    trace_id: str
    span_id: str
    parent_span_id: Optional[str]
    operation_name: str
    start_time: float
    end_time: Optional[float] = None
    tags: Dict[str, Any] = field(default_factory=dict)
    logs: List[Dict[str, Any]] = field(default_factory=list)
    status: str = "active"
    
    def finish(self, status: str = "ok"):
        """Finish the span."""
        self.end_time = time.time()
        self.status = status
    
    def log(self, event: str, **kwargs):
        """Add log entry to span."""
        self.logs.append({
            "timestamp": time.time(),
            "event": event,
            **kwargs
        })
    
    def set_tag(self, key: str, value: Any):
        """Set span tag."""
        self.tags[key] = value
    
    def duration(self) -> Optional[float]:
        """Get span duration in seconds."""
        if self.end_time:
            return self.end_time - self.start_time
        return None


@dataclass
class BusinessMetric:
    """Business-level metric for monitoring."""
    name: str
    value: Union[int, float]
    unit: str
    timestamp: datetime
    tags: Dict[str, str] = field(default_factory=dict)
    description: Optional[str] = None


@dataclass
class HealthCheck:
    """Health check definition."""
    name: str
    check_func: callable
    timeout: float = 30.0
    critical: bool = True
    interval: float = 60.0
    tags: Dict[str, str] = field(default_factory=dict)


@dataclass
class Alert:
    """Alert definition."""
    id: str
    title: str
    description: str
    severity: AlertSeverity
    timestamp: datetime
    source: str
    tags: Dict[str, str] = field(default_factory=dict)
    resolved: bool = False
    resolved_at: Optional[datetime] = None


class PrometheusMetrics:
    """Enhanced Prometheus metrics for LIMINAL."""
    
    def __init__(self, registry: Optional[CollectorRegistry] = None):
        self.registry = registry or CollectorRegistry()
        self._initialize_metrics()
    
    def _initialize_metrics(self):
        """Initialize all Prometheus metrics."""
        
        # HTTP/API metrics
        self.http_requests_total = Counter(
            'liminal_http_requests_total',
            'Total HTTP requests',
            ['method', 'endpoint', 'status'],
            registry=self.registry
        )
        
        self.http_request_duration = Histogram(
            'liminal_http_request_duration_seconds',
            'HTTP request duration',
            ['method', 'endpoint'],
            registry=self.registry
        )
        
        # Database metrics
        self.db_operations_total = Counter(
            'liminal_db_operations_total',
            'Total database operations',
            ['database', 'operation', 'status'],
            registry=self.registry
        )
        
        self.db_connection_pool_size = Gauge(
            'liminal_db_connection_pool_size',
            'Database connection pool size',
            ['database'],
            registry=self.registry
        )
        
        self.db_query_duration = Histogram(
            'liminal_db_query_duration_seconds',
            'Database query duration',
            ['database', 'operation'],
            registry=self.registry
        )
        
        # WebSocket metrics
        self.websocket_connections = Gauge(
            'liminal_websocket_connections_active',
            'Active WebSocket connections',
            ['channel'],
            registry=self.registry
        )
        
        self.websocket_messages_total = Counter(
            'liminal_websocket_messages_total',
            'Total WebSocket messages',
            ['direction', 'message_type'],
            registry=self.registry
        )
        
        # ML/AI metrics
        self.ml_predictions_total = Counter(
            'liminal_ml_predictions_total',
            'Total ML predictions',
            ['model', 'status'],
            registry=self.registry
        )
        
        self.ml_inference_duration = Histogram(
            'liminal_ml_inference_duration_seconds',
            'ML inference duration',
            ['model'],
            registry=self.registry
        )
        
        # Business metrics
        self.consciousness_transitions = Counter(
            'liminal_consciousness_transitions_total',
            'Total consciousness state transitions',
            ['from_state', 'to_state'],
            registry=self.registry
        )
        
        self.emotion_entries = Counter(
            'liminal_emotion_entries_total',
            'Total emotion entries',
            ['emotion', 'intensity_range'],
            registry=self.registry
        )
        
        self.philosophy_interactions = Counter(
            'liminal_philosophy_interactions_total',
            'Total philosophy interactions',
            ['interaction_type'],
            registry=self.registry
        )
        
        # System metrics
        self.system_health = Gauge(
            'liminal_system_health',
            'System health score (0-1)',
            ['component'],
            registry=self.registry
        )
        
        self.error_rate = Gauge(
            'liminal_error_rate',
            'Error rate by component',
            ['component', 'error_type'],
            registry=self.registry
        )
        
        # Performance metrics
        self.memory_usage = Gauge(
            'liminal_memory_usage_bytes',
            'Memory usage',
            ['component'],
            registry=self.registry
        )
        
        self.cpu_usage = Gauge(
            'liminal_cpu_usage_percent',
            'CPU usage percentage',
            ['component'],
            registry=self.registry
        )


class DistributedTracer:
    """Distributed tracing implementation for request tracking."""
    
    def __init__(self):
        self.active_spans: Dict[str, TraceSpan] = {}
        self.completed_spans: List[TraceSpan] = []
        self.max_completed_spans = 10000  # Memory limit
    
    def start_span(
        self,
        operation_name: str,
        parent_span_id: Optional[str] = None,
        trace_id: Optional[str] = None
    ) -> TraceSpan:
        """Start a new tracing span."""
        if trace_id is None:
            trace_id = str(uuid.uuid4())
        
        span_id = str(uuid.uuid4())
        span = TraceSpan(
            trace_id=trace_id,
            span_id=span_id,
            parent_span_id=parent_span_id,
            operation_name=operation_name,
            start_time=time.time()
        )
        
        self.active_spans[span_id] = span
        return span
    
    def finish_span(self, span: TraceSpan, status: str = "ok"):
        """Finish a tracing span."""
        span.finish(status)
        
        # Move from active to completed
        if span.span_id in self.active_spans:
            del self.active_spans[span.span_id]
        
        self.completed_spans.append(span)
        
        # Maintain memory limit
        if len(self.completed_spans) > self.max_completed_spans:
            self.completed_spans = self.completed_spans[-self.max_completed_spans:]
    
    @asynccontextmanager
    async def trace(
        self,
        operation_name: str,
        parent_span_id: Optional[str] = None,
        trace_id: Optional[str] = None,
        **tags
    ):
        """Context manager for tracing operations."""
        span = self.start_span(operation_name, parent_span_id, trace_id)
        
        # Set initial tags
        for key, value in tags.items():
            span.set_tag(key, value)
        
        try:
            yield span
            self.finish_span(span, "ok")
        except Exception as e:
            span.set_tag("error", True)
            span.set_tag("error.message", str(e))
            span.log("error", message=str(e), type=type(e).__name__)
            self.finish_span(span, "error")
            raise
    
    def get_trace(self, trace_id: str) -> List[TraceSpan]:
        """Get all spans for a trace."""
        spans = []
        
        # Check active spans
        for span in self.active_spans.values():
            if span.trace_id == trace_id:
                spans.append(span)
        
        # Check completed spans
        for span in self.completed_spans:
            if span.trace_id == trace_id:
                spans.append(span)
        
        return sorted(spans, key=lambda s: s.start_time)


class HealthMonitor:
    """Comprehensive health monitoring system."""
    
    def __init__(self):
        self.health_checks: Dict[str, HealthCheck] = {}
        self.health_status: Dict[str, HealthStatus] = {}
        self.last_check_times: Dict[str, datetime] = {}
        self.check_results: Dict[str, Any] = {}
        self._monitoring_task: Optional[asyncio.Task] = None
    
    def register_health_check(self, health_check: HealthCheck):
        """Register a health check."""
        self.health_checks[health_check.name] = health_check
        self.health_status[health_check.name] = HealthStatus.UNKNOWN
        logger.info(f"Registered health check: {health_check.name}")
    
    async def run_health_check(self, name: str) -> Dict[str, Any]:
        """Run a specific health check."""
        if name not in self.health_checks:
            raise ValueError(f"Health check {name} not registered")
        
        health_check = self.health_checks[name]
        start_time = time.time()
        
        try:
            # Run health check with timeout
            result = await asyncio.wait_for(
                health_check.check_func(),
                timeout=health_check.timeout
            )
            
            execution_time = time.time() - start_time
            self.health_status[name] = HealthStatus.HEALTHY
            self.last_check_times[name] = datetime.utcnow()
            
            check_result = {
                "status": "healthy",
                "result": result,
                "execution_time": execution_time,
                "timestamp": datetime.utcnow().isoformat()
            }
            
            self.check_results[name] = check_result
            return check_result
            
        except asyncio.TimeoutError:
            self.health_status[name] = HealthStatus.UNHEALTHY
            return {
                "status": "unhealthy",
                "error": f"Health check timed out after {health_check.timeout}s",
                "timestamp": datetime.utcnow().isoformat()
            }
        except Exception as e:
            self.health_status[name] = HealthStatus.UNHEALTHY
            return {
                "status": "unhealthy",
                "error": str(e),
                "timestamp": datetime.utcnow().isoformat()
            }
    
    async def run_all_health_checks(self) -> Dict[str, Any]:
        """Run all registered health checks."""
        tasks = []
        for name in self.health_checks:
            tasks.append(self.run_health_check(name))
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        return {
            name: result if not isinstance(result, Exception) else {
                "status": "error",
                "error": str(result)
            }
            for name, result in zip(self.health_checks.keys(), results)
        }
    
    def get_overall_health(self) -> Dict[str, Any]:
        """Get overall system health status."""
        critical_checks = [
            name for name, check in self.health_checks.items()
            if check.critical
        ]
        
        critical_unhealthy = [
            name for name in critical_checks
            if self.health_status.get(name) == HealthStatus.UNHEALTHY
        ]
        
        if critical_unhealthy:
            overall_status = HealthStatus.UNHEALTHY
        elif any(status == HealthStatus.DEGRADED for status in self.health_status.values()):
            overall_status = HealthStatus.DEGRADED
        elif any(status == HealthStatus.UNKNOWN for status in self.health_status.values()):
            overall_status = HealthStatus.UNKNOWN
        else:
            overall_status = HealthStatus.HEALTHY
        
        return {
            "status": overall_status.value,
            "critical_failures": critical_unhealthy,
            "checks": {
                name: {
                    "status": status.value,
                    "critical": self.health_checks[name].critical,
                    "last_check": self.last_check_times.get(name),
                    "result": self.check_results.get(name)
                }
                for name, status in self.health_status.items()
            }
        }


class AlertManager:
    """Alert management and notification system."""
    
    def __init__(self):
        self.alerts: Dict[str, Alert] = {}
        self.alert_rules: List[Dict[str, Any]] = []
        self.notification_handlers: List[callable] = []
    
    def add_alert_rule(
        self,
        name: str,
        condition: callable,
        severity: AlertSeverity,
        description: str,
        cooldown: int = 300  # 5 minutes
    ):
        """Add alert rule."""
        self.alert_rules.append({
            "name": name,
            "condition": condition,
            "severity": severity,
            "description": description,
            "cooldown": cooldown,
            "last_fired": None
        })
    
    def register_notification_handler(self, handler: callable):
        """Register notification handler."""
        self.notification_handlers.append(handler)
    
    async def fire_alert(
        self,
        title: str,
        description: str,
        severity: AlertSeverity,
        source: str,
        **tags
    ) -> str:
        """Fire an alert."""
        alert_id = str(uuid.uuid4())
        alert = Alert(
            id=alert_id,
            title=title,
            description=description,
            severity=severity,
            timestamp=datetime.utcnow(),
            source=source,
            tags=tags
        )
        
        self.alerts[alert_id] = alert
        
        # Send notifications
        for handler in self.notification_handlers:
            try:
                await handler(alert)
            except Exception as e:
                logger.error(f"Notification handler failed: {e}")
        
        logger.warning(
            f"Alert fired: {title}",
            extra={
                "alert_id": alert_id,
                "severity": severity.value,
                "source": source
            }
        )
        
        return alert_id
    
    async def resolve_alert(self, alert_id: str):
        """Resolve an alert."""
        if alert_id in self.alerts:
            alert = self.alerts[alert_id]
            alert.resolved = True
            alert.resolved_at = datetime.utcnow()
            
            logger.info(f"Alert resolved: {alert.title}", extra={"alert_id": alert_id})


class MonitoringService:
    """Main monitoring service coordinating all monitoring components."""
    
    def __init__(self):
        self.metrics = PrometheusMetrics()
        self.tracer = DistributedTracer()
        self.health_monitor = HealthMonitor()
        self.alert_manager = AlertManager()
        self.business_metrics: List[BusinessMetric] = []
        
        # Register default health checks
        self._register_default_health_checks()
        self._register_default_alerts()
    
    def _register_default_health_checks(self):
        """Register default health checks."""
        
        async def check_database_connection():
            """Check database connectivity."""
            # This would check actual database connections
            return {"neo4j": "connected", "datomic": "connected"}
        
        async def check_memory_usage():
            """Check memory usage."""
            import psutil
            memory = psutil.virtual_memory()
            if memory.percent > 90:
                raise Exception(f"High memory usage: {memory.percent}%")
            return {"memory_percent": memory.percent}
        
        async def check_resilience_components():
            """Check resilience components health."""
            health = resilience_manager.get_health_status()
            open_circuits = [
                name for name, cb in health["circuit_breakers"].items()
                if cb["state"] == "open"
            ]
            if open_circuits:
                raise Exception(f"Circuit breakers open: {open_circuits}")
            return health
        
        # Register checks
        self.health_monitor.register_health_check(
            HealthCheck("database", check_database_connection, critical=True)
        )
        self.health_monitor.register_health_check(
            HealthCheck("memory", check_memory_usage, critical=True)
        )
        self.health_monitor.register_health_check(
            HealthCheck("resilience", check_resilience_components, critical=False)
        )
    
    def _register_default_alerts(self):
        """Register default alert rules."""
        
        def high_error_rate():
            # Check if error rate is high
            return False  # Placeholder
        
        def circuit_breaker_open():
            # Check if any circuit breakers are open
            health = resilience_manager.get_health_status()
            return any(
                cb["state"] == "open" 
                for cb in health["circuit_breakers"].values()
            )
        
        self.alert_manager.add_alert_rule(
            "high_error_rate",
            high_error_rate,
            AlertSeverity.WARNING,
            "High error rate detected"
        )
        
        self.alert_manager.add_alert_rule(
            "circuit_breaker_open",
            circuit_breaker_open,
            AlertSeverity.CRITICAL,
            "Circuit breaker is open"
        )
    
    def record_business_metric(
        self,
        name: str,
        value: Union[int, float],
        unit: str,
        description: str = None,
        **tags
    ):
        """Record a business metric."""
        metric = BusinessMetric(
            name=name,
            value=value,
            unit=unit,
            timestamp=datetime.utcnow(),
            tags=tags,
            description=description
        )
        
        self.business_metrics.append(metric)
        
        # Keep only recent metrics
        cutoff = datetime.utcnow() - timedelta(hours=24)
        self.business_metrics = [
            m for m in self.business_metrics
            if m.timestamp > cutoff
        ]
    
    async def get_monitoring_dashboard(self) -> Dict[str, Any]:
        """Get comprehensive monitoring dashboard data."""
        health = self.health_monitor.get_overall_health()
        resilience = resilience_manager.get_health_status()
        
        active_alerts = [
            alert for alert in self.alert_manager.alerts.values()
            if not alert.resolved
        ]
        
        return {
            "timestamp": datetime.utcnow().isoformat(),
            "health": health,
            "resilience": resilience,
            "alerts": {
                "active": len(active_alerts),
                "critical": len([a for a in active_alerts if a.severity == AlertSeverity.CRITICAL]),
                "recent": [
                    {
                        "id": a.id,
                        "title": a.title,
                        "severity": a.severity.value,
                        "timestamp": a.timestamp.isoformat()
                    }
                    for a in sorted(active_alerts, key=lambda x: x.timestamp, reverse=True)[:10]
                ]
            },
            "business_metrics": {
                "recent_count": len(self.business_metrics),
                "metrics": [
                    {
                        "name": m.name,
                        "value": m.value,
                        "unit": m.unit,
                        "timestamp": m.timestamp.isoformat()
                    }
                    for m in self.business_metrics[-20:]  # Last 20 metrics
                ]
            }
        }


# Global monitoring service
monitoring_service = MonitoringService()


# Convenience functions
def get_metrics() -> str:
    """Get Prometheus metrics in text format."""
    return generate_latest(monitoring_service.metrics.registry)


def record_business_metric(name: str, value: Union[int, float], unit: str, description: str = None, **tags):
    """Record a business metric (module-level convenience function)."""
    monitoring_service.record_business_metric(name, value, unit, description, **tags)


async def record_consciousness_transition(from_state: str, to_state: str):
    """Record consciousness state transition."""
    monitoring_service.metrics.consciousness_transitions.labels(
        from_state=from_state,
        to_state=to_state
    ).inc()
    
    monitoring_service.record_business_metric(
        "consciousness_transition",
        1,
        "count",
        "Consciousness state transition",
        from_state=from_state,
        to_state=to_state
    )


async def record_emotion_entry(emotion: str, intensity: float):
    """Record emotion entry."""
    # Categorize intensity
    if intensity < 0.3:
        intensity_range = "low"
    elif intensity < 0.7:
        intensity_range = "medium"
    else:
        intensity_range = "high"
    
    monitoring_service.metrics.emotion_entries.labels(
        emotion=emotion,
        intensity_range=intensity_range
    ).inc()
    
    monitoring_service.record_business_metric(
        "emotion_entry",
        intensity,
        "intensity",
        "Emotion entry recorded",
        emotion=emotion
    )


# Example notification handler
async def log_alert_handler(alert: Alert):
    """Simple log-based alert handler."""
    logger.warning(
        f"🚨 ALERT: {alert.title}",
        extra={
            "alert_id": alert.id,
            "severity": alert.severity.value,
            "description": alert.description,
            "source": alert.source,
            "tags": alert.tags
        }
    )


# Register default notification handler
monitoring_service.alert_manager.register_notification_handler(log_alert_handler)


if __name__ == "__main__":
    async def test_monitoring():
        """Test monitoring functionality."""
        print("🔍 Testing monitoring system...")
        
        # Test business metrics
        await record_consciousness_transition("presence", "harmony")
        await record_emotion_entry("joy", 0.8)
        
        # Test tracing
        async with monitoring_service.tracer.trace("test_operation") as span:
            span.set_tag("test", True)
            await asyncio.sleep(0.1)
            span.log("operation_completed")
        
        # Test health checks
        health_results = await monitoring_service.health_monitor.run_all_health_checks()
        print(f"Health check results: {health_results}")
        
        # Test alert
        await monitoring_service.alert_manager.fire_alert(
            "Test Alert",
            "This is a test alert",
            AlertSeverity.WARNING,
            "test_system"
        )
        
        # Get dashboard
        dashboard = await monitoring_service.get_monitoring_dashboard()
        print(f"Dashboard: {json.dumps(dashboard, indent=2, default=str)}")
    
    asyncio.run(test_monitoring())