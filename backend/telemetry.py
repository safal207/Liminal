"""OpenTelemetry configuration and utilities."""
import os
from functools import wraps
from typing import Callable, TypeVar

from opentelemetry import trace
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.instrumentation.fastapi import FastAPIInstrumentation
from opentelemetry.instrumentation.neo4j import Neo4jInstrumentation
from opentelemetry.instrumentation.redis import RedisInstrumentation
from opentelemetry.sdk.resources import Resource
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor

T = TypeVar("T")

def setup_telemetry():
    """Initialize OpenTelemetry with all required instrumentations."""
    resource = Resource.create({"service.name": "liminal-backend"})
    
    trace.set_tracer_provider(TracerProvider(resource=resource))
    
    # Configure exporters
    otlp_exporter = OTLPSpanExporter(
        endpoint=os.getenv("OTEL_EXPORTER_OTLP_ENDPOINT", "http://otel-collector:4317")
    )
    
    trace.get_tracer_provider().add_span_processor(BatchSpanProcessor(otlp_exporter))
    
    # Enable automatic instrumentation
    FastAPIInstrumentation().instrument()
    Neo4jInstrumentation().instrument()
    RedisInstrumentation().instrument()

def traced(name: str = None) -> Callable[[T], T]:
    """Decorator to add tracing to a function."""
    def decorator(func: T) -> T:
        if not name:
            operation_name = func.__name__
        else:
            operation_name = name
            
        tracer = trace.get_tracer(__name__)
        
        @wraps(func)
        async def async_wrapper(*args, **kwargs):
            with tracer.start_as_current_span(operation_name) as span:
                # Add arguments to span for debugging
                span.set_attribute("args", str(args))
                span.set_attribute("kwargs", str(kwargs))
                return await func(*args, **kwargs)
        
        @wraps(func)
        def sync_wrapper(*args, **kwargs):
            with tracer.start_as_current_span(operation_name) as span:
                # Add arguments to span for debugging
                span.set_attribute("args", str(args))
                span.set_attribute("kwargs", str(kwargs))
                return func(*args, **kwargs)
        
        if asyncio.iscoroutinefunction(func):
            return async_wrapper
        return sync_wrapper
    
    return decorator
