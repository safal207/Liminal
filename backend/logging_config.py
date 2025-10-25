"""
Structured logging configuration for LIMINAL
Provides consistent, searchable, and structured logs across all components
"""
import logging
import sys
from typing import Any, Dict

try:
    import structlog
    from structlog.types import Processor
    STRUCTLOG_AVAILABLE = True
except ImportError:
    # Fallback for when structlog is not available
    STRUCTLOG_AVAILABLE = False
    
    class MockBoundLogger:
        def __init__(self, name):
            self.logger = logging.getLogger(name)
        
        def info(self, msg, **kwargs):
            self.logger.info(f"{msg} {kwargs}")
        
        def debug(self, msg, **kwargs):
            self.logger.debug(f"{msg} {kwargs}")
        
        def warning(self, msg, **kwargs):
            self.logger.warning(f"{msg} {kwargs}")
        
        def error(self, msg, **kwargs):
            self.logger.error(f"{msg} {kwargs}")
        
        def critical(self, msg, **kwargs):
            self.logger.critical(f"{msg} {kwargs}")


def configure_structured_logging(
    log_level: str = "INFO",
    service_name: str = "liminal",
    enable_json: bool = True
) -> None:
    """
    Configure structured logging for the application
    
    Args:
        log_level: Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
        service_name: Name of the service for log identification
        enable_json: Whether to output JSON format (True for production)
    """
    
    # Convert string level to logging constant
    numeric_level = getattr(logging, log_level.upper(), logging.INFO)
    
    # Configure standard library logging
    logging.basicConfig(
        format="%(message)s",
        stream=sys.stdout,
        level=numeric_level,
    )
    
    if not STRUCTLOG_AVAILABLE:
        # Use basic logging if structlog is not available
        logging.getLogger().info(f"Using basic logging for {service_name}")
        return
    
    # Shared processors for all loggers
    shared_processors = [
        # Add service name to all logs
        structlog.processors.add_log_level,
        structlog.processors.add_logger_name,
        structlog.stdlib.add_logger_name,
        structlog.stdlib.add_log_level,
        structlog.stdlib.PositionalArgumentsFormatter(),
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.StackInfoRenderer(),
        structlog.processors.format_exc_info,
        structlog.processors.UnicodeDecoder(),
        # Add service context
        lambda _, __, event_dict: {**event_dict, "service": service_name},
    ]
    
    if enable_json:
        # JSON output for production/structured logging systems
        shared_processors.append(structlog.processors.JSONRenderer())
    else:
        # Human-readable output for development
        shared_processors.append(
            structlog.dev.ConsoleRenderer(colors=True)
        )
    
    # Configure structlog
    structlog.configure(
        processors=shared_processors,
        wrapper_class=structlog.stdlib.BoundLogger,
        logger_factory=structlog.stdlib.LoggerFactory(),
        context_class=dict,
        cache_logger_on_first_use=True,
    )


def get_logger(name: str):
    """
    Get a structured logger instance
    
    Args:
        name: Logger name (usually __name__)
        
    Returns:
        Configured structured logger
    """
    if STRUCTLOG_AVAILABLE:
        return structlog.get_logger(name)
    else:
        return MockBoundLogger(name)


def log_request_context(
    user_id: str = None,
    request_id: str = None,
    endpoint: str = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Create request context for logging
    
    Args:
        user_id: User identifier
        request_id: Request trace ID
        endpoint: API endpoint being called
        **kwargs: Additional context
        
    Returns:
        Context dictionary for structured logging
    """
    context = {}
    
    if user_id:
        context["user_id"] = user_id
    if request_id:
        context["request_id"] = request_id
    if endpoint:
        context["endpoint"] = endpoint
        
    context.update(kwargs)
    return context


def log_ml_context(
    model_name: str = None,
    model_version: str = None,
    input_shape: tuple = None,
    processing_time_ms: float = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Create ML context for logging
    
    Args:
        model_name: Name of the ML model
        model_version: Version of the model
        input_shape: Shape of input data
        processing_time_ms: Processing time in milliseconds
        **kwargs: Additional ML context
        
    Returns:
        Context dictionary for ML logging
    """
    context = {"component": "ml"}
    
    if model_name:
        context["model_name"] = model_name
    if model_version:
        context["model_version"] = model_version
    if input_shape:
        context["input_shape"] = input_shape
    if processing_time_ms:
        context["processing_time_ms"] = processing_time_ms
        
    context.update(kwargs)
    return context


def log_db_context(
    operation: str = None,
    table: str = None,
    query_time_ms: float = None,
    affected_rows: int = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Create database context for logging
    
    Args:
        operation: Database operation (SELECT, INSERT, UPDATE, etc.)
        table: Table/collection name
        query_time_ms: Query execution time in milliseconds
        affected_rows: Number of affected rows
        **kwargs: Additional DB context
        
    Returns:
        Context dictionary for database logging
    """
    context = {"component": "database"}
    
    if operation:
        context["db_operation"] = operation
    if table:
        context["db_table"] = table
    if query_time_ms:
        context["query_time_ms"] = query_time_ms
    if affected_rows:
        context["affected_rows"] = affected_rows
        
    context.update(kwargs)
    return context


# Example usage patterns
if __name__ == "__main__":
    # Configure logging
    configure_structured_logging(log_level="DEBUG", enable_json=False)
    
    # Get logger
    logger = get_logger(__name__)
    
    # Basic logging
    logger.info("Application started")
    
    # With context
    logger.info(
        "User action completed",
        **log_request_context(
            user_id="user123",
            request_id="req456",
            endpoint="/api/health",
            action="health_check"
        )
    )
    
    # ML logging
    logger.info(
        "Model inference completed",
        **log_ml_context(
            model_name="temporal_transformer",
            model_version="v1.2.3",
            input_shape=(32, 128),
            processing_time_ms=45.2,
            confidence_score=0.87
        )
    )
    
    # Database logging
    logger.info(
        "Database query executed",
        **log_db_context(
            operation="SELECT",
            table="user_patterns",
            query_time_ms=12.5,
            affected_rows=1
        )
    )
