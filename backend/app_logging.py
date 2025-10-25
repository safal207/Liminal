"""
Application-wide logging initialization for LIMINAL
Configures structured logging on startup
"""
import os
from logging_config import configure_structured_logging


def init_logging():
    """Initialize structured logging for the application"""
    
    # Get configuration from environment
    log_level = os.getenv("LOG_LEVEL", "INFO")
    service_name = os.getenv("SERVICE_NAME", "liminal-backend")
    
    # Enable JSON in production, human-readable in development
    enable_json = os.getenv("LOG_FORMAT", "json").lower() == "json"
    
    # Configure structured logging
    configure_structured_logging(
        log_level=log_level,
        service_name=service_name,
        enable_json=enable_json
    )
    
    # Log initialization
    from logging_config import get_logger
    logger = get_logger(__name__)
    
    logger.info(
        "Structured logging initialized",
        log_level=log_level,
        service_name=service_name,
        json_format=enable_json
    )


if __name__ == "__main__":
    # Test logging configuration
    init_logging()
    
    from logging_config import get_logger
    logger = get_logger("test")
    
    logger.debug("Debug message")
    logger.info("Info message", test_field="test_value")
    logger.warning("Warning message", component="test")
    logger.error("Error message", error_code=500)
