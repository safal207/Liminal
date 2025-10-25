"""
Simple fallback implementation of structlog for testing.
This provides basic logging functionality when structlog is not available.
"""

import logging
import json
from typing import Any, Dict

# Configure basic logging
logging.basicConfig(level=logging.INFO)

class BoundLogger:
    """Simple bound logger that mimics structlog's BoundLogger."""
    
    def __init__(self, logger_name: str, context: Dict[str, Any] = None):
        self.logger = logging.getLogger(logger_name)
        self.context = context or {}
    
    def bind(self, **kwargs) -> 'BoundLogger':
        """Bind additional context."""
        new_context = {**self.context, **kwargs}
        return BoundLogger(self.logger.name, new_context)
    
    def _log(self, level: str, msg: str, **kwargs):
        """Internal logging method."""
        log_data = {**self.context, **kwargs}
        if log_data:
            formatted_msg = f"{msg} | {json.dumps(log_data)}"
        else:
            formatted_msg = msg
        
        log_method = getattr(self.logger, level.lower())
        log_method(formatted_msg)
    
    def debug(self, msg: str, **kwargs):
        self._log('debug', msg, **kwargs)
    
    def info(self, msg: str, **kwargs):
        self._log('info', msg, **kwargs)
    
    def warning(self, msg: str, **kwargs):
        self._log('warning', msg, **kwargs)
    
    def error(self, msg: str, **kwargs):
        self._log('error', msg, **kwargs)
    
    def critical(self, msg: str, **kwargs):
        self._log('critical', msg, **kwargs)

def get_logger(name: str = None) -> BoundLogger:
    """Get a logger instance."""
    if name is None:
        name = __name__
    return BoundLogger(name)

# Alias for compatibility
getLogger = get_logger

# Configuration function (no-op for basic implementation)
def configure():
    """Configure structlog (no-op in fallback implementation)."""
    pass

# Types module for compatibility
class types:
    """Mock types module."""
    
    @staticmethod
    def Processor(func):
        """Mock processor decorator."""
        return func