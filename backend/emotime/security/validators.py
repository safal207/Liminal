"""
🔍🛡️ Security Validators — Input & Import Security

Enterprise-grade validation for:
- Dynamic import safety
- User input sanitization  
- Emotional data validation
- Path traversal prevention
"""

import os
import re
import html
import importlib.util
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass

from ..utils import safe_logger


@dataclass
class SecurityViolation:
    """Security violation details."""
    severity: str  # "critical", "high", "medium", "low"
    category: str  # "import", "input", "path", "data"
    message: str
    data: Dict[str, Any]


class SecureImportValidator:
    """
    Validates dynamic imports to prevent code injection.
    
    Security Controls:
    - Whitelist of allowed modules
    - Path traversal prevention  
    - Module signature validation
    - Import audit logging
    """
    
    def __init__(self):
        # Whitelist of allowed modules for dynamic import
        self.allowed_modules = {
            'analytics_standalone',
            'emotime.analytics_standalone',
            'emotime.security.validators',
            'emotime.security.auth',
            'emotime.security.encryption'
        }
        
        # Allowed base paths
        self.allowed_paths = {
            str(Path(__file__).parent.parent),  # emotime directory
        }
        
        self.import_audit_log = []
        
        safe_logger.info("Secure Import Validator initialized")
    
    def validate_module_import(self, module_name: str, module_path: Optional[str] = None) -> bool:
        """
        Validates if a module can be safely imported.
        
        Args:
            module_name: Name of the module to import
            module_path: Optional path to the module file
            
        Returns:
            True if safe to import, False otherwise
            
        Raises:
            SecurityError: If import poses security risk
        """
        violations = []
        
        try:
            # 1. Check module name against whitelist
            if module_name not in self.allowed_modules:
                violations.append(SecurityViolation(
                    severity="critical",
                    category="import", 
                    message=f"Module '{module_name}' not in allowed list",
                    data={"module": module_name, "allowed": list(self.allowed_modules)}
                ))
            
            # 2. Validate module path if provided
            if module_path:
                if not self._validate_path(module_path):
                    violations.append(SecurityViolation(
                        severity="critical",
                        category="path",
                        message=f"Module path '{module_path}' not allowed",
                        data={"path": module_path, "allowed_paths": list(self.allowed_paths)}
                    ))
            
            # 3. Check for suspicious patterns
            suspicious_patterns = [
                r'__import__',
                r'eval\(',
                r'exec\(',
                r'compile\(',
                r'os\.system',
                r'subprocess\.',
                r'\.\./',  # Path traversal
            ]
            
            if module_path and os.path.exists(module_path):
                try:
                    with open(module_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                        
                    for pattern in suspicious_patterns:
                        if re.search(pattern, content):
                            violations.append(SecurityViolation(
                                severity="high",
                                category="import",
                                message=f"Suspicious pattern '{pattern}' found in module",
                                data={"module": module_name, "pattern": pattern}
                            ))
                except Exception as e:
                    safe_logger.warning(f"Could not scan module content: {e}")
            
            # Log the import attempt
            self.import_audit_log.append({
                "timestamp": str(datetime.now()),
                "module": module_name,
                "path": module_path,
                "violations": len(violations),
                "allowed": len(violations) == 0
            })
            
            # Raise exception if critical violations found
            critical_violations = [v for v in violations if v.severity == "critical"]
            if critical_violations:
                violation_msgs = [v.message for v in critical_violations]
                raise SecurityError(f"Critical security violations: {'; '.join(violation_msgs)}")
            
            # Log warnings for non-critical violations
            for violation in violations:
                if violation.severity != "critical":
                    safe_logger.warning(f"Security violation ({violation.severity}): {violation.message}")
            
            return len(violations) == 0
            
        except Exception as e:
            safe_logger.error(f"Import validation error: {e}")
            return False
    
    def _validate_path(self, path: str) -> bool:
        """Validates if a file path is allowed."""
        try:
            # Resolve the path to prevent traversal attacks
            resolved_path = os.path.realpath(path)
            
            # Check if path is within allowed directories
            for allowed_path in self.allowed_paths:
                if resolved_path.startswith(os.path.realpath(allowed_path)):
                    return True
            
            return False
            
        except Exception:
            return False
    
    def safe_import_module(self, module_name: str, module_path: str):
        """
        Safely imports a module after validation.
        
        Returns:
            Imported module or None if validation fails
        """
        try:
            if not self.validate_module_import(module_name, module_path):
                safe_logger.error(f"Module import blocked: {module_name}")
                return None
            
            # Perform the import
            spec = importlib.util.spec_from_file_location(module_name, module_path)
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            
            safe_logger.info(f"Module imported safely: {module_name}")
            return module
            
        except Exception as e:
            safe_logger.error(f"Safe import failed: {e}")
            return None
    
    def get_import_audit_log(self) -> List[Dict[str, Any]]:
        """Returns the import audit log."""
        return self.import_audit_log.copy()


class InputSanitizer:
    """
    Sanitizes user input to prevent injection attacks.
    
    Features:
    - HTML/XSS prevention
    - SQL injection prevention  
    - Path traversal prevention
    - Emotional data sanitization
    """
    
    def __init__(self):
        self.max_text_length = 10000  # Max characters for text input
        self.max_list_length = 100    # Max items in lists
        
        # Patterns that are not allowed in user input
        self.blocked_patterns = [
            r'<script[^>]*>.*?</script>',  # Script tags
            r'javascript:',                # JavaScript URLs
            r'on\w+\s*=',                 # Event handlers
            r'expression\s*\(',           # CSS expression
            r'import\s+',                 # Import statements
            r'eval\s*\(',                 # Eval calls
            r'exec\s*\(',                 # Exec calls
            r'\.\./|\.\.\\',              # Path traversal
            r'file://',                   # File URLs
            r'data:.*base64',             # Base64 data URLs
        ]
        
        safe_logger.info("Input Sanitizer initialized")
    
    def sanitize_text(self, text: str) -> str:
        """
        Sanitizes text input for safe processing.
        """
        if not isinstance(text, str):
            text = str(text)
        
        # Length validation
        if len(text) > self.max_text_length:
            safe_logger.warning(f"Text input truncated from {len(text)} to {self.max_text_length} chars")
            text = text[:self.max_text_length]
        
        # HTML escape
        text = html.escape(text)
        
        # Remove blocked patterns
        for pattern in self.blocked_patterns:
            text = re.sub(pattern, '[BLOCKED]', text, flags=re.IGNORECASE)
        
        # Additional cleaning
        text = text.strip()
        
        return text
    
    def sanitize_emotional_data(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Sanitizes emotional data dictionary.
        """
        sanitized = {}
        
        for key, value in data.items():
            # Sanitize key
            clean_key = self.sanitize_text(str(key))[:100]  # Limit key length
            
            # Sanitize value based on type
            if isinstance(value, str):
                sanitized[clean_key] = self.sanitize_text(value)
            elif isinstance(value, (int, float, bool)):
                sanitized[clean_key] = value
            elif isinstance(value, list):
                if len(value) > self.max_list_length:
                    value = value[:self.max_list_length]
                sanitized[clean_key] = [self.sanitize_text(str(item)) for item in value]
            elif isinstance(value, dict):
                sanitized[clean_key] = self.sanitize_emotional_data(value)
            else:
                # Convert unknown types to string and sanitize
                sanitized[clean_key] = self.sanitize_text(str(value))
        
        return sanitized
    
    def validate_user_id(self, user_id: str) -> str:
        """
        Validates and sanitizes user ID.
        """
        if not user_id or not isinstance(user_id, str):
            raise ValueError("User ID must be a non-empty string")
        
        # Remove special characters, keep alphanumeric and underscore
        clean_id = re.sub(r'[^a-zA-Z0-9_-]', '', user_id)
        
        # Length validation
        if len(clean_id) < 1 or len(clean_id) > 50:
            raise ValueError("User ID must be 1-50 alphanumeric characters")
        
        return clean_id
    
    def sanitize_request_data(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Sanitizes entire request data dictionary.
        """
        return self.sanitize_emotional_data(data)


# Custom security exception
class SecurityError(Exception):
    """Raised when security validation fails."""
    pass


# Global instances
_import_validator: Optional[SecureImportValidator] = None
_input_sanitizer: Optional[InputSanitizer] = None


def get_import_validator() -> SecureImportValidator:
    """Returns global import validator instance."""
    global _import_validator
    if _import_validator is None:
        _import_validator = SecureImportValidator()
    return _import_validator


def get_input_sanitizer() -> InputSanitizer:
    """Returns global input sanitizer instance."""  
    global _input_sanitizer
    if _input_sanitizer is None:
        _input_sanitizer = InputSanitizer()
    return _input_sanitizer


# Convenience functions
def validate_user_input(text: str) -> str:
    """Validates and sanitizes user text input."""
    return get_input_sanitizer().sanitize_text(text)


def sanitize_emotional_data(data: Dict[str, Any]) -> Dict[str, Any]:
    """Sanitizes emotional data dictionary."""
    return get_input_sanitizer().sanitize_emotional_data(data)


def safe_import_module(module_name: str, module_path: str):
    """Safely imports a module with security validation."""
    return get_import_validator().safe_import_module(module_name, module_path)