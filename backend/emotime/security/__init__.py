"""
🔒🛡️ Emotime Security Module — Enterprise-grade Security

Comprehensive security controls for Emotime system:
- Dynamic import validation
- Input sanitization
- Authentication & authorization
- Encryption utilities
- Rate limiting
- Audit logging
"""

from .auth import JWTAuthenticator, generate_secure_token, validate_token
from .encryption import (
    EmotionalDataEncryptor,
    decrypt_sensitive_data,
    encrypt_sensitive_data,
)
from .validators import (
    InputSanitizer,
    SecureImportValidator,
    sanitize_emotional_data,
    validate_user_input,
)

__all__ = [
    "SecureImportValidator",
    "InputSanitizer",
    "validate_user_input",
    "sanitize_emotional_data",
    "JWTAuthenticator",
    "generate_secure_token",
    "validate_token",
    "EmotionalDataEncryptor",
    "encrypt_sensitive_data",
    "decrypt_sensitive_data",
]
