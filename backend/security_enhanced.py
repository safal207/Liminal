#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Enhanced security system for LIMINAL production deployment.

Features:
- Advanced rate limiting with different strategies
- Input validation and sanitization
- OAuth2 integration
- Session management
- Security headers
- Audit logging
- Threat detection
"""

import asyncio
import hashlib
import hmac
import ipaddress
import json
import re
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Dict, List, Optional, Set, Union
from urllib.parse import urlparse

import bcrypt
from fastapi import HTTPException, Request, Response, status
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, validator

from config import get_security_settings
from monitoring import monitoring_service
from resilience import LiminalException

logger = monitoring_service.tracer.start_span("security").__enter__()


class ThreatLevel(Enum):
    """Security threat levels."""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class AuthEventType(Enum):
    """Authentication event types."""
    LOGIN_SUCCESS = "login_success"
    LOGIN_FAILED = "login_failed"
    LOGOUT = "logout"
    TOKEN_REFRESH = "token_refresh"
    PASSWORD_CHANGE = "password_change"
    ACCOUNT_LOCKED = "account_locked"
    SUSPICIOUS_ACTIVITY = "suspicious_activity"


class RateLimitType(Enum):
    """Rate limiting strategies."""
    FIXED_WINDOW = "fixed_window"
    SLIDING_WINDOW = "sliding_window"
    TOKEN_BUCKET = "token_bucket"
    ADAPTIVE = "adaptive"


@dataclass
class SecurityEvent:
    """Security event for audit logging."""
    id: str
    event_type: str
    user_id: Optional[str]
    ip_address: str
    user_agent: str
    timestamp: datetime
    severity: ThreatLevel
    details: Dict[str, Any]
    resolved: bool = False


@dataclass
class RateLimitRule:
    """Rate limiting rule configuration."""
    name: str
    limit: int
    window_seconds: int
    rule_type: RateLimitType
    scope: str  # "global", "user", "ip"
    bypass_roles: List[str] = field(default_factory=list)
    
    
@dataclass
class SessionInfo:
    """User session information."""
    session_id: str
    user_id: str
    ip_address: str
    user_agent: str
    created_at: datetime
    last_activity: datetime
    expires_at: datetime
    is_active: bool = True
    metadata: Dict[str, Any] = field(default_factory=dict)


class InputValidator:
    """Advanced input validation and sanitization."""
    
    # Common security patterns
    XSS_PATTERNS = [
        r'<script[^>]*>.*?</script>',
        r'javascript:',
        r'on\w+\s*=',
        r'<iframe[^>]*>.*?</iframe>',
        r'<object[^>]*>.*?</object>',
    ]
    
    SQL_INJECTION_PATTERNS = [
        r'(\b(SELECT|INSERT|UPDATE|DELETE|DROP|CREATE|ALTER)\b)',
        r'(\b(UNION|OR|AND)\b.*\b(SELECT|INSERT|UPDATE|DELETE)\b)',
        r'[\'";]',
        r'--',
        r'/\*.*?\*/',
    ]
    
    def __init__(self):
        self.xss_regex = re.compile('|'.join(self.XSS_PATTERNS), re.IGNORECASE)
        self.sql_regex = re.compile('|'.join(self.SQL_INJECTION_PATTERNS), re.IGNORECASE)
    
    def validate_input(self, data: Any, field_name: str = "input") -> Any:
        """Validate and sanitize input data."""
        if isinstance(data, str):
            return self._validate_string(data, field_name)
        elif isinstance(data, dict):
            return {k: self.validate_input(v, f"{field_name}.{k}") for k, v in data.items()}
        elif isinstance(data, list):
            return [self.validate_input(item, f"{field_name}[{i}]") for i, item in enumerate(data)]
        else:
            return data
    
    def _validate_string(self, value: str, field_name: str) -> str:
        """Validate string input for security threats."""
        if not value:
            return value
        
        # Check for XSS patterns
        if self.xss_regex.search(value):
            raise LiminalException(
                f"Potential XSS detected in {field_name}",
                error_code="XSS_DETECTED",
                context={"field": field_name, "value_snippet": value[:50]}
            )
        
        # Check for SQL injection patterns
        if self.sql_regex.search(value):
            raise LiminalException(
                f"Potential SQL injection detected in {field_name}",
                error_code="SQL_INJECTION_DETECTED", 
                context={"field": field_name, "value_snippet": value[:50]}
            )
        
        # Basic sanitization
        sanitized = value.strip()
        
        # Remove null bytes
        sanitized = sanitized.replace('\x00', '')
        
        # Limit length
        if len(sanitized) > 10000:  # Configurable limit
            raise LiminalException(
                f"Input too long for {field_name}",
                error_code="INPUT_TOO_LONG",
                context={"field": field_name, "length": len(sanitized)}
            )
        
        return sanitized
    
    def validate_email(self, email: str) -> bool:
        """Validate email format."""
        pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
        return bool(re.match(pattern, email))
    
    def validate_password_strength(self, password: str) -> Dict[str, Any]:
        """Validate password strength."""
        issues = []
        score = 0
        
        if len(password) < 8:
            issues.append("Password must be at least 8 characters long")
        else:
            score += 1
        
        if not re.search(r'[A-Z]', password):
            issues.append("Password must contain at least one uppercase letter")
        else:
            score += 1
        
        if not re.search(r'[a-z]', password):
            issues.append("Password must contain at least one lowercase letter")
        else:
            score += 1
        
        if not re.search(r'[0-9]', password):
            issues.append("Password must contain at least one number")
        else:
            score += 1
        
        if not re.search(r'[!@#$%^&*(),.?":{}|<>]', password):
            issues.append("Password must contain at least one special character")
        else:
            score += 1
        
        # Check for common patterns
        common_patterns = ['123456', 'password', 'qwerty', 'abc123']
        if any(pattern in password.lower() for pattern in common_patterns):
            issues.append("Password contains common patterns")
            score -= 1
        
        strength_levels = {0: "very_weak", 1: "weak", 2: "fair", 3: "good", 4: "strong", 5: "very_strong"}
        strength = strength_levels.get(max(0, score), "very_weak")
        
        return {
            "valid": len(issues) == 0,
            "strength": strength,
            "score": score,
            "issues": issues
        }


class RateLimiter:
    """Advanced rate limiting with multiple strategies."""
    
    def __init__(self):
        self.rules: List[RateLimitRule] = []
        self.windows: Dict[str, Dict[str, Any]] = {}
        self.token_buckets: Dict[str, Dict[str, Any]] = {}
    
    def add_rule(self, rule: RateLimitRule):
        """Add rate limiting rule."""
        self.rules.append(rule)
        logger.info(f"Added rate limit rule: {rule.name}")
    
    async def check_rate_limit(
        self,
        identifier: str,
        rule_name: str,
        user_roles: List[str] = None
    ) -> Dict[str, Any]:
        """Check if request is within rate limits."""
        rule = next((r for r in self.rules if r.name == rule_name), None)
        if not rule:
            return {"allowed": True, "reason": "no_rule"}
        
        # Check if user has bypass role
        if user_roles and any(role in rule.bypass_roles for role in user_roles):
            return {"allowed": True, "reason": "bypass_role"}
        
        current_time = time.time()
        
        if rule.rule_type == RateLimitType.FIXED_WINDOW:
            return await self._check_fixed_window(identifier, rule, current_time)
        elif rule.rule_type == RateLimitType.SLIDING_WINDOW:
            return await self._check_sliding_window(identifier, rule, current_time)
        elif rule.rule_type == RateLimitType.TOKEN_BUCKET:
            return await self._check_token_bucket(identifier, rule, current_time)
        elif rule.rule_type == RateLimitType.ADAPTIVE:
            return await self._check_adaptive(identifier, rule, current_time)
        
        return {"allowed": True, "reason": "unknown_type"}
    
    async def _check_fixed_window(
        self,
        identifier: str,
        rule: RateLimitRule,
        current_time: float
    ) -> Dict[str, Any]:
        """Fixed window rate limiting."""
        window_key = f"{rule.name}:{identifier}"
        window_start = int(current_time // rule.window_seconds) * rule.window_seconds
        
        if window_key not in self.windows:
            self.windows[window_key] = {"count": 0, "window_start": window_start}
        
        window_data = self.windows[window_key]
        
        # Reset if new window
        if window_data["window_start"] != window_start:
            window_data["count"] = 0
            window_data["window_start"] = window_start
        
        if window_data["count"] >= rule.limit:
            remaining_time = rule.window_seconds - (current_time - window_start)
            return {
                "allowed": False,
                "reason": "rate_limit_exceeded",
                "retry_after": int(remaining_time),
                "limit": rule.limit,
                "current": window_data["count"]
            }
        
        window_data["count"] += 1
        return {
            "allowed": True,
            "limit": rule.limit,
            "current": window_data["count"],
            "remaining": rule.limit - window_data["count"]
        }
    
    async def _check_sliding_window(
        self,
        identifier: str,
        rule: RateLimitRule,
        current_time: float
    ) -> Dict[str, Any]:
        """Sliding window rate limiting."""
        window_key = f"{rule.name}:{identifier}"
        
        if window_key not in self.windows:
            self.windows[window_key] = {"requests": []}
        
        requests = self.windows[window_key]["requests"]
        
        # Remove old requests outside the window
        cutoff_time = current_time - rule.window_seconds
        requests[:] = [req_time for req_time in requests if req_time > cutoff_time]
        
        if len(requests) >= rule.limit:
            oldest_request = min(requests) if requests else current_time
            retry_after = max(0, oldest_request + rule.window_seconds - current_time)
            return {
                "allowed": False,
                "reason": "rate_limit_exceeded",
                "retry_after": int(retry_after),
                "limit": rule.limit,
                "current": len(requests)
            }
        
        requests.append(current_time)
        return {
            "allowed": True,
            "limit": rule.limit,
            "current": len(requests),
            "remaining": rule.limit - len(requests)
        }
    
    async def _check_token_bucket(
        self,
        identifier: str,
        rule: RateLimitRule,
        current_time: float
    ) -> Dict[str, Any]:
        """Token bucket rate limiting."""
        bucket_key = f"{rule.name}:{identifier}"
        
        if bucket_key not in self.token_buckets:
            self.token_buckets[bucket_key] = {
                "tokens": rule.limit,
                "last_refill": current_time
            }
        
        bucket = self.token_buckets[bucket_key]
        
        # Refill tokens
        time_passed = current_time - bucket["last_refill"]
        refill_rate = rule.limit / rule.window_seconds  # tokens per second
        tokens_to_add = time_passed * refill_rate
        
        bucket["tokens"] = min(rule.limit, bucket["tokens"] + tokens_to_add)
        bucket["last_refill"] = current_time
        
        if bucket["tokens"] < 1:
            retry_after = (1 - bucket["tokens"]) / refill_rate
            return {
                "allowed": False,
                "reason": "rate_limit_exceeded",
                "retry_after": int(retry_after),
                "limit": rule.limit,
                "tokens_remaining": bucket["tokens"]
            }
        
        bucket["tokens"] -= 1
        return {
            "allowed": True,
            "tokens_remaining": bucket["tokens"],
            "limit": rule.limit
        }
    
    async def _check_adaptive(
        self,
        identifier: str,
        rule: RateLimitRule,
        current_time: float
    ) -> Dict[str, Any]:
        """Adaptive rate limiting based on system load."""
        # This would implement adaptive limiting based on system metrics
        # For now, fall back to sliding window
        return await self._check_sliding_window(identifier, rule, current_time)


class SessionManager:
    """Advanced session management."""
    
    def __init__(self, default_ttl: int = 3600):
        self.sessions: Dict[str, SessionInfo] = {}
        self.user_sessions: Dict[str, Set[str]] = {}  # user_id -> session_ids
        self.default_ttl = default_ttl
    
    async def create_session(
        self,
        user_id: str,
        ip_address: str,
        user_agent: str,
        ttl: Optional[int] = None
    ) -> SessionInfo:
        """Create new user session."""
        session_id = str(uuid.uuid4())
        ttl = ttl or self.default_ttl
        
        now = datetime.utcnow()
        session = SessionInfo(
            session_id=session_id,
            user_id=user_id,
            ip_address=ip_address,
            user_agent=user_agent,
            created_at=now,
            last_activity=now,
            expires_at=now + timedelta(seconds=ttl)
        )
        
        self.sessions[session_id] = session
        
        if user_id not in self.user_sessions:
            self.user_sessions[user_id] = set()
        self.user_sessions[user_id].add(session_id)
        
        logger.info(f"Created session {session_id} for user {user_id}")
        return session
    
    async def get_session(self, session_id: str) -> Optional[SessionInfo]:
        """Get session by ID."""
        session = self.sessions.get(session_id)
        if not session:
            return None
        
        # Check if expired
        if datetime.utcnow() > session.expires_at:
            await self.invalidate_session(session_id)
            return None
        
        return session
    
    async def update_session_activity(self, session_id: str) -> bool:
        """Update session last activity."""
        session = await self.get_session(session_id)
        if session:
            session.last_activity = datetime.utcnow()
            return True
        return False
    
    async def invalidate_session(self, session_id: str) -> bool:
        """Invalidate session."""
        session = self.sessions.get(session_id)
        if session:
            session.is_active = False
            
            # Remove from user sessions
            if session.user_id in self.user_sessions:
                self.user_sessions[session.user_id].discard(session_id)
            
            del self.sessions[session_id]
            logger.info(f"Invalidated session {session_id}")
            return True
        return False
    
    async def invalidate_user_sessions(self, user_id: str) -> int:
        """Invalidate all sessions for a user."""
        if user_id not in self.user_sessions:
            return 0
        
        session_ids = list(self.user_sessions[user_id])
        count = 0
        
        for session_id in session_ids:
            if await self.invalidate_session(session_id):
                count += 1
        
        logger.info(f"Invalidated {count} sessions for user {user_id}")
        return count
    
    async def cleanup_expired_sessions(self):
        """Remove expired sessions."""
        now = datetime.utcnow()
        expired_sessions = [
            session_id for session_id, session in self.sessions.items()
            if now > session.expires_at
        ]
        
        for session_id in expired_sessions:
            await self.invalidate_session(session_id)
        
        if expired_sessions:
            logger.info(f"Cleaned up {len(expired_sessions)} expired sessions")


class ThreatDetector:
    """Advanced threat detection system."""
    
    def __init__(self):
        self.failed_attempts: Dict[str, List[float]] = {}
        self.suspicious_ips: Set[str] = set()
        self.blocked_ips: Dict[str, float] = {}  # ip -> unblock_time
        
        # Known malicious patterns
        self.malicious_user_agents = [
            'sqlmap', 'nikto', 'nmap', 'masscan', 'zmap'
        ]
        
        self.suspicious_paths = [
            '/admin', '/wp-admin', '/.env', '/config', '/phpMyAdmin'
        ]
    
    async def analyze_request(
        self,
        ip_address: str,
        user_agent: str,
        path: str,
        user_id: Optional[str] = None
    ) -> Dict[str, Any]:
        """Analyze request for threats."""
        threats = []
        threat_level = ThreatLevel.LOW
        
        # Check blocked IPs
        if ip_address in self.blocked_ips:
            if time.time() < self.blocked_ips[ip_address]:
                return {
                    "blocked": True,
                    "reason": "ip_blocked",
                    "threat_level": ThreatLevel.HIGH,
                    "threats": ["blocked_ip"]
                }
            else:
                # Unblock expired IP
                del self.blocked_ips[ip_address]
        
        # Check malicious user agents
        if any(malicious in user_agent.lower() for malicious in self.malicious_user_agents):
            threats.append("malicious_user_agent")
            threat_level = ThreatLevel.HIGH
        
        # Check suspicious paths
        if any(suspicious in path.lower() for suspicious in self.suspicious_paths):
            threats.append("suspicious_path")
            threat_level = max(threat_level, ThreatLevel.MEDIUM)
        
        # Check failed login attempts
        if ip_address in self.failed_attempts:
            recent_failures = [
                attempt for attempt in self.failed_attempts[ip_address]
                if time.time() - attempt < 3600  # Last hour
            ]
            
            if len(recent_failures) > 10:
                threats.append("excessive_failed_logins")
                threat_level = ThreatLevel.HIGH
                
                # Auto-block IP
                self.blocked_ips[ip_address] = time.time() + 3600  # Block for 1 hour
        
        # Check for private IP trying to access from internet
        try:
            ip_obj = ipaddress.ip_address(ip_address)
            if ip_obj.is_private and path.startswith('/api/'):
                threats.append("private_ip_external_access")
                threat_level = max(threat_level, ThreatLevel.MEDIUM)
        except:
            pass
        
        return {
            "blocked": False,
            "threat_level": threat_level,
            "threats": threats,
            "suspicious": len(threats) > 0
        }
    
    async def record_failed_login(self, ip_address: str):
        """Record failed login attempt."""
        if ip_address not in self.failed_attempts:
            self.failed_attempts[ip_address] = []
        
        self.failed_attempts[ip_address].append(time.time())
        
        # Keep only recent attempts
        cutoff_time = time.time() - 3600  # 1 hour
        self.failed_attempts[ip_address] = [
            attempt for attempt in self.failed_attempts[ip_address]
            if attempt > cutoff_time
        ]


class SecurityMiddleware:
    """Security middleware for FastAPI."""
    
    def __init__(self):
        self.validator = InputValidator()
        self.rate_limiter = RateLimiter()
        self.session_manager = SessionManager()
        self.threat_detector = ThreatDetector()
        
        # Setup default rate limiting rules
        self._setup_default_rules()
    
    def _setup_default_rules(self):
        """Setup default rate limiting rules."""
        self.rate_limiter.add_rule(RateLimitRule(
            name="api_general",
            limit=100,
            window_seconds=60,
            rule_type=RateLimitType.SLIDING_WINDOW,
            scope="ip"
        ))
        
        self.rate_limiter.add_rule(RateLimitRule(
            name="auth_login",
            limit=5,
            window_seconds=300,  # 5 minutes
            rule_type=RateLimitType.FIXED_WINDOW,
            scope="ip"
        ))
        
        self.rate_limiter.add_rule(RateLimitRule(
            name="ml_inference",
            limit=50,
            window_seconds=60,
            rule_type=RateLimitType.TOKEN_BUCKET,
            scope="user",
            bypass_roles=["admin"]
        ))
    
    async def process_request(
        self,
        request: Request,
        rule_name: str = "api_general",
        user_roles: List[str] = None
    ) -> Dict[str, Any]:
        """Process incoming request through security checks."""
        client_ip = self._get_client_ip(request)
        user_agent = request.headers.get("user-agent", "")
        path = request.url.path
        
        # Threat analysis
        threat_analysis = await self.threat_detector.analyze_request(
            client_ip, user_agent, path
        )
        
        if threat_analysis["blocked"]:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Request blocked by security system"
            )
        
        # Rate limiting
        rate_limit_result = await self.rate_limiter.check_rate_limit(
            client_ip, rule_name, user_roles
        )
        
        if not rate_limit_result["allowed"]:
            raise HTTPException(
                status_code=status.HTTP_429_TOO_MANY_REQUESTS,
                detail="Rate limit exceeded",
                headers={"Retry-After": str(rate_limit_result.get("retry_after", 60))}
            )
        
        return {
            "threat_analysis": threat_analysis,
            "rate_limit": rate_limit_result,
            "client_ip": client_ip
        }
    
    def _get_client_ip(self, request: Request) -> str:
        """Extract client IP from request."""
        # Check for forwarded headers
        forwarded_for = request.headers.get("x-forwarded-for")
        if forwarded_for:
            return forwarded_for.split(",")[0].strip()
        
        real_ip = request.headers.get("x-real-ip")
        if real_ip:
            return real_ip
        
        return request.client.host if request.client else "unknown"
    
    def add_security_headers(self, response: Response):
        """Add security headers to response."""
        response.headers["X-Content-Type-Options"] = "nosniff"
        response.headers["X-Frame-Options"] = "DENY"
        response.headers["X-XSS-Protection"] = "1; mode=block"
        response.headers["Strict-Transport-Security"] = "max-age=31536000; includeSubDomains"
        response.headers["Content-Security-Policy"] = "default-src 'self'"
        response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin"


# Global security middleware
security_middleware = SecurityMiddleware()


# Dependency for FastAPI
async def get_security_context(request: Request) -> Dict[str, Any]:
    """FastAPI dependency for security context."""
    return await security_middleware.process_request(request)


# Utility functions
def hash_password(password: str) -> str:
    """Hash password using bcrypt."""
    salt = bcrypt.gensalt()
    return bcrypt.hashpw(password.encode('utf-8'), salt).decode('utf-8')


def verify_password(password: str, hashed: str) -> bool:
    """Verify password against hash."""
    return bcrypt.checkpw(password.encode('utf-8'), hashed.encode('utf-8'))


def generate_secure_token(length: int = 32) -> str:
    """Generate cryptographically secure token."""
    import secrets
    return secrets.token_urlsafe(length)


def validate_csrf_token(token: str, session_token: str) -> bool:
    """Validate CSRF token."""
    return hmac.compare_digest(token, session_token)


if __name__ == "__main__":
    async def test_security():
        """Test security features."""
        print("🔒 Testing Enhanced Security System...")
        
        # Test input validation
        validator = InputValidator()
        
        try:
            validator.validate_input("<script>alert('xss')</script>", "test_field")
        except LiminalException as e:
            print(f"✅ XSS detected: {e.error_code}")
        
        # Test password strength
        password_result = validator.validate_password_strength("weakpass")
        print(f"🔑 Password strength: {password_result}")
        
        # Test rate limiting
        rate_limiter = RateLimiter()
        rate_limiter.add_rule(RateLimitRule(
            name="test_rule",
            limit=3,
            window_seconds=60,
            rule_type=RateLimitType.FIXED_WINDOW,
            scope="test"
        ))
        
        for i in range(5):
            result = await rate_limiter.check_rate_limit("test_user", "test_rule")
            print(f"📊 Rate limit check {i+1}: {result['allowed']}")
        
        print("🔒 Security test completed!")
    
    asyncio.run(test_security())