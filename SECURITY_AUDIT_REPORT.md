# 🔒 Security Audit Report - LIMINAL Backend
**Date:** November 8, 2025
**Auditor:** Claude (Technical Security Analysis)
**Scope:** Backend FastAPI Application
**Status:** ⚠️ CRITICAL ISSUES FOUND

---

## 🚨 Critical Security Issues (Must Fix Immediately)

### 1. **CORS Wildcard Configuration** ⚠️ CRITICAL
**File:** `backend/app/main.py:57`
**Issue:**
```python
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # ⚠️ ALLOWS ANY WEBSITE TO ACCESS API
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

**Risk:**
- Any website can make requests to your API
- CSRF attacks possible
- Data leakage to malicious sites
- Credential theft through XSS

**Severity:** **CRITICAL** 🔴
**Impact:** Complete bypass of same-origin policy

**Fix:**
```python
# Production configuration
ALLOWED_ORIGINS = [
    "https://liminal.app",
    "https://www.liminal.app",
    "https://app.liminal.io",
]

# Development only
if settings.environment == "development":
    ALLOWED_ORIGINS.extend([
        "http://localhost:3000",
        "http://localhost:8000",
    ])

app.add_middleware(
    CORSMiddleware,
    allow_origins=ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["GET", "POST", "PUT", "DELETE", "OPTIONS"],
    allow_headers=["Content-Type", "Authorization"],
)
```

---

### 2. **Hardcoded Secrets in Code** ⚠️ CRITICAL
**File:** `backend/config.py:36, 29`
**Issues:**

```python
jwt_secret_key: str = "<set via JWT_SECRET_KEY>"  # ⚠️
neo4j_password: str = "<set via NEO4J_PASSWORD>"  # ⚠️
```

**Risk:**
- JWT tokens can be forged by anyone with access to code
- Database compromise if repository is leaked
- Secrets visible in git history

**Severity:** **CRITICAL** 🔴
**Impact:** Complete authentication bypass, database access

**Fix:**
```python
class Settings(BaseModel):
    jwt_secret_key: str = Field(
        default=...,  # Required, no default
        description="JWT secret key - MUST be set via environment variable"
    )
    neo4j_password: str = Field(
        default=...,  # Required, no default
        description="Neo4j password - MUST be set via environment variable"
    )

    def __init__(self, **kwargs):
        # Require secrets from environment
        if not os.getenv('JWT_SECRET_KEY'):
            raise ValueError("JWT_SECRET_KEY environment variable is required")
        if not os.getenv('NEO4J_PASSWORD'):
            raise ValueError("NEO4J_PASSWORD environment variable is required")

        env_values = {
            'jwt_secret_key': os.getenv('JWT_SECRET_KEY'),
            'neo4j_password': os.getenv('NEO4J_PASSWORD'),
            # ... other settings
        }
        kwargs.update(env_values)
        super().__init__(**kwargs)
```

---

### 3. **No Rate Limiting** ⚠️ HIGH
**Files:** All API endpoints
**Issue:** No protection against:
- Brute force attacks on `/auth/login`
- DDoS attacks
- API abuse
- Credential stuffing

**Risk:**
- Attackers can try unlimited passwords
- Service can be overwhelmed
- High infrastructure costs

**Severity:** **HIGH** 🟠
**Impact:** Service disruption, security breaches

**Fix:** Implement slowapi middleware:
```python
from slowapi import Limiter, _rate_limit_exceeded_handler
from slowapi.util import get_remote_address
from slowapi.errors import RateLimitExceeded

limiter = Limiter(key_func=get_remote_address)
app.state.limiter = limiter
app.add_exception_handler(RateLimitExceeded, _rate_limit_exceeded_handler)

@app.post("/auth/login")
@limiter.limit("5/minute")  # Max 5 login attempts per minute
async def login(...):
    ...
```

---

### 4. **Debug Mode Enabled by Default** ⚠️ MEDIUM
**File:** `backend/config.py:20`
**Issue:**
```python
debug: bool = True  # ⚠️ Should be False in production
```

**Risk:**
- Exposes stack traces to users
- Reveals internal code structure
- Information leakage in error messages

**Severity:** **MEDIUM** 🟡
**Impact:** Information disclosure

**Fix:**
```python
debug: bool = Field(
    default=False,
    description="Debug mode - should only be True in development"
)

def __init__(self, **kwargs):
    env_values = {
        'debug': os.getenv('ENV') == 'development',
        ...
    }
```

---

### 5. **Missing Input Validation** ⚠️ MEDIUM
**Files:** Multiple route handlers
**Issue:**
- WebSocket messages not validated for size
- No sanitization of user input
- Potential for injection attacks

**Risk:**
- Memory exhaustion attacks
- XSS through stored messages
- Malformed data causing crashes

**Severity:** **MEDIUM** 🟡
**Impact:** Service disruption, data corruption

**Fix:**
```python
from pydantic import BaseModel, Field, validator

class WebSocketMessage(BaseModel):
    type: str = Field(..., max_length=50)
    content: str = Field(..., max_length=10000)  # Limit message size
    channel: Optional[str] = Field(None, max_length=100)

    @validator('type')
    def validate_type(cls, v):
        allowed_types = {'subscribe', 'unsubscribe', 'broadcast', 'auth'}
        if v not in allowed_types:
            raise ValueError(f'Invalid message type: {v}')
        return v

    @validator('content')
    def sanitize_content(cls, v):
        # Remove potentially dangerous characters
        import bleach
        return bleach.clean(v)
```

---

### 6. **Missing Security Headers** ⚠️ MEDIUM
**File:** `backend/app/main.py`
**Issue:** No security headers middleware

**Risk:**
- Clickjacking attacks
- XSS vulnerabilities
- MIME-type sniffing

**Severity:** **MEDIUM** 🟡
**Impact:** Client-side attacks

**Fix:**
```python
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from starlette.middleware.httpsredirect import HTTPSRedirectMiddleware

# Security headers middleware
@app.middleware("http")
async def add_security_headers(request: Request, call_next):
    response = await call_next(request)
    response.headers["X-Content-Type-Options"] = "nosniff"
    response.headers["X-Frame-Options"] = "DENY"
    response.headers["X-XSS-Protection"] = "1; mode=block"
    response.headers["Strict-Transport-Security"] = "max-age=31536000; includeSubDomains"
    response.headers["Content-Security-Policy"] = "default-src 'self'"
    return response

# HTTPS redirect in production
if settings.environment == "production":
    app.add_middleware(HTTPSRedirectMiddleware)
    app.add_middleware(TrustedHostMiddleware, allowed_hosts=["liminal.app", "*.liminal.app"])
```

---

### 7. **WebSocket Missing Imports** ⚠️ HIGH
**File:** `backend/app/routes/ws.py`
**Issue:** Code references undefined variables:
- `json` module not imported
- `datetime` not imported
- `WebSocketDisconnect` not imported
- `get_websocket_service` not imported but used
- `manager` variable undefined
- `timeline` variable undefined
- `ml_service` variable undefined

**Risk:**
- Runtime crashes
- WebSocket connections failing
- Service unavailable

**Severity:** **HIGH** 🟠
**Impact:** Feature completely broken

**Fix:**
```python
import json
from datetime import datetime
from fastapi import WebSocketDisconnect

# Fix dependency injection
@router.websocket("/ws/timeline")
async def websocket_timeline(
    websocket: WebSocket,
    token: Optional[str] = None,
    manager=Depends(get_connection_manager),
    timeline=Depends(get_memory_timeline),
    ml_service=Depends(get_ml_service),
):
    # ... rest of code
```

---

## 📊 Security Score: 4/10 ⚠️

### Breakdown:
- **Authentication:** 6/10 (Has JWT but hardcoded secrets)
- **Authorization:** 5/10 (Basic token verification)
- **Data Protection:** 3/10 (No encryption at rest mentioned)
- **Network Security:** 2/10 (CORS wildcard, no HTTPS enforcement)
- **Input Validation:** 4/10 (Minimal validation)
- **Monitoring:** 6/10 (Has Prometheus metrics)
- **Rate Limiting:** 0/10 (Not implemented)
- **Secrets Management:** 2/10 (Hardcoded secrets)

---

## ✅ Remediation Priority

### 🔴 **Priority 1 (This Week):**
1. ✅ Fix CORS configuration
2. ✅ Move secrets to environment variables
3. ✅ Fix WebSocket imports (code broken)
4. ✅ Add rate limiting to auth endpoints

### 🟠 **Priority 2 (Next 2 Weeks):**
5. ✅ Add security headers middleware
6. ✅ Implement input validation schemas
7. ✅ Disable debug mode in production
8. ✅ Add HTTPS redirect for production

### 🟡 **Priority 3 (Month 1):**
9. ✅ Implement password hashing audit
10. ✅ Add security logging and monitoring
11. ✅ Implement API request signing
12. ✅ Add encryption for sensitive data

---

## 📋 Security Checklist

### Authentication & Authorization
- [x] JWT authentication implemented
- [ ] Secrets stored in environment variables
- [ ] Password hashing (bcrypt/argon2)
- [ ] Multi-factor authentication (future)
- [ ] Session management
- [ ] Token refresh mechanism
- [ ] Rate limiting on auth endpoints

### Network Security
- [ ] CORS properly configured
- [ ] HTTPS enforced in production
- [ ] Security headers implemented
- [ ] TLS 1.3 minimum
- [ ] Certificate pinning (future)

### Data Protection
- [ ] Input validation on all endpoints
- [ ] Output encoding
- [ ] SQL injection prevention (using ORM)
- [ ] XSS prevention
- [ ] CSRF protection
- [ ] Encryption at rest (sensitive data)

### Monitoring & Logging
- [x] Prometheus metrics
- [ ] Security event logging
- [ ] Failed authentication tracking
- [ ] Anomaly detection
- [ ] Alerting for suspicious activity

### Infrastructure
- [ ] Secrets management (env vars)
- [ ] Debug mode disabled in production
- [ ] Error handling without info leakage
- [ ] Rate limiting
- [ ] DDoS protection

---

## 🎯 Target Security Score: 9/10

**Timeline:** 90 days
**Current:** 4/10
**Progress needed:** +5 points

**Week 1-2:** Priority 1 fixes → **Score: 6/10**
**Week 3-4:** Priority 2 fixes → **Score: 7.5/10**
**Month 2-3:** Priority 3 fixes → **Score: 9/10**

---

## 📞 Recommended Actions

### Immediate (Today):
1. Create `.env.example` template
2. Update deployment docs with secret requirements
3. Add environment validation on startup

### This Week:
1. Implement all Priority 1 fixes
2. Test with security scanner (OWASP ZAP)
3. Review and update this document

### This Month:
1. Complete all Priority 1 & 2 fixes
2. Conduct penetration testing
3. Create security incident response plan

---

**Next Steps:** Start with fixing CORS and secrets management (Priority 1).

---

*Report Generated: 2025-11-08*
*Next Review: After Priority 1 fixes completed*
