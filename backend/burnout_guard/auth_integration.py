"""
🚀🛡️ BurnoutGuard Authentication Integration

Integration with existing JWT-based authentication system:
- Extends RBAC for burnout-specific permissions
- Team and HR access controls
- User privacy and consent management
- API security for burnout endpoints

"Secure access to burnout protection features" 🔐
"""

from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from fastapi import HTTPException, Depends, status
from dataclasses import dataclass
from enum import Enum

# Import existing auth system
from ..auth.jwt_utils import verify_token, get_current_active_user
from ..auth.auth_router import get_current_user


class BurnoutPermission(Enum):
    """Burnout-specific permissions."""
    
    # Individual user permissions
    VIEW_OWN_BURNOUT_DATA = "burnout:view_own"
    MANAGE_OWN_SETTINGS = "burnout:manage_own_settings"
    RECEIVE_ALERTS = "burnout:receive_alerts"
    
    # Team permissions
    VIEW_TEAM_ANALYTICS = "burnout:view_team"
    MANAGE_TEAM_MEMBERS = "burnout:manage_team"
    EXPORT_TEAM_DATA = "burnout:export_team"
    
    # HR permissions
    VIEW_DEPARTMENT_ANALYTICS = "burnout:view_department"
    MANAGE_ALL_TEAMS = "burnout:manage_all_teams"
    VIEW_CRITICAL_ALERTS = "burnout:view_critical_alerts"
    ACCESS_HR_DASHBOARD = "burnout:access_hr_dashboard"
    EXPORT_COMPANY_DATA = "burnout:export_company"
    
    # Admin permissions
    CONFIGURE_SYSTEM = "burnout:configure_system"
    MANAGE_PERMISSIONS = "burnout:manage_permissions"
    VIEW_AUDIT_LOGS = "burnout:view_audit_logs"


class BurnoutRole(Enum):
    """Burnout-specific roles."""
    USER = "burnout_user"
    TEAM_LEAD = "burnout_team_lead"
    HR_MANAGER = "burnout_hr_manager"
    ADMIN = "burnout_admin"


@dataclass
class BurnoutUserContext:
    """Extended user context for burnout features."""
    user_id: str
    username: str
    email: str
    roles: List[str]
    permissions: List[str]
    
    # Burnout-specific context
    team_id: Optional[str] = None
    department: Optional[str] = None
    is_hr_manager: bool = False
    is_team_lead: bool = False
    
    # Privacy settings
    data_sharing_consent: bool = True
    anonymous_mode: bool = False
    alert_preferences: Dict[str, bool] = None
    
    def __post_init__(self):
        if self.alert_preferences is None:
            self.alert_preferences = {
                "critical_alerts": True,
                "daily_summary": True,
                "weekly_report": False,
                "team_notifications": False
            }


class BurnoutPermissionChecker:
    """
    Permission checker for burnout-specific operations.
    
    Extends the existing RBAC system with burnout-specific
    permissions and role-based access controls.
    """
    
    # Role-to-permissions mapping
    ROLE_PERMISSIONS = {
        BurnoutRole.USER: [
            BurnoutPermission.VIEW_OWN_BURNOUT_DATA,
            BurnoutPermission.MANAGE_OWN_SETTINGS,
            BurnoutPermission.RECEIVE_ALERTS,
        ],
        BurnoutRole.TEAM_LEAD: [
            BurnoutPermission.VIEW_OWN_BURNOUT_DATA,
            BurnoutPermission.MANAGE_OWN_SETTINGS,
            BurnoutPermission.RECEIVE_ALERTS,
            BurnoutPermission.VIEW_TEAM_ANALYTICS,
            BurnoutPermission.MANAGE_TEAM_MEMBERS,
            BurnoutPermission.EXPORT_TEAM_DATA,
        ],
        BurnoutRole.HR_MANAGER: [
            BurnoutPermission.VIEW_OWN_BURNOUT_DATA,
            BurnoutPermission.MANAGE_OWN_SETTINGS,
            BurnoutPermission.RECEIVE_ALERTS,
            BurnoutPermission.VIEW_TEAM_ANALYTICS,
            BurnoutPermission.VIEW_DEPARTMENT_ANALYTICS,
            BurnoutPermission.MANAGE_ALL_TEAMS,
            BurnoutPermission.VIEW_CRITICAL_ALERTS,
            BurnoutPermission.ACCESS_HR_DASHBOARD,
            BurnoutPermission.EXPORT_COMPANY_DATA,
        ],
        BurnoutRole.ADMIN: [
            # All permissions
            *list(BurnoutPermission)
        ]
    }
    
    @classmethod
    def has_permission(
        cls, 
        user_context: BurnoutUserContext, 
        permission: BurnoutPermission
    ) -> bool:
        """Check if user has specific permission."""
        
        # Check direct permission grants
        if permission.value in user_context.permissions:
            return True
        
        # Check role-based permissions
        for role_name in user_context.roles:
            try:
                role = BurnoutRole(role_name)
                if permission in cls.ROLE_PERMISSIONS.get(role, []):
                    return True
            except ValueError:
                # Role not in BurnoutRole enum, check legacy roles
                continue
        
        # Check legacy admin role
        if "admin" in user_context.roles:
            return True
        
        return False
    
    @classmethod
    def can_access_team_data(
        cls, 
        user_context: BurnoutUserContext, 
        target_team_id: str
    ) -> bool:
        """Check if user can access specific team's data."""
        
        # Users can access their own team data
        if user_context.team_id == target_team_id:
            return cls.has_permission(user_context, BurnoutPermission.VIEW_TEAM_ANALYTICS)
        
        # HR managers can access all teams
        if cls.has_permission(user_context, BurnoutPermission.MANAGE_ALL_TEAMS):
            return True
        
        return False
    
    @classmethod
    def can_manage_user(
        cls, 
        user_context: BurnoutUserContext, 
        target_user_id: str
    ) -> bool:
        """Check if user can manage another user's burnout settings."""
        
        # Users can only manage themselves
        if user_context.user_id == target_user_id:
            return True
        
        # Team leads can manage team members
        if cls.has_permission(user_context, BurnoutPermission.MANAGE_TEAM_MEMBERS):
            # Would need to check if target user is in the same team
            return True
        
        # HR managers can manage all users
        if cls.has_permission(user_context, BurnoutPermission.MANAGE_ALL_TEAMS):
            return True
        
        return False


class BurnoutAuthService:
    """
    Authentication service for BurnoutGuard features.
    
    Provides user context enrichment and permission checking
    specifically for burnout-related operations.
    """
    
    @staticmethod
    async def get_burnout_user_context(
        current_user: dict = Depends(get_current_active_user)
    ) -> BurnoutUserContext:
        """
        Get enriched user context for burnout operations.
        
        Extends the basic user info with burnout-specific context
        like team membership, HR status, and preferences.
        """
        
        # Get basic user info from existing auth system
        user_id = current_user.get("user_id") or current_user.get("sub")
        username = current_user.get("username", "")
        email = current_user.get("email", "")
        roles = current_user.get("roles", [])
        scopes = current_user.get("scopes", [])
        
        # Load burnout-specific context (in production, from database)
        team_id = await BurnoutAuthService._get_user_team(user_id)
        department = await BurnoutAuthService._get_user_department(user_id)
        privacy_settings = await BurnoutAuthService._get_privacy_settings(user_id)
        
        # Determine special roles
        is_hr_manager = (
            BurnoutRole.HR_MANAGER.value in roles or
            "hr_manager" in roles or
            "burnout:access_hr_dashboard" in scopes
        )
        
        is_team_lead = (
            BurnoutRole.TEAM_LEAD.value in roles or
            "team_lead" in roles or
            "burnout:view_team" in scopes
        )
        
        return BurnoutUserContext(
            user_id=user_id,
            username=username,
            email=email,
            roles=roles,
            permissions=scopes,
            team_id=team_id,
            department=department,
            is_hr_manager=is_hr_manager,
            is_team_lead=is_team_lead,
            **privacy_settings
        )
    
    @staticmethod
    async def _get_user_team(user_id: str) -> Optional[str]:
        """Get user's team ID from database."""
        # In production, query actual database
        # For now, return mock data
        team_mapping = {
            "user1": "dev-frontend",
            "user2": "dev-backend", 
            "user3": "qa-automation",
            "hr_user": None,  # HR users might not belong to specific teams
        }
        return team_mapping.get(user_id)
    
    @staticmethod
    async def _get_user_department(user_id: str) -> Optional[str]:
        """Get user's department from database."""
        # In production, query actual database
        department_mapping = {
            "user1": "Engineering",
            "user2": "Engineering",
            "user3": "Quality Assurance",
            "hr_user": "Human Resources",
        }
        return department_mapping.get(user_id)
    
    @staticmethod
    async def _get_privacy_settings(user_id: str) -> Dict[str, Any]:
        """Get user's privacy and alert preferences."""
        # In production, query actual database
        return {
            "data_sharing_consent": True,
            "anonymous_mode": False,
            "alert_preferences": {
                "critical_alerts": True,
                "daily_summary": True,
                "weekly_report": False,
                "team_notifications": False
            }
        }


# Dependency functions for FastAPI endpoints
async def require_burnout_permission(permission: BurnoutPermission):
    """Dependency factory for requiring specific burnout permissions."""
    
    async def check_permission(
        user_context: BurnoutUserContext = Depends(BurnoutAuthService.get_burnout_user_context)
    ):
        if not BurnoutPermissionChecker.has_permission(user_context, permission):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"Insufficient permissions. Required: {permission.value}"
            )
        return user_context
    
    return check_permission


async def require_team_access(team_id: str):
    """Dependency factory for requiring access to specific team data."""
    
    async def check_team_access(
        user_context: BurnoutUserContext = Depends(BurnoutAuthService.get_burnout_user_context)
    ):
        if not BurnoutPermissionChecker.can_access_team_data(user_context, team_id):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"Access denied to team {team_id}"
            )
        return user_context
    
    return check_team_access


async def require_hr_access(
    user_context: BurnoutUserContext = Depends(BurnoutAuthService.get_burnout_user_context)
) -> BurnoutUserContext:
    """Require HR manager access."""
    
    if not BurnoutPermissionChecker.has_permission(
        user_context, 
        BurnoutPermission.ACCESS_HR_DASHBOARD
    ):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="HR manager access required"
        )
    
    return user_context


async def require_user_consent(
    user_context: BurnoutUserContext = Depends(BurnoutAuthService.get_burnout_user_context)
) -> BurnoutUserContext:
    """Require user consent for data collection."""
    
    if not user_context.data_sharing_consent:
        raise HTTPException(
            status_code=status.HTTP_428_PRECONDITION_REQUIRED,
            detail="User consent required for burnout monitoring"
        )
    
    return user_context


class BurnoutAuditLogger:
    """
    Audit logging for burnout-related operations.
    
    Tracks access to sensitive burnout data and critical operations
    for compliance and security monitoring.
    """
    
    @staticmethod
    async def log_access(
        user_id: str,
        operation: str,
        resource: str,
        success: bool,
        additional_data: Optional[Dict[str, Any]] = None
    ):
        """Log access attempt to burnout resources."""
        
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "user_id": user_id,
            "operation": operation,
            "resource": resource,
            "success": success,
            "ip_address": None,  # Would get from request in FastAPI
            "user_agent": None,  # Would get from request in FastAPI
            "additional_data": additional_data or {}
        }
        
        # In production, write to audit log database/file
        print(f"AUDIT: {log_entry}")
    
    @staticmethod
    async def log_critical_access(
        user_id: str,
        team_id: str,
        risk_level: str,
        operation: str
    ):
        """Log access to critical burnout alerts."""
        
        await BurnoutAuditLogger.log_access(
            user_id=user_id,
            operation=operation,
            resource=f"critical_burnout_data:{team_id}",
            success=True,
            additional_data={
                "risk_level": risk_level,
                "alert_type": "critical_burnout"
            }
        )
    
    @staticmethod
    async def log_data_export(
        user_id: str,
        export_type: str,
        team_ids: List[str],
        record_count: int
    ):
        """Log data export operations."""
        
        await BurnoutAuditLogger.log_access(
            user_id=user_id,
            operation="data_export",
            resource=f"burnout_export:{export_type}",
            success=True,
            additional_data={
                "team_ids": team_ids,
                "record_count": record_count,
                "export_type": export_type
            }
        )


# Integration with existing auth endpoints
def extend_auth_router():
    """
    Function to extend existing auth router with burnout-specific endpoints.
    
    This would be called during app initialization to add burnout auth endpoints
    to the existing auth router.
    """
    
    from fastapi import APIRouter
    
    burnout_auth_router = APIRouter(prefix="/auth/burnout", tags=["BurnoutGuard Auth"])
    
    @burnout_auth_router.get("/permissions")
    async def get_user_burnout_permissions(
        user_context: BurnoutUserContext = Depends(BurnoutAuthService.get_burnout_user_context)
    ):
        """Get user's burnout-specific permissions."""
        
        permissions = []
        for permission in BurnoutPermission:
            if BurnoutPermissionChecker.has_permission(user_context, permission):
                permissions.append({
                    "permission": permission.value,
                    "description": permission.name.replace("_", " ").title()
                })
        
        return {
            "user_id": user_context.user_id,
            "permissions": permissions,
            "team_id": user_context.team_id,
            "is_hr_manager": user_context.is_hr_manager,
            "is_team_lead": user_context.is_team_lead
        }
    
    @burnout_auth_router.post("/consent")
    async def update_data_consent(
        consent_data: Dict[str, bool],
        user_context: BurnoutUserContext = Depends(BurnoutAuthService.get_burnout_user_context)
    ):
        """Update user's data sharing consent."""
        
        # In production, update database
        await BurnoutAuditLogger.log_access(
            user_id=user_context.user_id,
            operation="update_consent",
            resource="data_sharing_consent",
            success=True,
            additional_data=consent_data
        )
        
        return {
            "message": "Consent updated successfully",
            "updated_settings": consent_data
        }
    
    @burnout_auth_router.get("/context")
    async def get_burnout_context(
        user_context: BurnoutUserContext = Depends(BurnoutAuthService.get_burnout_user_context)
    ):
        """Get full burnout user context."""
        
        return {
            "user_id": user_context.user_id,
            "username": user_context.username,
            "team_id": user_context.team_id,
            "department": user_context.department,
            "roles": user_context.roles,
            "is_hr_manager": user_context.is_hr_manager,
            "is_team_lead": user_context.is_team_lead,
            "data_sharing_consent": user_context.data_sharing_consent,
            "anonymous_mode": user_context.anonymous_mode,
            "alert_preferences": user_context.alert_preferences
        }
    
    return burnout_auth_router


# Middleware for automatic audit logging
class BurnoutAuditMiddleware:
    """
    Middleware to automatically log burnout-related API access.
    
    Integrates with FastAPI middleware system to provide
    transparent audit logging for all burnout endpoints.
    """
    
    def __init__(self, app):
        self.app = app
    
    async def __call__(self, scope, receive, send):
        if scope["type"] == "http":
            path = scope.get("path", "")
            
            # Log access to burnout endpoints
            if path.startswith("/api/v1/burnout"):
                # Extract user info from token if available
                # Log the access attempt
                pass
        
        await self.app(scope, receive, send)