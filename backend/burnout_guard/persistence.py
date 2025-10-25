"""
🚀🛡️ BurnoutGuard Data Persistence Layer

Integration with existing dual database architecture:
- Neo4j: User relationships, team structures, burnout patterns
- Datomic: Temporal burnout data, risk history, audit trails
- Unified interface for burnout data operations
- Data retention and privacy compliance

"Persistent protection through intelligent data management" 💾
"""

import asyncio
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, asdict
from enum import Enum

# Import existing database components
try:
    from ..database_adapter import DatabaseAdapter, DataType
    from ..datomic_client import DatomicClient
    from ..consciousness_neo4j import ConsciousnessNeo4jWriter
    DB_AVAILABLE = True
except ImportError:
    DB_AVAILABLE = False

from .core import BurnoutState, BurnoutRisk
from .modes import BurnoutMode, BurnoutModeType, BurnoutRiskLevel
from .analytics import TeamAnalytics, TeamMember
from .utils import safe_logger


class BurnoutDataType(Enum):
    """Data types for burnout persistence routing."""
    
    # Temporal data (→ Datomic)
    RISK_HISTORY = "burnout_risk_history"
    STATE_TIMELINE = "burnout_state_timeline"
    RECOMMENDATION_USAGE = "burnout_recommendation_usage"
    ALERT_HISTORY = "burnout_alert_history"
    AUDIT_LOG = "burnout_audit_log"
    
    # Structural data (→ Neo4j)
    USER_PROFILE = "burnout_user_profile"
    TEAM_STRUCTURE = "burnout_team_structure"
    RISK_PATTERNS = "burnout_risk_patterns"
    RECOMMENDATION_NETWORK = "burnout_recommendation_network"


@dataclass
class BurnoutDataPoint:
    """Single burnout data point for persistence."""
    user_id: str
    timestamp: datetime
    risk_score: float
    risk_level: str
    burnout_mode: str
    confidence: float
    
    # Additional metadata
    emotional_indicators: List[str]
    session_duration: Optional[float] = None
    work_context: Optional[Dict[str, Any]] = None
    intervention_taken: bool = False


@dataclass
class BurnoutUserProfile:
    """User profile for burnout tracking."""
    user_id: str
    team_id: Optional[str]
    department: str
    role: str
    
    # Tracking preferences
    monitoring_enabled: bool = True
    data_retention_days: int = 90
    anonymize_data: bool = False
    
    # Personal factors
    baseline_stress_level: float = 0.3
    preferred_break_duration: int = 15  # minutes
    work_schedule: Dict[str, Any] = None
    
    # Privacy settings
    share_with_team: bool = False
    share_with_hr: bool = True
    
    def __post_init__(self):
        if self.work_schedule is None:
            self.work_schedule = {
                "start_hour": 9,
                "end_hour": 17,
                "break_hours": [12, 15],
                "timezone": "UTC"
            }


class BurnoutDatabaseAdapter:
    """
    Adapter for burnout data persistence using existing dual database architecture.
    
    Routes different types of burnout data to appropriate storage systems:
    - Temporal data → Datomic (risk history, timelines, audit logs)
    - Structural data → Neo4j (user relationships, team patterns, networks)
    """
    
    def __init__(self):
        self.db_adapter = None
        self.neo4j_writer = None
        self.datomic_client = None
        
        if DB_AVAILABLE:
            self.db_adapter = DatabaseAdapter()
            # These would be initialized with the existing database connections
        
    async def initialize(self):
        """Initialize database connections."""
        if self.db_adapter:
            try:
                # Use existing database initialization
                await self.db_adapter.initialize()
                safe_logger.info("BurnoutGuard database adapter initialized")
            except Exception as e:
                safe_logger.error(f"Failed to initialize database adapter: {e}")
                # Fall back to mock mode
                self.db_adapter = None
    
    # ========== Risk Data Persistence ==========
    
    async def store_risk_assessment(
        self, 
        user_id: str, 
        risk_assessment: BurnoutRisk,
        context: Optional[Dict[str, Any]] = None
    ) -> bool:
        """Store risk assessment data (temporal → Datomic)."""
        
        try:
            data_point = {
                "user_id": user_id,
                "timestamp": risk_assessment.timestamp.isoformat(),
                "risk_score": risk_assessment.score,
                "risk_level": risk_assessment.level.value,
                "confidence": risk_assessment.confidence,
                "factors": risk_assessment.factors,
                "emotional_indicators": risk_assessment.emotional_indicators,
                "behavioral_patterns": risk_assessment.behavioral_patterns,
                "duration_risk": risk_assessment.duration_risk,
                "trend_risk": risk_assessment.trend_risk,
                "context": context or {}
            }
            
            if self.db_adapter:
                # Use existing Datomic storage
                return await self.db_adapter.store_data(
                    data_point, 
                    DataType.EMOTION_HISTORY  # Map to existing emotion data type
                )
            else:
                # Mock storage
                safe_logger.info(f"Mock: Stored risk assessment for user {user_id}")
                return True
                
        except Exception as e:
            safe_logger.error(f"Failed to store risk assessment: {e}")
            return False
    
    async def get_risk_history(
        self, 
        user_id: str, 
        days: int = 30
    ) -> List[Dict[str, Any]]:
        """Retrieve risk history for user (from Datomic)."""
        
        try:
            if self.db_adapter:
                # Query Datomic for historical risk data
                # This would use the existing temporal query capabilities
                cutoff_date = datetime.now() - timedelta(days=days)
                
                # Mock query structure - in production, use actual Datomic queries
                query_params = {
                    "user_id": user_id,
                    "start_date": cutoff_date.isoformat(),
                    "data_type": "emotion_history"  # Map to existing type
                }
                
                # Would use existing query methods
                results = []  # await self.db_adapter.query_temporal_data(query_params)
                return results
            else:
                # Mock data
                return self._generate_mock_risk_history(user_id, days)
                
        except Exception as e:
            safe_logger.error(f"Failed to retrieve risk history: {e}")
            return []
    
    # ========== Burnout State Persistence ==========
    
    async def store_burnout_state(
        self, 
        user_id: str, 
        burnout_state: BurnoutState
    ) -> bool:
        """Store complete burnout state (temporal → Datomic)."""
        
        try:
            state_data = {
                "user_id": user_id,
                "timestamp": burnout_state.timestamp.isoformat(),
                "burnout_mode": {
                    "type": burnout_state.burnout_mode.type.value,
                    "risk_level": burnout_state.burnout_mode.risk_level.value,
                    "risk_score": burnout_state.burnout_mode.risk_score,
                    "indicators": burnout_state.burnout_mode.primary_indicators,
                    "confidence": burnout_state.burnout_mode.confidence
                },
                "risk_assessment": asdict(burnout_state.risk_assessment),
                "risk_history": burnout_state.risk_history,
                "mode_stability": burnout_state.mode_stability,
                "intervention_needed": burnout_state.intervention_needed
            }
            
            if self.db_adapter:
                return await self.db_adapter.store_data(
                    state_data,
                    DataType.TEMPORAL  # Use existing temporal data type
                )
            else:
                safe_logger.info(f"Mock: Stored burnout state for user {user_id}")
                return True
                
        except Exception as e:
            safe_logger.error(f"Failed to store burnout state: {e}")
            return False
    
    async def get_latest_burnout_state(self, user_id: str) -> Optional[Dict[str, Any]]:
        """Get latest burnout state for user."""
        
        try:
            if self.db_adapter:
                # Query for latest state
                # Would use existing temporal query methods
                return None  # await self.db_adapter.get_latest_data(user_id, "burnout_state")
            else:
                # Mock data
                return {
                    "user_id": user_id,
                    "timestamp": datetime.now().isoformat(),
                    "risk_score": 0.4,
                    "risk_level": "medium",
                    "burnout_mode": "healthy"
                }
                
        except Exception as e:
            safe_logger.error(f"Failed to get latest burnout state: {e}")
            return None
    
    # ========== User Profile Management ==========
    
    async def store_user_profile(self, profile: BurnoutUserProfile) -> bool:
        """Store user profile (structural → Neo4j)."""
        
        try:
            profile_data = asdict(profile)
            
            if self.db_adapter:
                return await self.db_adapter.store_data(
                    profile_data,
                    DataType.USER_NETWORK  # Map to existing user network type
                )
            else:
                safe_logger.info(f"Mock: Stored user profile for {profile.user_id}")
                return True
                
        except Exception as e:
            safe_logger.error(f"Failed to store user profile: {e}")
            return False
    
    async def get_user_profile(self, user_id: str) -> Optional[BurnoutUserProfile]:
        """Get user profile."""
        
        try:
            if self.db_adapter:
                # Query Neo4j for user profile
                # Would use existing graph query methods
                data = None  # await self.db_adapter.get_user_data(user_id)
                if data:
                    return BurnoutUserProfile(**data)
            else:
                # Mock profile
                return BurnoutUserProfile(
                    user_id=user_id,
                    team_id="team1",
                    department="Engineering",
                    role="Developer"
                )
                
        except Exception as e:
            safe_logger.error(f"Failed to get user profile: {e}")
            return None
    
    # ========== Team Analytics Persistence ==========
    
    async def store_team_analytics(
        self, 
        team_id: str, 
        analytics: TeamAnalytics
    ) -> bool:
        """Store team analytics (structural → Neo4j)."""
        
        try:
            analytics_data = {
                "team_id": team_id,
                "timestamp": analytics.analysis_timestamp.isoformat(),
                "total_members": analytics.total_members,
                "active_members": analytics.active_members,
                "average_risk_score": analytics.average_risk_score,
                "alert_level": analytics.team_alert_level.value,
                "risk_distribution": {
                    level.value: count 
                    for level, count in analytics.risk_distribution.items()
                },
                "team_indicators": analytics.team_indicators,
                "workload_metrics": analytics.workload_metrics,
                "intervention_priority": analytics.intervention_priority
            }
            
            if self.db_adapter:
                return await self.db_adapter.store_data(
                    analytics_data,
                    DataType.RELATIONSHIP  # Map to existing relationship type
                )
            else:
                safe_logger.info(f"Mock: Stored team analytics for {team_id}")
                return True
                
        except Exception as e:
            safe_logger.error(f"Failed to store team analytics: {e}")
            return False
    
    async def get_team_analytics_history(
        self, 
        team_id: str, 
        days: int = 30
    ) -> List[Dict[str, Any]]:
        """Get team analytics history."""
        
        try:
            if self.db_adapter:
                # Query for team analytics history
                cutoff_date = datetime.now() - timedelta(days=days)
                # Would use existing query methods
                return []
            else:
                # Mock data
                return self._generate_mock_team_analytics(team_id, days)
                
        except Exception as e:
            safe_logger.error(f"Failed to get team analytics history: {e}")
            return []
    
    # ========== Recommendation Tracking ==========
    
    async def track_recommendation_usage(
        self, 
        user_id: str,
        recommendation_id: str,
        action: str,  # "viewed", "accepted", "dismissed", "completed"
        effectiveness_rating: Optional[float] = None
    ) -> bool:
        """Track recommendation usage (temporal → Datomic)."""
        
        try:
            usage_data = {
                "user_id": user_id,
                "recommendation_id": recommendation_id,
                "action": action,
                "timestamp": datetime.now().isoformat(),
                "effectiveness_rating": effectiveness_rating
            }
            
            if self.db_adapter:
                return await self.db_adapter.store_data(
                    usage_data,
                    DataType.EVENT  # Map to existing event type
                )
            else:
                safe_logger.info(f"Mock: Tracked recommendation usage for {user_id}")
                return True
                
        except Exception as e:
            safe_logger.error(f"Failed to track recommendation usage: {e}")
            return False
    
    async def get_recommendation_effectiveness(
        self, 
        recommendation_id: str
    ) -> Dict[str, Any]:
        """Get recommendation effectiveness metrics."""
        
        try:
            if self.db_adapter:
                # Query for recommendation usage statistics
                # Would use existing analytics queries
                return {}
            else:
                # Mock effectiveness data
                return {
                    "recommendation_id": recommendation_id,
                    "total_views": 42,
                    "acceptance_rate": 0.73,
                    "completion_rate": 0.68,
                    "average_effectiveness": 0.82,
                    "last_updated": datetime.now().isoformat()
                }
                
        except Exception as e:
            safe_logger.error(f"Failed to get recommendation effectiveness: {e}")
            return {}
    
    # ========== Audit and Compliance ==========
    
    async def store_audit_log(
        self, 
        user_id: str,
        operation: str,
        resource: str,
        details: Dict[str, Any]
    ) -> bool:
        """Store audit log entry (temporal → Datomic)."""
        
        try:
            audit_entry = {
                "user_id": user_id,
                "operation": operation,
                "resource": resource,
                "timestamp": datetime.now().isoformat(),
                "details": details
            }
            
            if self.db_adapter:
                return await self.db_adapter.store_data(
                    audit_entry,
                    DataType.AUDIT  # Use existing audit type
                )
            else:
                safe_logger.info(f"Mock: Stored audit log for {user_id}: {operation}")
                return True
                
        except Exception as e:
            safe_logger.error(f"Failed to store audit log: {e}")
            return False
    
    async def cleanup_expired_data(self, retention_days: int = 90) -> int:
        """Clean up expired burnout data for privacy compliance."""
        
        try:
            cutoff_date = datetime.now() - timedelta(days=retention_days)
            
            if self.db_adapter:
                # Would use existing data cleanup methods
                # This respects user privacy and data retention policies
                deleted_count = 0  # await self.db_adapter.cleanup_old_data(cutoff_date)
                safe_logger.info(f"Cleaned up {deleted_count} expired burnout records")
                return deleted_count
            else:
                safe_logger.info("Mock: Cleaned up expired data")
                return 100
                
        except Exception as e:
            safe_logger.error(f"Failed to cleanup expired data: {e}")
            return 0
    
    # ========== Analytics and Reporting ==========
    
    async def generate_user_report(
        self, 
        user_id: str, 
        start_date: datetime,
        end_date: datetime
    ) -> Dict[str, Any]:
        """Generate comprehensive user burnout report."""
        
        try:
            # Aggregate data from both databases
            risk_history = await self.get_risk_history(user_id, (end_date - start_date).days)
            user_profile = await self.get_user_profile(user_id)
            
            # Calculate statistics
            if risk_history:
                risk_scores = [r.get("risk_score", 0) for r in risk_history]
                avg_risk = sum(risk_scores) / len(risk_scores)
                max_risk = max(risk_scores)
                trend = "stable"  # Would calculate actual trend
            else:
                avg_risk = 0.0
                max_risk = 0.0
                trend = "unknown"
            
            return {
                "user_id": user_id,
                "report_period": {
                    "start": start_date.isoformat(),
                    "end": end_date.isoformat()
                },
                "summary": {
                    "average_risk": avg_risk,
                    "maximum_risk": max_risk,
                    "trend": trend,
                    "data_points": len(risk_history)
                },
                "profile": asdict(user_profile) if user_profile else None,
                "generated_at": datetime.now().isoformat()
            }
            
        except Exception as e:
            safe_logger.error(f"Failed to generate user report: {e}")
            return {}
    
    # ========== Mock Data Generators (for testing) ==========
    
    def _generate_mock_risk_history(self, user_id: str, days: int) -> List[Dict[str, Any]]:
        """Generate mock risk history for testing."""
        
        history = []
        base_date = datetime.now() - timedelta(days=days)
        
        for i in range(days * 4):  # 4 data points per day
            timestamp = base_date + timedelta(hours=i * 6)
            risk_score = 0.3 + (i % 7) * 0.1  # Varying risk
            
            history.append({
                "user_id": user_id,
                "timestamp": timestamp.isoformat(),
                "risk_score": min(1.0, risk_score),
                "risk_level": "medium" if risk_score > 0.5 else "low",
                "confidence": 0.8
            })
        
        return history
    
    def _generate_mock_team_analytics(self, team_id: str, days: int) -> List[Dict[str, Any]]:
        """Generate mock team analytics for testing."""
        
        analytics = []
        base_date = datetime.now() - timedelta(days=days)
        
        for i in range(days):
            timestamp = base_date + timedelta(days=i)
            
            analytics.append({
                "team_id": team_id,
                "timestamp": timestamp.isoformat(),
                "average_risk_score": 0.4 + (i % 5) * 0.1,
                "total_members": 8,
                "active_members": 7,
                "alert_level": "yellow" if i % 3 == 0 else "green"
            })
        
        return analytics


# Global instance
burnout_db = BurnoutDatabaseAdapter()