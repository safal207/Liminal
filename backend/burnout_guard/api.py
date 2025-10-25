"""
🚀🛡️ BurnoutGuard API — REST API для HR дашборда

API endpoints для командной аналитики и управления:
- Team analytics endpoints
- Department overview
- Real-time alerts
- Export и reporting
- Integration with existing auth system

"API для превращения данных выгорания в управленческие решения" 🔗
"""

from fastapi import APIRouter, HTTPException, Depends, Query, BackgroundTasks
from fastapi.responses import JSONResponse, StreamingResponse
from datetime import datetime, timedelta
from typing import List, Dict, Optional, Any
import json
import asyncio
from io import StringIO
import csv

from .analytics import (
    TeamBurnoutAnalyzer, DepartmentAnalyzer, TeamAnalytics, 
    DepartmentAnalytics, TeamMember, TeamAlertLevel
)
from .core import BurnoutGuardEngine, BurnoutState
from .modes import BurnoutRiskLevel
from .utils import safe_logger
from .auth_integration import (
    BurnoutUserContext, BurnoutPermission, BurnoutPermissionChecker,
    BurnoutAuthService, require_burnout_permission, require_hr_access,
    require_team_access, require_user_consent, BurnoutAuditLogger
)

# Create API router
router = APIRouter(prefix="/api/v1/burnout", tags=["BurnoutGuard"])

# Global analyzers (in production, these would be dependency injected)
team_analyzer = TeamBurnoutAnalyzer()
department_analyzer = DepartmentAnalyzer(team_analyzer)


# Dependency for authentication (integrated with existing system)
async def get_current_user():
    """Integration with existing auth system."""
    # In production, this uses the actual JWT auth dependency
    return {"user_id": "demo_user", "username": "demo", "roles": ["burnout_hr_manager"]}


async def check_hr_permissions():
    """Check if user has HR permissions using integrated auth."""
    # This now uses the real auth integration
    return await require_hr_access()


@router.get("/teams/{team_id}/analytics")
async def get_team_analytics(
    team_id: str,
    include_trends: bool = Query(True, description="Include trend analysis"),
    timeframe: str = Query("24h", description="Timeframe for analysis: 24h, 7d, 30d"),
    user_context: BurnoutUserContext = Depends(require_burnout_permission(BurnoutPermission.VIEW_TEAM_ANALYTICS))
) -> Dict[str, Any]:
    """Get comprehensive team burnout analytics."""
    
    try:
        analytics = await team_analyzer.analyze_team(
            team_id=team_id,
            team_name=f"Team {team_id}",
            include_trends=include_trends
        )
        
        if not analytics:
            raise HTTPException(status_code=404, detail=f"Team {team_id} not found or no data available")
        
        # Convert to dict for JSON response
        result = {
            "team_id": analytics.team_id,
            "team_name": analytics.team_name,
            "analysis_timestamp": analytics.analysis_timestamp.isoformat(),
            "summary": {
                "total_members": analytics.total_members,
                "active_members": analytics.active_members,
                "average_risk_score": round(analytics.average_risk_score, 3),
                "team_alert_level": analytics.team_alert_level.value,
                "intervention_priority": analytics.intervention_priority
            },
            "risk_distribution": {
                level.value: count for level, count in analytics.risk_distribution.items()
            },
            "indicators": analytics.team_indicators,
            "workload_metrics": analytics.workload_metrics,
            "hr_recommendations": analytics.hr_recommendations,
            "high_risk_members_count": len(analytics.high_risk_members)
        }
        
        # Add trends if requested
        if include_trends and analytics.trend_24h:
            result["trends"] = {
                "24h": {
                    "direction": analytics.trend_24h.direction.value,
                    "change_percentage": analytics.trend_24h.change_percentage,
                    "affected_members": analytics.trend_24h.affected_members
                }
            }
            
            if analytics.trend_7d:
                result["trends"]["7d"] = {
                    "direction": analytics.trend_7d.direction.value,
                    "change_percentage": analytics.trend_7d.change_percentage,
                    "affected_members": analytics.trend_7d.affected_members
                }
        
        safe_logger.info(f"Team analytics generated for {team_id} by user {user['user_id']}")
        return result
        
    except Exception as e:
        safe_logger.error(f"Error generating team analytics for {team_id}: {e}")
        raise HTTPException(status_code=500, detail="Internal server error")


@router.get("/departments/{department_name}/analytics")
async def get_department_analytics(
    department_name: str,
    team_ids: List[str] = Query(..., description="List of team IDs in the department"),
    user: dict = Depends(check_hr_permissions)
) -> Dict[str, Any]:
    """Get department-level burnout analytics."""
    
    try:
        analytics = await department_analyzer.analyze_department(
            department_name=department_name,
            team_ids=team_ids
        )
        
        if not analytics:
            raise HTTPException(status_code=404, detail=f"No data available for department {department_name}")
        
        result = {
            "department_name": analytics.department_name,
            "summary": {
                "total_employees": analytics.total_employees,
                "total_teams": len(analytics.teams),
                "average_department_risk": round(analytics.average_department_risk, 3),
                "critical_teams_count": len(analytics.critical_teams)
            },
            "teams": [
                {
                    "team_id": team.team_id,
                    "team_name": team.team_name,
                    "risk_score": round(team.average_risk_score, 3),
                    "alert_level": team.team_alert_level.value,
                    "members_count": team.active_members,
                    "intervention_priority": team.intervention_priority
                }
                for team in analytics.teams
            ],
            "critical_teams": analytics.critical_teams,
            "benchmark_comparison": analytics.benchmark_comparison,
            "best_practices": analytics.best_practices
        }
        
        safe_logger.info(f"Department analytics generated for {department_name} by user {user['user_id']}")
        return result
        
    except Exception as e:
        safe_logger.error(f"Error generating department analytics for {department_name}: {e}")
        raise HTTPException(status_code=500, detail="Internal server error")


@router.get("/alerts/active")
async def get_active_alerts(
    severity: Optional[str] = Query(None, description="Filter by severity: critical, high, medium"),
    team_ids: Optional[List[str]] = Query(None, description="Filter by team IDs"),
    user: dict = Depends(check_hr_permissions)
) -> Dict[str, Any]:
    """Get active burnout alerts across teams."""
    
    try:
        all_alerts = []
        
        # Get all teams (in production, this would come from database)
        teams_to_check = team_ids if team_ids else list(team_analyzer.team_members.keys())
        
        for team_id in teams_to_check:
            analytics = await team_analyzer.analyze_team(team_id)
            if analytics:
                # Create alert if intervention needed
                if analytics.intervention_priority >= 3:
                    alert = {
                        "alert_id": f"alert_{team_id}_{int(datetime.now().timestamp())}",
                        "team_id": analytics.team_id,
                        "team_name": analytics.team_name,
                        "severity": _map_priority_to_severity(analytics.intervention_priority),
                        "alert_level": analytics.team_alert_level.value,
                        "risk_score": round(analytics.average_risk_score, 3),
                        "affected_members": len(analytics.high_risk_members),
                        "primary_indicators": analytics.team_indicators[:3],
                        "timestamp": analytics.analysis_timestamp.isoformat(),
                        "urgent": analytics.team_alert_level == TeamAlertLevel.RED
                    }
                    
                    # Apply severity filter
                    if not severity or alert["severity"] == severity:
                        all_alerts.append(alert)
        
        # Sort by priority (most urgent first)
        all_alerts.sort(key=lambda x: (x["urgent"], x["risk_score"]), reverse=True)
        
        result = {
            "total_alerts": len(all_alerts),
            "critical_alerts": len([a for a in all_alerts if a["severity"] == "critical"]),
            "alerts": all_alerts,
            "last_updated": datetime.now().isoformat()
        }
        
        safe_logger.info(f"Active alerts retrieved by user {user['user_id']}: {len(all_alerts)} alerts")
        return result
        
    except Exception as e:
        safe_logger.error(f"Error retrieving active alerts: {e}")
        raise HTTPException(status_code=500, detail="Internal server error")


@router.post("/teams/{team_id}/members")
async def add_team_member(
    team_id: str,
    member_data: Dict[str, Any],
    user: dict = Depends(check_hr_permissions)
) -> Dict[str, Any]:
    """Add or update a team member."""
    
    try:
        # Validate required fields
        required_fields = ["user_id", "name", "role", "department", "current_risk_score", "risk_level"]
        for field in required_fields:
            if field not in member_data:
                raise HTTPException(status_code=400, detail=f"Missing required field: {field}")
        
        # Create TeamMember object
        member = TeamMember(
            user_id=member_data["user_id"],
            name=member_data["name"],
            role=member_data["role"],
            department=member_data["department"],
            current_risk_score=float(member_data["current_risk_score"]),
            risk_level=BurnoutRiskLevel(member_data["risk_level"]),
            last_update=datetime.now(),
            days_since_break=member_data.get("days_since_break", 0),
            anonymized=member_data.get("anonymized", False),
            share_details=member_data.get("share_details", True)
        )
        
        # Add to team
        success = await team_analyzer.add_team_member(team_id, member)
        
        if success:
            # Clear cache for this team
            if team_id in team_analyzer.analytics_cache:
                del team_analyzer.analytics_cache[team_id]
            
            result = {
                "success": True,
                "message": f"Member {member.user_id} added to team {team_id}",
                "member_id": member.user_id
            }
            
            safe_logger.info(f"Team member {member.user_id} added to {team_id} by user {user['user_id']}")
            return result
        else:
            raise HTTPException(status_code=500, detail="Failed to add team member")
            
    except ValueError as e:
        raise HTTPException(status_code=400, detail=f"Invalid data: {e}")
    except Exception as e:
        safe_logger.error(f"Error adding team member: {e}")
        raise HTTPException(status_code=500, detail="Internal server error")


@router.get("/export/team-report/{team_id}")
async def export_team_report(
    team_id: str,
    format: str = Query("csv", description="Export format: csv, json"),
    user: dict = Depends(check_hr_permissions)
) -> StreamingResponse:
    """Export team burnout report."""
    
    try:
        analytics = await team_analyzer.analyze_team(team_id)
        if not analytics:
            raise HTTPException(status_code=404, detail=f"No data available for team {team_id}")
        
        if format.lower() == "csv":
            return _export_csv_report(analytics, team_id)
        elif format.lower() == "json":
            return _export_json_report(analytics, team_id)
        else:
            raise HTTPException(status_code=400, detail="Unsupported format. Use 'csv' or 'json'")
            
    except Exception as e:
        safe_logger.error(f"Error exporting report for team {team_id}: {e}")
        raise HTTPException(status_code=500, detail="Internal server error")


@router.get("/dashboard/overview")
async def get_dashboard_overview(
    user: dict = Depends(check_hr_permissions)
) -> Dict[str, Any]:
    """Get high-level dashboard overview."""
    
    try:
        # Aggregate data across all teams
        all_teams = list(team_analyzer.team_members.keys())
        total_employees = 0
        total_high_risk = 0
        total_critical = 0
        team_summaries = []
        
        for team_id in all_teams:
            analytics = await team_analyzer.analyze_team(team_id)
            if analytics:
                total_employees += analytics.active_members
                
                # Count high-risk members
                high_risk_count = analytics.risk_distribution.get(BurnoutRiskLevel.HIGH, 0)
                critical_count = analytics.risk_distribution.get(BurnoutRiskLevel.CRITICAL, 0)
                
                total_high_risk += high_risk_count
                total_critical += critical_count
                
                team_summaries.append({
                    "team_id": team_id,
                    "team_name": analytics.team_name,
                    "alert_level": analytics.team_alert_level.value,
                    "risk_score": round(analytics.average_risk_score, 3),
                    "members_count": analytics.active_members
                })
        
        # Calculate company-wide metrics
        company_risk_percentage = (total_high_risk + total_critical) / max(total_employees, 1) * 100
        
        result = {
            "overview": {
                "total_employees": total_employees,
                "total_teams": len(all_teams),
                "high_risk_employees": total_high_risk,
                "critical_employees": total_critical,
                "company_risk_percentage": round(company_risk_percentage, 1)
            },
            "team_summaries": sorted(team_summaries, key=lambda x: x["risk_score"], reverse=True),
            "top_alerts": [
                team for team in team_summaries 
                if team["alert_level"] in ["red", "orange"]
            ][:5],
            "last_updated": datetime.now().isoformat()
        }
        
        safe_logger.info(f"Dashboard overview retrieved by user {user['user_id']}")
        return result
        
    except Exception as e:
        safe_logger.error(f"Error generating dashboard overview: {e}")
        raise HTTPException(status_code=500, detail="Internal server error")


# Helper functions
def _map_priority_to_severity(priority: int) -> str:
    """Map intervention priority to severity level."""
    if priority >= 5:
        return "critical"
    elif priority >= 4:
        return "high"
    elif priority >= 3:
        return "medium"
    else:
        return "low"


def _export_csv_report(analytics: TeamAnalytics, team_id: str) -> StreamingResponse:
    """Export team analytics as CSV."""
    
    output = StringIO()
    writer = csv.writer(output)
    
    # Write headers and data
    writer.writerow(["Team Report", team_id])
    writer.writerow(["Generated", analytics.analysis_timestamp.isoformat()])
    writer.writerow([])
    
    writer.writerow(["Metric", "Value"])
    writer.writerow(["Team Name", analytics.team_name])
    writer.writerow(["Total Members", analytics.total_members])
    writer.writerow(["Active Members", analytics.active_members])
    writer.writerow(["Average Risk Score", round(analytics.average_risk_score, 3)])
    writer.writerow(["Alert Level", analytics.team_alert_level.value])
    writer.writerow(["Intervention Priority", analytics.intervention_priority])
    
    writer.writerow([])
    writer.writerow(["Risk Distribution"])
    for level, count in analytics.risk_distribution.items():
        writer.writerow([level.value, count])
    
    writer.writerow([])
    writer.writerow(["Team Indicators"])
    for indicator in analytics.team_indicators:
        writer.writerow([indicator])
    
    writer.writerow([])
    writer.writerow(["HR Recommendations"])
    for rec in analytics.hr_recommendations:
        writer.writerow([rec])
    
    output.seek(0)
    content = output.getvalue()
    output.close()
    
    return StreamingResponse(
        StringIO(content),
        media_type="text/csv",
        headers={"Content-Disposition": f"attachment; filename=team_{team_id}_report.csv"}
    )


def _export_json_report(analytics: TeamAnalytics, team_id: str) -> StreamingResponse:
    """Export team analytics as JSON."""
    
    data = {
        "team_id": analytics.team_id,
        "team_name": analytics.team_name,
        "analysis_timestamp": analytics.analysis_timestamp.isoformat(),
        "summary": {
            "total_members": analytics.total_members,
            "active_members": analytics.active_members,
            "average_risk_score": analytics.average_risk_score,
            "team_alert_level": analytics.team_alert_level.value,
            "intervention_priority": analytics.intervention_priority
        },
        "risk_distribution": {
            level.value: count for level, count in analytics.risk_distribution.items()
        },
        "team_indicators": analytics.team_indicators,
        "workload_metrics": analytics.workload_metrics,
        "hr_recommendations": analytics.hr_recommendations
    }
    
    content = json.dumps(data, indent=2, ensure_ascii=False)
    
    return StreamingResponse(
        StringIO(content),
        media_type="application/json",
        headers={"Content-Disposition": f"attachment; filename=team_{team_id}_report.json"}
    )


# Background task for periodic analytics updates
async def update_team_analytics_background():
    """Background task to update team analytics periodically."""
    while True:
        try:
            # Update analytics for all teams
            for team_id in team_analyzer.team_members.keys():
                await team_analyzer.analyze_team(team_id)
            
            safe_logger.debug("Background analytics update completed")
            
        except Exception as e:
            safe_logger.error(f"Error in background analytics update: {e}")
        
        # Wait 5 minutes before next update
        await asyncio.sleep(300)


# WebSocket endpoint for real-time updates (placeholder)
@router.websocket("/ws/team/{team_id}/alerts")
async def team_alerts_websocket(websocket, team_id: str):
    """WebSocket endpoint for real-time team alerts."""
    # This would integrate with the existing WebSocket infrastructure
    pass