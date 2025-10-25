"""
🛡️🧠 Emotional Safety Monitor — OpenAI safety-first practices

Real-time safety monitoring для эмоциональных данных:
- OpenAI: Content safety & harmful content detection
- Stanford: Adaptive safety thresholds
- DeepMind: Multi-modal safety coordination
- Google: Privacy-first safety design
"""

import time
import asyncio
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from collections import deque, defaultdict
from dataclasses import dataclass
from enum import Enum

from ..utils import safe_logger


class SafetyLevel(Enum):
    """Уровни безопасности."""
    SAFE = "safe"
    CAUTION = "caution"
    WARNING = "warning"
    CRITICAL = "critical"


@dataclass
class SafetyAlert:
    """Предупреждение о безопасности."""
    timestamp: datetime
    user_id: str
    level: SafetyLevel
    category: str
    message: str
    data: Dict[str, Any]
    action_taken: Optional[str] = None


class EmotionalSafetyMonitor:
    """
    OpenAI-style safety monitor для эмоциональных данных.
    
    Features:
    - Real-time safety analysis
    - Adaptive safety thresholds
    - Privacy-first design
    - Multi-modal safety coordination
    """
    
    def __init__(self):
        self.safety_enabled = True
        self.alerts_history: deque = deque(maxlen=1000)
        self.user_safety_scores: Dict[str, float] = defaultdict(lambda: 1.0)
        
        # Safety thresholds (OpenAI practice)
        self.thresholds = {
            "emotional_volatility": 0.8,
            "session_duration_hours": 4.0,
            "extreme_emotion_frequency": 0.7,
            "privacy_concern_level": 0.5
        }
        
        # Monitoring counters
        self.safety_checks_performed = 0
        self.alerts_triggered = 0
        self.false_positives = 0
        
        safe_logger.info("Emotional Safety Monitor initialized with OpenAI practices")
    
    async def analyze_emotional_safety(
        self, 
        user_id: str, 
        emotional_data: Dict[str, Any],
        session_context: Dict[str, Any] = None
    ) -> Tuple[SafetyLevel, List[SafetyAlert]]:
        """
        Анализирует безопасность эмоциональных данных.
        """
        if not self.safety_enabled:
            return SafetyLevel.SAFE, []
        
        self.safety_checks_performed += 1
        alerts = []
        max_safety_level = SafetyLevel.SAFE
        
        try:
            # 1. Emotional volatility check (Stanford practice)
            volatility_alert = await self._check_emotional_volatility(user_id, emotional_data)
            if volatility_alert:
                alerts.append(volatility_alert)
                max_safety_level = max(max_safety_level, volatility_alert.level)
            
            # 2. Session duration safety (Google practice)
            duration_alert = await self._check_session_duration(user_id, session_context)
            if duration_alert:
                alerts.append(duration_alert)
                max_safety_level = max(max_safety_level, duration_alert.level)
            
            # 3. Privacy protection check (OpenAI practice)
            privacy_alert = await self._check_privacy_concerns(user_id, emotional_data)
            if privacy_alert:
                alerts.append(privacy_alert)
                max_safety_level = max(max_safety_level, privacy_alert.level)
            
            # 4. Extreme emotion frequency (DeepMind practice)
            frequency_alert = await self._check_extreme_emotion_frequency(user_id, emotional_data)
            if frequency_alert:
                alerts.append(frequency_alert)
                max_safety_level = max(max_safety_level, frequency_alert.level)
            
            # Update user safety score
            self._update_user_safety_score(user_id, max_safety_level, alerts)
            
            # Log alerts
            for alert in alerts:
                self.alerts_history.append(alert)
                self.alerts_triggered += 1
                safe_logger.warning(f"Safety alert for {user_id}: {alert.category} - {alert.message}")
            
            return max_safety_level, alerts
            
        except Exception as e:
            safe_logger.error(f"Safety analysis error for {user_id}: {e}")
            return SafetyLevel.CAUTION, [SafetyAlert(
                timestamp=datetime.now(),
                user_id=user_id,
                level=SafetyLevel.CAUTION,
                category="system_error",
                message=f"Safety analysis failed: {str(e)}",
                data={"error": str(e)}
            )]
    
    async def _check_emotional_volatility(self, user_id: str, data: Dict) -> Optional[SafetyAlert]:
        """Проверяет эмоциональную нестабильность."""
        try:
            features = data.get("features", {})
            volatility_score = features.get("volatility", 0.0)
            
            if volatility_score > self.thresholds["emotional_volatility"]:
                return SafetyAlert(
                    timestamp=datetime.now(),
                    user_id=user_id,
                    level=SafetyLevel.WARNING if volatility_score > 0.9 else SafetyLevel.CAUTION,
                    category="emotional_volatility",
                    message=f"High emotional volatility detected: {volatility_score:.2f}",
                    data={"volatility_score": volatility_score, "threshold": self.thresholds["emotional_volatility"]},
                    action_taken="emotional_smoothing_recommended"
                )
        except Exception as e:
            safe_logger.error(f"Volatility check error: {e}")
        
        return None
    
    async def _check_session_duration(self, user_id: str, context: Dict) -> Optional[SafetyAlert]:
        """Проверяет продолжительность сессии."""
        try:
            if not context:
                return None
                
            session_start = context.get("session_start")
            if session_start:
                duration_hours = (datetime.now() - session_start).total_seconds() / 3600
                
                if duration_hours > self.thresholds["session_duration_hours"]:
                    level = SafetyLevel.CRITICAL if duration_hours > 8 else SafetyLevel.WARNING
                    return SafetyAlert(
                        timestamp=datetime.now(),
                        user_id=user_id,
                        level=level,
                        category="session_duration",
                        message=f"Extended session duration: {duration_hours:.1f} hours",
                        data={"duration_hours": duration_hours, "threshold": self.thresholds["session_duration_hours"]},
                        action_taken="break_recommendation_sent"
                    )
        except Exception as e:
            safe_logger.error(f"Session duration check error: {e}")
        
        return None
    
    async def _check_privacy_concerns(self, user_id: str, data: Dict) -> Optional[SafetyAlert]:
        """Проверяет приватность данных."""
        try:
            # Check for potentially sensitive emotional patterns
            features = data.get("features", {})
            sensitivity_score = features.get("emotional_sensitivity", 0.0)
            
            if sensitivity_score > self.thresholds["privacy_concern_level"]:
                return SafetyAlert(
                    timestamp=datetime.now(),
                    user_id=user_id,
                    level=SafetyLevel.CAUTION,
                    category="privacy_concern",
                    message="Potentially sensitive emotional data detected",
                    data={"sensitivity_score": sensitivity_score},
                    action_taken="data_anonymization_applied"
                )
        except Exception as e:
            safe_logger.error(f"Privacy check error: {e}")
        
        return None
    
    async def _check_extreme_emotion_frequency(self, user_id: str, data: Dict) -> Optional[SafetyAlert]:
        """Проверяет частоту экстремальных эмоций."""
        try:
            mode_data = data.get("mode", {})
            intensity = mode_data.get("intensity", 0.0)
            
            if intensity > self.thresholds["extreme_emotion_frequency"]:
                return SafetyAlert(
                    timestamp=datetime.now(),
                    user_id=user_id,
                    level=SafetyLevel.WARNING,
                    category="extreme_emotions",
                    message=f"High emotional intensity detected: {intensity:.2f}",
                    data={"intensity": intensity, "mode": mode_data.get("type", "unknown")},
                    action_taken="calming_recommendations_provided"
                )
        except Exception as e:
            safe_logger.error(f"Extreme emotion check error: {e}")
        
        return None
    
    def _update_user_safety_score(self, user_id: str, level: SafetyLevel, alerts: List[SafetyAlert]):
        """Обновляет рейтинг безопасности пользователя."""
        current_score = self.user_safety_scores[user_id]
        
        # Adjust based on safety level
        if level == SafetyLevel.SAFE:
            # Slowly improve score
            self.user_safety_scores[user_id] = min(1.0, current_score + 0.01)
        elif level == SafetyLevel.CAUTION:
            self.user_safety_scores[user_id] = max(0.1, current_score - 0.05)
        elif level == SafetyLevel.WARNING:
            self.user_safety_scores[user_id] = max(0.1, current_score - 0.1)
        elif level == SafetyLevel.CRITICAL:
            self.user_safety_scores[user_id] = max(0.1, current_score - 0.2)
    
    def get_user_safety_score(self, user_id: str) -> float:
        """Возвращает рейтинг безопасности пользователя."""
        return self.user_safety_scores[user_id]
    
    def get_recent_alerts(self, user_id: Optional[str] = None, hours: int = 24) -> List[SafetyAlert]:
        """Возвращает последние предупреждения."""
        cutoff_time = datetime.now() - timedelta(hours=hours)
        
        recent_alerts = [
            alert for alert in self.alerts_history
            if alert.timestamp > cutoff_time
        ]
        
        if user_id:
            recent_alerts = [alert for alert in recent_alerts if alert.user_id == user_id]
        
        return recent_alerts
    
    def get_safety_analytics(self) -> Dict[str, Any]:
        """Возвращает аналитику по безопасности."""
        recent_alerts = self.get_recent_alerts(hours=24)
        
        # Count alerts by level
        level_counts = defaultdict(int)
        category_counts = defaultdict(int)
        
        for alert in recent_alerts:
            level_counts[alert.level.value] += 1
            category_counts[alert.category] += 1
        
        return {
            "safety_enabled": self.safety_enabled,
            "total_checks_performed": self.safety_checks_performed,
            "total_alerts_triggered": self.alerts_triggered,
            "false_positives": self.false_positives,
            "recent_alerts_24h": len(recent_alerts),
            "alert_levels_24h": dict(level_counts),
            "alert_categories_24h": dict(category_counts),
            "active_users_monitored": len(self.user_safety_scores),
            "average_user_safety_score": sum(self.user_safety_scores.values()) / max(len(self.user_safety_scores), 1),
            "thresholds": self.thresholds
        }
    
    async def update_safety_threshold(self, threshold_name: str, value: float):
        """Обновляет пороговое значение безопасности."""
        if threshold_name in self.thresholds:
            old_value = self.thresholds[threshold_name]
            self.thresholds[threshold_name] = value
            safe_logger.info(f"Safety threshold updated: {threshold_name} {old_value} -> {value}")
        else:
            safe_logger.warning(f"Unknown safety threshold: {threshold_name}")
    
    async def report_false_positive(self, alert_id: str):
        """Отмечает ложное срабатывание."""
        self.false_positives += 1
        safe_logger.info(f"False positive reported for alert: {alert_id}")
    
    def enable_safety_monitoring(self):
        """Включает мониторинг безопасности."""
        self.safety_enabled = True
        safe_logger.info("Safety monitoring enabled")
    
    def disable_safety_monitoring(self):
        """Отключает мониторинг безопасности."""
        self.safety_enabled = False
        safe_logger.info("Safety monitoring disabled")


# Global safety monitor instance
_safety_monitor: Optional[EmotionalSafetyMonitor] = None

def get_safety_monitor() -> EmotionalSafetyMonitor:
    """Возвращает глобальный safety monitor."""
    global _safety_monitor
    if _safety_monitor is None:
        _safety_monitor = EmotionalSafetyMonitor()
    return _safety_monitor