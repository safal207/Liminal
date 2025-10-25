"""
Biomarkers and Predictive Analytics Engine for LIMINAL RGL
AI-powered early detection and prediction system for mental health and cognitive states

Features:
- Multi-modal biomarker analysis
- Predictive modeling for early intervention
- Real-time risk assessment
- Personalized health insights
- Integration with RGL neural navigation
"""

import numpy as np
from typing import Dict, List, Tuple, Optional, Any, Union
import asyncio
import logging
from datetime import datetime, timedelta
from dataclasses import dataclass, field
from enum import Enum
import json
import math
from collections import defaultdict, deque

class BiomarkerType(Enum):
    """Types of biomarkers for analysis"""
    NEURAL = "neural"                    # EEG, neural oscillations
    PHYSIOLOGICAL = "physiological"     # Heart rate, breathing, skin conductance
    BEHAVIORAL = "behavioral"           # Activity patterns, response times
    COGNITIVE = "cognitive"             # Memory, attention, processing speed
    EMOTIONAL = "emotional"             # Mood, stress, emotional regulation
    CIRCADIAN = "circadian"             # Sleep, activity cycles
    LINGUISTIC = "linguistic"           # Speech patterns, text analysis

class RiskLevel(Enum):
    """Risk assessment levels"""
    LOW = "low"
    MODERATE = "moderate"
    HIGH = "high"
    CRITICAL = "critical"

class HealthCondition(Enum):
    """Health conditions for prediction"""
    DEPRESSION = "depression"
    ANXIETY = "anxiety"
    ADHD = "adhd"
    BIPOLAR = "bipolar"
    PTSD = "ptsd"
    BURNOUT = "burnout"
    COGNITIVE_DECLINE = "cognitive_decline"
    SLEEP_DISORDER = "sleep_disorder"

@dataclass
class Biomarker:
    """Individual biomarker measurement"""
    type: BiomarkerType
    name: str
    value: float
    timestamp: datetime
    confidence: float = 0.8
    source: str = "sensor"
    normalized_value: float = 0.0

@dataclass
class PredictionResult:
    """Result of predictive analysis"""
    condition: HealthCondition
    probability: float
    risk_level: RiskLevel
    confidence: float
    contributing_biomarkers: List[str]
    recommendation: str
    timeline_days: int
    prevention_score: float

@dataclass
class HealthProfile:
    """User health profile for personalized analysis"""
    user_id: str
    age: int
    gender: str
    medical_history: List[str] = field(default_factory=list)
    current_medications: List[str] = field(default_factory=list)
    risk_factors: Dict[str, float] = field(default_factory=dict)
    baseline_biomarkers: Dict[str, float] = field(default_factory=dict)
    last_updated: datetime = field(default_factory=datetime.now)

class BiomarkerAnalyzer:
    """Advanced biomarker analysis and feature extraction"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        
        # Biomarker normalization ranges (based on research)
        self.biomarker_ranges = {
            'theta_power': (0.1, 2.0),      # μV²/Hz
            'gamma_power': (0.05, 1.0),     # μV²/Hz
            'heart_rate': (60, 100),        # BPM
            'hrv_rmssd': (20, 50),          # ms
            'skin_conductance': (1, 20),    # μS
            'reaction_time': (200, 800),    # ms
            'attention_score': (0, 100),    # %
            'mood_score': (-10, 10),        # scale
            'sleep_efficiency': (0.7, 0.95), # ratio
            'stress_level': (0, 10),        # scale
        }
        
        # Feature importance weights (learned from research data)
        self.feature_weights = {
            HealthCondition.DEPRESSION: {
                'theta_power': 0.15,
                'gamma_power': 0.12,
                'heart_rate': 0.10,
                'mood_score': 0.25,
                'sleep_efficiency': 0.20,
                'activity_level': 0.18
            },
            HealthCondition.ANXIETY: {
                'heart_rate': 0.20,
                'hrv_rmssd': 0.18,
                'skin_conductance': 0.15,
                'gamma_power': 0.12,
                'stress_level': 0.25,
                'breathing_rate': 0.10
            },
            HealthCondition.ADHD: {
                'theta_power': 0.25,
                'beta_power': 0.20,
                'attention_score': 0.20,
                'reaction_time': 0.15,
                'hyperactivity_score': 0.20
            }
        }
    
    def normalize_biomarker(self, biomarker: Biomarker) -> float:
        """Normalize biomarker value to 0-1 range"""
        if biomarker.name not in self.biomarker_ranges:
            return biomarker.value  # Return as-is if no range defined
        
        min_val, max_val = self.biomarker_ranges[biomarker.name]
        normalized = (biomarker.value - min_val) / (max_val - min_val)
        normalized = max(0.0, min(1.0, normalized))  # Clamp to [0,1]
        
        biomarker.normalized_value = normalized
        return normalized
    
    def extract_temporal_features(self, biomarkers: List[Biomarker], 
                                window_hours: int = 24) -> Dict[str, float]:
        """Extract temporal features from biomarker time series"""
        features = {}
        
        # Group biomarkers by type and name
        biomarker_groups = defaultdict(list)
        cutoff_time = datetime.now() - timedelta(hours=window_hours)
        
        for biomarker in biomarkers:
            if biomarker.timestamp >= cutoff_time:
                key = f"{biomarker.type.value}_{biomarker.name}"
                biomarker_groups[key].append(biomarker)
        
        # Calculate temporal features for each biomarker type
        for biomarker_key, group in biomarker_groups.items():
            if len(group) < 2:
                continue
            
            values = [self.normalize_biomarker(b) for b in group]
            timestamps = [b.timestamp for b in group]
            
            # Statistical features
            features[f"{biomarker_key}_mean"] = np.mean(values)
            features[f"{biomarker_key}_std"] = np.std(values)
            features[f"{biomarker_key}_min"] = np.min(values)
            features[f"{biomarker_key}_max"] = np.max(values)
            
            # Temporal features
            if len(values) >= 3:
                # Trend analysis
                time_numeric = [(t - timestamps[0]).total_seconds() for t in timestamps]
                correlation = np.corrcoef(time_numeric, values)[0, 1]
                features[f"{biomarker_key}_trend"] = correlation if not np.isnan(correlation) else 0.0
                
                # Volatility
                differences = np.diff(values)
                features[f"{biomarker_key}_volatility"] = np.std(differences)
                
                # Circadian alignment (if relevant)
                if biomarker_key in ['sleep_efficiency', 'activity_level', 'mood_score']:
                    hours = [t.hour for t in timestamps]
                    circadian_score = self._calculate_circadian_alignment(hours, values)
                    features[f"{biomarker_key}_circadian"] = circadian_score
        
        return features
    
    def _calculate_circadian_alignment(self, hours: List[int], values: List[float]) -> float:
        """Calculate how well biomarker aligns with circadian rhythm"""
        # Expected circadian patterns
        circadian_patterns = {
            'sleep_efficiency': [0.9, 0.95, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 
                               0.4, 0.3, 0.3, 0.3, 0.3, 0.3, 0.4, 0.5,
                               0.6, 0.7, 0.8, 0.85, 0.9, 0.92, 0.93, 0.9],
            'activity_level': [0.1, 0.05, 0.05, 0.05, 0.05, 0.1, 0.3, 0.6,
                             0.8, 0.9, 0.95, 0.9, 0.85, 0.8, 0.75, 0.7,
                             0.6, 0.5, 0.4, 0.3, 0.25, 0.2, 0.15, 0.1],
            'mood_score': [5, 4, 3, 2, 2, 3, 5, 7, 8, 8, 8, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5]
        }
        
        # Use activity pattern as default
        expected_pattern = circadian_patterns.get('activity_level', circadian_patterns['activity_level'])
        
        # Calculate correlation with expected pattern
        try:
            expected_values = [expected_pattern[hour] for hour in hours]
            correlation = np.corrcoef(values, expected_values)[0, 1]
            return correlation if not np.isnan(correlation) else 0.0
        except:
            return 0.0

class PredictiveModel:
    """AI predictive model for health condition risk assessment"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.analyzer = BiomarkerAnalyzer()
        
        # Model parameters (would be trained on real data)
        self.model_weights = {
            HealthCondition.DEPRESSION: {
                'theta_power_mean': -0.3,
                'gamma_power_mean': -0.2,
                'mood_score_mean': -0.8,
                'sleep_efficiency_mean': -0.6,
                'activity_level_mean': -0.5,
                'heart_rate_std': 0.3,
                'intercept': 0.2
            },
            HealthCondition.ANXIETY: {
                'heart_rate_mean': 0.6,
                'hrv_rmssd_mean': -0.5,
                'skin_conductance_mean': 0.4,
                'stress_level_mean': 0.7,
                'gamma_power_std': 0.3,
                'breathing_rate_mean': 0.4,
                'intercept': 0.1
            },
            HealthCondition.ADHD: {
                'theta_power_mean': 0.7,
                'beta_power_mean': -0.4,
                'attention_score_mean': -0.8,
                'reaction_time_mean': 0.5,
                'hyperactivity_score_mean': 0.6,
                'theta_power_volatility': 0.4,
                'intercept': 0.15
            }
        }
        
        # Risk thresholds
        self.risk_thresholds = {
            RiskLevel.LOW: (0.0, 0.3),
            RiskLevel.MODERATE: (0.3, 0.6),
            RiskLevel.HIGH: (0.6, 0.8),
            RiskLevel.CRITICAL: (0.8, 1.0)
        }
    
    def predict_condition_risk(self, biomarkers: List[Biomarker], 
                             health_profile: HealthProfile,
                             condition: HealthCondition) -> PredictionResult:
        """Predict risk for specific health condition"""
        
        try:
            # Extract features from biomarkers
            features = self.analyzer.extract_temporal_features(biomarkers)
            
            # Apply personalization based on health profile
            personalized_features = self._personalize_features(features, health_profile)
            
            # Calculate risk score using linear model (would be neural network in production)
            risk_score = self._calculate_risk_score(personalized_features, condition)
            
            # Determine risk level
            risk_level = self._classify_risk_level(risk_score)
            
            # Calculate confidence based on data quality
            confidence = self._calculate_prediction_confidence(biomarkers, features)
            
            # Identify contributing biomarkers
            contributing_biomarkers = self._identify_contributing_biomarkers(
                personalized_features, condition
            )
            
            # Generate recommendation
            recommendation = self._generate_recommendation(condition, risk_level, contributing_biomarkers)
            
            # Estimate timeline
            timeline_days = self._estimate_timeline(risk_score, condition)
            
            # Calculate prevention potential
            prevention_score = self._calculate_prevention_score(risk_score, health_profile)
            
            return PredictionResult(
                condition=condition,
                probability=risk_score,
                risk_level=risk_level,
                confidence=confidence,
                contributing_biomarkers=contributing_biomarkers,
                recommendation=recommendation,
                timeline_days=timeline_days,
                prevention_score=prevention_score
            )
            
        except Exception as e:
            self.logger.error(f"Prediction failed for {condition.value}: {e}")
            return self._create_error_prediction(condition, str(e))
    
    def _personalize_features(self, features: Dict[str, float], 
                            health_profile: HealthProfile) -> Dict[str, float]:
        """Personalize features based on user health profile"""
        personalized = features.copy()
        
        # Age adjustments
        age_factor = max(0.5, 1.0 - (health_profile.age - 30) * 0.01)  # Gradual decline after 30
        
        # Apply age adjustments to cognitive and physiological markers
        cognitive_markers = [k for k in personalized.keys() if 'attention' in k or 'reaction' in k]
        for marker in cognitive_markers:
            personalized[marker] *= age_factor
        
        # Gender adjustments (research-based differences)
        if health_profile.gender.lower() == 'female':
            # Women typically have higher anxiety biomarker sensitivity
            anxiety_markers = [k for k in personalized.keys() if 'heart_rate' in k or 'stress' in k]
            for marker in anxiety_markers:
                personalized[marker] *= 1.1
        
        # Medical history risk factors
        for condition in health_profile.medical_history:
            if 'depression' in condition.lower():
                mood_markers = [k for k in personalized.keys() if 'mood' in k]
                for marker in mood_markers:
                    personalized[marker] *= 1.2  # Increased sensitivity
        
        # Baseline comparison
        for marker_name, baseline_value in health_profile.baseline_biomarkers.items():
            matching_features = [k for k in personalized.keys() if marker_name in k and 'mean' in k]
            for feature in matching_features:
                if feature in personalized:
                    # Calculate deviation from personal baseline
                    deviation = abs(personalized[feature] - baseline_value)
                    personalized[f"{feature}_baseline_deviation"] = deviation
        
        return personalized
    
    def _calculate_risk_score(self, features: Dict[str, float], 
                            condition: HealthCondition) -> float:
        """Calculate risk score using linear model"""
        if condition not in self.model_weights:
            return 0.0
        
        weights = self.model_weights[condition]
        score = weights.get('intercept', 0.0)
        
        for feature_name, weight in weights.items():
            if feature_name == 'intercept':
                continue
            
            feature_value = features.get(feature_name, 0.0)
            score += weight * feature_value
        
        # Apply sigmoid to get probability
        probability = 1.0 / (1.0 + math.exp(-score))
        return probability
    
    def _classify_risk_level(self, risk_score: float) -> RiskLevel:
        """Classify risk score into risk level"""
        for level, (min_score, max_score) in self.risk_thresholds.items():
            if min_score <= risk_score < max_score:
                return level
        return RiskLevel.CRITICAL  # Default to critical if above all thresholds
    
    def _calculate_prediction_confidence(self, biomarkers: List[Biomarker], 
                                       features: Dict[str, float]) -> float:
        """Calculate confidence in prediction based on data quality"""
        # Data quantity factor
        quantity_factor = min(1.0, len(biomarkers) / 50.0)  # Optimal at 50+ biomarkers
        
        # Data recency factor
        recent_biomarkers = [b for b in biomarkers 
                           if (datetime.now() - b.timestamp).total_seconds() < 3600]  # Last hour
        recency_factor = min(1.0, len(recent_biomarkers) / 10.0)  # Optimal at 10+ recent
        
        # Feature completeness factor
        expected_features = 10  # Expected number of key features
        completeness_factor = min(1.0, len(features) / expected_features)
        
        # Sensor confidence factor
        sensor_confidences = [b.confidence for b in biomarkers]
        sensor_factor = np.mean(sensor_confidences) if sensor_confidences else 0.5
        
        # Overall confidence
        confidence = (quantity_factor + recency_factor + completeness_factor + sensor_factor) / 4.0
        return max(0.1, min(1.0, confidence))
    
    def _identify_contributing_biomarkers(self, features: Dict[str, float], 
                                        condition: HealthCondition) -> List[str]:
        """Identify biomarkers contributing most to prediction"""
        if condition not in self.model_weights:
            return []
        
        weights = self.model_weights[condition]
        feature_impacts = []
        
        for feature_name, weight in weights.items():
            if feature_name == 'intercept':
                continue
            
            feature_value = features.get(feature_name, 0.0)
            impact = abs(weight * feature_value)
            feature_impacts.append((feature_name, impact))
        
        # Sort by impact and return top contributors
        feature_impacts.sort(key=lambda x: x[1], reverse=True)
        return [name for name, impact in feature_impacts[:5]]
    
    def _generate_recommendation(self, condition: HealthCondition, 
                               risk_level: RiskLevel, 
                               contributing_biomarkers: List[str]) -> str:
        """Generate personalized recommendation"""
        base_recommendations = {
            HealthCondition.DEPRESSION: {
                RiskLevel.LOW: "Maintain healthy lifestyle habits. Continue regular exercise and social activities.",
                RiskLevel.MODERATE: "Consider mindfulness practices and ensure adequate sleep. Monitor mood patterns.",
                RiskLevel.HIGH: "Recommend professional consultation. Implement daily mood tracking and support system.",
                RiskLevel.CRITICAL: "Urgent professional intervention needed. Contact mental health professional immediately."
            },
            HealthCondition.ANXIETY: {
                RiskLevel.LOW: "Practice relaxation techniques. Maintain regular exercise routine.",
                RiskLevel.MODERATE: "Implement stress reduction strategies. Consider breathing exercises and meditation.",
                RiskLevel.HIGH: "Seek anxiety management support. Practice daily calming techniques.",
                RiskLevel.CRITICAL: "Professional anxiety treatment recommended. Contact healthcare provider."
            },
            HealthCondition.ADHD: {
                RiskLevel.LOW: "Maintain structured routines. Continue organizational strategies.",
                RiskLevel.MODERATE: "Enhance focus techniques. Consider attention training exercises.",
                RiskLevel.HIGH: "Professional ADHD assessment recommended. Implement comprehensive management plan.",
                RiskLevel.CRITICAL: "Comprehensive ADHD evaluation needed. Contact specialist immediately."
            }
        }
        
        base_rec = base_recommendations.get(condition, {}).get(risk_level, "Consult healthcare professional.")
        
        # Add specific recommendations based on contributing biomarkers
        specific_recs = []
        for biomarker in contributing_biomarkers[:3]:  # Top 3 contributors
            if 'sleep' in biomarker:
                specific_recs.append("Focus on sleep hygiene improvement.")
            elif 'heart_rate' in biomarker:
                specific_recs.append("Monitor cardiovascular health.")
            elif 'mood' in biomarker:
                specific_recs.append("Track daily mood patterns.")
            elif 'attention' in biomarker:
                specific_recs.append("Practice attention training exercises.")
        
        if specific_recs:
            return f"{base_rec} Specific focus: {' '.join(specific_recs)}"
        return base_rec
    
    def _estimate_timeline(self, risk_score: float, condition: HealthCondition) -> int:
        """Estimate timeline for condition development/intervention"""
        base_timelines = {
            HealthCondition.DEPRESSION: 30,    # days
            HealthCondition.ANXIETY: 14,       # days
            HealthCondition.ADHD: 90,          # days (assessment period)
            HealthCondition.BURNOUT: 21,       # days
            HealthCondition.COGNITIVE_DECLINE: 365  # days
        }
        
        base_timeline = base_timelines.get(condition, 30)
        
        # Adjust based on risk score
        urgency_factor = 1.0 - risk_score  # Higher risk = shorter timeline
        adjusted_timeline = int(base_timeline * urgency_factor)
        
        return max(1, adjusted_timeline)  # At least 1 day
    
    def _calculate_prevention_score(self, risk_score: float, 
                                  health_profile: HealthProfile) -> float:
        """Calculate potential for prevention/improvement"""
        # Base prevention potential (inverse of risk)
        base_prevention = 1.0 - risk_score
        
        # Age factor (younger = better prevention potential)
        age_factor = max(0.5, 1.0 - (health_profile.age - 20) * 0.01)
        
        # Medical history factor
        history_factor = max(0.6, 1.0 - len(health_profile.medical_history) * 0.1)
        
        # Overall prevention score
        prevention_score = base_prevention * age_factor * history_factor
        return max(0.0, min(1.0, prevention_score))
    
    def _create_error_prediction(self, condition: HealthCondition, error_msg: str) -> PredictionResult:
        """Create error prediction result"""
        return PredictionResult(
            condition=condition,
            probability=0.0,
            risk_level=RiskLevel.LOW,
            confidence=0.0,
            contributing_biomarkers=[],
            recommendation=f"Prediction error: {error_msg}",
            timeline_days=0,
            prevention_score=0.0
        )

class PredictiveAnalyticsEngine:
    """Main engine for biomarker analysis and predictive analytics"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.model = PredictiveModel()
        
        # Storage for user data
        self.user_profiles: Dict[str, HealthProfile] = {}
        self.biomarker_history: Dict[str, deque] = defaultdict(lambda: deque(maxlen=10000))
        
        # Real-time monitoring
        self.monitoring_active = False
        self.alert_thresholds = {
            RiskLevel.HIGH: True,      # Alert on high risk
            RiskLevel.CRITICAL: True   # Alert on critical risk
        }
    
    async def analyze_user_health(self, user_id: str, 
                                biomarkers: List[Biomarker]) -> Dict[str, PredictionResult]:
        """Comprehensive health analysis for user"""
        try:
            # Get or create user profile
            health_profile = self.user_profiles.get(user_id)
            if not health_profile:
                health_profile = HealthProfile(
                    user_id=user_id,
                    age=30,  # Default age
                    gender="unknown"
                )
                self.user_profiles[user_id] = health_profile
            
            # Store biomarkers in history
            for biomarker in biomarkers:
                self.biomarker_history[user_id].append(biomarker)
            
            # Get recent biomarkers for analysis
            recent_biomarkers = list(self.biomarker_history[user_id])
            
            # Analyze for all conditions
            results = {}
            conditions_to_analyze = [
                HealthCondition.DEPRESSION,
                HealthCondition.ANXIETY,
                HealthCondition.ADHD,
                HealthCondition.BURNOUT
            ]
            
            for condition in conditions_to_analyze:
                result = self.model.predict_condition_risk(
                    recent_biomarkers, health_profile, condition
                )
                results[condition.value] = result
                
                # Check for alerts
                if result.risk_level in self.alert_thresholds:
                    await self._send_alert(user_id, result)
            
            # Update user profile baseline if needed
            await self._update_baseline_biomarkers(user_id, biomarkers)
            
            self.logger.info(f"Health analysis complete for user {user_id}: {len(results)} conditions analyzed")
            return results
            
        except Exception as e:
            self.logger.error(f"Health analysis failed for user {user_id}: {e}")
            return {}
    
    async def real_time_monitoring(self, user_id: str, biomarker: Biomarker) -> Optional[PredictionResult]:
        """Real-time biomarker monitoring for immediate alerts"""
        if not self.monitoring_active:
            return None
        
        try:
            # Add to history
            self.biomarker_history[user_id].append(biomarker)
            
            # Quick risk assessment for critical biomarkers
            if biomarker.type in [BiomarkerType.NEURAL, BiomarkerType.EMOTIONAL, BiomarkerType.PHYSIOLOGICAL]:
                recent_biomarkers = list(self.biomarker_history[user_id])[-50:]  # Last 50 measurements
                
                health_profile = self.user_profiles.get(user_id)
                if not health_profile:
                    return None
                
                # Fast screening for high-risk conditions
                high_priority_conditions = [HealthCondition.DEPRESSION, HealthCondition.ANXIETY]
                
                for condition in high_priority_conditions:
                    result = self.model.predict_condition_risk(
                        recent_biomarkers, health_profile, condition
                    )
                    
                    if result.risk_level in [RiskLevel.HIGH, RiskLevel.CRITICAL]:
                        await self._send_immediate_alert(user_id, result)
                        return result
            
            return None
            
        except Exception as e:
            self.logger.error(f"Real-time monitoring failed for {user_id}: {e}")
            return None
    
    async def generate_health_insights(self, user_id: str) -> Dict[str, Any]:
        """Generate comprehensive health insights for user"""
        if user_id not in self.biomarker_history:
            return {'error': 'No data available for user'}
        
        try:
            biomarkers = list(self.biomarker_history[user_id])
            health_profile = self.user_profiles.get(user_id)
            
            if not health_profile:
                return {'error': 'No health profile found'}
            
            insights = {
                'user_id': user_id,
                'analysis_timestamp': datetime.now().isoformat(),
                'data_period_days': self._calculate_data_period(biomarkers),
                'biomarker_summary': self._summarize_biomarkers(biomarkers),
                'trend_analysis': await self._analyze_trends(biomarkers),
                'risk_summary': {},
                'recommendations': []
            }
            
            # Get current risk predictions
            current_analysis = await self.analyze_user_health(user_id, biomarkers[-10:])  # Last 10 biomarkers
            
            for condition, result in current_analysis.items():
                insights['risk_summary'][condition] = {
                    'probability': result.probability,
                    'risk_level': result.risk_level.value,
                    'confidence': result.confidence,
                    'timeline_days': result.timeline_days,
                    'prevention_score': result.prevention_score
                }
                
                if result.recommendation:
                    insights['recommendations'].append({
                        'condition': condition,
                        'recommendation': result.recommendation,
                        'priority': result.risk_level.value
                    })
            
            return insights
            
        except Exception as e:
            self.logger.error(f"Health insights generation failed for {user_id}: {e}")
            return {'error': str(e)}
    
    async def _send_alert(self, user_id: str, result: PredictionResult):
        """Send health alert (would integrate with notification system)"""
        alert_message = f"Health Alert for {user_id}: {result.condition.value.title()} risk level {result.risk_level.value} detected. {result.recommendation}"
        self.logger.warning(f"HEALTH ALERT: {alert_message}")
        
        # In production, would send to notification system
        # await notification_service.send_alert(user_id, alert_message)
    
    async def _send_immediate_alert(self, user_id: str, result: PredictionResult):
        """Send immediate alert for critical situations"""
        alert_message = f"IMMEDIATE ATTENTION: {user_id} - {result.condition.value.title()} critical risk detected. {result.recommendation}"
        self.logger.critical(f"IMMEDIATE HEALTH ALERT: {alert_message}")
        
        # In production, would trigger emergency protocols
        # await emergency_service.trigger_alert(user_id, result)
    
    async def _update_baseline_biomarkers(self, user_id: str, biomarkers: List[Biomarker]):
        """Update user baseline biomarkers"""
        health_profile = self.user_profiles.get(user_id)
        if not health_profile:
            return
        
        # Calculate new baselines from recent stable periods
        stable_biomarkers = self._identify_stable_periods(biomarkers)
        
        for biomarker in stable_biomarkers:
            baseline_key = f"{biomarker.type.value}_{biomarker.name}"
            health_profile.baseline_biomarkers[baseline_key] = biomarker.normalized_value
        
        health_profile.last_updated = datetime.now()
    
    def _identify_stable_periods(self, biomarkers: List[Biomarker]) -> List[Biomarker]:
        """Identify stable measurement periods for baseline calculation"""
        # Simplified: return recent biomarkers with low volatility
        if len(biomarkers) < 10:
            return biomarkers
        
        # Group by type and calculate stability
        stable_biomarkers = []
        biomarker_groups = defaultdict(list)
        
        for biomarker in biomarkers[-50:]:  # Last 50 measurements
            key = f"{biomarker.type.value}_{biomarker.name}"
            biomarker_groups[key].append(biomarker)
        
        for group in biomarker_groups.values():
            if len(group) >= 5:
                values = [b.value for b in group]
                if np.std(values) < np.mean(values) * 0.2:  # Low volatility
                    stable_biomarkers.extend(group[-3:])  # Last 3 stable measurements
        
        return stable_biomarkers
    
    def _calculate_data_period(self, biomarkers: List[Biomarker]) -> int:
        """Calculate data collection period in days"""
        if len(biomarkers) < 2:
            return 0
        
        oldest = min(biomarkers, key=lambda b: b.timestamp)
        newest = max(biomarkers, key=lambda b: b.timestamp)
        
        return (newest.timestamp - oldest.timestamp).days
    
    def _summarize_biomarkers(self, biomarkers: List[Biomarker]) -> Dict[str, Any]:
        """Summarize biomarker data"""
        summary = {
            'total_measurements': len(biomarkers),
            'types_measured': len(set(b.type for b in biomarkers)),
            'average_confidence': np.mean([b.confidence for b in biomarkers]),
            'measurement_frequency_per_day': 0.0
        }
        
        if len(biomarkers) >= 2:
            data_period_days = self._calculate_data_period(biomarkers)
            if data_period_days > 0:
                summary['measurement_frequency_per_day'] = len(biomarkers) / data_period_days
        
        # Type distribution
        type_counts = defaultdict(int)
        for biomarker in biomarkers:
            type_counts[biomarker.type.value] += 1
        
        summary['type_distribution'] = dict(type_counts)
        
        return summary
    
    async def _analyze_trends(self, biomarkers: List[Biomarker]) -> Dict[str, Any]:
        """Analyze biomarker trends over time"""
        trends = {}
        
        # Group biomarkers by type and name
        biomarker_groups = defaultdict(list)
        for biomarker in biomarkers:
            key = f"{biomarker.type.value}_{biomarker.name}"
            biomarker_groups[key].append(biomarker)
        
        for biomarker_key, group in biomarker_groups.items():
            if len(group) >= 5:  # Need at least 5 points for trend analysis
                # Sort by timestamp
                group.sort(key=lambda b: b.timestamp)
                
                values = [b.value for b in group]
                timestamps = [(b.timestamp - group[0].timestamp).total_seconds() for b in group]
                
                # Linear trend
                correlation = np.corrcoef(timestamps, values)[0, 1]
                trend_direction = "increasing" if correlation > 0.1 else ("decreasing" if correlation < -0.1 else "stable")
                
                trends[biomarker_key] = {
                    'trend_direction': trend_direction,
                    'correlation': float(correlation) if not np.isnan(correlation) else 0.0,
                    'recent_value': values[-1],
                    'change_from_start': values[-1] - values[0],
                    'volatility': float(np.std(values))
                }
        
        return trends
    
    def get_system_statistics(self) -> Dict[str, Any]:
        """Get system statistics"""
        return {
            'timestamp': datetime.now().isoformat(),
            'total_users': len(self.user_profiles),
            'total_biomarker_measurements': sum(len(history) for history in self.biomarker_history.values()),
            'monitoring_active': self.monitoring_active,
            'average_measurements_per_user': (
                sum(len(history) for history in self.biomarker_history.values()) / max(1, len(self.biomarker_history))
            ),
            'alert_thresholds_active': len([level for level, active in self.alert_thresholds.items() if active])
        }

if __name__ == "__main__":
    # Demo predictive analytics engine
    async def predictive_analytics_demo():
        engine = PredictiveAnalyticsEngine()
        
        print("=== Biomarkers & Predictive Analytics Demo ===")
        
        # Create sample biomarkers
        user_id = "demo_user_001"
        sample_biomarkers = [
            Biomarker(BiomarkerType.NEURAL, "theta_power", 1.2, datetime.now(), 0.9),
            Biomarker(BiomarkerType.NEURAL, "gamma_power", 0.3, datetime.now(), 0.8),
            Biomarker(BiomarkerType.PHYSIOLOGICAL, "heart_rate", 85, datetime.now(), 0.95),
            Biomarker(BiomarkerType.EMOTIONAL, "mood_score", -2, datetime.now(), 0.7),
            Biomarker(BiomarkerType.COGNITIVE, "attention_score", 65, datetime.now(), 0.8),
            Biomarker(BiomarkerType.PHYSIOLOGICAL, "sleep_efficiency", 0.75, datetime.now(), 0.9)
        ]
        
        # Analyze health
        results = await engine.analyze_user_health(user_id, sample_biomarkers)
        
        print("=== Health Risk Analysis ===")
        for condition, result in results.items():
            print(f"{condition.upper()}: Risk={result.risk_level.value} ({result.probability:.3f})")
            print(f"  Confidence: {result.confidence:.3f}")
            print(f"  Timeline: {result.timeline_days} days")
            print(f"  Prevention Score: {result.prevention_score:.3f}")
            print(f"  Recommendation: {result.recommendation[:80]}...")
            print()
        
        # Generate health insights
        insights = await engine.generate_health_insights(user_id)
        
        print("=== Health Insights ===")
        print(f"Data Period: {insights['data_period_days']} days")
        print(f"Total Measurements: {insights['biomarker_summary']['total_measurements']}")
        print(f"Average Confidence: {insights['biomarker_summary']['average_confidence']:.3f}")
        
        # Risk summary
        print("\n=== Risk Summary ===")
        for condition, risk_info in insights['risk_summary'].items():
            print(f"{condition}: {risk_info['risk_level']} (P={risk_info['probability']:.3f})")
        
        # System statistics
        stats = engine.get_system_statistics()
        print(f"\n=== System Stats ===")
        print(f"Total Users: {stats['total_users']}")
        print(f"Total Measurements: {stats['total_biomarker_measurements']}")
        print(f"Monitoring Active: {stats['monitoring_active']}")
    
    # Run demo
    import asyncio
    asyncio.run(predictive_analytics_demo())