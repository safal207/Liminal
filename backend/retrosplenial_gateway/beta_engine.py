#!/usr/bin/env python3
"""
⚡ Beta Wave Engine (13-30Hz) - Cognitive Control & Executive Function

Beta waves are associated with:
- Active concentration and focused attention
- Problem-solving and analytical thinking
- Working memory and executive control
- Alert mental states and conscious processing
- Goal-directed behavior and cognitive flexibility

Based on neuroscience research on beta rhythms and their role in
cognitive control, attention regulation, and executive functioning.
"""

import time
import math
import random
from dataclasses import dataclass
from typing import Dict, List, Optional, Any, Tuple
from enum import Enum

class BetaState(Enum):
    """Beta wave states for different cognitive control modes"""
    FOCUSED_ATTENTION = "focused_attention"        # 13-16Hz - Sustained attention
    ACTIVE_PROCESSING = "active_processing"        # 16-20Hz - Active thinking
    HIGH_CONCENTRATION = "high_concentration"      # 20-25Hz - Intense focus
    EXECUTIVE_CONTROL = "executive_control"        # 25-30Hz - Top-down control

@dataclass
class BetaOscillation:
    """Beta wave oscillation state"""
    beta_state: BetaState
    frequency: float  # Hz
    amplitude: float  # Relative power
    phase: float     # Current phase (0-2π)
    synchronization: float # Cross-cortical synchronization
    control_strength: float # Executive control power
    attention_intensity: float # Attention focus level

class BetaWaveEngine:
    """
    Beta Wave Engine for Cognitive Control & Executive Function
    
    Generates 13-30Hz beta oscillations that facilitate:
    - Sustained attention and focused concentration
    - Working memory and cognitive flexibility
    - Problem-solving and analytical reasoning
    - Executive control and goal-directed behavior
    """
    
    def __init__(self):
        self.frequency_range = (13.0, 30.0)  # Hz
        self.current_oscillation = self._initialize_beta_state()
        self.attention_episodes: List[Dict] = []
        self.control_events: List[float] = []
        self.synchronization_history: List[float] = []
        
        print("Beta Wave Engine initialized - Cognitive control system online!")
    
    def _initialize_beta_state(self) -> BetaOscillation:
        """Initialize beta wave state"""
        return BetaOscillation(
            beta_state=BetaState.FOCUSED_ATTENTION,
            frequency=15.0,  # Default focused attention
            amplitude=0.6,
            phase=0.0,
            synchronization=0.6,
            control_strength=0.7,
            attention_intensity=0.6
        )
    
    def select_beta_state(self, content: str, urgency_level: float, 
                         cognitive_demand: float) -> BetaState:
        """Select appropriate beta state based on cognitive demands"""
        
        # Keywords for different beta states
        focus_keywords = [
            "focus", "attention", "concentrate", "listen", "observe", "watch",
            "study", "read", "analyze", "examine", "review", "investigate"
        ]
        
        processing_keywords = [
            "think", "process", "calculate", "reason", "logic", "understand",
            "work", "solve", "figure", "compute", "evaluate", "assess"
        ]
        
        concentration_keywords = [
            "complex", "difficult", "challenging", "intense", "deep", "thorough",
            "detailed", "precise", "accurate", "careful", "meticulous"
        ]
        
        control_keywords = [
            "control", "manage", "organize", "plan", "coordinate", "direct",
            "execute", "implement", "regulate", "monitor", "supervise", "lead"
        ]
        
        content_lower = content.lower()
        
        # Calculate state scores
        focus_score = sum(1 for keyword in focus_keywords if keyword in content_lower)
        processing_score = sum(1 for keyword in processing_keywords if keyword in content_lower)
        concentration_score = sum(1 for keyword in concentration_keywords if keyword in content_lower)
        control_score = sum(1 for keyword in control_keywords if keyword in content_lower)
        
        # Factor in urgency and cognitive demand
        urgency_factor = urgency_level * 2
        demand_factor = cognitive_demand * 3
        
        # Adjust scores
        if urgency_factor > 1.5:
            concentration_score += urgency_factor
            control_score += urgency_factor
        
        if demand_factor > 2.0:
            processing_score += demand_factor
            concentration_score += demand_factor
        
        # Select state based on highest score
        scores = {
            BetaState.FOCUSED_ATTENTION: focus_score + 1,  # Baseline
            BetaState.ACTIVE_PROCESSING: processing_score,
            BetaState.HIGH_CONCENTRATION: concentration_score,
            BetaState.EXECUTIVE_CONTROL: control_score
        }
        
        return max(scores, key=scores.get)
    
    def generate_beta_frequency(self, beta_state: BetaState, 
                              urgency_level: float, 
                              cognitive_demand: float) -> float:
        """Generate frequency based on beta state and cognitive context"""
        
        base_frequencies = {
            BetaState.FOCUSED_ATTENTION: 15.0,      # Low beta
            BetaState.ACTIVE_PROCESSING: 18.0,      # Mid beta
            BetaState.HIGH_CONCENTRATION: 23.0,     # High beta
            BetaState.EXECUTIVE_CONTROL: 28.0       # Very high beta
        }
        
        base_freq = base_frequencies[beta_state]
        
        # Modulate based on urgency and cognitive demand
        urgency_adjustment = urgency_level * 3.0  # Up to 3Hz increase
        demand_adjustment = cognitive_demand * 2.0  # Up to 2Hz increase
        
        # Add some variability
        variability = random.uniform(-0.5, 0.5)
        
        final_frequency = base_freq + urgency_adjustment + demand_adjustment + variability
        
        # Ensure within beta range
        return max(13.0, min(30.0, final_frequency))
    
    def calculate_control_strength(self, beta_state: BetaState, 
                                 frequency: float, urgency_level: float) -> float:
        """Calculate executive control strength"""
        
        # Higher frequencies generally indicate stronger control
        freq_factor = (frequency - 13.0) / 17.0  # Normalize to 0-1
        
        # State-specific control factors
        state_factor = {
            BetaState.FOCUSED_ATTENTION: 0.6,
            BetaState.ACTIVE_PROCESSING: 0.7,
            BetaState.HIGH_CONCENTRATION: 0.9,
            BetaState.EXECUTIVE_CONTROL: 1.0
        }[beta_state]
        
        # Urgency enhances control strength
        urgency_factor = urgency_level * 0.3
        
        control_strength = (freq_factor * state_factor) + urgency_factor
        
        return min(1.0, max(0.3, control_strength))
    
    def calculate_attention_intensity(self, beta_state: BetaState,
                                    amplitude: float, 
                                    synchronization: float) -> float:
        """Calculate attention focus intensity"""
        
        # Base attention levels for different states
        base_attention = {
            BetaState.FOCUSED_ATTENTION: 0.8,
            BetaState.ACTIVE_PROCESSING: 0.7,
            BetaState.HIGH_CONCENTRATION: 0.95,
            BetaState.EXECUTIVE_CONTROL: 0.6  # Distributed attention
        }[beta_state]
        
        # Amplitude and synchronization enhance attention
        amplitude_factor = amplitude * 0.2
        sync_factor = synchronization * 0.1
        
        attention = base_attention + amplitude_factor + sync_factor
        
        return min(1.0, max(0.4, attention))
    
    def generate_beta_oscillation(self, content: str,
                                urgency_level: float,
                                cognitive_demand: float,
                                emotional_context: Dict[str, float]) -> BetaOscillation:
        """Generate beta oscillation for given cognitive context"""
        
        # Select beta state
        beta_state = self.select_beta_state(content, urgency_level, cognitive_demand)
        
        # Generate frequency
        frequency = self.generate_beta_frequency(beta_state, urgency_level, cognitive_demand)
        
        # Calculate amplitude based on focus and determination
        focus = emotional_context.get("focus", 0.5)
        determination = emotional_context.get("determination", 0.5)
        stress = emotional_context.get("stress", 0.0)
        
        # Moderate stress can enhance performance, high stress impairs
        stress_factor = stress if stress < 0.7 else (1.0 - stress)
        
        base_amplitude = 0.5 + (focus + determination) * 0.2 + stress_factor * 0.1
        amplitude = min(1.0, max(0.3, base_amplitude + random.uniform(-0.1, 0.1)))
        
        # Calculate synchronization (cross-cortical coordination)
        attention = emotional_context.get("attention", 0.5)
        concentration = emotional_context.get("concentration", 0.5)
        synchronization = min(1.0, 0.4 + attention * 0.3 + concentration * 0.3)
        
        # Calculate control strength
        control_strength = self.calculate_control_strength(beta_state, frequency, urgency_level)
        
        # Calculate attention intensity
        attention_intensity = self.calculate_attention_intensity(beta_state, amplitude, synchronization)
        
        # Update phase
        time_delta = time.time() - getattr(self, '_last_beta_time', time.time())
        phase_increment = 2 * math.pi * frequency * time_delta
        new_phase = (self.current_oscillation.phase + phase_increment) % (2 * math.pi)
        self._last_beta_time = time.time()
        
        # Create oscillation
        oscillation = BetaOscillation(
            beta_state=beta_state,
            frequency=frequency,
            amplitude=amplitude,
            phase=new_phase,
            synchronization=synchronization,
            control_strength=control_strength,
            attention_intensity=attention_intensity
        )
        
        # Update tracking
        self._update_beta_tracking(oscillation)
        
        return oscillation
    
    def _update_beta_tracking(self, oscillation: BetaOscillation):
        """Update beta wave tracking and history"""
        
        # Track attention episodes
        if oscillation.attention_intensity > 0.8:
            self.attention_episodes.append({
                "timestamp": time.time(),
                "intensity": oscillation.attention_intensity,
                "state": oscillation.beta_state.value,
                "frequency": oscillation.frequency,
                "duration": 1.0  # Will be updated on episode end
            })
            # Keep only recent episodes
            if len(self.attention_episodes) > 30:
                self.attention_episodes.pop(0)
        
        # Track control events
        if oscillation.control_strength > 0.7:
            self.control_events.append(oscillation.control_strength)
            if len(self.control_events) > 50:
                self.control_events.pop(0)
        
        # Track synchronization history
        self.synchronization_history.append(oscillation.synchronization)
        if len(self.synchronization_history) > 100:
            self.synchronization_history.pop(0)
        
        self.current_oscillation = oscillation
    
    def get_cognitive_enhancement_factor(self) -> float:
        """Calculate cognitive enhancement factor based on beta state"""
        
        if not self.current_oscillation:
            return 1.0
        
        # Base enhancement from attention intensity
        attention_enhancement = 1.0 + (self.current_oscillation.attention_intensity * 0.5)
        
        # Control strength enhancement
        control_enhancement = 1.0 + (self.current_oscillation.control_strength * 0.4)
        
        # Synchronization enhancement
        sync_enhancement = 1.0 + (self.current_oscillation.synchronization * 0.3)
        
        # Combined enhancement
        total_enhancement = (attention_enhancement + control_enhancement + sync_enhancement) / 3
        
        return min(2.5, max(1.0, total_enhancement))
    
    def is_high_performance_state(self) -> bool:
        """Check if in high cognitive performance state"""
        if not self.current_oscillation:
            return False
        
        return (
            self.current_oscillation.attention_intensity > 0.8 and
            self.current_oscillation.control_strength > 0.7 and
            self.current_oscillation.synchronization > 0.6
        )
    
    def is_executive_control_active(self) -> bool:
        """Check if executive control is actively engaged"""
        if not self.current_oscillation:
            return False
        
        return (
            self.current_oscillation.beta_state == BetaState.EXECUTIVE_CONTROL and
            self.current_oscillation.control_strength > 0.7
        )
    
    def get_beta_analytics(self) -> Dict[str, Any]:
        """Get comprehensive beta wave analytics"""
        
        if not self.current_oscillation:
            return {"status": "not_active"}
        
        # Calculate averages
        avg_sync = sum(self.synchronization_history) / len(self.synchronization_history) if self.synchronization_history else 0
        avg_control = sum(self.control_events) / len(self.control_events) if self.control_events else 0
        recent_attention_episodes = len([ep for ep in self.attention_episodes if time.time() - ep["timestamp"] < 600])  # Last 10 minutes
        
        return {
            "current_state": {
                "beta_state": self.current_oscillation.beta_state.value,
                "frequency": round(self.current_oscillation.frequency, 2),
                "amplitude": round(self.current_oscillation.amplitude, 3),
                "synchronization": round(self.current_oscillation.synchronization, 3),
                "control_strength": round(self.current_oscillation.control_strength, 3),
                "attention_intensity": round(self.current_oscillation.attention_intensity, 3)
            },
            "analytics": {
                "cognitive_enhancement": round(self.get_cognitive_enhancement_factor(), 3),
                "avg_synchronization": round(avg_sync, 3),
                "avg_control_strength": round(avg_control, 3),
                "recent_attention_episodes": recent_attention_episodes,
                "is_high_performance": self.is_high_performance_state(),
                "is_executive_active": self.is_executive_control_active()
            },
            "optimization": {
                "focus_potential": round(self.current_oscillation.attention_intensity, 3),
                "control_readiness": round(self.current_oscillation.control_strength, 3),
                "cognitive_synchrony": round(self.current_oscillation.synchronization, 3)
            }
        }