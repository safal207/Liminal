#!/usr/bin/env python3
"""
🌙 Delta Wave Engine (0.5-4Hz) - Deep Recovery & Unconscious Integration

Delta waves are associated with:
- Deep sleep and unconscious processing
- Memory consolidation and integration
- Healing and regenerative processes
- Unconscious problem-solving and insight formation
- Emotional processing and trauma integration

Based on neuroscience research on delta rhythms and their role in
deep recovery, memory consolidation, and unconscious information processing.
"""

import time
import math
import random
from dataclasses import dataclass
from typing import Dict, List, Optional, Any, Tuple
from enum import Enum

class DeltaState(Enum):
    """Delta wave states for different deep processing modes"""
    DEEP_RECOVERY = "deep_recovery"              # 0.5-1Hz - Deepest regeneration
    MEMORY_CONSOLIDATION = "memory_consolidation" # 1-2Hz - Memory integration
    UNCONSCIOUS_PROCESSING = "unconscious_processing" # 2-3Hz - Unconscious work
    HEALING_INTEGRATION = "healing_integration"   # 3-4Hz - Emotional healing

@dataclass
class DeltaOscillation:
    """Delta wave oscillation state"""
    delta_state: DeltaState
    frequency: float  # Hz
    amplitude: float  # Relative power
    phase: float     # Current phase (0-2π)
    consolidation_depth: float # Memory consolidation strength
    healing_power: float # Regenerative healing strength
    integration_level: float # Unconscious integration level

class DeltaWaveEngine:
    """
    Delta Wave Engine for Deep Recovery & Unconscious Integration
    
    Generates 0.5-4Hz delta oscillations that facilitate:
    - Deep restorative processes and healing
    - Memory consolidation and long-term storage
    - Unconscious problem-solving and insight formation
    - Emotional integration and trauma processing
    """
    
    def __init__(self):
        self.frequency_range = (0.5, 4.0)  # Hz
        self.current_oscillation = self._initialize_delta_state()
        self.consolidation_events: List[Dict] = []
        self.healing_episodes: List[float] = []
        self.integration_cycles: List[Dict] = []
        
        print("Delta Wave Engine initialized - Deep recovery system online!")
    
    def _initialize_delta_state(self) -> DeltaOscillation:
        """Initialize delta wave state"""
        return DeltaOscillation(
            delta_state=DeltaState.DEEP_RECOVERY,
            frequency=1.0,  # Default deep recovery
            amplitude=0.8,  # Delta waves typically have high amplitude
            phase=0.0,
            consolidation_depth=0.6,
            healing_power=0.7,
            integration_level=0.5
        )
    
    def select_delta_state(self, content: str, fatigue_level: float,
                          emotional_intensity: float) -> DeltaState:
        """Select appropriate delta state based on recovery needs"""
        
        # Keywords for different delta states
        recovery_keywords = [
            "tired", "exhausted", "rest", "sleep", "recover", "restore",
            "fatigue", "worn", "drained", "depleted", "weary", "regenerate"
        ]
        
        memory_keywords = [
            "remember", "memory", "recall", "consolidate", "integrate", "learn",
            "store", "retain", "encode", "process", "absorb", "internalize"
        ]
        
        unconscious_keywords = [
            "unconscious", "subconscious", "intuition", "insight", "revelation",
            "realization", "understanding", "wisdom", "deep", "profound"
        ]
        
        healing_keywords = [
            "heal", "trauma", "emotional", "pain", "hurt", "grief", "loss",
            "therapy", "recovery", "integration", "processing", "release"
        ]
        
        content_lower = content.lower()
        
        # Calculate state scores
        recovery_score = (
            sum(1 for keyword in recovery_keywords if keyword in content_lower) +
            fatigue_level * 3
        )
        
        memory_score = sum(1 for keyword in memory_keywords if keyword in content_lower)
        
        unconscious_score = sum(1 for keyword in unconscious_keywords if keyword in content_lower)
        
        healing_score = (
            sum(1 for keyword in healing_keywords if keyword in content_lower) +
            emotional_intensity * 2 if emotional_intensity > 0.7 else 0
        )
        
        # Select state based on highest score
        scores = {
            DeltaState.DEEP_RECOVERY: recovery_score + 1,  # Baseline preference
            DeltaState.MEMORY_CONSOLIDATION: memory_score,
            DeltaState.UNCONSCIOUS_PROCESSING: unconscious_score,
            DeltaState.HEALING_INTEGRATION: healing_score
        }
        
        return max(scores, key=scores.get)
    
    def generate_delta_frequency(self, delta_state: DeltaState,
                                fatigue_level: float,
                                emotional_intensity: float) -> float:
        """Generate frequency based on delta state and physiological context"""
        
        base_frequencies = {
            DeltaState.DEEP_RECOVERY: 0.8,           # Very slow for deep rest
            DeltaState.MEMORY_CONSOLIDATION: 1.5,    # Optimal for consolidation
            DeltaState.UNCONSCIOUS_PROCESSING: 2.5,  # Higher for processing
            DeltaState.HEALING_INTEGRATION: 3.2      # Higher for active healing
        }
        
        base_freq = base_frequencies[delta_state]
        
        # Modulate based on fatigue (more fatigue = lower frequency)
        fatigue_adjustment = -fatigue_level * 0.5
        
        # Emotional intensity can increase frequency slightly
        emotion_adjustment = emotional_intensity * 0.3 if emotional_intensity > 0.5 else 0
        
        # Add natural variability
        variability = random.uniform(-0.1, 0.1)
        
        final_frequency = base_freq + fatigue_adjustment + emotion_adjustment + variability
        
        # Ensure within delta range
        return max(0.5, min(4.0, final_frequency))
    
    def calculate_consolidation_depth(self, delta_state: DeltaState,
                                    amplitude: float, frequency: float) -> float:
        """Calculate memory consolidation depth"""
        
        # Optimal consolidation frequencies (research-based)
        optimal_freq = 1.5
        freq_factor = 1.0 - abs(frequency - optimal_freq) / 2.0
        
        # State-specific consolidation factors
        state_factor = {
            DeltaState.DEEP_RECOVERY: 0.4,           # Passive consolidation
            DeltaState.MEMORY_CONSOLIDATION: 1.0,    # Maximum consolidation
            DeltaState.UNCONSCIOUS_PROCESSING: 0.7,  # Moderate consolidation
            DeltaState.HEALING_INTEGRATION: 0.5      # Emotional consolidation
        }[delta_state]
        
        # Higher amplitude enhances consolidation
        amplitude_factor = amplitude
        
        consolidation = freq_factor * state_factor * amplitude_factor
        
        return min(1.0, max(0.2, consolidation))
    
    def calculate_healing_power(self, delta_state: DeltaState,
                              amplitude: float, emotional_intensity: float) -> float:
        """Calculate healing and regenerative power"""
        
        # Base healing power for different states
        base_healing = {
            DeltaState.DEEP_RECOVERY: 0.9,           # Maximum physical healing
            DeltaState.MEMORY_CONSOLIDATION: 0.3,    # Minimal healing focus
            DeltaState.UNCONSCIOUS_PROCESSING: 0.5,  # Moderate healing
            DeltaState.HEALING_INTEGRATION: 1.0      # Maximum emotional healing
        }[delta_state]
        
        # Amplitude enhances healing power
        amplitude_factor = amplitude * 0.3
        
        # Emotional intensity can enhance healing (trauma processing)
        emotion_factor = emotional_intensity * 0.2 if emotional_intensity > 0.6 else 0
        
        healing = base_healing + amplitude_factor + emotion_factor
        
        return min(1.0, max(0.2, healing))
    
    def calculate_integration_level(self, delta_state: DeltaState,
                                  frequency: float, amplitude: float) -> float:
        """Calculate unconscious integration level"""
        
        # Integration is enhanced in specific frequency ranges
        if 2.0 <= frequency <= 3.0:
            freq_factor = 1.0
        else:
            freq_factor = 0.7
        
        # State-specific integration factors
        state_factor = {
            DeltaState.DEEP_RECOVERY: 0.3,
            DeltaState.MEMORY_CONSOLIDATION: 0.6,
            DeltaState.UNCONSCIOUS_PROCESSING: 1.0,
            DeltaState.HEALING_INTEGRATION: 0.8
        }[delta_state]
        
        # Amplitude contribution
        amplitude_factor = amplitude * 0.4
        
        integration = freq_factor * state_factor + amplitude_factor
        
        return min(1.0, max(0.1, integration))
    
    def generate_delta_oscillation(self, content: str,
                                 fatigue_level: float,
                                 emotional_intensity: float,
                                 recovery_context: Dict[str, float]) -> DeltaOscillation:
        """Generate delta oscillation for given recovery context"""
        
        # Select delta state
        delta_state = self.select_delta_state(content, fatigue_level, emotional_intensity)
        
        # Generate frequency
        frequency = self.generate_delta_frequency(delta_state, fatigue_level, emotional_intensity)
        
        # Calculate amplitude (delta waves typically have high amplitude)
        rest_quality = recovery_context.get("rest_quality", 0.7)
        relaxation = recovery_context.get("relaxation", 0.6)
        stress = recovery_context.get("stress", 0.0)
        
        # Lower stress enhances delta amplitude
        stress_factor = 1.0 - (stress * 0.3)
        
        base_amplitude = 0.7 + (rest_quality + relaxation) * 0.15
        amplitude = min(1.0, max(0.4, base_amplitude * stress_factor))
        
        # Calculate consolidation depth
        consolidation_depth = self.calculate_consolidation_depth(delta_state, amplitude, frequency)
        
        # Calculate healing power
        healing_power = self.calculate_healing_power(delta_state, amplitude, emotional_intensity)
        
        # Calculate integration level
        integration_level = self.calculate_integration_level(delta_state, frequency, amplitude)
        
        # Update phase (delta waves are very slow)
        time_delta = time.time() - getattr(self, '_last_delta_time', time.time())
        phase_increment = 2 * math.pi * frequency * time_delta
        new_phase = (self.current_oscillation.phase + phase_increment) % (2 * math.pi)
        self._last_delta_time = time.time()
        
        # Create oscillation
        oscillation = DeltaOscillation(
            delta_state=delta_state,
            frequency=frequency,
            amplitude=amplitude,
            phase=new_phase,
            consolidation_depth=consolidation_depth,
            healing_power=healing_power,
            integration_level=integration_level
        )
        
        # Update tracking
        self._update_delta_tracking(oscillation)
        
        return oscillation
    
    def _update_delta_tracking(self, oscillation: DeltaOscillation):
        """Update delta wave tracking and history"""
        
        # Track consolidation events
        if oscillation.consolidation_depth > 0.7:
            self.consolidation_events.append({
                "timestamp": time.time(),
                "depth": oscillation.consolidation_depth,
                "state": oscillation.delta_state.value,
                "frequency": oscillation.frequency
            })
            if len(self.consolidation_events) > 20:
                self.consolidation_events.pop(0)
        
        # Track healing episodes
        if oscillation.healing_power > 0.8:
            self.healing_episodes.append(oscillation.healing_power)
            if len(self.healing_episodes) > 30:
                self.healing_episodes.pop(0)
        
        # Track integration cycles
        if oscillation.integration_level > 0.6:
            self.integration_cycles.append({
                "timestamp": time.time(),
                "level": oscillation.integration_level,
                "state": oscillation.delta_state.value
            })
            if len(self.integration_cycles) > 15:
                self.integration_cycles.pop(0)
        
        self.current_oscillation = oscillation
    
    def get_recovery_enhancement_factor(self) -> float:
        """Calculate recovery enhancement factor based on delta state"""
        
        if not self.current_oscillation:
            return 1.0
        
        # Base enhancement from healing power
        healing_enhancement = 1.0 + (self.current_oscillation.healing_power * 0.6)
        
        # Consolidation enhancement
        consolidation_enhancement = 1.0 + (self.current_oscillation.consolidation_depth * 0.4)
        
        # Integration enhancement
        integration_enhancement = 1.0 + (self.current_oscillation.integration_level * 0.3)
        
        # Combined enhancement
        total_enhancement = (healing_enhancement + consolidation_enhancement + integration_enhancement) / 3
        
        return min(2.0, max(1.0, total_enhancement))
    
    def is_deep_recovery_active(self) -> bool:
        """Check if in deep recovery state"""
        if not self.current_oscillation:
            return False
        
        return (
            self.current_oscillation.delta_state == DeltaState.DEEP_RECOVERY and
            self.current_oscillation.healing_power > 0.7
        )
    
    def is_consolidation_optimal(self) -> bool:
        """Check if memory consolidation is optimal"""
        if not self.current_oscillation:
            return False
        
        return (
            self.current_oscillation.consolidation_depth > 0.8 and
            1.0 <= self.current_oscillation.frequency <= 2.0
        )
    
    def get_delta_analytics(self) -> Dict[str, Any]:
        """Get comprehensive delta wave analytics"""
        
        if not self.current_oscillation:
            return {"status": "not_active"}
        
        # Calculate averages
        avg_consolidation = sum(event["depth"] for event in self.consolidation_events) / len(self.consolidation_events) if self.consolidation_events else 0
        avg_healing = sum(self.healing_episodes) / len(self.healing_episodes) if self.healing_episodes else 0
        recent_integration_cycles = len([cycle for cycle in self.integration_cycles if time.time() - cycle["timestamp"] < 3600])  # Last hour
        
        return {
            "current_state": {
                "delta_state": self.current_oscillation.delta_state.value,
                "frequency": round(self.current_oscillation.frequency, 2),
                "amplitude": round(self.current_oscillation.amplitude, 3),
                "consolidation_depth": round(self.current_oscillation.consolidation_depth, 3),
                "healing_power": round(self.current_oscillation.healing_power, 3),
                "integration_level": round(self.current_oscillation.integration_level, 3)
            },
            "analytics": {
                "recovery_enhancement": round(self.get_recovery_enhancement_factor(), 3),
                "avg_consolidation_depth": round(avg_consolidation, 3),
                "avg_healing_power": round(avg_healing, 3),
                "recent_integration_cycles": recent_integration_cycles,
                "is_deep_recovery": self.is_deep_recovery_active(),
                "is_consolidation_optimal": self.is_consolidation_optimal()
            },
            "optimization": {
                "recovery_potential": round(self.current_oscillation.healing_power, 3),
                "memory_consolidation": round(self.current_oscillation.consolidation_depth, 3),
                "unconscious_integration": round(self.current_oscillation.integration_level, 3)
            }
        }