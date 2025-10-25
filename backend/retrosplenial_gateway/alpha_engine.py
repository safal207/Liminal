#!/usr/bin/env python3
"""
🌊 Alpha Wave Engine (8-13Hz) - Relaxed Awareness State

Alpha waves are associated with:
- Relaxed wakefulness and calm awareness
- Creative flow states and insight preparation
- Meditative consciousness and mindfulness
- Bridge between conscious and unconscious processing
- Enhanced pattern recognition and intuitive thinking

Based on neuroscience research on alpha rhythms and their role in
cognitive flexibility, creativity, and conscious-unconscious integration.
"""

import time
import math
import random
from dataclasses import dataclass
from typing import Dict, List, Optional, Any, Tuple
from enum import Enum

class AlphaState(Enum):
    """Alpha wave states for different consciousness modes"""
    RELAXED_AWARENESS = "relaxed_awareness"      # 8-10Hz - Calm, alert relaxation
    CREATIVE_FLOW = "creative_flow"              # 10-12Hz - Flow state, creativity
    MEDITATIVE_DEPTH = "meditative_depth"        # 8-9Hz - Deep meditation
    INSIGHT_PREPARATION = "insight_preparation"   # 11-13Hz - Pre-insight state

@dataclass
class AlphaOscillation:
    """Alpha wave oscillation state"""
    alpha_state: AlphaState
    frequency: float  # Hz
    amplitude: float  # Relative power
    phase: float     # Current phase (0-2π)
    coherence: float # Inter-hemispheric coherence
    flow_index: float # Flow state indicator
    awareness_level: float # Conscious awareness level

class AlphaWaveEngine:
    """
    Alpha Wave Engine for Relaxed Awareness Processing
    
    Generates 8-13Hz alpha oscillations that facilitate:
    - Relaxed yet alert states of consciousness
    - Creative flow and insight preparation
    - Meditative awareness and mindfulness
    - Bridge between focused attention and open monitoring
    """
    
    def __init__(self):
        self.frequency_range = (8.0, 13.0)  # Hz
        self.current_oscillation = self._initialize_alpha_state()
        self.coherence_history: List[float] = []
        self.flow_episodes: List[Dict] = []
        self.awareness_peaks: List[float] = []
        
        print("Alpha Wave Engine initialized - Relaxed awareness state online!")
    
    def _initialize_alpha_state(self) -> AlphaOscillation:
        """Initialize alpha wave state"""
        return AlphaOscillation(
            alpha_state=AlphaState.RELAXED_AWARENESS,
            frequency=10.0,  # Default relaxed alpha
            amplitude=0.7,
            phase=0.0,
            coherence=0.6,
            flow_index=0.0,
            awareness_level=0.7
        )
    
    def select_alpha_state(self, content: str, emotional_context: Dict[str, float]) -> AlphaState:
        """Select appropriate alpha state based on content and emotion"""
        
        # Keywords for different alpha states
        creative_keywords = [
            "create", "imagine", "design", "art", "music", "story", "innovation",
            "brainstorm", "ideate", "inspiration", "flow", "creative", "artistic"
        ]
        
        meditative_keywords = [
            "meditate", "mindful", "awareness", "present", "breathing", "calm",
            "peaceful", "serene", "contemplat", "reflect", "inner", "spiritual"
        ]
        
        insight_keywords = [
            "understand", "realize", "insight", "revelation", "clarity", "solution",
            "breakthrough", "epiphany", "comprehend", "grasp", "perceive"
        ]
        
        content_lower = content.lower()
        
        # Check emotional context
        relaxation = emotional_context.get("relaxation", 0.0)
        creativity = emotional_context.get("creativity", 0.0)
        focus = emotional_context.get("focus", 0.0)
        openness = emotional_context.get("openness", 0.0)
        
        # Calculate state scores
        creative_score = (
            sum(1 for keyword in creative_keywords if keyword in content_lower) +
            creativity * 2 + openness
        )
        
        meditative_score = (
            sum(1 for keyword in meditative_keywords if keyword in content_lower) +
            relaxation * 2
        )
        
        insight_score = (
            sum(1 for keyword in insight_keywords if keyword in content_lower) +
            focus + openness
        )
        
        # Select state based on highest score
        if creative_score > max(meditative_score, insight_score):
            return AlphaState.CREATIVE_FLOW
        elif meditative_score > insight_score:
            return AlphaState.MEDITATIVE_DEPTH
        elif insight_score > 1.0:
            return AlphaState.INSIGHT_PREPARATION
        else:
            return AlphaState.RELAXED_AWARENESS
    
    def generate_alpha_frequency(self, alpha_state: AlphaState, 
                                emotional_valence: float) -> float:
        """Generate frequency based on alpha state and emotional context"""
        
        base_frequencies = {
            AlphaState.RELAXED_AWARENESS: 9.0,     # Calm baseline
            AlphaState.CREATIVE_FLOW: 11.0,        # Higher for creativity
            AlphaState.MEDITATIVE_DEPTH: 8.5,      # Lower for deep states
            AlphaState.INSIGHT_PREPARATION: 12.0   # Higher for pre-insight
        }
        
        base_freq = base_frequencies[alpha_state]
        
        # Modulate based on emotional valence
        if emotional_valence > 0.5:  # Positive emotions
            freq_adjustment = random.uniform(0.5, 1.5)
        elif emotional_valence < -0.3:  # Negative emotions
            freq_adjustment = random.uniform(-1.0, -0.3)
        else:  # Neutral
            freq_adjustment = random.uniform(-0.5, 0.5)
        
        final_frequency = base_freq + freq_adjustment
        
        # Ensure within alpha range
        return max(8.0, min(13.0, final_frequency))
    
    def calculate_flow_index(self, alpha_state: AlphaState, 
                           coherence: float, frequency: float) -> float:
        """Calculate flow state index based on alpha characteristics"""
        
        # Optimal flow frequencies
        flow_optimal_freq = 10.5  # Peak flow frequency
        freq_factor = 1.0 - abs(frequency - flow_optimal_freq) / 3.0
        
        # Flow enhancement factors
        state_factor = {
            AlphaState.CREATIVE_FLOW: 1.0,
            AlphaState.RELAXED_AWARENESS: 0.7,
            AlphaState.INSIGHT_PREPARATION: 0.8,
            AlphaState.MEDITATIVE_DEPTH: 0.6
        }[alpha_state]
        
        # Coherence contribution
        coherence_factor = coherence
        
        # Calculate flow index
        flow_index = (freq_factor * state_factor * coherence_factor)
        
        # Add dynamic enhancement
        time_factor = math.sin(time.time() * 0.1) * 0.1 + 0.9
        
        return min(1.0, max(0.0, flow_index * time_factor))
    
    def calculate_awareness_level(self, alpha_state: AlphaState,
                                amplitude: float, coherence: float) -> float:
        """Calculate conscious awareness level"""
        
        # Base awareness levels for different states
        base_awareness = {
            AlphaState.RELAXED_AWARENESS: 0.8,
            AlphaState.CREATIVE_FLOW: 0.6,       # Lower due to flow immersion
            AlphaState.MEDITATIVE_DEPTH: 0.9,    # High mindful awareness
            AlphaState.INSIGHT_PREPARATION: 0.7
        }[alpha_state]
        
        # Modulate by amplitude and coherence
        amplitude_factor = amplitude * 0.3
        coherence_factor = coherence * 0.2
        
        awareness = base_awareness + amplitude_factor + coherence_factor
        
        return min(1.0, max(0.3, awareness))
    
    def generate_alpha_oscillation(self, content: str, 
                                 emotional_valence: float,
                                 emotional_context: Dict[str, float]) -> AlphaOscillation:
        """Generate alpha oscillation for given context"""
        
        # Select alpha state
        alpha_state = self.select_alpha_state(content, emotional_context)
        
        # Generate frequency
        frequency = self.generate_alpha_frequency(alpha_state, emotional_valence)
        
        # Calculate amplitude based on relaxation and openness
        relaxation = emotional_context.get("relaxation", 0.5)
        openness = emotional_context.get("openness", 0.5)
        base_amplitude = 0.6 + (relaxation + openness) * 0.2
        amplitude = min(1.0, max(0.3, base_amplitude + random.uniform(-0.1, 0.1)))
        
        # Calculate coherence (inter-hemispheric synchronization)
        coherence = min(1.0, 0.5 + relaxation * 0.3 + openness * 0.2)
        
        # Calculate flow index
        flow_index = self.calculate_flow_index(alpha_state, coherence, frequency)
        
        # Calculate awareness level
        awareness_level = self.calculate_awareness_level(alpha_state, amplitude, coherence)
        
        # Update phase
        time_delta = time.time() - getattr(self, '_last_alpha_time', time.time())
        phase_increment = 2 * math.pi * frequency * time_delta
        new_phase = (self.current_oscillation.phase + phase_increment) % (2 * math.pi)
        self._last_alpha_time = time.time()
        
        # Create oscillation
        oscillation = AlphaOscillation(
            alpha_state=alpha_state,
            frequency=frequency,
            amplitude=amplitude,
            phase=new_phase,
            coherence=coherence,
            flow_index=flow_index,
            awareness_level=awareness_level
        )
        
        # Update tracking
        self._update_alpha_tracking(oscillation)
        
        return oscillation
    
    def _update_alpha_tracking(self, oscillation: AlphaOscillation):
        """Update alpha wave tracking and history"""
        
        # Update coherence history
        self.coherence_history.append(oscillation.coherence)
        if len(self.coherence_history) > 50:
            self.coherence_history.pop(0)
        
        # Track flow episodes
        if oscillation.flow_index > 0.7:
            self.flow_episodes.append({
                "timestamp": time.time(),
                "flow_index": oscillation.flow_index,
                "state": oscillation.alpha_state.value,
                "frequency": oscillation.frequency
            })
            # Keep only recent episodes
            if len(self.flow_episodes) > 20:
                self.flow_episodes.pop(0)
        
        # Track awareness peaks
        if oscillation.awareness_level > 0.8:
            self.awareness_peaks.append(oscillation.awareness_level)
            if len(self.awareness_peaks) > 30:
                self.awareness_peaks.pop(0)
        
        self.current_oscillation = oscillation
    
    def get_alpha_enhancement_factor(self) -> float:
        """Calculate enhancement factor based on alpha state"""
        
        if not self.current_oscillation:
            return 1.0
        
        # Base enhancement from flow state
        flow_enhancement = 1.0 + (self.current_oscillation.flow_index * 0.4)
        
        # Coherence enhancement
        coherence_enhancement = 1.0 + (self.current_oscillation.coherence * 0.3)
        
        # Awareness enhancement
        awareness_enhancement = 1.0 + (self.current_oscillation.awareness_level * 0.2)
        
        # Combined enhancement
        total_enhancement = (flow_enhancement + coherence_enhancement + awareness_enhancement) / 3
        
        return min(2.0, max(1.0, total_enhancement))
    
    def is_flow_state_active(self) -> bool:
        """Check if currently in flow state"""
        if not self.current_oscillation:
            return False
        
        return (
            self.current_oscillation.flow_index > 0.6 and
            self.current_oscillation.alpha_state == AlphaState.CREATIVE_FLOW
        )
    
    def is_meditative_state_active(self) -> bool:
        """Check if in meditative state"""
        if not self.current_oscillation:
            return False
        
        return (
            self.current_oscillation.awareness_level > 0.8 and
            self.current_oscillation.alpha_state == AlphaState.MEDITATIVE_DEPTH
        )
    
    def get_alpha_analytics(self) -> Dict[str, Any]:
        """Get comprehensive alpha wave analytics"""
        
        if not self.current_oscillation:
            return {"status": "not_active"}
        
        avg_coherence = sum(self.coherence_history) / len(self.coherence_history) if self.coherence_history else 0
        recent_flow_episodes = len([ep for ep in self.flow_episodes if time.time() - ep["timestamp"] < 300])  # Last 5 minutes
        avg_awareness = sum(self.awareness_peaks) / len(self.awareness_peaks) if self.awareness_peaks else 0
        
        return {
            "current_state": {
                "alpha_state": self.current_oscillation.alpha_state.value,
                "frequency": round(self.current_oscillation.frequency, 2),
                "amplitude": round(self.current_oscillation.amplitude, 3),
                "coherence": round(self.current_oscillation.coherence, 3),
                "flow_index": round(self.current_oscillation.flow_index, 3),
                "awareness_level": round(self.current_oscillation.awareness_level, 3)
            },
            "analytics": {
                "enhancement_factor": round(self.get_alpha_enhancement_factor(), 3),
                "avg_coherence": round(avg_coherence, 3),
                "recent_flow_episodes": recent_flow_episodes,
                "avg_awareness_peaks": round(avg_awareness, 3),
                "is_flow_active": self.is_flow_state_active(),
                "is_meditative": self.is_meditative_state_active()
            },
            "optimization": {
                "flow_potential": round(self.current_oscillation.flow_index, 3),
                "creativity_readiness": round(self.current_oscillation.coherence * self.current_oscillation.amplitude, 3),
                "mindfulness_depth": round(self.current_oscillation.awareness_level, 3)
            }
        }