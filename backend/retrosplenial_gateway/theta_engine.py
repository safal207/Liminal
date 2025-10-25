# 🌊 Theta Oscillation Engine
# Based on 2024-2025 Neuroscience Discoveries
# 
# Key Insights:
# - Human theta: 1-4Hz (slower than rodents at 8Hz)
# - Dual theta system: Fast posterior (8Hz) spatial, slow anterior (3Hz) non-spatial
# - Active exploration increases theta power during choice points
# - Theta coordinates spatial navigation and memory formation

import asyncio
import math
import time
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple
from enum import Enum

from .directions import SemanticDirection
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .core import NavigationEvent, DirectionalVector


class ThetaType(Enum):
    """Types of theta oscillations based on brain region and function"""
    FAST_SPATIAL = "fast_spatial"      # 8Hz - posterior hippocampus
    SLOW_CONCEPTUAL = "slow_conceptual"  # 3Hz - anterior hippocampus
    ADAPTIVE_HUMAN = "adaptive_human"    # 1-4Hz - human-specific range


@dataclass
class ThetaState:
    """Current state of theta oscillations"""
    frequency: float
    phase: float  # 0-2π
    amplitude: float  # 0-1
    theta_type: ThetaType
    power: float  # Current theta power (0-1)
    timestamp: float


class ThetaOscillationEngine:
    """
    Integrate theta rhythms into direction encoding for brain-accurate navigation.
    
    Based on latest discoveries:
    - Theta provides temporal structure for memory encoding
    - Peak theta = strong encoding, trough = weak encoding
    - Different frequencies for different content types
    """
    
    def __init__(self):
        self.base_frequency = 4.0  # Human theta baseline
        self.current_state = ThetaState(
            frequency=self.base_frequency,
            phase=0.0,
            amplitude=0.8,
            theta_type=ThetaType.ADAPTIVE_HUMAN,
            power=0.5,
            timestamp=time.time()
        )
        
        # Theta modulation parameters
        self.spatial_theta_freq = 8.0    # Fast theta for spatial events
        self.conceptual_theta_freq = 3.0  # Slow theta for emotional/conceptual
        self.exploration_boost = 0.3     # Power increase during active exploration
        
        # Encoding strength modulation
        self.base_encoding_strength = 1.0
        self.theta_modulation_depth = 0.3  # How much theta affects encoding
        
    def calculate_theta_phase(self, timestamp: Optional[float] = None) -> float:
        """Calculate current theta phase based on timestamp"""
        if timestamp is None:
            timestamp = time.time()
            
        time_diff = timestamp - self.current_state.timestamp
        phase_advance = 2 * math.pi * self.current_state.frequency * time_diff
        
        # Update phase with wrap-around
        new_phase = (self.current_state.phase + phase_advance) % (2 * math.pi)
        return new_phase
    
    def is_theta_peak(self, phase: Optional[float] = None) -> bool:
        """Check if we're at theta peak (optimal encoding time)"""
        if phase is None:
            phase = self.calculate_theta_phase()
        
        # Peak is around 0 and 2π (cosine maximum)
        return abs(math.cos(phase)) > 0.8
    
    def is_theta_trough(self, phase: Optional[float] = None) -> bool:
        """Check if we're at theta trough (minimal encoding time)"""
        if phase is None:
            phase = self.calculate_theta_phase()
        
        # Trough is around π (cosine minimum)
        return abs(math.cos(phase)) < 0.2
    
    def determine_optimal_theta_frequency(self, event: 'NavigationEvent') -> Tuple[float, ThetaType]:
        """
        Determine optimal theta frequency based on event type.
        
        Spatial events -> Fast theta (8Hz)
        Conceptual/emotional events -> Slow theta (3Hz)
        Mixed events -> Adaptive human theta (1-4Hz)
        """
        
        # Analyze event content for spatial vs conceptual
        spatial_indicators = [
            'move', 'navigate', 'direction', 'location', 'path', 'route',
            'coordinate', 'position', 'spatial', 'geography', 'map'
        ]
        
        conceptual_indicators = [
            'think', 'feel', 'emotion', 'idea', 'concept', 'meaning',
            'understand', 'reflect', 'consider', 'contemplate', 'insight'
        ]
        
        context_str = str(event.context_metadata) if event.context_metadata else ""
        content_text = f"{event.content} {context_str}".lower()
        
        spatial_score = sum(1 for indicator in spatial_indicators 
                          if indicator in content_text)
        conceptual_score = sum(1 for indicator in conceptual_indicators 
                             if indicator in content_text)
        
        if spatial_score > conceptual_score and spatial_score > 0:
            return self.spatial_theta_freq, ThetaType.FAST_SPATIAL
        elif conceptual_score > spatial_score and conceptual_score > 0:
            return self.conceptual_theta_freq, ThetaType.SLOW_CONCEPTUAL
        else:
            # Default to adaptive human theta
            return self.base_frequency, ThetaType.ADAPTIVE_HUMAN
    
    def calculate_exploration_theta_boost(self, event: 'NavigationEvent') -> float:
        """
        Calculate theta power boost during active exploration.
        Based on discovery: Active exploration increases theta power at choice points.
        """
        
        exploration_keywords = [
            'explore', 'discover', 'search', 'investigate', 'examine',
            'analyze', 'study', 'research', 'probe', 'inquire',
            'question', 'wonder', 'curious', 'unknown', 'new'
        ]
        
        context_str = str(event.context_metadata) if event.context_metadata else ""
        content_text = f"{event.content} {context_str}".lower()
        exploration_matches = sum(1 for keyword in exploration_keywords 
                                if keyword in content_text)
        
        # Boost theta power proportional to exploration indicators
        if exploration_matches > 0:
            boost = min(exploration_matches * self.exploration_boost, 0.8)
            return boost
        
        return 0.0
    
    def encode_with_theta_rhythm(self, event: 'NavigationEvent', 
                               base_strength: float = 1.0) -> Tuple[float, ThetaState]:
        """
        Modulate direction encoding strength by theta phase and frequency.
        
        Returns:
        - encoding_strength: Modulated encoding strength
        - theta_state: Updated theta state
        """
        
        current_time = time.time()
        
        # Determine optimal theta frequency for this event
        optimal_freq, theta_type = self.determine_optimal_theta_frequency(event)
        
        # Calculate exploration boost
        exploration_boost = self.calculate_exploration_theta_boost(event)
        
        # Update theta state
        self.current_state.frequency = optimal_freq
        self.current_state.theta_type = theta_type
        self.current_state.power = min(0.5 + exploration_boost, 1.0)
        self.current_state.timestamp = current_time
        
        # Calculate current theta phase
        theta_phase = self.calculate_theta_phase(current_time)
        self.current_state.phase = theta_phase
        
        # Modulate encoding strength by theta phase
        # Peak theta (cosine = 1) = strongest encoding
        # Trough theta (cosine = -1) = weakest encoding
        theta_modulation = 1 + self.theta_modulation_depth * math.cos(theta_phase)
        
        # Apply theta power boost
        power_modulation = 1 + (self.current_state.power - 0.5)  # 0.5-1.5 range
        
        # Calculate final encoding strength
        encoding_strength = base_strength * theta_modulation * power_modulation
        
        return encoding_strength, self.current_state
    
    def get_theta_optimal_encoding_window(self) -> Tuple[bool, float]:
        """
        Check if current time window is optimal for encoding.
        
        Returns:
        - is_optimal: True if at theta peak (optimal encoding)
        - confidence: How close to optimal (0-1)
        """
        
        current_phase = self.calculate_theta_phase()
        
        # Calculate how close we are to theta peak
        cosine_value = math.cos(current_phase)
        confidence = max(0, cosine_value)  # 0-1 scale
        
        is_optimal = confidence > 0.8
        
        return is_optimal, confidence
    
    def predict_next_optimal_window(self) -> float:
        """
        Predict when the next optimal encoding window will occur.
        
        Returns:
        - seconds_until_optimal: Time in seconds until next theta peak
        """
        
        current_phase = self.calculate_theta_phase()
        
        # Calculate phase distance to next peak
        # Peaks occur at 0, 2π, 4π, etc.
        if current_phase <= math.pi:
            phase_to_peak = 2 * math.pi - current_phase
        else:
            phase_to_peak = 4 * math.pi - current_phase
        
        # Convert phase difference to time
        time_to_peak = phase_to_peak / (2 * math.pi * self.current_state.frequency)
        
        return time_to_peak
    
    async def wait_for_optimal_encoding(self, max_wait: float = 1.0) -> bool:
        """
        Wait for optimal theta phase for encoding (if reasonable wait time).
        
        Args:
        - max_wait: Maximum time to wait in seconds
        
        Returns:
        - waited: True if we waited, False if optimal time was too far
        """
        
        time_to_optimal = self.predict_next_optimal_window()
        
        if time_to_optimal <= max_wait:
            await asyncio.sleep(time_to_optimal)
            return True
        
        return False
    
    def get_theta_synchronized_directions(self, directions: List['DirectionalVector']) -> List['DirectionalVector']:
        """
        Synchronize multiple directional vectors with theta rhythm.
        Stronger directions get enhanced during theta peaks.
        """
        
        current_phase = self.calculate_theta_phase()
        theta_strength = max(0.1, math.cos(current_phase))  # 0.1-1.0 range
        
        from .core import DirectionalVector  # Import at runtime to avoid circular import
        
        synchronized_directions = []
        
        for direction in directions:
            # Enhance stronger directions more during theta peaks
            enhancement_factor = 1 + (direction.confidence * theta_strength * 0.5)
            
            synchronized_direction = DirectionalVector(
                primary_direction=direction.primary_direction,
                secondary_direction=direction.secondary_direction,
                strength=direction.strength * enhancement_factor,
                confidence=min(1.0, direction.confidence * enhancement_factor),
                stability=direction.stability,
                memory_anchor=direction.memory_anchor
            )
            
            synchronized_directions.append(synchronized_direction)
        
        return synchronized_directions
    
    def get_current_theta_state(self) -> ThetaState:
        """Get current theta state with updated phase"""
        current_phase = self.calculate_theta_phase()
        self.current_state.phase = current_phase
        return self.current_state