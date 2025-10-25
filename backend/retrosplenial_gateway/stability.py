"""
⚖️🧭 Context Stability — Maintaining Direction Through Change

The most challenging aspect of liminal transitions: maintaining orientation 
while everything changes around you.

Like a blade of grass in the wind that always points toward the sun,
this system maintains directional stability through environmental turbulence.

Core Components:
- ContextStabilizer: Maintains directional coherence
- TransitionBalance: Manages stability during state changes  
- OrientationMaintainer: Preserves long-term directional consistency

Inspired by neuroscience research showing the brain maintains directional 
orientation independent of environmental context changes.
"""

import math
import time
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
from collections import deque

from .directions import SemanticDirection, DirectionalSpace, NavigationContext


@dataclass
class TransitionBalance:
    """
    Represents the balance state during a liminal transition.
    
    Like walking a tightrope, this captures the delicate equilibrium
    needed to navigate between states while maintaining direction.
    """
    stability_score: float          # Overall stability (0.0 to 1.0)
    direction_confidence: float     # Confidence in current direction  
    transition_readiness: float     # Readiness to complete transition
    
    # Balance factors
    momentum_factor: float = 0.5    # Forward momentum strength
    anchor_factor: float = 0.5      # Stability from anchored memories
    volatility_factor: float = 0.0  # Environmental volatility impact
    
    # Temporal aspects
    created_at: datetime = None
    last_updated: datetime = None
    duration_seconds: float = 0.0
    
    def __post_init__(self):
        if self.created_at is None:
            self.created_at = datetime.now()
        if self.last_updated is None:
            self.last_updated = self.created_at
    
    def update_balance(self, stability_delta: float, confidence_delta: float):
        """Update balance scores with deltas."""
        self.stability_score = max(0.0, min(1.0, self.stability_score + stability_delta))
        self.direction_confidence = max(0.0, min(1.0, self.direction_confidence + confidence_delta))
        
        # Update temporal tracking
        now = datetime.now()
        self.duration_seconds = (now - self.created_at).total_seconds()
        self.last_updated = now
    
    def is_stable(self, threshold: float = 0.6) -> bool:
        """Check if transition is stable enough to proceed."""
        return (self.stability_score >= threshold and 
                self.direction_confidence >= threshold and
                self.transition_readiness >= threshold)
    
    def get_balance_vector(self) -> Tuple[float, float]:
        """Get balance as a 2D vector (stability, confidence)."""
        return (self.stability_score, self.direction_confidence)


class ContextStabilizer:
    """
    Maintains directional stability through context changes.
    
    Like the brain's retrosplenial complex maintaining spatial orientation
    during environmental changes, this preserves directional coherence.
    """
    
    def __init__(self):
        self.stability_history: deque = deque(maxlen=100)
        self.stabilization_parameters = {
            "momentum_weight": 0.7,      # How much to weight previous direction
            "adaptation_rate": 0.3,      # How quickly to adapt to new direction
            "stability_threshold": 0.5,  # Minimum stability required
            "volatility_damping": 0.8    # Damping factor for volatility
        }
        
        self.stabilization_count = 0
        print("Context Stabilizer initialized - maintaining directional coherence")
    
    def stabilize_direction(self, new_vector: 'DirectionalVector', 
                          previous_vector: 'DirectionalVector') -> 'DirectionalVector':
        """
        Stabilize direction by blending new direction with previous orientation.
        
        This prevents erratic direction changes while allowing gradual adaptation.
        """
        try:
            from .core import DirectionalVector  # Avoid circular import
            
            momentum_weight = self.stabilization_parameters["momentum_weight"]
            adaptation_rate = self.stabilization_parameters["adaptation_rate"]
            
            # Calculate directional similarity
            similarity = self._calculate_directional_similarity(new_vector, previous_vector)
            
            # If directions are very different, reduce adaptation rate
            if similarity < 0.3:
                effective_adaptation_rate = adaptation_rate * 0.5
            else:
                effective_adaptation_rate = adaptation_rate
            
            # Blend strengths
            blended_strength = (previous_vector.strength * momentum_weight + 
                              new_vector.strength * effective_adaptation_rate)
            
            # Blend confidence  
            blended_confidence = (previous_vector.confidence * momentum_weight +
                                new_vector.confidence * effective_adaptation_rate)
            
            # Calculate stability based on consistency
            stability = min(1.0, similarity + (blended_confidence * 0.3))
            
            # Determine final direction
            if similarity > 0.7:
                # High similarity - keep previous direction
                final_direction = previous_vector.primary_direction
            else:
                # Low similarity - transition to new direction gradually
                final_direction = new_vector.primary_direction
            
            # Create stabilized vector
            stabilized_vector = DirectionalVector(
                primary_direction=final_direction,
                secondary_direction=new_vector.secondary_direction,
                strength=blended_strength,
                confidence=blended_confidence,
                stability=stability,
                memory_anchor=new_vector.memory_anchor
            )
            
            # Track stabilization
            self.stability_history.append({
                "timestamp": datetime.now(),
                "original_direction": new_vector.primary_direction,
                "stabilized_direction": final_direction,
                "similarity": similarity,
                "stability_score": stability
            })
            
            self.stabilization_count += 1
            
            print(f"Direction stabilized: {new_vector.primary_direction.value} -> "
                  f"{final_direction.value} (similarity: {similarity:.2f}, stability: {stability:.2f})")
            
            return stabilized_vector
            
        except Exception as e:
            print(f"Direction stabilization error: {e}")
            return new_vector  # Return original if stabilization fails
    
    def _calculate_directional_similarity(self, vector1: 'DirectionalVector', 
                                        vector2: 'DirectionalVector') -> float:
        """Calculate similarity between two directional vectors."""
        try:
            # Same primary direction = high similarity
            if vector1.primary_direction == vector2.primary_direction:
                return 0.9
            
            # Complementary directions = low similarity
            if vector1.primary_direction == vector2.primary_direction.complementary_direction:
                return 0.1
            
            # Adjacent directions = medium similarity  
            if vector2.primary_direction in vector1.primary_direction.adjacent_directions:
                return 0.6
            
            # Different directions = medium-low similarity
            return 0.3
            
        except Exception:
            return 0.5  # Default similarity if calculation fails
    
    def create_balance(self, current_vector: 'DirectionalVector', 
                      transition_context: Dict[str, Any]) -> TransitionBalance:
        """Create a transition balance for maintaining stability."""
        try:
            # Calculate stability based on vector properties
            stability_score = current_vector.stability * 0.7 + current_vector.confidence * 0.3
            
            # Direction confidence from vector
            direction_confidence = current_vector.confidence
            
            # Transition readiness based on context
            transition_readiness = min(1.0, stability_score * 1.2)
            
            # Calculate balance factors
            momentum_factor = current_vector.strength
            anchor_factor = current_vector.stability
            
            # Volatility based on recent stability history
            recent_stability = self._calculate_recent_stability()
            volatility_factor = 1.0 - recent_stability
            
            balance = TransitionBalance(
                stability_score=stability_score,
                direction_confidence=direction_confidence,
                transition_readiness=transition_readiness,
                momentum_factor=momentum_factor,
                anchor_factor=anchor_factor,
                volatility_factor=volatility_factor
            )
            
            print(f"Transition balance created: stability={stability_score:.2f}, "
                  f"confidence={direction_confidence:.2f}, readiness={transition_readiness:.2f}")
            
            return balance
            
        except Exception as e:
            print(f"Balance creation error: {e}")
            # Return neutral balance
            return TransitionBalance(
                stability_score=0.5,
                direction_confidence=0.5,
                transition_readiness=0.5
            )
    
    def _calculate_recent_stability(self) -> float:
        """Calculate stability based on recent history."""
        if len(self.stability_history) < 3:
            return 0.7  # Default stability for insufficient data
        
        # Average stability of recent entries
        recent_entries = list(self.stability_history)[-10:]
        avg_stability = sum(entry["stability_score"] for entry in recent_entries) / len(recent_entries)
        
        return avg_stability
    
    def get_stability_analytics(self) -> Dict[str, Any]:
        """Get analytics about stabilization performance."""
        if not self.stability_history:
            return {"status": "no_data"}
        
        recent_entries = list(self.stability_history)[-20:]
        
        return {
            "total_stabilizations": self.stabilization_count,
            "recent_stability_avg": sum(e["stability_score"] for e in recent_entries) / len(recent_entries),
            "recent_similarity_avg": sum(e["similarity"] for e in recent_entries) / len(recent_entries),
            "stability_trend": self._calculate_stability_trend(recent_entries),
            "parameters": self.stabilization_parameters
        }
    
    def _calculate_stability_trend(self, entries: List[Dict]) -> str:
        """Calculate trend in stability over recent entries."""
        if len(entries) < 5:
            return "insufficient_data"
        
        # Compare first half vs second half
        midpoint = len(entries) // 2
        first_half_avg = sum(e["stability_score"] for e in entries[:midpoint]) / midpoint
        second_half_avg = sum(e["stability_score"] for e in entries[midpoint:]) / (len(entries) - midpoint)
        
        difference = second_half_avg - first_half_avg
        
        if difference > 0.1:
            return "improving"
        elif difference < -0.1:
            return "declining"
        else:
            return "stable"


class OrientationMaintainer:
    """
    Preserves long-term directional consistency across multiple transitions.
    
    Like the brain's persistent sense of direction even during complex
    navigation sequences, this maintains global orientation coherence.
    """
    
    def __init__(self):
        self.orientation_history: deque = deque(maxlen=200)
        self.global_direction_weights = {
            SemanticDirection.NORTH: 0.25,
            SemanticDirection.SOUTH: 0.25,
            SemanticDirection.EAST: 0.25,
            SemanticDirection.WEST: 0.25
        }
        
        self.orientation_drift = 0.0  # Accumulated drift from ideal orientation
        self.correction_threshold = 0.3  # When to apply drift correction
        
        print("Orientation Maintainer initialized - preserving long-term directional coherence")
    
    def update_global_orientation(self, direction_vector: 'DirectionalVector'):
        """Update the global orientation tracking."""
        try:
            # Add to history
            self.orientation_history.append({
                "timestamp": datetime.now(),
                "direction": direction_vector.primary_direction,
                "strength": direction_vector.strength,
                "confidence": direction_vector.confidence
            })
            
            # Update global direction weights
            self._update_direction_weights(direction_vector)
            
            # Calculate orientation drift
            self.orientation_drift = self._calculate_orientation_drift()
            
            print(f"Global orientation updated: {direction_vector.primary_direction.value}, "
                  f"drift: {self.orientation_drift:.3f}")
            
        except Exception as e:
            print(f"Orientation update error: {e}")
    
    def _update_direction_weights(self, direction_vector: 'DirectionalVector'):
        """Update global direction weights based on recent usage."""
        try:
            # Increase weight for used direction
            current_weight = self.global_direction_weights[direction_vector.primary_direction]
            strength_factor = direction_vector.strength * 0.1  # Small incremental change
            
            self.global_direction_weights[direction_vector.primary_direction] = min(1.0, current_weight + strength_factor)
            
            # Normalize weights to sum to 1.0
            total_weight = sum(self.global_direction_weights.values())
            if total_weight > 0:
                for direction in self.global_direction_weights:
                    self.global_direction_weights[direction] /= total_weight
            
        except Exception as e:
            print(f"Weight update error: {e}")
    
    def _calculate_orientation_drift(self) -> float:
        """Calculate how much the orientation has drifted from balance."""
        try:
            # Ideal is equal weights (0.25 each)
            ideal_weight = 0.25
            drift = 0.0
            
            for direction, weight in self.global_direction_weights.items():
                drift += abs(weight - ideal_weight)
            
            # Normalize drift to 0-1 range
            return min(1.0, drift / 2.0)  # Max drift is 2.0 when one direction is 1.0
            
        except Exception:
            return 0.0
    
    def should_apply_drift_correction(self) -> bool:
        """Check if drift correction should be applied."""
        return self.orientation_drift > self.correction_threshold
    
    def apply_drift_correction(self) -> Dict[SemanticDirection, float]:
        """Apply correction to reduce orientation drift."""
        try:
            if not self.should_apply_drift_correction():
                return self.global_direction_weights.copy()
            
            # Gradually move weights toward balance (0.25 each)
            correction_factor = 0.1  # How much to correct per application
            ideal_weight = 0.25
            
            corrected_weights = {}
            for direction, current_weight in self.global_direction_weights.items():
                # Move toward ideal weight
                correction = (ideal_weight - current_weight) * correction_factor
                corrected_weights[direction] = current_weight + correction
            
            # Normalize
            total = sum(corrected_weights.values())
            if total > 0:
                for direction in corrected_weights:
                    corrected_weights[direction] /= total
            
            self.global_direction_weights = corrected_weights
            self.orientation_drift = self._calculate_orientation_drift()
            
            print(f"Drift correction applied, new drift: {self.orientation_drift:.3f}")
            
            return corrected_weights
            
        except Exception as e:
            print(f"Drift correction error: {e}")
            return self.global_direction_weights.copy()
    
    def get_dominant_direction(self) -> SemanticDirection:
        """Get the currently dominant global direction."""
        return max(self.global_direction_weights.keys(), 
                  key=lambda k: self.global_direction_weights[k])
    
    def get_orientation_analytics(self) -> Dict[str, Any]:
        """Get comprehensive orientation analytics."""
        recent_directions = [entry["direction"] for entry in list(self.orientation_history)[-20:]]
        
        from collections import Counter
        direction_counts = Counter(recent_directions)
        
        return {
            "global_direction_weights": self.global_direction_weights.copy(),
            "orientation_drift": self.orientation_drift,
            "drift_correction_needed": self.should_apply_drift_correction(),
            "dominant_direction": self.get_dominant_direction().value,
            "recent_direction_distribution": dict(direction_counts),
            "orientation_history_length": len(self.orientation_history),
            "recent_average_confidence": sum(
                entry["confidence"] for entry in list(self.orientation_history)[-10:]
            ) / min(len(self.orientation_history), 10) if self.orientation_history else 0.0
        }