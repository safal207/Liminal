"""
🧭🧠 Retrosplenial Gateway Core — Brain's Navigation Hub

Core implementation of the brain-inspired navigation system based on neuroscience 
research about the retrosplenial complex (RSC) and superior parietal lobule.

Key Components:
- RetrosplenialGateway: Main navigation coordinator
- DirectionEncoder: Maps events to semantic directions  
- MemoryCompass: Links spatial and temporal memory
- TransitionNavigator: Guides liminal state transitions

Research Foundation:
University of Pennsylvania found that RSC + superior parietal lobule maintain
directional orientation independent of visual environment changes.
"""

import asyncio
import math
import time
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple, Union
from dataclasses import dataclass, asdict
from enum import Enum
from collections import defaultdict, deque

from .directions import SemanticDirection, DirectionalSpace, NavigationContext
from .stability import ContextStabilizer, TransitionBalance
from .theta_engine import ThetaOscillationEngine, ThetaState
from .gamma_engine import GammaSynchronyMemoryCompass, GammaState


@dataclass
class NavigationEvent:
    """Event that needs directional encoding."""
    event_id: str
    event_type: str
    content: Any
    timestamp: datetime
    source_layer: str  # perception, memory, scripts, etc.
    emotional_valence: Optional[float] = None
    urgency_level: Optional[float] = None
    context_metadata: Dict[str, Any] = None


@dataclass
class DirectionalVector:
    """Encoded direction with strength and confidence."""
    primary_direction: SemanticDirection
    secondary_direction: Optional[SemanticDirection] = None
    strength: float = 1.0  # 0.0 to 1.0
    confidence: float = 1.0  # 0.0 to 1.0
    stability: float = 1.0  # How stable this direction is over time
    memory_anchor: Optional[str] = None  # Link to memory system


class DirectionEncoder:
    """
    Maps events to semantic directions like the brain's directional cells.
    
    Semantic Directions:
    - North (EVOLVE): Growth, learning, transcendence, development
    - South (INSTINCT): Survival, basic needs, safety, regression  
    - East (CREATE): Innovation, expression, manifestation, action
    - West (REFLECT): Introspection, analysis, understanding, contemplation
    """
    
    def __init__(self):
        self.direction_patterns = self._initialize_direction_patterns()
        self.encoding_history: deque = deque(maxlen=1000)
        self.direction_frequencies = defaultdict(int)
        
        # Learning parameters
        self.adaptation_rate = 0.1
        self.pattern_threshold = 0.6
        
        print("Direction Encoder initialized with semantic compass")
    
    def _initialize_direction_patterns(self) -> Dict[str, Dict]:
        """Initialize patterns that map to each semantic direction."""
        return {
            SemanticDirection.NORTH.value: {
                "keywords": [
                    "growth", "evolve", "learn", "develop", "transcend", "improve",
                    "advance", "progress", "upgrade", "enhance", "expand", "rise"
                ],
                "emotional_range": (0.3, 1.0),  # Positive emotions
                "urgency_range": (0.0, 0.7),   # Not urgent, long-term
                "event_types": ["learning", "development", "insight", "breakthrough"]
            },
            
            SemanticDirection.SOUTH.value: {
                "keywords": [
                    "survive", "protect", "defend", "basic", "need", "safety",
                    "retreat", "conserve", "maintain", "preserve", "secure", "guard"
                ],
                "emotional_range": (-1.0, 0.2),  # Negative to neutral
                "urgency_range": (0.7, 1.0),     # High urgency
                "event_types": ["threat", "need", "problem", "crisis", "maintenance"]
            },
            
            SemanticDirection.EAST.value: {
                "keywords": [
                    "create", "build", "make", "express", "manifest", "produce",
                    "generate", "craft", "design", "implement", "execute", "act"
                ],
                "emotional_range": (0.0, 1.0),   # Neutral to very positive
                "urgency_range": (0.3, 0.8),     # Moderate urgency
                "event_types": ["creation", "implementation", "expression", "action"]
            },
            
            SemanticDirection.WEST.value: {
                "keywords": [
                    "reflect", "think", "analyze", "understand", "contemplate", "ponder",
                    "introspect", "examine", "study", "consider", "review", "meditate"
                ],
                "emotional_range": (-0.5, 0.5),  # Neutral range
                "urgency_range": (0.0, 0.4),     # Low urgency
                "event_types": ["analysis", "reflection", "study", "contemplation"]
            }
        }
    
    def encode_direction(self, event: NavigationEvent) -> DirectionalVector:
        """
        Encode an event into a directional vector.
        
        Like the brain's retrosplenial complex, this maintains consistent
        directional encoding regardless of environmental context changes.
        """
        try:
            # Extract features for direction encoding
            content_text = str(event.content).lower()
            event_type = event.event_type.lower()
            valence = event.emotional_valence or 0.0
            urgency = event.urgency_level or 0.5
            
            # Calculate direction scores for each semantic direction
            direction_scores = {}
            
            for direction, patterns in self.direction_patterns.items():
                score = 0.0
                
                # Keyword matching (semantic content analysis)
                keyword_matches = sum(1 for keyword in patterns["keywords"] 
                                    if keyword in content_text)
                if patterns["keywords"]:
                    score += (keyword_matches / len(patterns["keywords"])) * 0.4
                
                # Event type matching
                if event_type in patterns["event_types"]:
                    score += 0.3
                
                # Emotional valence alignment
                val_min, val_max = patterns["emotional_range"]
                if val_min <= valence <= val_max:
                    # Closer to center of range = higher score
                    val_center = (val_min + val_max) / 2
                    val_alignment = 1.0 - abs(valence - val_center) / max(abs(val_max - val_center), 0.1)
                    score += val_alignment * 0.2
                
                # Urgency alignment
                urg_min, urg_max = patterns["urgency_range"]
                if urg_min <= urgency <= urg_max:
                    urg_center = (urg_min + urg_max) / 2
                    urg_alignment = 1.0 - abs(urgency - urg_center) / max(abs(urg_max - urg_center), 0.1)
                    score += urg_alignment * 0.1
                
                direction_scores[direction] = score
            
            # Find primary direction (highest score)
            primary_dir_str = max(direction_scores.keys(), key=lambda k: direction_scores[k])
            primary_direction = SemanticDirection(primary_dir_str)
            
            # Find secondary direction (second highest if significant)
            sorted_directions = sorted(direction_scores.items(), key=lambda x: x[1], reverse=True)
            secondary_direction = None
            
            if len(sorted_directions) > 1 and sorted_directions[1][1] > self.pattern_threshold:
                secondary_direction = SemanticDirection(sorted_directions[1][0])
            
            # Calculate strength and confidence
            primary_strength = min(1.0, direction_scores[primary_dir_str])
            confidence = primary_strength
            
            # Stability based on recent encoding history
            stability = self._calculate_stability(primary_direction)
            
            # Create directional vector
            vector = DirectionalVector(
                primary_direction=primary_direction,
                secondary_direction=secondary_direction,
                strength=primary_strength,
                confidence=confidence,
                stability=stability
            )
            
            # Update history and learning
            self.encoding_history.append((event.event_id, vector))
            self.direction_frequencies[primary_direction.value] += 1
            
            print(f"Encoded event '{event.event_type}' -> {primary_direction.value} "
                  f"(strength: {primary_strength:.2f}, confidence: {confidence:.2f})")
            
            return vector
            
        except Exception as e:
            print(f"Direction encoding error: {e}")
            # Fallback to neutral direction
            return DirectionalVector(
                primary_direction=SemanticDirection.NORTH,  # Default to growth
                strength=0.1,
                confidence=0.1,
                stability=0.5
            )
    
    def _calculate_stability(self, direction: SemanticDirection) -> float:
        """Calculate directional stability based on recent history."""
        if len(self.encoding_history) < 5:
            return 0.5  # Neutral stability for insufficient data
        
        # Look at last 10 encodings
        recent_directions = [vector.primary_direction for _, vector in list(self.encoding_history)[-10:]]
        
        # Calculate frequency of this direction
        direction_frequency = sum(1 for d in recent_directions if d == direction)
        stability = direction_frequency / len(recent_directions)
        
        return stability
    
    def get_direction_analytics(self) -> Dict[str, Any]:
        """Get analytics about direction encoding patterns."""
        total_encodings = sum(self.direction_frequencies.values())
        
        return {
            "total_encodings": total_encodings,
            "direction_distribution": {
                direction: count / max(total_encodings, 1)
                for direction, count in self.direction_frequencies.items()
            },
            "recent_stability": sum(
                vector.stability for _, vector in list(self.encoding_history)[-10:]
            ) / min(len(self.encoding_history), 10),
            "encoding_history_size": len(self.encoding_history)
        }


class MemoryCompass:
    """
    Links directional navigation with memory systems.
    
    Like the brain's connection between RSC and hippocampus,
    this creates spatial-temporal anchors for navigation.
    """
    
    def __init__(self):
        self.memory_anchors: Dict[str, Dict] = {}
        self.spatial_memory: Dict[str, DirectionalVector] = {}
        self.temporal_patterns: deque = deque(maxlen=500)
        
        print("Memory Compass initialized - linking navigation with memory")
    
    def create_memory_anchor(self, anchor_id: str, direction_vector: DirectionalVector, 
                           context: NavigationContext) -> bool:
        """Create a memory anchor linking direction with context."""
        try:
            self.memory_anchors[anchor_id] = {
                "direction_vector": direction_vector,
                "context": context,
                "created_at": datetime.now(),
                "access_count": 0,
                "strength": 1.0
            }
            
            # Update spatial memory
            self.spatial_memory[context.current_state] = direction_vector
            
            print(f"Memory anchor created: {anchor_id} -> {direction_vector.primary_direction.value}")
            return True
            
        except Exception as e:
            print(f"Memory anchor creation failed: {e}")
            return False
    
    def retrieve_direction_from_memory(self, context_similarity: float = 0.7) -> Optional[DirectionalVector]:
        """Retrieve direction based on memory of similar contexts."""
        # Implementation would compare current context with stored anchors
        # For now, return the most frequently used direction
        
        if not self.memory_anchors:
            return None
        
        # Find most accessed anchor
        best_anchor = max(self.memory_anchors.values(), 
                         key=lambda a: a["access_count"])
        
        best_anchor["access_count"] += 1
        return best_anchor["direction_vector"]
    
    def update_temporal_patterns(self, direction_vector: DirectionalVector):
        """Update temporal pattern learning."""
        self.temporal_patterns.append({
            "direction": direction_vector.primary_direction,
            "timestamp": datetime.now(),
            "strength": direction_vector.strength
        })


class TransitionNavigator:
    """
    Guides navigation through liminal states and transitions.
    
    Maintains orientation during the "difficult transitions" that are
    the core challenge of liminal experiences.
    """
    
    def __init__(self, stabilizer: ContextStabilizer):
        self.stabilizer = stabilizer
        self.transition_history: deque = deque(maxlen=200)
        self.active_transitions: Dict[str, Dict] = {}
        
        print("Transition Navigator initialized - guiding liminal state navigation")
    
    def navigate_transition(self, from_state: str, to_state: str, 
                          current_direction: DirectionalVector) -> TransitionBalance:
        """Navigate a transition between states while maintaining direction."""
        try:
            transition_id = f"{from_state}->{to_state}"
            
            # Create transition balance
            balance = self.stabilizer.create_balance(
                current_vector=current_direction,
                transition_context={"from": from_state, "to": to_state}
            )
            
            # Track active transition
            self.active_transitions[transition_id] = {
                "balance": balance,
                "started_at": datetime.now(),
                "direction": current_direction,
                "from_state": from_state,
                "to_state": to_state
            }
            
            print(f"Navigating transition: {transition_id} with {current_direction.primary_direction.value} orientation")
            return balance
            
        except Exception as e:
            print(f"Transition navigation error: {e}")
            # Return neutral balance
            return TransitionBalance(
                stability_score=0.5,
                direction_confidence=0.5,
                transition_readiness=0.5
            )
    
    def complete_transition(self, transition_id: str) -> bool:
        """Mark a transition as complete."""
        if transition_id in self.active_transitions:
            transition_data = self.active_transitions.pop(transition_id)
            
            # Add to history
            self.transition_history.append({
                "transition_id": transition_id,
                "completed_at": datetime.now(),
                "duration": (datetime.now() - transition_data["started_at"]).total_seconds(),
                "success": True
            })
            
            print(f"Transition completed: {transition_id}")
            return True
        
        return False


class RetrosplenialGateway:
    """
    Main coordinator of the brain-inspired navigation system.
    
    Like the retrosplenial complex in the brain, this serves as the central
    hub connecting perception, memory, and navigation.
    """
    
    def __init__(self):
        self.direction_encoder = DirectionEncoder()
        self.memory_compass = MemoryCompass()
        self.context_stabilizer = ContextStabilizer()
        self.transition_navigator = TransitionNavigator(self.context_stabilizer)
        
        # 🌊 NEW: Theta Oscillation Engine for brain-accurate timing
        self.theta_engine = ThetaOscillationEngine()
        
        # 🎯 NEW: Gamma Synchrony Memory Compass for enhanced binding
        self.gamma_compass = GammaSynchronyMemoryCompass()
        
        # Navigation state
        self.current_orientation: Optional[DirectionalVector] = None
        self.navigation_context: Optional[NavigationContext] = None
        
        # Performance tracking
        self.events_processed = 0
        self.navigation_accuracy = 0.0
        
        print("Retrosplenial Gateway Layer initialized - Brain's compass with theta oscillations online!")
    
    async def process_navigation_event(self, event: NavigationEvent) -> DirectionalVector:
        """
        Process an event through the complete navigation pipeline with theta-modulated encoding.
        
        This is the main entry point for the brain compass system.
        """
        try:
            # 1. Encode direction from event with theta modulation
            direction_vector = self.direction_encoder.encode_direction(event)
            
            # 🌊 NEW: Apply theta oscillation modulation for brain-accurate encoding
            theta_modulated_strength, theta_state = self.theta_engine.encode_with_theta_rhythm(
                event, direction_vector.strength
            )
            
            # Update direction vector with theta-modulated strength
            direction_vector.strength = theta_modulated_strength
            
            # Log theta state for analysis
            print(f"Theta modulation: {theta_state.theta_type.value} @ {theta_state.frequency:.1f}Hz "
                  f"(power: {theta_state.power:.2f}, phase: {theta_state.phase:.2f})")
            
            # 2. Check memory compass for context
            memory_direction = self.memory_compass.retrieve_direction_from_memory()
            if memory_direction and memory_direction.confidence > direction_vector.confidence:
                direction_vector = memory_direction
                print(f"Using memory compass direction: {direction_vector.primary_direction.value}")
            
            # 3. Apply context stability
            if self.current_orientation:
                stabilized_vector = self.context_stabilizer.stabilize_direction(
                    new_vector=direction_vector,
                    previous_vector=self.current_orientation
                )
                direction_vector = stabilized_vector
            
            # 4. Check for optimal encoding window and wait if beneficial
            is_optimal, confidence = self.theta_engine.get_theta_optimal_encoding_window()
            if not is_optimal and confidence < 0.3:
                # If we're in a very poor encoding window, wait for better timing
                waited = await self.theta_engine.wait_for_optimal_encoding(max_wait=0.5)
                if waited:
                    print("Waited for optimal theta encoding window")
            
            # 5. Update memory compass with theta-gamma synchronized memory anchor
            if self.navigation_context:
                anchor_id = f"event_{event.event_id}_{int(time.time())}"
                
                # 🎯 NEW: Create gamma-synchronized memory anchor with enhanced binding
                context_dict = {
                    "current_state": self.navigation_context.current_state,
                    "target_state": self.navigation_context.target_state,
                    "emotional_context": self.navigation_context.emotional_context,
                    "temporal_context": self.navigation_context.temporal_context,
                    "event_content": event.content
                }
                
                # Create gamma-enhanced memory anchor
                binding_id, gamma_anchor_strength = self.gamma_compass.create_synchronized_memory_anchor(
                    direction_vector, context_dict, binding_scope="integration"
                )
                
                # Check for gamma-theta coupling enhancement
                theta_state = self.theta_engine.get_current_theta_state()
                coupling_enhanced_strength = self.gamma_compass.enhance_binding_with_theta_coupling(
                    theta_state.frequency, theta_state.phase, gamma_anchor_strength
                )
                
                # Create stronger memory anchor during theta peaks
                if self.theta_engine.is_theta_peak():
                    print("Creating enhanced memory anchor during theta peak with gamma synchrony")
                    coupling_enhanced_strength *= 1.15  # Additional 15% boost during optimal timing
                
                # Update direction vector with final enhanced strength
                direction_vector.strength = coupling_enhanced_strength
                
                # Also create traditional memory anchor for compatibility
                self.memory_compass.create_memory_anchor(
                    anchor_id, direction_vector, self.navigation_context
                )
                
                print(f"Gamma-theta enhanced anchor: {gamma_anchor_strength:.3f} -> {coupling_enhanced_strength:.3f}")
            
            # 6. Update state
            self.current_orientation = direction_vector
            self.events_processed += 1
            
            print(f"Navigation processed: {direction_vector.primary_direction.value} "
                  f"(theta-strength: {direction_vector.strength:.2f})")
            
            return direction_vector
            
        except Exception as e:
            print(f"Navigation processing error: {e}")
            # Return safe default
            return DirectionalVector(
                primary_direction=SemanticDirection.NORTH,
                strength=0.3,
                confidence=0.3,
                stability=0.5
            )
    
    def set_navigation_context(self, context: NavigationContext):
        """Set the current navigation context."""
        self.navigation_context = context
        print(f"Navigation context set: {context.current_state}")
    
    def get_current_orientation(self) -> Optional[DirectionalVector]:
        """Get the current directional orientation."""
        return self.current_orientation
    
    def get_navigation_analytics(self) -> Dict[str, Any]:
        """Get comprehensive navigation system analytics."""
        direction_analytics = self.direction_encoder.get_direction_analytics()
        theta_state = self.theta_engine.get_current_theta_state()
        gamma_state = self.gamma_compass.get_current_gamma_state()
        gamma_analytics = self.gamma_compass.get_binding_analytics()
        
        # Calculate gamma-theta coupling
        gamma_theta_coupling = self.gamma_compass.get_gamma_coupling_with_theta(
            theta_state.frequency, theta_state.phase
        )
        
        return {
            "system_status": "operational",
            "events_processed": self.events_processed,
            "current_orientation": {
                "direction": self.current_orientation.primary_direction.value if self.current_orientation else None,
                "strength": self.current_orientation.strength if self.current_orientation else 0.0,
                "confidence": self.current_orientation.confidence if self.current_orientation else 0.0
            } if self.current_orientation else None,
            "direction_encoding": direction_analytics,
            "memory_anchors": len(self.memory_compass.memory_anchors),
            "active_transitions": len(self.transition_navigator.active_transitions),
            "navigation_accuracy": self.navigation_accuracy,
            "theta_oscillations": {
                "current_frequency": theta_state.frequency,
                "current_phase": theta_state.phase,
                "current_power": theta_state.power,
                "theta_type": theta_state.theta_type.value,
                "is_optimal_window": self.theta_engine.get_theta_optimal_encoding_window()[0],
                "next_optimal_in_seconds": self.theta_engine.predict_next_optimal_window()
            },
            "gamma_synchrony": {
                "current_frequency": gamma_state.frequency,
                "gamma_band": gamma_state.band.value,
                "synchrony_strength": gamma_state.synchrony_strength,
                "binding_power": gamma_state.binding_power,
                "active_bindings": len(gamma_state.active_bindings),
                "gamma_theta_coupling": gamma_theta_coupling,
                "binding_analytics": gamma_analytics
            }
        }