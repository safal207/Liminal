"""
🧭🌐 Semantic Directions — The Brain's Directional Language

Inspired by neuroscience research on directional encoding in the brain,
this module defines the semantic directions that give meaning to navigation.

The brain encodes direction not just as geographic North/South/East/West,
but as meaningful orientations in conceptual space.

Semantic Direction System:
- NORTH (EVOLVE): Growth, transcendence, development, learning
- SOUTH (INSTINCT): Survival, safety, basic needs, protection  
- EAST (CREATE): Innovation, manifestation, expression, action
- WEST (REFLECT): Introspection, understanding, contemplation, analysis

Each direction represents both a spatial and temporal orientation in the
landscape of consciousness and meaning.
"""

from enum import Enum
from dataclasses import dataclass
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime


class SemanticDirection(Enum):
    """
    The four primary semantic directions of consciousness navigation.
    
    Like a compass rose for the mind, these directions provide consistent
    orientation regardless of environmental context changes.
    """
    
    NORTH = "north_evolve"      # Growth, transcendence, development
    SOUTH = "south_instinct"    # Survival, safety, basic needs
    EAST = "east_create"        # Innovation, manifestation, action  
    WEST = "west_reflect"       # Introspection, analysis, understanding
    
    @property
    def description(self) -> str:
        """Get human-readable description of the direction."""
        descriptions = {
            self.NORTH: "Growth and Evolution - Transcendence, learning, development, rising above",
            self.SOUTH: "Instinct and Survival - Safety, basic needs, protection, grounding",
            self.EAST: "Creation and Action - Innovation, manifestation, expression, building",
            self.WEST: "Reflection and Understanding - Introspection, analysis, contemplation, wisdom"
        }
        return descriptions[self]
    
    @property 
    def keywords(self) -> List[str]:
        """Get associated keywords for this direction."""
        keyword_map = {
            self.NORTH: ["evolve", "grow", "transcend", "develop", "learn", "improve", "ascend", "advance"],
            self.SOUTH: ["survive", "protect", "secure", "maintain", "preserve", "defend", "ground", "stabilize"],
            self.EAST: ["create", "build", "manifest", "express", "innovate", "generate", "produce", "actualize"],
            self.WEST: ["reflect", "understand", "analyze", "contemplate", "introspect", "examine", "ponder", "study"]
        }
        return keyword_map[self]
    
    @property
    def complementary_direction(self) -> 'SemanticDirection':
        """Get the complementary (opposite) direction."""
        complements = {
            self.NORTH: self.SOUTH,  # Growth ↔ Survival
            self.SOUTH: self.NORTH,  # Survival ↔ Growth
            self.EAST: self.WEST,    # Create ↔ Reflect
            self.WEST: self.EAST     # Reflect ↔ Create
        }
        return complements[self]
    
    @property
    def adjacent_directions(self) -> Tuple['SemanticDirection', 'SemanticDirection']:
        """Get the two adjacent directions (90 degrees each side)."""
        adjacencies = {
            self.NORTH: (self.WEST, self.EAST),   # West ← North → East
            self.SOUTH: (self.EAST, self.WEST),   # East ← South → West  
            self.EAST: (self.NORTH, self.SOUTH),  # North ← East → South
            self.WEST: (self.SOUTH, self.NORTH)   # South ← West → North
        }
        return adjacencies[self]


@dataclass
class DirectionalSpace:
    """
    Represents a multi-dimensional space with directional orientations.
    
    Like the brain's spatial representation, this maintains orientation
    across different scales and contexts.
    """
    primary_direction: SemanticDirection
    secondary_direction: Optional[SemanticDirection] = None
    
    # Directional strength in each dimension (0.0 to 1.0)
    north_strength: float = 0.0  # Evolution/Growth dimension
    south_strength: float = 0.0  # Instinct/Survival dimension  
    east_strength: float = 0.0   # Creation/Action dimension
    west_strength: float = 0.0   # Reflection/Understanding dimension
    
    # Spatial metadata
    created_at: datetime = None
    context_id: Optional[str] = None
    stability_score: float = 1.0
    
    def __post_init__(self):
        if self.created_at is None:
            self.created_at = datetime.now()
    
    @classmethod
    def from_vector_components(cls, north: float, south: float, east: float, west: float) -> 'DirectionalSpace':
        """Create DirectionalSpace from component strengths."""
        # Normalize components
        total = north + south + east + west
        if total > 0:
            north, south, east, west = north/total, south/total, east/total, west/total
        
        # Find primary direction
        components = {
            SemanticDirection.NORTH: north,
            SemanticDirection.SOUTH: south, 
            SemanticDirection.EAST: east,
            SemanticDirection.WEST: west
        }
        
        primary = max(components.keys(), key=lambda k: components[k])
        
        # Find secondary direction (if significant)
        sorted_components = sorted(components.items(), key=lambda x: x[1], reverse=True)
        secondary = sorted_components[1][0] if sorted_components[1][1] > 0.2 else None
        
        return cls(
            primary_direction=primary,
            secondary_direction=secondary,
            north_strength=north,
            south_strength=south,
            east_strength=east,
            west_strength=west
        )
    
    def get_dominant_axis(self) -> Tuple[SemanticDirection, SemanticDirection]:
        """Get the dominant axis (N-S or E-W)."""
        ns_strength = abs(self.north_strength - self.south_strength)
        ew_strength = abs(self.east_strength - self.west_strength)
        
        if ns_strength > ew_strength:
            return (SemanticDirection.NORTH, SemanticDirection.SOUTH)
        else:
            return (SemanticDirection.EAST, SemanticDirection.WEST)
    
    def calculate_angular_position(self) -> float:
        """Calculate angular position in degrees (0-360)."""
        # Convert directional strengths to angle
        # North = 0°, East = 90°, South = 180°, West = 270°
        
        # Net directional components
        net_north = self.north_strength - self.south_strength  # -1 to +1
        net_east = self.east_strength - self.west_strength     # -1 to +1
        
        # Calculate angle using atan2
        import math
        angle_rad = math.atan2(net_east, net_north)
        angle_deg = math.degrees(angle_rad)
        
        # Normalize to 0-360
        if angle_deg < 0:
            angle_deg += 360
            
        return angle_deg
    
    def distance_to(self, other: 'DirectionalSpace') -> float:
        """Calculate directional distance to another space."""
        # Euclidean distance in 4D directional space
        import math
        
        distance = math.sqrt(
            (self.north_strength - other.north_strength) ** 2 +
            (self.south_strength - other.south_strength) ** 2 +
            (self.east_strength - other.east_strength) ** 2 +
            (self.west_strength - other.west_strength) ** 2
        )
        
        return distance
    
    def blend_with(self, other: 'DirectionalSpace', weight: float = 0.5) -> 'DirectionalSpace':
        """Blend this directional space with another."""
        return DirectionalSpace(
            primary_direction=self.primary_direction,  # Keep primary from self
            secondary_direction=other.primary_direction,  # Secondary from other
            north_strength=self.north_strength * (1 - weight) + other.north_strength * weight,
            south_strength=self.south_strength * (1 - weight) + other.south_strength * weight,
            east_strength=self.east_strength * (1 - weight) + other.east_strength * weight,
            west_strength=self.west_strength * (1 - weight) + other.west_strength * weight,
            stability_score=min(self.stability_score, other.stability_score)
        )


@dataclass 
class NavigationContext:
    """
    Context information for navigation decisions.
    
    Like the brain's contextual processing, this provides the environmental
    and situational information needed for accurate directional encoding.
    """
    current_state: str
    previous_state: Optional[str] = None
    target_state: Optional[str] = None
    
    # Context metadata
    emotional_context: Dict[str, float] = None  # Emotional state information
    temporal_context: Dict[str, Any] = None     # Time-based context
    social_context: Dict[str, Any] = None       # Social interaction context
    environmental_context: Dict[str, Any] = None  # Environmental factors
    
    # Navigation history
    recent_directions: List[SemanticDirection] = None
    transition_history: List[str] = None
    
    # Context quality metrics
    context_clarity: float = 1.0    # How clear is the current context (0-1)
    context_stability: float = 1.0  # How stable is the context (0-1)
    context_relevance: float = 1.0  # How relevant for navigation (0-1)
    
    def __post_init__(self):
        if self.emotional_context is None:
            self.emotional_context = {}
        if self.temporal_context is None:
            self.temporal_context = {}
        if self.social_context is None:
            self.social_context = {}
        if self.environmental_context is None:
            self.environmental_context = {}
        if self.recent_directions is None:
            self.recent_directions = []
        if self.transition_history is None:
            self.transition_history = []
    
    def add_directional_history(self, direction: SemanticDirection):
        """Add a direction to the recent history."""
        self.recent_directions.append(direction)
        if len(self.recent_directions) > 10:  # Keep last 10
            self.recent_directions = self.recent_directions[-10:]
    
    def add_transition_to_history(self, transition: str):
        """Add a state transition to history."""
        self.transition_history.append(transition)
        if len(self.transition_history) > 20:  # Keep last 20
            self.transition_history = self.transition_history[-20:]
    
    def get_directional_tendency(self) -> Optional[SemanticDirection]:
        """Get the most frequent recent direction."""
        if not self.recent_directions:
            return None
        
        from collections import Counter
        direction_counts = Counter(self.recent_directions)
        return direction_counts.most_common(1)[0][0]
    
    def calculate_context_score(self) -> float:
        """Calculate overall context quality score."""
        return (self.context_clarity + self.context_stability + self.context_relevance) / 3
    
    def is_in_transition(self) -> bool:
        """Check if currently in a state transition."""
        return self.target_state is not None and self.target_state != self.current_state
    
    def get_transition_direction(self) -> Optional[str]:
        """Get the current transition direction if in transition."""
        if self.is_in_transition():
            return f"{self.current_state} -> {self.target_state}"
        return None


class DirectionalAnalyzer:
    """
    Analyzes patterns in directional navigation over time.
    
    Like the brain's pattern recognition systems, this identifies
    trends, cycles, and anomalies in directional behavior.
    """
    
    def __init__(self):
        self.analysis_history = []
        self.pattern_cache = {}
    
    def analyze_directional_pattern(self, directions: List[SemanticDirection]) -> Dict[str, Any]:
        """Analyze patterns in a sequence of directions."""
        if not directions:
            return {"pattern_type": "empty", "confidence": 0.0}
        
        from collections import Counter
        
        # Basic statistics
        direction_counts = Counter(directions)
        most_common = direction_counts.most_common()
        
        # Pattern detection
        pattern_analysis = {
            "length": len(directions),
            "unique_directions": len(set(directions)),
            "most_frequent": most_common[0] if most_common else None,
            "distribution": dict(direction_counts),
            "dominant_percentage": most_common[0][1] / len(directions) if most_common else 0.0
        }
        
        # Detect specific patterns
        pattern_type = "random"
        if len(set(directions)) == 1:
            pattern_type = "stable"
        elif self._detect_oscillation(directions):
            pattern_type = "oscillating"
        elif self._detect_progression(directions):
            pattern_type = "progressive"
        elif self._detect_spiral(directions):
            pattern_type = "spiral"
        
        pattern_analysis["pattern_type"] = pattern_type
        pattern_analysis["confidence"] = self._calculate_pattern_confidence(directions, pattern_type)
        
        return pattern_analysis
    
    def _detect_oscillation(self, directions: List[SemanticDirection]) -> bool:
        """Detect back-and-forth oscillation between directions."""
        if len(directions) < 4:
            return False
        
        # Check for alternating pattern
        alternating_count = 0
        for i in range(len(directions) - 1):
            if i > 0 and directions[i] == directions[i-2]:  # Same as 2 positions back
                alternating_count += 1
        
        return alternating_count > len(directions) * 0.4
    
    def _detect_progression(self, directions: List[SemanticDirection]) -> bool:
        """Detect progressive movement through directions."""
        if len(directions) < 3:
            return False
        
        # Check for sequential movement around the compass
        direction_order = [SemanticDirection.NORTH, SemanticDirection.EAST, 
                          SemanticDirection.SOUTH, SemanticDirection.WEST]
        
        sequential_count = 0
        for i in range(len(directions) - 1):
            current_idx = direction_order.index(directions[i])
            next_idx = direction_order.index(directions[i + 1])
            
            # Check if next direction is adjacent in sequence
            if (next_idx - current_idx) % 4 == 1 or (current_idx - next_idx) % 4 == 1:
                sequential_count += 1
        
        return sequential_count > len(directions) * 0.3
    
    def _detect_spiral(self, directions: List[SemanticDirection]) -> bool:
        """Detect spiral pattern through all four directions."""
        if len(directions) < 8:  # Need at least 2 full cycles
            return False
        
        # Check for complete cycles through all 4 directions
        direction_set = set(directions)
        return len(direction_set) == 4 and self._detect_progression(directions)
    
    def _calculate_pattern_confidence(self, directions: List[SemanticDirection], pattern_type: str) -> float:
        """Calculate confidence in detected pattern."""
        if pattern_type == "stable":
            return 1.0 if len(set(directions)) == 1 else 0.0
        elif pattern_type == "random":
            return 0.3  # Low confidence for random patterns
        else:
            # For other patterns, confidence based on pattern strength
            return min(1.0, len(directions) / 10.0)  # Higher confidence with more data