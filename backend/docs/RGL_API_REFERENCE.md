# 📚 RGL API Reference

## Core Classes

### RetrosplenialGateway

Main coordinator of the brain-inspired navigation system.

```python
class RetrosplenialGateway:
    """
    Main coordinator of the brain-inspired navigation system.
    Like the retrosplenial complex in the brain, serves as central hub.
    """
    
    def __init__(self):
        # Initializes all subsystems
    
    async def process_navigation_event(self, event: NavigationEvent) -> DirectionalVector:
        """
        Process event through complete navigation pipeline with theta-gamma modulation.
        
        Args:
            event: NavigationEvent to process
            
        Returns:
            DirectionalVector: Enhanced directional encoding
        """
    
    def set_navigation_context(self, context: NavigationContext):
        """Set current navigation context for processing."""
    
    def get_current_orientation(self) -> Optional[DirectionalVector]:
        """Get current directional orientation."""
    
    def get_navigation_analytics(self) -> Dict[str, Any]:
        """Get comprehensive navigation system analytics."""
```

**Usage Example**:
```python
gateway = RetrosplenialGateway()

# Set context
context = NavigationContext(
    current_state="exploring_possibilities", 
    target_state="creative_breakthrough",
    emotional_context={"curiosity": 0.8, "focus": 0.9}
)
gateway.set_navigation_context(context)

# Process event
event = NavigationEvent(
    event_id="creative_001",
    event_type="creative_ideation", 
    content="Generate innovative solutions for complex problem",
    timestamp=datetime.now(),
    source_layer="consciousness",
    emotional_valence=0.7,
    urgency_level=0.4
)

direction = await gateway.process_navigation_event(event)
print(f"Direction: {direction.primary_direction.value}")
print(f"Strength: {direction.strength:.3f}")
```

---

### ThetaOscillationEngine

Integrates theta rhythms (4-8Hz) for brain-accurate direction encoding.

```python
class ThetaOscillationEngine:
    """Theta oscillation integration for brain-accurate navigation timing."""
    
    def encode_with_theta_rhythm(self, event: NavigationEvent, 
                               base_strength: float = 1.0) -> Tuple[float, ThetaState]:
        """
        Modulate direction encoding strength by theta phase and frequency.
        
        Returns:
        - encoding_strength: Theta-modulated encoding strength  
        - theta_state: Current theta oscillation state
        """
    
    def get_theta_optimal_encoding_window(self) -> Tuple[bool, float]:
        """
        Check if current time is optimal for encoding.
        
        Returns:
        - is_optimal: True if at theta peak
        - confidence: Closeness to optimal (0-1)
        """
    
    async def wait_for_optimal_encoding(self, max_wait: float = 1.0) -> bool:
        """Wait for optimal theta phase if reasonable wait time."""
    
    def get_theta_synchronized_directions(self, directions: List[DirectionalVector]) -> List[DirectionalVector]:
        """Synchronize multiple directions with theta rhythm."""
```

**Theta States**:
```python
class ThetaType(Enum):
    FAST_SPATIAL = "fast_spatial"      # 8Hz - spatial navigation
    SLOW_CONCEPTUAL = "slow_conceptual" # 3Hz - emotional/conceptual  
    ADAPTIVE_HUMAN = "adaptive_human"   # 1-4Hz - human-specific range

@dataclass
class ThetaState:
    frequency: float      # Current theta frequency (Hz)
    phase: float         # Phase 0-2π  
    amplitude: float     # Amplitude 0-1
    theta_type: ThetaType
    power: float         # Current theta power 0-1
    timestamp: float
```

**Usage Example**:
```python
theta_engine = ThetaOscillationEngine()

# Check optimal timing
is_optimal, confidence = theta_engine.get_theta_optimal_encoding_window()
if not is_optimal and confidence < 0.3:
    waited = await theta_engine.wait_for_optimal_encoding()

# Encode with theta modulation  
strength, theta_state = theta_engine.encode_with_theta_rhythm(event, base_strength=0.8)
print(f"Theta type: {theta_state.theta_type.value}")
print(f"Frequency: {theta_state.frequency:.1f}Hz") 
print(f"Enhanced strength: {strength:.3f}")
```

---

### GammaSynchronyMemoryCompass

Gamma synchrony (30-100Hz) for enhanced memory binding.

```python
class GammaSynchronyMemoryCompass:
    """Gamma synchrony for stronger memory anchoring and information binding."""
    
    def create_synchronized_memory_anchor(self, direction_vector: DirectionalVector,
                                        context: Dict[str, Any], 
                                        binding_scope: str = "local") -> Tuple[str, float]:
        """
        Create gamma-synchronized memory anchor.
        
        Args:
            direction_vector: Direction to anchor
            context: Context for binding
            binding_scope: "local", "cross_modal", "consciousness"
            
        Returns:
            binding_id: Unique binding identifier
            anchor_strength: Final anchor strength (can exceed 2.0)
        """
    
    def retrieve_gamma_bound_memory(self, query_context: Dict[str, Any],
                                  similarity_threshold: float = 0.7) -> Optional[MemoryBinding]:
        """Retrieve memory based on gamma synchrony similarity."""
    
    def get_gamma_coupling_with_theta(self, theta_frequency: float, 
                                    theta_phase: float) -> float:
        """Calculate gamma-theta coupling strength."""
    
    def enhance_binding_with_theta_coupling(self, theta_frequency: float,
                                          theta_phase: float, 
                                          base_strength: float) -> float:
        """Enhance memory binding using gamma-theta coupling."""
```

**Gamma Frequency Bands**:
```python
class GammaFrequencyBand(Enum):
    LOW_GAMMA = "low_gamma"        # 30-50Hz - local binding
    HIGH_GAMMA = "high_gamma"      # 50-100Hz - long-range binding  
    ULTRA_GAMMA = "ultra_gamma"    # 100-150Hz - consciousness binding

@dataclass
class MemoryBinding:
    binding_id: str
    elements: List[Dict[str, Any]]  # Bound context elements
    synchrony_strength: float       # Gamma synchrony (0-1)
    gamma_burst_strength: float     # Burst strength (0-1)  
    anchor_strength: float          # Final binding strength
    binding_timestamp: float
```

**Usage Example**:
```python
gamma_compass = GammaSynchronyMemoryCompass()

# Create context for binding
context = {
    "current_state": "creative_flow",
    "emotional_context": {"inspiration": 0.9, "focus": 0.8},
    "spatial_elements": ["workspace", "tools", "materials"],
    "temporal_context": {"flow_duration": 45, "peak_creativity": True}
}

# Create gamma-synchronized anchor
binding_id, anchor_strength = gamma_compass.create_synchronized_memory_anchor(
    direction_vector, context, binding_scope="cross_modal"
)

print(f"Created binding: {binding_id}")
print(f"Anchor strength: {anchor_strength:.3f}")

# Test theta-gamma coupling
coupling = gamma_compass.get_gamma_coupling_with_theta(4.0, math.pi/4)
enhanced_strength = gamma_compass.enhance_binding_with_theta_coupling(
    4.0, math.pi/4, anchor_strength
)
print(f"Coupling strength: {coupling:.3f}")
print(f"Enhanced: {anchor_strength:.3f} -> {enhanced_strength:.3f}")
```

---

## Navigation Context & Events

### NavigationContext

```python
@dataclass
class NavigationContext:
    """Context for navigation decisions."""
    current_state: str                    # Current system state
    target_state: Optional[str] = None    # Target destination state
    
    # Context metadata
    emotional_context: Dict[str, float] = None     # Emotional state values
    temporal_context: Dict[str, Any] = None        # Time-based context
    social_context: Dict[str, Any] = None          # Social factors
    environmental_context: Dict[str, Any] = None   # Environment
    
    # Navigation history  
    recent_directions: List[SemanticDirection] = None
    transition_history: List[str] = None
    
    # Quality metrics
    context_clarity: float = 1.0      # How clear is context (0-1)
    context_stability: float = 1.0    # How stable (0-1)
    context_relevance: float = 1.0    # How relevant for navigation (0-1)
```

**Methods**:
```python
def add_directional_history(self, direction: SemanticDirection)
def add_transition_to_history(self, transition: str)  
def get_directional_tendency(self) -> Optional[SemanticDirection]
def calculate_context_score(self) -> float
def is_in_transition(self) -> bool
def get_transition_direction(self) -> Optional[str]
```

### Semantic Directions

```python
class SemanticDirection(Enum):
    NORTH = "north_evolve"     # Growth, transcendence, development
    SOUTH = "south_instinct"   # Survival, safety, basic needs
    EAST = "east_create"       # Innovation, manifestation, action
    WEST = "west_reflect"      # Introspection, analysis, understanding
    
    @property
    def description(self) -> str:
        """Human-readable direction description."""
    
    @property  
    def keywords(self) -> List[str]:
        """Associated keywords for this direction."""
    
    @property
    def complementary_direction(self) -> 'SemanticDirection':
        """Get opposite direction."""
    
    @property
    def adjacent_directions(self) -> Tuple['SemanticDirection', 'SemanticDirection']:
        """Get adjacent directions (90 degrees each side)."""
```

---

## Analytics & Monitoring

### Navigation Analytics

```python
analytics = gateway.get_navigation_analytics()

# System status
print(f"Status: {analytics['system_status']}")
print(f"Events processed: {analytics['events_processed']}")

# Current orientation
if analytics['current_orientation']:
    print(f"Direction: {analytics['current_orientation']['direction']}")
    print(f"Strength: {analytics['current_orientation']['strength']:.3f}")

# Theta oscillations
theta = analytics['theta_oscillations']
print(f"Theta: {theta['theta_type']} @ {theta['current_frequency']:.1f}Hz")
print(f"Optimal window: {theta['is_optimal_window']}")
print(f"Next optimal in: {theta['next_optimal_in_seconds']:.2f}s")

# Gamma synchrony  
gamma = analytics['gamma_synchrony']
print(f"Gamma: {gamma['gamma_band']} @ {gamma['current_frequency']:.1f}Hz")
print(f"Synchrony: {gamma['synchrony_strength']:.3f}")
print(f"Theta-Gamma coupling: {gamma['gamma_theta_coupling']:.3f}")
print(f"Active bindings: {gamma['active_bindings']}")
```

### Performance Metrics

Key performance indicators:

- **Processing Time**: <1ms typical, <5ms max
- **Direction Strength**: 0.1-2.66 range (2.0+ is exceptional)
- **Confidence**: 0.0-1.0 (>0.7 is high confidence)  
- **Theta Power**: 0.5-1.0 (1.0 during exploration)
- **Gamma Synchrony**: 0.1-1.0 (>0.7 is strong synchrony)
- **Theta-Gamma Coupling**: 0.0-1.0 (>0.7 is optimal)

---

## Error Handling

### Common Exceptions

```python
# Navigation processing errors
try:
    direction = await gateway.process_navigation_event(event)
except NavigationProcessingError as e:
    print(f"Navigation error: {e}")
    # System returns safe default direction

# Memory binding errors  
try:
    binding_id, strength = gamma_compass.create_synchronized_memory_anchor(...)
except MemoryBindingError as e:
    print(f"Binding error: {e}")
    # Fallback to standard memory anchor

# Theta synchronization errors
try:
    directions = theta_engine.get_theta_synchronized_directions(directions)
except ThetaSynchronizationError as e:
    print(f"Theta sync error: {e}")
    # Returns original directions unchanged
```

### Safe Defaults

System designed for graceful degradation:
- **Failed direction encoding**: Returns NORTH (growth) direction
- **Missing context**: Uses reasonable defaults
- **Oscillation errors**: Falls back to standard processing
- **Memory failures**: Creates basic anchors without enhancement

---

## Integration Examples

### FastAPI Integration

```python
from fastapi import FastAPI
from retrosplenial_gateway import RetrosplenialGateway, NavigationEvent

app = FastAPI()
gateway = RetrosplenialGateway()

@app.post("/navigate")
async def navigate_event(event_data: dict):
    event = NavigationEvent(**event_data)
    direction = await gateway.process_navigation_event(event)
    
    return {
        "direction": direction.primary_direction.value,
        "strength": direction.strength,
        "confidence": direction.confidence,
        "analytics": gateway.get_navigation_analytics()
    }

@app.get("/analytics")
async def get_analytics():
    return gateway.get_navigation_analytics()
```

### WebSocket Streaming

```python
import asyncio
from websockets import WebSocketServerProtocol

async def stream_navigation_analytics(websocket: WebSocketServerProtocol):
    while True:
        analytics = gateway.get_navigation_analytics()
        await websocket.send(json.dumps(analytics))
        await asyncio.sleep(0.1)  # 10Hz updates
```

### LIMINAL Integration

```python
# In LIMINAL architecture
from retrosplenial_gateway import RetrosplenialGateway

class LiminalSystem:
    def __init__(self):
        self.navigation = RetrosplenialGateway()
        # Set LIMINAL-specific context
        context = NavigationContext(
            current_state="liminal_processing",
            emotional_context={"uncertainty": 0.6, "openness": 0.8}
        )
        self.navigation.set_navigation_context(context)
    
    async def process_liminal_transition(self, transition_data):
        event = NavigationEvent(
            event_id=f"transition_{int(time.time())}",
            event_type="liminal_transition",
            content=transition_data,
            timestamp=datetime.now(),
            source_layer="liminal_processor"
        )
        
        direction = await self.navigation.process_navigation_event(event)
        return self.apply_directional_guidance(direction)
```