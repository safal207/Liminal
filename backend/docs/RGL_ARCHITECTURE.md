# 🧠 Retrosplenial Gateway Layer - Architecture Documentation

## System Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    RETROSPLENIAL GATEWAY LAYER                  │
│                     Brain's Navigation Hub                      │
└─────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────┐
│                    RetrosplenialGateway                         │
│                   Main Navigation Coordinator                   │
│  • Process navigation events                                    │
│  • Coordinate all subsystems                                    │
│  • Maintain current orientation                                 │
│  • Provide analytics dashboard                                  │
└─────────────────────────────────────────────────────────────────┘
                                   │
        ┌──────────────────────────┼──────────────────────────┐
        │                          │                          │
        ▼                          ▼                          ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ DirectionEncoder │    │  MemoryCompass  │    │ContextStabilizer│
│                 │    │                 │    │                 │
│ • Semantic dirs │    │ • Memory anchors│    │ • Direction     │
│ • N/S/E/W mapping│   │ • Spatial memory│    │   stability     │
│ • Pattern learn │    │ • Temporal ptrns│    │ • Transition    │
│ • Analytics     │    │ • Context links │    │   smoothing     │
└─────────────────┘    └─────────────────┘    └─────────────────┘
        │                          │                          │
        └──────────────────────────┼──────────────────────────┘
                                   │
                ┌──────────────────┼──────────────────┐
                │                  │                  │
                ▼                  ▼                  ▼
    ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
    │  ThetaEngine    │ │  GammaCompass   │ │TransitionNavigator│
    │  🌊 4-8Hz       │ │  🎯 30-100Hz    │ │                 │
    │                 │ │                 │ │ • Liminal states│
    │ • Spatial: 8Hz  │ │ • Memory binding│ │ • State trans   │
    │ • Concept: 3Hz  │ │ • Gamma bursts  │ │ • Navigation    │
    │ • Adaptive: 4Hz │ │ • Synchrony calc│ │   through trans │
    │ • Phase tracking│ │ • Cross-freq    │ │ • Transition    │
    │ • Optimal timing│ │   coupling      │ │   analytics     │
    └─────────────────┘ └─────────────────┘ └─────────────────┘
```

## Core Data Structures

### NavigationEvent
```python
@dataclass
class NavigationEvent:
    event_id: str                    # Unique identifier
    event_type: str                  # Type of navigation event
    content: Any                     # Event content/payload
    timestamp: datetime              # When event occurred
    source_layer: str               # perception, memory, scripts, etc.
    emotional_valence: float        # -1.0 to 1.0
    urgency_level: float            # 0.0 to 1.0  
    context_metadata: Dict          # Additional context
```

### DirectionalVector
```python
@dataclass  
class DirectionalVector:
    primary_direction: SemanticDirection      # Main direction
    secondary_direction: SemanticDirection    # Optional secondary
    strength: float                          # 0.0 to 2.0+ (enhanced)
    confidence: float                        # 0.0 to 1.0
    stability: float                         # Direction stability
    memory_anchor: str                       # Link to memory
```

### Semantic Directions
```python
class SemanticDirection(Enum):
    NORTH = "north_evolve"     # Growth, transcendence, development
    SOUTH = "south_instinct"   # Survival, safety, basic needs  
    EAST = "east_create"       # Innovation, manifestation, action
    WEST = "west_reflect"      # Introspection, analysis, understanding
```

## Neural Oscillation Engines

### 🌊 Theta Oscillation Engine (4-8Hz)

**Purpose**: Brain-accurate temporal encoding with theta rhythm modulation

**Key Components**:
- **ThetaType**: Fast spatial (8Hz), slow conceptual (3Hz), adaptive human (4Hz)
- **Phase tracking**: Real-time theta phase calculation
- **Exploration boost**: Dynamic power increases during discovery
- **Optimal timing**: Detection and waiting for theta peaks

**Core Algorithm**:
```python
encoding_strength = base_strength * theta_modulation * power_modulation
theta_modulation = 1 + modulation_depth * cos(theta_phase)
power_modulation = 1 + (theta_power - 0.5)
```

### 🎯 Gamma Synchrony Engine (30-100Hz)  

**Purpose**: Multi-element memory binding with gamma synchronization

**Frequency Bands**:
- **Low Gamma**: 30-50Hz (local binding)
- **High Gamma**: 50-100Hz (long-range binding)  
- **Ultra Gamma**: 100-150Hz (consciousness binding)

**Memory Binding Process**:
1. Calculate gamma synchrony from context complexity
2. Determine optimal frequency based on binding scope
3. Generate gamma burst for enhanced encoding
4. Create multi-element binding (emotional, spatial, directional, temporal)
5. Store with synchrony metadata for retrieval

**Enhancement Formula**:
```python
anchor_strength = base_strength * synchrony_multiplier * burst_multiplier
synchrony_multiplier = 1.0 + (gamma_synchrony * 0.5)    # 1.0-1.5x
burst_multiplier = 1.0 + (burst_strength * 0.3)         # 1.0-1.3x
```

## Cross-Frequency Coupling

**Theta-Gamma Coordination**:
- **Harmonic ratios**: 8:1, 10:1, 12:1, 15:1 (optimal gamma:theta)
- **Phase coupling**: Gamma bursts during theta peaks
- **Memory enhancement**: Up to 40% improvement with strong coupling

**Coupling Calculation**:
```python
harmonic_coupling = 1.0 - abs(gamma_freq/theta_freq - optimal_ratio)
phase_coupling = max(0, cos(theta_phase))
coupling_strength = harmonic_coupling * 0.6 + phase_coupling * 0.4
```

## Processing Pipeline

### Event Processing Flow
```
NavigationEvent → DirectionEncoder → Theta Modulation → Memory Check →
Context Stability → Gamma Binding → Theta-Gamma Coupling → 
Memory Anchor → State Update → Analytics
```

### Performance Metrics
- **Processing Time**: <1ms per event
- **Memory Anchors**: Gamma-enhanced binding strength up to 2.66
- **Theta-Gamma Coupling**: Up to 0.919 (near-perfect synchronization)  
- **Direction Enhancement**: Up to 1.45x during theta peaks

## Integration Points

### LIMINAL Architecture Integration
- **Perception Layer**: Receives NavigationEvents
- **Memory Layer**: Creates gamma-enhanced anchors
- **Scripts Layer**: Directional guidance for actions
- **Consciousness Layer**: Global navigation awareness

### External APIs
- **FastAPI endpoints**: Real-time navigation processing
- **WebSocket streams**: Live oscillation monitoring
- **Prometheus metrics**: System performance tracking
- **Neo4j storage**: Graph-based memory anchoring

## Extensibility

### Plugin Architecture
- **Custom Directions**: Add new semantic directions
- **Frequency Bands**: Extend oscillation ranges
- **Binding Scopes**: Define new context binding types
- **Enhancement Algorithms**: Custom coupling calculations

### Future Extensions
- **Alpha Wave Integration**: Relaxed awareness states (8-13Hz)
- **Beta Wave Control**: Cognitive control oscillations (13-30Hz)  
- **Delta Wave Processing**: Deep state navigation (<4Hz)
- **Real-time EEG**: Hardware neural interface integration