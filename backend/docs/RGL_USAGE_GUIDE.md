# 🚀 RGL Usage Guide

## Quick Start

### Installation

```bash
# Navigate to project directory
cd resonance-liminal/backend

# Install dependencies (if not already done)
pip install -r requirements.txt

# Test the system
python test_theta_navigation.py
```

### Basic Usage

```python
import asyncio
from datetime import datetime
from retrosplenial_gateway import RetrosplenialGateway, NavigationEvent, NavigationContext

async def basic_navigation_example():
    # Initialize the gateway
    gateway = RetrosplenialGateway()
    
    # Set navigation context
    context = NavigationContext(
        current_state="learning_new_skill",
        target_state="mastery_achieved", 
        emotional_context={"curiosity": 0.8, "focus": 0.7}
    )
    gateway.set_navigation_context(context)
    
    # Create navigation event
    event = NavigationEvent(
        event_id="learn_001",
        event_type="learning",
        content="Understanding complex neural oscillations in navigation",
        timestamp=datetime.now(),
        source_layer="consciousness",
        emotional_valence=0.6,
        urgency_level=0.3
    )
    
    # Process the event
    direction = await gateway.process_navigation_event(event)
    
    print(f"Recommended direction: {direction.primary_direction.value}")
    print(f"Strength: {direction.strength:.3f}")
    print(f"Confidence: {direction.confidence:.3f}")
    
    return direction

# Run the example
direction = asyncio.run(basic_navigation_example())
```

---

## Use Cases

### 1. Emotional Navigation

Guide emotional states using brain-accurate navigation:

```python
async def emotional_navigation_example():
    gateway = RetrosplenialGateway()
    
    # Context for emotional processing
    context = NavigationContext(
        current_state="feeling_overwhelmed",
        target_state="calm_and_centered",
        emotional_context={
            "anxiety": 0.8,
            "stress": 0.7, 
            "hope": 0.4
        }
    )
    gateway.set_navigation_context(context)
    
    # Different emotional events
    events = [
        # Crisis event (should suggest SOUTH - survival/grounding)
        NavigationEvent(
            event_id="crisis_001",
            event_type="emotional_crisis",
            content="Feeling completely overwhelmed by multiple stressors",
            timestamp=datetime.now(),
            source_layer="perception",
            emotional_valence=-0.7,
            urgency_level=0.9
        ),
        
        # Reflection event (should suggest WEST - introspection)  
        NavigationEvent(
            event_id="reflect_001",
            event_type="self_reflection",
            content="Need to understand what's causing these feelings",
            timestamp=datetime.now(),
            source_layer="consciousness", 
            emotional_valence=-0.2,
            urgency_level=0.3
        ),
        
        # Growth event (should suggest NORTH - development)
        NavigationEvent(
            event_id="growth_001", 
            event_type="personal_development",
            content="Ready to develop better coping strategies",
            timestamp=datetime.now(),
            source_layer="scripts",
            emotional_valence=0.5,
            urgency_level=0.4
        )
    ]
    
    print("=== EMOTIONAL NAVIGATION GUIDANCE ===")
    for event in events:
        direction = await gateway.process_navigation_event(event)
        
        print(f"\nEvent: {event.event_type}")
        print(f"Direction: {direction.primary_direction.value}")
        print(f"Guidance: {direction.primary_direction.description}")
        print(f"Strength: {direction.strength:.3f}")
        
        await asyncio.sleep(0.2)  # Allow theta phases to evolve
    
    # Get final analytics
    analytics = gateway.get_navigation_analytics()
    print(f"\nFinal orientation: {analytics['current_orientation']['direction']}")
    print(f"Memory anchors created: {analytics['memory_anchors']}")

asyncio.run(emotional_navigation_example())
```

### 2. Creative Process Navigation

Guide creative workflows with neural oscillation accuracy:

```python
async def creative_navigation_example():
    gateway = RetrosplenialGateway()
    
    # Creative context
    context = NavigationContext(
        current_state="creative_exploration", 
        target_state="innovative_breakthrough",
        emotional_context={
            "curiosity": 0.9,
            "playfulness": 0.8,
            "focus": 0.6
        },
        temporal_context={
            "flow_state": True,
            "creative_session_duration": 30
        }
    )
    gateway.set_navigation_context(context)
    
    # Creative process stages
    creative_events = [
        # Exploration phase (EAST - creation)
        NavigationEvent(
            event_id="explore_001",
            event_type="creative_exploration", 
            content="Exploring multiple creative possibilities and generating ideas",
            timestamp=datetime.now(),
            source_layer="consciousness",
            emotional_valence=0.7,
            urgency_level=0.3
        ),
        
        # Reflection phase (WEST - analysis)
        NavigationEvent(
            event_id="analyze_001",
            event_type="creative_analysis",
            content="Analyzing and refining the most promising creative directions", 
            timestamp=datetime.now(),
            source_layer="scripts",
            emotional_valence=0.4,
            urgency_level=0.5
        ),
        
        # Implementation phase (EAST - manifestation)
        NavigationEvent(
            event_id="create_001",
            event_type="creative_implementation",
            content="Bringing the creative vision into concrete reality",
            timestamp=datetime.now(), 
            source_layer="perception",
            emotional_valence=0.8,
            urgency_level=0.7
        )
    ]
    
    print("=== CREATIVE PROCESS NAVIGATION ===")
    
    for i, event in enumerate(creative_events, 1):
        direction = await gateway.process_navigation_event(event)
        analytics = gateway.get_navigation_analytics()
        
        print(f"\nStage {i}: {event.event_type}")
        print(f"Direction: {direction.primary_direction.value}")
        print(f"Theta: {analytics['theta_oscillations']['theta_type']} @ {analytics['theta_oscillations']['current_frequency']:.1f}Hz")
        print(f"Gamma: {analytics['gamma_synchrony']['gamma_band']} @ {analytics['gamma_synchrony']['current_frequency']:.1f}Hz")
        print(f"Coupling: {analytics['gamma_synchrony']['gamma_theta_coupling']:.3f}")
        
        # Check for strong theta-gamma coupling (optimal for memory formation)
        if analytics['gamma_synchrony']['gamma_theta_coupling'] > 0.7:
            print("🔥 STRONG COUPLING: Optimal for creative breakthrough!")
        
        await asyncio.sleep(0.3)  # Time for oscillations to evolve

asyncio.run(creative_navigation_example())
```

### 3. Learning & Development Navigation

Guide learning processes with theta-enhanced encoding:

```python
async def learning_navigation_example():
    gateway = RetrosplenialGateway()
    
    # Learning context
    context = NavigationContext(
        current_state="skill_acquisition",
        target_state="competent_practitioner", 
        emotional_context={
            "motivation": 0.8,
            "curiosity": 0.9,
            "confidence": 0.5
        }
    )
    gateway.set_navigation_context(context)
    
    # Learning journey events
    learning_events = [
        # Initial learning (NORTH - growth)
        NavigationEvent(
            event_id="learn_basics_001",
            event_type="foundational_learning",
            content="Learning fundamental concepts and building knowledge base",
            timestamp=datetime.now(),
            source_layer="scripts",
            emotional_valence=0.6,
            urgency_level=0.4
        ),
        
        # Practice phase (EAST - application) 
        NavigationEvent(
            event_id="practice_001",
            event_type="skill_practice",
            content="Applying knowledge through hands-on practice and experimentation",
            timestamp=datetime.now(),
            source_layer="perception", 
            emotional_valence=0.5,
            urgency_level=0.6
        ),
        
        # Reflection phase (WEST - understanding)
        NavigationEvent(
            event_id="reflect_progress_001", 
            event_type="progress_reflection",
            content="Reflecting on progress and understanding deeper patterns",
            timestamp=datetime.now(),
            source_layer="consciousness",
            emotional_valence=0.4,
            urgency_level=0.2
        ),
        
        # Advanced learning (NORTH - transcendence) 
        NavigationEvent(
            event_id="advanced_001",
            event_type="advanced_learning",
            content="Transcending basic understanding to achieve mastery",
            timestamp=datetime.now(),
            source_layer="scripts",
            emotional_valence=0.8,
            urgency_level=0.3
        )
    ]
    
    print("=== LEARNING NAVIGATION WITH THETA ENHANCEMENT ===")
    
    total_theta_enhancement = 0
    memory_anchors_created = 0
    
    for i, event in enumerate(learning_events, 1):
        # Wait for optimal theta encoding if beneficial
        theta_engine = gateway.theta_engine
        is_optimal, confidence = theta_engine.get_theta_optimal_encoding_window()
        
        if not is_optimal and confidence < 0.4:
            print(f"⏳ Waiting for optimal theta encoding...")
            waited = await theta_engine.wait_for_optimal_encoding(max_wait=0.5)
            if waited:
                print(f"✅ Optimal theta window achieved!")
        
        # Process the learning event
        direction = await gateway.process_navigation_event(event)
        analytics = gateway.get_navigation_analytics()
        
        print(f"\n--- Learning Stage {i}: {event.event_type} ---")
        print(f"Direction: {direction.primary_direction.value}")
        print(f"Learning strength: {direction.strength:.3f}")
        
        # Theta analysis
        theta = analytics['theta_oscillations']
        print(f"Theta: {theta['theta_type']} @ {theta['current_frequency']:.1f}Hz")
        print(f"Theta power: {theta['current_power']:.3f}")
        
        # Memory binding analysis
        gamma = analytics['gamma_synchrony'] 
        print(f"Gamma synchrony: {gamma['synchrony_strength']:.3f}")
        print(f"Memory anchors: {analytics['memory_anchors']}")
        
        # Track learning enhancement
        theta_enhancement = direction.strength - 1.0  # Enhancement above baseline
        total_theta_enhancement += max(0, theta_enhancement)
        memory_anchors_created = analytics['memory_anchors']
        
        await asyncio.sleep(0.2)
    
    print(f"\n=== LEARNING SUMMARY ===")
    print(f"Total theta enhancement: {total_theta_enhancement:.3f}")
    print(f"Memory anchors created: {memory_anchors_created}")
    print(f"Final direction: {analytics['current_orientation']['direction']}")
    print(f"Learning strength: {analytics['current_orientation']['strength']:.3f}")

asyncio.run(learning_navigation_example())
```

---

## Advanced Features

### 1. Real-time Oscillation Monitoring

Monitor neural oscillations in real-time:

```python
async def oscillation_monitoring_example():
    gateway = RetrosplenialGateway()
    
    print("=== REAL-TIME OSCILLATION MONITORING ===")
    print("Monitoring theta and gamma oscillations...")
    
    for i in range(10):
        # Get current oscillation state
        analytics = gateway.get_navigation_analytics()
        theta = analytics['theta_oscillations']
        gamma = analytics['gamma_synchrony']
        
        print(f"\n--- Time {i+1} ---")
        print(f"Theta: {theta['current_frequency']:.1f}Hz, Phase: {theta['current_phase']:.2f}, Power: {theta['current_power']:.3f}")
        print(f"Gamma: {gamma['current_frequency']:.1f}Hz, Synchrony: {gamma['synchrony_strength']:.3f}")
        print(f"Coupling: {gamma['gamma_theta_coupling']:.3f}")
        
        # Check for optimal states
        if theta['is_optimal_window']:
            print("🌊 THETA PEAK: Optimal for memory encoding!")
        
        if gamma['gamma_theta_coupling'] > 0.7:
            print("🎯 STRONG COUPLING: Enhanced memory formation!")
        
        await asyncio.sleep(0.5)  # 2Hz monitoring

asyncio.run(oscillation_monitoring_example())
```

### 2. Memory Anchor Analytics

Analyze memory binding patterns:

```python
async def memory_analytics_example():
    gateway = RetrosplenialGateway()
    
    # Create several navigation events to build memory
    events = [
        NavigationEvent("spatial_001", "spatial_navigation", "Navigate through complex space", datetime.now(), "perception", 0.3, 0.6),
        NavigationEvent("creative_001", "creative_ideation", "Generate innovative solutions", datetime.now(), "consciousness", 0.8, 0.4),
        NavigationEvent("reflective_001", "deep_reflection", "Contemplate meaning and significance", datetime.now(), "scripts", 0.2, 0.2)
    ]
    
    context = NavigationContext(
        current_state="memory_formation_test",
        emotional_context={"focus": 0.8, "clarity": 0.9}
    )
    gateway.set_navigation_context(context)
    
    print("=== MEMORY ANCHOR ANALYTICS ===")
    
    # Process events and build memory anchors
    for event in events:
        direction = await gateway.process_navigation_event(event)
        print(f"Processed {event.event_type}: {direction.primary_direction.value} (strength: {direction.strength:.3f})")
    
    # Analyze gamma binding performance
    gamma_analytics = gateway.gamma_compass.get_binding_analytics()
    
    print(f"\n--- Gamma Binding Analysis ---")
    print(f"Total bindings: {gamma_analytics['total_bindings']}")
    print(f"Active bindings: {gamma_analytics['active_bindings']}") 
    print(f"Average binding strength: {gamma_analytics['average_binding_strength']:.3f}")
    print(f"Average synchrony: {gamma_analytics['average_synchrony']:.3f}")
    print(f"Current binding power: {gamma_analytics['current_binding_power']:.3f}")
    
    # Frequency distribution
    freq_dist = gamma_analytics['gamma_frequency_distribution']
    print(f"\nGamma frequency distribution:")
    for freq_range, count in freq_dist.items():
        print(f"  {freq_range}: {count} bindings")
    
    # Test memory retrieval
    print(f"\n--- Memory Retrieval Test ---")
    test_context = {
        "current_state": "memory_recall_test",
        "emotional_context": {"focus": 0.7, "clarity": 0.8},
        "event_content": "Recalling creative solutions and innovative approaches"
    }
    
    retrieved = gateway.gamma_compass.retrieve_gamma_bound_memory(test_context)
    if retrieved:
        print(f"Retrieved binding: {retrieved.binding_id}")
        print(f"Synchrony strength: {retrieved.synchrony_strength:.3f}")
        print(f"Bound elements: {len(retrieved.elements)}")
    else:
        print("No matching memory binding found")

asyncio.run(memory_analytics_example())
```

### 3. Custom Direction Patterns

Extend the system with custom semantic directions:

```python
from retrosplenial_gateway.directions import SemanticDirection
from retrosplenial_gateway.core import DirectionEncoder

class CustomDirectionEncoder(DirectionEncoder):
    """Extended direction encoder with custom patterns."""
    
    def _initialize_direction_patterns(self):
        patterns = super()._initialize_direction_patterns()
        
        # Add custom patterns for specific domain
        patterns[SemanticDirection.NORTH.value].update({
            "keywords": patterns[SemanticDirection.NORTH.value]["keywords"] + [
                "evolve", "transcend", "breakthrough", "quantum_leap", "metamorphosis"
            ]
        })
        
        patterns[SemanticDirection.EAST.value].update({
            "keywords": patterns[SemanticDirection.EAST.value]["keywords"] + [
                "innovate", "disrupt", "revolutionize", "prototype", "materialize"  
            ]
        })
        
        return patterns

# Use custom encoder
gateway = RetrosplenialGateway()
gateway.direction_encoder = CustomDirectionEncoder()
```

---

## Integration Patterns

### 1. FastAPI Service

Create a navigation microservice:

```python
from fastapi import FastAPI, BackgroundTasks
from pydantic import BaseModel
from typing import Optional
import asyncio

app = FastAPI(title="RGL Navigation Service", version="1.0.0")
gateway = RetrosplenialGateway()

class NavigationRequest(BaseModel):
    event_id: str
    event_type: str  
    content: str
    emotional_valence: Optional[float] = 0.0
    urgency_level: Optional[float] = 0.5
    
class ContextRequest(BaseModel):
    current_state: str
    target_state: Optional[str] = None
    emotional_context: Optional[dict] = {}

@app.post("/set-context")
async def set_navigation_context(context_req: ContextRequest):
    context = NavigationContext(
        current_state=context_req.current_state,
        target_state=context_req.target_state,
        emotional_context=context_req.emotional_context or {}
    )
    gateway.set_navigation_context(context)
    return {"status": "context_set", "current_state": context_req.current_state}

@app.post("/navigate")
async def navigate(nav_req: NavigationRequest):
    event = NavigationEvent(
        event_id=nav_req.event_id,
        event_type=nav_req.event_type,
        content=nav_req.content,
        timestamp=datetime.now(),
        source_layer="api",
        emotional_valence=nav_req.emotional_valence,
        urgency_level=nav_req.urgency_level
    )
    
    direction = await gateway.process_navigation_event(event)
    analytics = gateway.get_navigation_analytics()
    
    return {
        "direction": {
            "primary": direction.primary_direction.value,
            "secondary": direction.secondary_direction.value if direction.secondary_direction else None,
            "strength": direction.strength,
            "confidence": direction.confidence,
            "description": direction.primary_direction.description
        },
        "oscillations": {
            "theta": analytics['theta_oscillations'],
            "gamma": analytics['gamma_synchrony'] 
        },
        "processing_time": analytics.get('processing_time', 0)
    }

@app.get("/analytics")
async def get_full_analytics():
    return gateway.get_navigation_analytics()

@app.get("/health")
async def health_check():
    analytics = gateway.get_navigation_analytics()
    return {
        "status": "healthy",
        "system_status": analytics['system_status'],
        "events_processed": analytics['events_processed']
    }

# Run with: uvicorn main:app --reload
```

### 2. WebSocket Real-time Stream

Stream navigation data in real-time:

```python
import websockets
import json
import asyncio

async def navigation_stream(websocket, path):
    print(f"Client connected: {websocket.remote_address}")
    
    try:
        while True:
            # Get current analytics
            analytics = gateway.get_navigation_analytics()
            
            # Stream data
            await websocket.send(json.dumps({
                "timestamp": time.time(),
                "analytics": analytics
            }))
            
            await asyncio.sleep(0.1)  # 10Hz updates
            
    except websockets.exceptions.ConnectionClosed:
        print(f"Client disconnected: {websocket.remote_address}")

# Start WebSocket server
start_server = websockets.serve(navigation_stream, "localhost", 8765)
asyncio.get_event_loop().run_until_complete(start_server)
print("Navigation stream server started on ws://localhost:8765")
asyncio.get_event_loop().run_forever()
```

---

## Performance Optimization

### 1. Batch Processing

Process multiple events efficiently:

```python
async def batch_navigation_example():
    gateway = RetrosplenialGateway()
    
    # Create batch of events
    events = [
        NavigationEvent(f"batch_{i}", "batch_processing", f"Event {i}", datetime.now(), "batch", 0.5, 0.5)
        for i in range(100)
    ]
    
    start_time = time.time()
    
    # Process in parallel (if events are independent)
    directions = await asyncio.gather(*[
        gateway.process_navigation_event(event) for event in events
    ])
    
    processing_time = time.time() - start_time
    
    print(f"Processed {len(events)} events in {processing_time:.3f}s")
    print(f"Average: {processing_time/len(events)*1000:.2f}ms per event")
    
    # Analytics
    analytics = gateway.get_navigation_analytics()
    print(f"Memory anchors created: {analytics['memory_anchors']}")

asyncio.run(batch_navigation_example())
```

### 2. Memory Management

Optimize memory usage for long-running systems:

```python
# Configure system for long-running operation
gateway = RetrosplenialGateway()

# Periodically clean up old memory bindings (optional)
async def memory_cleanup_task():
    while True:
        await asyncio.sleep(3600)  # Every hour
        
        # Get current bindings count
        analytics = gateway.get_navigation_analytics()
        bindings_before = analytics['gamma_synchrony']['binding_analytics']['total_bindings']
        
        # Cleanup old/weak bindings (implementation specific)
        # This is optional - system handles memory automatically
        
        print(f"Memory cleanup completed. Bindings: {bindings_before}")

# Run cleanup in background
asyncio.create_task(memory_cleanup_task())
```

---

## Troubleshooting

### Common Issues

1. **Low direction strength**: Usually indicates unclear context or ambiguous events
   - Solution: Provide more specific event content and better emotional context

2. **Weak theta-gamma coupling**: May indicate suboptimal timing
   - Solution: Use `wait_for_optimal_encoding()` for critical events

3. **Memory retrieval failures**: Context similarity too low
   - Solution: Lower similarity threshold or improve context description

4. **Processing delays**: Usually due to optimal timing waits
   - Solution: Reduce `max_wait` parameter or skip optimal timing for non-critical events

### Debug Mode

Enable detailed logging:

```python
import logging

# Enable debug logging
logging.basicConfig(level=logging.DEBUG)

# The system will now output detailed processing information
direction = await gateway.process_navigation_event(event)
```

### Performance Monitoring

Track system performance:

```python
async def performance_monitor():
    while True:
        analytics = gateway.get_navigation_analytics()
        
        print(f"Events processed: {analytics['events_processed']}")
        print(f"Memory usage: {analytics['memory_anchors']} anchors")
        print(f"Current coupling: {analytics['gamma_synchrony']['gamma_theta_coupling']:.3f}")
        
        await asyncio.sleep(10)

# Run in background
asyncio.create_task(performance_monitor())
```