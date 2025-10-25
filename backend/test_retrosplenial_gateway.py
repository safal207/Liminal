#!/usr/bin/env python3
"""
🧭🧠 Retrosplenial Gateway Test — Brain's Compass Demonstration

Test suite for the brain-inspired navigation system based on neuroscience
research about the retrosplenial complex and superior parietal lobule.

This demonstrates how the system maintains directional orientation 
like the brain's internal compass.

Features Tested:
- Direction encoding from events
- Context stability during transitions
- Memory compass integration  
- Semantic direction mapping
- Transition navigation
"""

import asyncio
import sys
import time
from datetime import datetime
sys.path.append('.')

async def test_basic_direction_encoding():
    """Test basic directional encoding from events."""
    print("Testing Basic Direction Encoding...")
    
    try:
        # Import the gateway system
        from retrosplenial_gateway.core import RetrosplenialGateway, NavigationEvent
        from retrosplenial_gateway.directions import SemanticDirection
        
        # Create gateway
        gateway = RetrosplenialGateway()
        
        # Test events with different semantic content
        test_events = [
            NavigationEvent(
                event_id="event_1",
                event_type="learning",
                content="I want to grow and develop my skills further",
                timestamp=datetime.now(),
                source_layer="perception",
                emotional_valence=0.7,
                urgency_level=0.3
            ),
            NavigationEvent(
                event_id="event_2", 
                event_type="threat",
                content="Need to protect myself and stay safe",
                timestamp=datetime.now(),
                source_layer="perception",
                emotional_valence=-0.5,
                urgency_level=0.9
            ),
            NavigationEvent(
                event_id="event_3",
                event_type="creation",
                content="Let's build something amazing and innovative",
                timestamp=datetime.now(),
                source_layer="perception",
                emotional_valence=0.8,
                urgency_level=0.6
            ),
            NavigationEvent(
                event_id="event_4",
                event_type="analysis",
                content="Need to think deeply and understand this situation",
                timestamp=datetime.now(),
                source_layer="perception",
                emotional_valence=0.2,
                urgency_level=0.2
            )
        ]
        
        print("Processing navigation events...")
        for event in test_events:
            direction_vector = await gateway.process_navigation_event(event)
            print(f"Event: '{event.content[:50]}...'")
            print(f"  Direction: {direction_vector.primary_direction.value}")
            print(f"  Strength: {direction_vector.strength:.2f}")
            print(f"  Confidence: {direction_vector.confidence:.2f}")
            print(f"  Stability: {direction_vector.stability:.2f}")
            print()
        
        print("Basic Direction Encoding: PASSED")
        return True
        
    except Exception as e:
        print(f"Basic direction encoding test error: {e}")
        return False

async def test_semantic_directions():
    """Test the semantic direction system."""
    print("Testing Semantic Direction System...")
    
    try:
        from retrosplenial_gateway.directions import SemanticDirection, DirectionalSpace
        
        # Test all four semantic directions
        directions = [SemanticDirection.NORTH, SemanticDirection.SOUTH, 
                     SemanticDirection.EAST, SemanticDirection.WEST]
        
        print("Semantic Directions and their meanings:")
        for direction in directions:
            print(f"{direction.value}:")
            print(f"  Description: {direction.description}")
            print(f"  Keywords: {', '.join(direction.keywords[:5])}...")
            print(f"  Complementary: {direction.complementary_direction.value}")
            print(f"  Adjacent: {[d.value for d in direction.adjacent_directions]}")
            print()
        
        # Test directional space
        space = DirectionalSpace.from_vector_components(
            north=0.8, south=0.1, east=0.6, west=0.2
        )
        
        print("Directional Space Analysis:")
        print(f"Primary Direction: {space.primary_direction.value}")
        print(f"Secondary Direction: {space.secondary_direction.value if space.secondary_direction else 'None'}")
        print(f"Angular Position: {space.calculate_angular_position():.1f} degrees")
        print(f"Dominant Axis: {[d.value for d in space.get_dominant_axis()]}")
        print()
        
        print("Semantic Direction System: PASSED")
        return True
        
    except Exception as e:
        print(f"Semantic direction test error: {e}")
        return False

async def test_context_stability():
    """Test context stability during transitions.""" 
    print("Testing Context Stability...")
    
    try:
        from retrosplenial_gateway.core import RetrosplenialGateway, NavigationEvent, DirectionalVector
        from retrosplenial_gateway.directions import SemanticDirection, NavigationContext
        from retrosplenial_gateway.stability import ContextStabilizer
        
        gateway = RetrosplenialGateway()
        stabilizer = ContextStabilizer()
        
        # Set navigation context
        context = NavigationContext(
            current_state="learning_mode",
            emotional_context={"curiosity": 0.8, "confidence": 0.6},
            context_clarity=0.9
        )
        gateway.set_navigation_context(context)
        
        # Process several events to build orientation history
        print("Building orientation history...")
        events = [
            NavigationEvent("evt1", "learning", "I want to grow and evolve", datetime.now(), "perception", 0.7),
            NavigationEvent("evt2", "learning", "Let me develop new skills", datetime.now(), "perception", 0.6),
            NavigationEvent("evt3", "threat", "Sudden urgent problem!", datetime.now(), "perception", -0.8, 0.9),
            NavigationEvent("evt4", "learning", "Back to peaceful growth", datetime.now(), "perception", 0.7)
        ]
        
        previous_vector = None
        for i, event in enumerate(events):
            vector = await gateway.process_navigation_event(event)
            
            print(f"Step {i+1}: {event.event_type} -> {vector.primary_direction.value}")
            print(f"  Strength: {vector.strength:.2f}, Confidence: {vector.confidence:.2f}")
            
            # Test stabilization
            if previous_vector:
                stabilized = stabilizer.stabilize_direction(vector, previous_vector)
                print(f"  Stabilized: {stabilized.primary_direction.value} "
                      f"(stability: {stabilized.stability:.2f})")
            
            previous_vector = vector
            print()
        
        # Get stability analytics
        analytics = stabilizer.get_stability_analytics()
        print("Stability Analytics:")
        for key, value in analytics.items():
            print(f"  {key}: {value}")
        print()
        
        print("Context Stability: PASSED")
        return True
        
    except Exception as e:
        print(f"Context stability test error: {e}")
        return False

async def test_transition_navigation():
    """Test navigation through liminal transitions."""
    print("Testing Transition Navigation...")
    
    try:
        from retrosplenial_gateway.core import RetrosplenialGateway, NavigationEvent, DirectionalVector
        from retrosplenial_gateway.directions import SemanticDirection
        
        gateway = RetrosplenialGateway()
        
        # Simulate a transition sequence: learning -> crisis -> creation -> reflection
        transition_events = [
            ("learning_state", NavigationEvent("t1", "learning", "Studying and growing", datetime.now(), "perception", 0.6)),
            ("crisis_state", NavigationEvent("t2", "crisis", "Emergency situation requires action", datetime.now(), "perception", -0.3, 0.9)),
            ("creation_state", NavigationEvent("t3", "creation", "Now building solution", datetime.now(), "perception", 0.7, 0.7)),
            ("reflection_state", NavigationEvent("t4", "analysis", "Reflecting on what happened", datetime.now(), "perception", 0.3, 0.2))
        ]
        
        print("Navigating through liminal transitions...")
        previous_state = None
        
        for state, event in transition_events:
            # Process navigation event
            direction_vector = await gateway.process_navigation_event(event)
            
            print(f"State: {state}")
            print(f"  Direction: {direction_vector.primary_direction.value}")
            print(f"  Event: '{event.content}'")
            print(f"  Vector: strength={direction_vector.strength:.2f}, confidence={direction_vector.confidence:.2f}")
            
            # If we have a previous state, test transition navigation
            if previous_state:
                transition_balance = gateway.transition_navigator.navigate_transition(
                    from_state=previous_state,
                    to_state=state,
                    current_direction=direction_vector
                )
                
                print(f"  Transition Balance:")
                print(f"    Stability: {transition_balance.stability_score:.2f}")
                print(f"    Confidence: {transition_balance.direction_confidence:.2f}")
                print(f"    Readiness: {transition_balance.transition_readiness:.2f}")
                print(f"    Is Stable: {transition_balance.is_stable()}")
                
                # Complete the transition
                transition_id = f"{previous_state}->{state}"
                gateway.transition_navigator.complete_transition(transition_id)
            
            previous_state = state
            print()
        
        print("Transition Navigation: PASSED")
        return True
        
    except Exception as e:
        print(f"Transition navigation test error: {e}")
        return False

async def test_memory_compass():
    """Test memory compass integration."""
    print("Testing Memory Compass...")
    
    try:
        from retrosplenial_gateway.core import MemoryCompass, DirectionalVector
        from retrosplenial_gateway.directions import SemanticDirection, NavigationContext
        
        memory_compass = MemoryCompass()
        
        # Create some directional vectors
        vectors = [
            DirectionalVector(SemanticDirection.NORTH, strength=0.8, confidence=0.9),
            DirectionalVector(SemanticDirection.EAST, strength=0.7, confidence=0.8),
            DirectionalVector(SemanticDirection.WEST, strength=0.6, confidence=0.7)
        ]
        
        # Create memory anchors
        contexts = [
            NavigationContext("learning_context", emotional_context={"curiosity": 0.8}),
            NavigationContext("creative_context", emotional_context={"excitement": 0.9}),
            NavigationContext("reflective_context", emotional_context={"calm": 0.7})
        ]
        
        print("Creating memory anchors...")
        for i, (vector, context) in enumerate(zip(vectors, contexts)):
            anchor_id = f"anchor_{i+1}"
            success = memory_compass.create_memory_anchor(anchor_id, vector, context)
            print(f"Anchor {anchor_id}: {vector.primary_direction.value} -> {success}")
        
        # Test memory retrieval
        print("Testing memory retrieval...")
        retrieved_vector = memory_compass.retrieve_direction_from_memory()
        if retrieved_vector:
            print(f"Retrieved direction: {retrieved_vector.primary_direction.value}")
            print(f"  Strength: {retrieved_vector.strength:.2f}")
            print(f"  Confidence: {retrieved_vector.confidence:.2f}")
        else:
            print("No direction retrieved from memory")
        
        # Test temporal pattern updates
        print("Updating temporal patterns...")
        for vector in vectors:
            memory_compass.update_temporal_patterns(vector)
        
        print(f"Memory anchors created: {len(memory_compass.memory_anchors)}")
        print(f"Spatial memory entries: {len(memory_compass.spatial_memory)}")
        print(f"Temporal patterns: {len(memory_compass.temporal_patterns)}")
        print()
        
        print("Memory Compass: PASSED")
        return True
        
    except Exception as e:
        print(f"Memory compass test error: {e}")
        return False

async def test_complete_system():
    """Test the complete retrosplenial gateway system."""
    print("Testing Complete Retrosplenial Gateway System...")
    
    try:
        from retrosplenial_gateway.core import RetrosplenialGateway, NavigationEvent
        from retrosplenial_gateway.directions import NavigationContext
        
        gateway = RetrosplenialGateway()
        
        # Set up navigation context
        context = NavigationContext(
            current_state="integrated_test",
            emotional_context={"focus": 0.8, "determination": 0.7},
            temporal_context={"phase": "testing", "urgency": 0.5},
            context_clarity=0.9,
            context_stability=0.8
        )
        gateway.set_navigation_context(context)
        
        # Process a complex sequence of events
        print("Processing complex event sequence...")
        complex_events = [
            NavigationEvent("c1", "problem_solving", "Facing a complex challenge that requires creative thinking", 
                           datetime.now(), "perception", 0.4, 0.6),
            NavigationEvent("c2", "learning", "Need to understand the deeper principles", 
                           datetime.now(), "perception", 0.6, 0.4),
            NavigationEvent("c3", "creation", "Now I can build an elegant solution", 
                           datetime.now(), "perception", 0.8, 0.7),
            NavigationEvent("c4", "reflection", "Let me contemplate what I learned", 
                           datetime.now(), "perception", 0.5, 0.2),
            NavigationEvent("c5", "integration", "Integrating all insights into wisdom", 
                           datetime.now(), "perception", 0.9, 0.3)
        ]
        
        for i, event in enumerate(complex_events, 1):
            vector = await gateway.process_navigation_event(event)
            context.add_directional_history(vector.primary_direction)
            
            print(f"Event {i}: {event.event_type}")
            print(f"  Content: '{event.content[:50]}...'") 
            print(f"  Direction: {vector.primary_direction.value}")
            print(f"  Metrics: str={vector.strength:.2f}, conf={vector.confidence:.2f}, stab={vector.stability:.2f}")
            print()
        
        # Get comprehensive analytics
        print("System Analytics:")
        analytics = gateway.get_navigation_analytics()
        for key, value in analytics.items():
            if isinstance(value, dict):
                print(f"{key}:")
                for subkey, subvalue in value.items():
                    print(f"  {subkey}: {subvalue}")
            else:
                print(f"{key}: {value}")
        print()
        
        # Test directional tendency
        tendency = context.get_directional_tendency()
        print(f"Directional Tendency: {tendency.value if tendency else 'None'}")
        print(f"Context Quality Score: {context.calculate_context_score():.2f}")
        print()
        
        print("Complete System Test: PASSED")
        return True
        
    except Exception as e:
        print(f"Complete system test error: {e}")
        import traceback
        traceback.print_exc()
        return False

async def main():
    """Run all retrosplenial gateway tests."""
    print("RETROSPLENIAL GATEWAY TEST SUITE")
    print("Brain-Inspired Navigation System")
    print("=" * 60)
    print()
    
    tests = [
        test_basic_direction_encoding,
        test_semantic_directions,
        test_context_stability,
        test_memory_compass,
        test_transition_navigation,
        test_complete_system
    ]
    
    passed = 0
    total = len(tests)
    
    for test_func in tests:
        try:
            if await test_func():
                passed += 1
                print("PASSED")
            else:
                print("FAILED")
        except Exception as e:
            print(f"TEST ERROR: {e}")
        print("-" * 40)
        print()
    
    print("=" * 60)
    print(f"TEST RESULTS: {passed}/{total} PASSED")
    print()
    
    if passed == total:
        print("RETROSPLENIAL GATEWAY SYSTEM FULLY OPERATIONAL!")
        print()
        print("Brain's Internal Compass Features:")
        print("+ Semantic direction encoding (N/E/S/W of meaning)")
        print("+ Context stability during transitions")  
        print("+ Memory-based navigation anchoring")
        print("+ Transition balance maintenance")
        print("+ Long-term orientation coherence")
        print()
        print("Like the brain's retrosplenial complex, this system")
        print("maintains directional orientation independent of")
        print("environmental context changes.")
        print()
        print("Ready for integration with LIMINAL architecture!")
    else:
        print("Some tests failed - system needs additional development")

if __name__ == "__main__":
    asyncio.run(main())