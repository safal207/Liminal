#!/usr/bin/env python3
"""
🌊🧭🎯 Theta-Gamma Enhanced Navigation System Test

This script demonstrates the integration of theta oscillations and gamma synchrony
into our brain-inspired Retrosplenial Gateway navigation system.

Key Features Being Tested:
1. Theta-modulated direction encoding (4-8Hz)
2. Gamma-synchronized memory binding (30-100Hz)
3. Theta-gamma coupling for enhanced memory formation
4. Optimal timing windows for memory anchoring
5. Different theta frequencies for spatial vs conceptual events
6. Cross-frequency neural oscillation coordination
7. Brain-accurate navigation processing
"""

import asyncio
import time
from datetime import datetime
from typing import List, Dict, Any

# Safe imports with fallbacks
try:
    from retrosplenial_gateway.core import RetrosplenialGateway, NavigationEvent
    from retrosplenial_gateway.directions import NavigationContext, SemanticDirection
    from retrosplenial_gateway.theta_engine import ThetaType
except ImportError as e:
    print(f"Import error: {e}")
    print("Running test from backend directory...")
    import sys
    import os
    sys.path.append(os.path.dirname(os.path.abspath(__file__)))
    
    from retrosplenial_gateway.core import RetrosplenialGateway, NavigationEvent
    from retrosplenial_gateway.directions import NavigationContext, SemanticDirection
    from retrosplenial_gateway.theta_engine import ThetaType


class ThetaGammaNavigationTester:
    """Comprehensive testing system for theta-gamma enhanced navigation."""
    
    def __init__(self):
        self.gateway = RetrosplenialGateway()
        self.test_results: List[Dict] = []
        
        # Set up navigation context
        context = NavigationContext(
            current_state="testing_theta_navigation",
            target_state="demonstrate_brain_accuracy",
            emotional_context={"curiosity": 0.8, "focus": 0.9},
            temporal_context={"max_processing_time": 2.0}
        )
        self.gateway.set_navigation_context(context)
    
    def create_test_events(self) -> List[NavigationEvent]:
        """Create diverse events to test theta oscillation responses."""
        
        events = [
            # Spatial navigation events (should trigger fast theta - 8Hz)
            NavigationEvent(
                event_id="spatial_001",
                event_type="spatial_navigation",
                content="Navigate to the coordinate system and map the route carefully",
                timestamp=datetime.now(),
                source_layer="perception",
                emotional_valence=0.3,
                urgency_level=0.6
            ),
            
            # Conceptual/emotional events (should trigger slow theta - 3Hz)
            NavigationEvent(
                event_id="conceptual_001", 
                event_type="emotional_reflection",
                content="Reflect deeply on the meaning and emotional significance of this experience",
                timestamp=datetime.now(),
                source_layer="memory",
                emotional_valence=0.7,
                urgency_level=0.2
            ),
            
            # Exploration events (should boost theta power)
            NavigationEvent(
                event_id="exploration_001",
                event_type="active_exploration",
                content="Explore unknown territories and discover new possibilities through curious investigation",
                timestamp=datetime.now(),
                source_layer="scripts",
                emotional_valence=0.8,
                urgency_level=0.5
            ),
            
            # Learning events (North direction + adaptive theta)
            NavigationEvent(
                event_id="learning_001",
                event_type="learning",
                content="Learn and develop new skills through transcendent growth experiences",
                timestamp=datetime.now(),
                source_layer="consciousness",
                emotional_valence=0.9,
                urgency_level=0.3
            ),
            
            # Crisis events (South direction + high urgency)
            NavigationEvent(
                event_id="crisis_001",
                event_type="crisis",
                content="Survive this immediate threat and protect essential safety systems",
                timestamp=datetime.now(),
                source_layer="perception",
                emotional_valence=-0.6,
                urgency_level=0.95
            ),
            
            # Creative events (East direction + moderate urgency)
            NavigationEvent(
                event_id="creative_001",
                event_type="creation",
                content="Create beautiful expressions and manifest innovative artistic designs",
                timestamp=datetime.now(),
                source_layer="scripts",
                emotional_valence=0.6,
                urgency_level=0.4
            )
        ]
        
        return events
    
    async def test_theta_modulation(self, event: NavigationEvent) -> Dict[str, Any]:
        """Test theta modulation for a single event."""
        
        print(f"\n{'='*60}")
        print(f"Testing Event: {event.event_id}")
        print(f"Type: {event.event_type}")
        print(f"Content: {event.content[:50]}...")
        print(f"{'='*60}")
        
        # Process the navigation event
        start_time = time.time()
        direction_vector = await self.gateway.process_navigation_event(event)
        processing_time = time.time() - start_time
        
        # Get theta state
        theta_state = self.gateway.theta_engine.get_current_theta_state()
        
        # Get navigation analytics
        analytics = self.gateway.get_navigation_analytics()
        
        # Analyze results
        gamma_info = analytics["gamma_synchrony"]
        
        test_result = {
            "event_id": event.event_id,
            "event_type": event.event_type,
            "direction_encoded": direction_vector.primary_direction.value,
            "direction_strength": direction_vector.strength,
            "direction_confidence": direction_vector.confidence,
            "theta_frequency": theta_state.frequency,
            "theta_type": theta_state.theta_type.value,
            "theta_power": theta_state.power,
            "theta_phase": theta_state.phase,
            "gamma_frequency": gamma_info["current_frequency"],
            "gamma_band": gamma_info["gamma_band"],
            "gamma_synchrony": gamma_info["synchrony_strength"],
            "gamma_binding_power": gamma_info["binding_power"],
            "gamma_theta_coupling": gamma_info["gamma_theta_coupling"],
            "optimal_encoding": analytics["theta_oscillations"]["is_optimal_window"],
            "processing_time": processing_time,
            "timestamp": datetime.now().isoformat()
        }
        
        # Print results
        print(f"[OK] Direction: {direction_vector.primary_direction.value}")
        print(f"[STR] Strength: {direction_vector.strength:.3f}")
        print(f"[THETA] {theta_state.theta_type.value} @ {theta_state.frequency:.1f}Hz")
        print(f"[PWR] Power: {theta_state.power:.3f} | Phase: {theta_state.phase:.2f}")
        print(f"[GAMMA] {gamma_info['gamma_band']} @ {gamma_info['current_frequency']:.1f}Hz")
        print(f"[SYNC] Synchrony: {gamma_info['synchrony_strength']:.3f} | Binding: {gamma_info['binding_power']:.3f}")
        print(f"[COUPLING] Theta-Gamma: {gamma_info['gamma_theta_coupling']:.3f}")
        print(f"[TIME] Processing: {processing_time:.3f}s")
        
        return test_result
    
    async def test_optimal_timing(self):
        """Test optimal timing window detection and waiting."""
        
        print(f"\n{'='*60}")
        print("TESTING OPTIMAL TIMING WINDOWS")
        print(f"{'='*60}")
        
        # Check current timing
        is_optimal, confidence = self.gateway.theta_engine.get_theta_optimal_encoding_window()
        time_to_optimal = self.gateway.theta_engine.predict_next_optimal_window()
        
        print(f"Current window optimal: {is_optimal}")
        print(f"Confidence: {confidence:.3f}")
        print(f"Time to next optimal: {time_to_optimal:.3f}s")
        
        # Test waiting for optimal window (if reasonable)
        if time_to_optimal <= 1.0:
            print("Waiting for optimal encoding window...")
            waited = await self.gateway.theta_engine.wait_for_optimal_encoding()
            print(f"Waited successfully: {waited}")
            
            # Check again
            is_optimal_after, confidence_after = self.gateway.theta_engine.get_theta_optimal_encoding_window()
            print(f"After waiting - optimal: {is_optimal_after}, confidence: {confidence_after:.3f}")
    
    async def test_theta_synchronization(self):
        """Test theta synchronization of multiple directions."""
        
        print(f"\n{'='*60}")
        print("TESTING THETA SYNCHRONIZATION")
        print(f"{'='*60}")
        
        # Create some mock directional vectors
        from retrosplenial_gateway.core import DirectionalVector
        
        mock_directions = [
            DirectionalVector(primary_direction=SemanticDirection.NORTH, strength=0.8, confidence=0.9),
            DirectionalVector(primary_direction=SemanticDirection.EAST, strength=0.6, confidence=0.7),
            DirectionalVector(primary_direction=SemanticDirection.WEST, strength=0.4, confidence=0.5)
        ]
        
        # Synchronize with theta rhythm
        synchronized_directions = self.gateway.theta_engine.get_theta_synchronized_directions(mock_directions)
        
        print("Original vs Theta-Synchronized Directions:")
        for i, (original, synchronized) in enumerate(zip(mock_directions, synchronized_directions)):
            print(f"Direction {i+1}: {original.primary_direction.value}")
            print(f"  Original strength: {original.strength:.3f} -> Synchronized: {synchronized.strength:.3f}")
            print(f"  Enhancement factor: {synchronized.strength / original.strength:.3f}x")
    
    async def test_gamma_memory_binding(self):
        """Test gamma-synchronized memory binding and retrieval."""
        
        print(f"\n{'='*60}")
        print("TESTING GAMMA MEMORY BINDING")
        print(f"{'='*60}")
        
        # Create a complex context for testing binding
        test_context = {
            "current_state": "complex_navigation_scenario",
            "emotional_context": {
                "curiosity": 0.8,
                "focus": 0.9,
                "confidence": 0.7
            },
            "event_content": "Navigate through challenging multi-dimensional space with emotional awareness",
            "spatial_elements": ["coordinate_system", "path_planning", "obstacle_avoidance"],
            "temporal_elements": ["sequence_planning", "timing_optimization", "rhythm_coordination"]
        }
        
        # Create a directional vector for binding
        from retrosplenial_gateway.core import DirectionalVector
        test_vector = DirectionalVector(
            primary_direction=SemanticDirection.EAST,
            secondary_direction=SemanticDirection.NORTH,
            strength=0.85,
            confidence=0.92,
            stability=0.88
        )
        
        print("Creating gamma-synchronized memory binding...")
        binding_id, anchor_strength = self.gateway.gamma_compass.create_synchronized_memory_anchor(
            test_vector, test_context, binding_scope="cross_modal"
        )
        
        print(f"Created binding: {binding_id}")
        print(f"Anchor strength: {anchor_strength:.3f}")
        
        # Test memory retrieval
        print("\nTesting gamma-based memory retrieval...")
        
        # Create similar context for retrieval
        query_context = {
            "current_state": "similar_navigation_scenario",
            "emotional_context": {
                "curiosity": 0.75,
                "focus": 0.85,
                "excitement": 0.6
            },
            "event_content": "Navigate through multi-dimensional awareness space",
            "spatial_elements": ["coordinate_system", "path_optimization"]
        }
        
        retrieved_binding = self.gateway.gamma_compass.retrieve_gamma_bound_memory(query_context)
        
        if retrieved_binding:
            print(f"Retrieved binding: {retrieved_binding.binding_id}")
            print(f"Binding synchrony: {retrieved_binding.synchrony_strength:.3f}")
            print(f"Gamma burst strength: {retrieved_binding.gamma_burst_strength:.3f}")
            print(f"Bound elements: {len(retrieved_binding.elements)}")
        else:
            print("No matching binding found")
        
        # Get gamma analytics
        gamma_analytics = self.gateway.gamma_compass.get_binding_analytics()
        print(f"\nGamma Binding Analytics:")
        print(f"  Total bindings: {gamma_analytics['total_bindings']}")
        print(f"  Active bindings: {gamma_analytics['active_bindings']}")
        print(f"  Average binding strength: {gamma_analytics['average_binding_strength']:.3f}")
        print(f"  Average synchrony: {gamma_analytics['average_synchrony']:.3f}")
        print(f"  Current binding power: {gamma_analytics['current_binding_power']:.3f}")
    
    async def test_theta_gamma_coupling(self):
        """Test theta-gamma coupling for enhanced memory formation."""
        
        print(f"\n{'='*60}")
        print("TESTING THETA-GAMMA COUPLING")
        print(f"{'='*60}")
        
        # Get current theta and gamma states
        theta_state = self.gateway.theta_engine.get_current_theta_state()
        gamma_state = self.gateway.gamma_compass.get_current_gamma_state()
        
        print(f"Current Theta: {theta_state.frequency:.1f}Hz (phase: {theta_state.phase:.2f})")
        print(f"Current Gamma: {gamma_state.frequency:.1f}Hz ({gamma_state.band.value})")
        
        # Calculate coupling
        coupling_strength = self.gateway.gamma_compass.get_gamma_coupling_with_theta(
            theta_state.frequency, theta_state.phase
        )
        
        print(f"Theta-Gamma Coupling Strength: {coupling_strength:.3f}")
        
        if coupling_strength > 0.7:
            print("[STRONG] COUPLING: Optimal for memory formation!")
        elif coupling_strength > 0.4:
            print("[MODERATE] COUPLING: Good memory encoding conditions")
        else:
            print("[WEAK] COUPLING: Suboptimal for memory binding")
        
        # Test enhanced binding with coupling
        base_strength = 0.8
        enhanced_strength = self.gateway.gamma_compass.enhance_binding_with_theta_coupling(
            theta_state.frequency, theta_state.phase, base_strength
        )
        
        enhancement_factor = enhanced_strength / base_strength
        print(f"Memory Enhancement: {base_strength:.3f} -> {enhanced_strength:.3f} ({enhancement_factor:.2f}x)")
        
        return coupling_strength
    
    async def run_comprehensive_test(self):
        """Run comprehensive theta-gamma navigation test suite."""
        
        print("THETA-GAMMA ENHANCED BRAIN NAVIGATION TEST")
        print("Based on 2024-2025 Neuroscience Discoveries")
        print("Features: Theta oscillations + Gamma synchrony + Cross-frequency coupling")
        print(f"{'='*80}")
        
        # Test events
        events = self.create_test_events()
        
        print(f"Testing {len(events)} events with theta-gamma modulation...")
        
        for event in events:
            test_result = await self.test_theta_modulation(event)
            self.test_results.append(test_result)
            
            # Small delay to see theta phase evolution
            await asyncio.sleep(0.2)
        
        # Test optimal timing
        await self.test_optimal_timing()
        
        # Test synchronization
        await self.test_theta_synchronization()
        
        # 🎯 NEW: Test gamma memory binding
        await self.test_gamma_memory_binding()
        
        # 🔄 NEW: Test theta-gamma coupling
        coupling_strength = await self.test_theta_gamma_coupling()
        
        # Print summary
        await self.print_test_summary(coupling_strength)
    
    async def print_test_summary(self, coupling_strength: float = 0.0):
        """Print comprehensive test summary."""
        
        print(f"\n{'='*80}")
        print("THETA-GAMMA NAVIGATION TEST SUMMARY")
        print(f"{'='*80}")
        
        if not self.test_results:
            print("No test results to summarize.")
            return
        
        # Analyze theta types
        theta_types = {}
        directions = {}
        
        for result in self.test_results:
            theta_type = result["theta_type"]
            direction = result["direction_encoded"]
            
            theta_types[theta_type] = theta_types.get(theta_type, 0) + 1
            directions[direction] = directions.get(direction, 0) + 1
        
        print("[FREQ] THETA FREQUENCY DISTRIBUTION:")
        for theta_type, count in theta_types.items():
            print(f"  {theta_type}: {count} events")
        
        print("\n[DIR] DIRECTION DISTRIBUTION:")
        for direction, count in directions.items():
            print(f"  {direction}: {count} events")
        
        # Calculate averages
        avg_strength = sum(r["direction_strength"] for r in self.test_results) / len(self.test_results)
        avg_confidence = sum(r["direction_confidence"] for r in self.test_results) / len(self.test_results)
        avg_power = sum(r["theta_power"] for r in self.test_results) / len(self.test_results)
        avg_processing_time = sum(r["processing_time"] for r in self.test_results) / len(self.test_results)
        
        print(f"\n[AVG] AVERAGE PERFORMANCE:")
        print(f"  Direction Strength: {avg_strength:.3f}")
        print(f"  Direction Confidence: {avg_confidence:.3f}")
        print(f"  Theta Power: {avg_power:.3f}")
        print(f"  Processing Time: {avg_processing_time:.3f}s")
        
        # Get final system analytics
        final_analytics = self.gateway.get_navigation_analytics()
        
        print(f"\n[FINAL] SYSTEM STATE:")
        print(f"  Events Processed: {final_analytics['events_processed']}")
        print(f"  Memory Anchors Created: {final_analytics['memory_anchors']}")
        
        if final_analytics["current_orientation"]:
            print(f"  Current Orientation: {final_analytics['current_orientation']['direction']}")
            print(f"  Orientation Strength: {final_analytics['current_orientation']['strength']:.3f}")
        
        theta_info = final_analytics["theta_oscillations"]
        print(f"  Current Theta: {theta_info['theta_type']} @ {theta_info['current_frequency']:.1f}Hz")
        print(f"  Next Optimal Window: {theta_info['next_optimal_in_seconds']:.2f}s")
        
        print(f"\n{'='*80}")
        print("[SUCCESS] THETA-GAMMA NAVIGATION SYSTEM TEST COMPLETED!")
        print("Brain-accurate navigation with neural oscillations operational!")
        print(f"{'='*80}")


async def main():
    """Main test execution."""
    tester = ThetaGammaNavigationTester()
    await tester.run_comprehensive_test()


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("\n[STOP] Test interrupted by user")
    except Exception as e:
        print(f"\nX Test error: {e}")
        import traceback
        traceback.print_exc()