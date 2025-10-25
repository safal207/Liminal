# 🎯 Gamma Synchrony Memory Binding Engine
# Based on 2024-2025 Neuroscience Discoveries
# 
# Key Insights:
# - Gamma waves (30-100Hz) bind disparate information into coherent memories
# - Cross-cortical gamma synchrony improves memory encoding
# - Gamma couples with theta for optimal information processing
# - Stronger gamma = stronger, more integrated memory anchors

import asyncio
import math
import time
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple, Any, Set
from enum import Enum

from .directions import SemanticDirection
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .core import NavigationEvent, DirectionalVector


class GammaFrequencyBand(Enum):
    """Gamma frequency bands with different functions"""
    LOW_GAMMA = "low_gamma"        # 30-50Hz - local binding
    HIGH_GAMMA = "high_gamma"      # 50-100Hz - long-range binding
    ULTRA_GAMMA = "ultra_gamma"    # 100-150Hz - consciousness binding


@dataclass
class GammaState:
    """Current state of gamma oscillations"""
    frequency: float
    band: GammaFrequencyBand
    synchrony_strength: float  # 0-1, how synchronized gamma is
    binding_power: float       # 0-1, current binding strength
    active_bindings: Set[str]  # Currently active binding operations
    timestamp: float


@dataclass
class MemoryBinding:
    """A gamma-synchronized memory binding"""
    binding_id: str
    elements: List[Dict[str, Any]]  # Elements being bound together
    synchrony_strength: float
    binding_timestamp: float
    gamma_burst_strength: float
    anchor_strength: float


class GammaSynchronyMemoryCompass:
    """
    Use gamma synchrony for stronger memory anchoring and information binding.
    
    Based on latest discoveries:
    - Gamma synchrony binds disparate information into coherent memories
    - Cross-cortical synchronization strengthens memory formation
    - Gamma-theta coupling optimizes information processing
    - Higher gamma synchrony = more integrated, accessible memories
    """
    
    def __init__(self):
        # Gamma oscillation parameters
        self.base_gamma_freq = 40.0  # Hz - baseline gamma frequency
        self.low_gamma_range = (30, 50)
        self.high_gamma_range = (50, 100)
        self.ultra_gamma_range = (100, 150)
        
        # Current gamma state
        self.current_state = GammaState(
            frequency=self.base_gamma_freq,
            band=GammaFrequencyBand.LOW_GAMMA,
            synchrony_strength=0.5,
            binding_power=0.6,
            active_bindings=set(),
            timestamp=time.time()
        )
        
        # Memory binding storage
        self.memory_bindings: Dict[str, MemoryBinding] = {}
        self.binding_history: List[MemoryBinding] = []
        
        # Synchrony parameters
        self.max_synchrony_strength = 1.0
        self.synchrony_decay_rate = 0.1  # How quickly synchrony decays
        self.binding_threshold = 0.6     # Minimum synchrony for successful binding
        
        print("Gamma Synchrony Memory Compass initialized - Neural binding engine online!")
    
    def calculate_gamma_synchrony(self, context: Dict[str, Any]) -> float:
        """
        Calculate gamma synchrony strength based on context complexity and coherence.
        
        More coherent, well-structured contexts generate stronger gamma synchrony.
        """
        
        synchrony_factors = []
        
        # Factor 1: Context complexity (more elements = potential for more synchrony)
        context_elements = len(str(context).split())
        complexity_factor = min(context_elements / 50.0, 1.0)  # Normalize to 0-1
        synchrony_factors.append(complexity_factor * 0.3)
        
        # Factor 2: Emotional coherence (stronger emotions = better binding)
        if 'emotional_context' in context:
            emotional_strength = 0.0
            for emotion, value in context['emotional_context'].items():
                emotional_strength += abs(value)
            emotional_factor = min(emotional_strength / 2.0, 1.0)  # Max 2 emotions
            synchrony_factors.append(emotional_factor * 0.3)
        else:
            synchrony_factors.append(0.1)  # Minimal emotional binding
        
        # Factor 3: Semantic coherence (related concepts bind better)
        semantic_coherence = self._assess_semantic_coherence(context)
        synchrony_factors.append(semantic_coherence * 0.4)
        
        # Calculate final synchrony strength
        base_synchrony = sum(synchrony_factors)
        
        # Add some natural variation (gamma isn't perfectly regular)
        variation = math.sin(time.time() * 2 * math.pi) * 0.1
        final_synchrony = max(0.1, min(1.0, base_synchrony + variation))
        
        return final_synchrony
    
    def _assess_semantic_coherence(self, context: Dict[str, Any]) -> float:
        """Assess how semantically coherent the context elements are."""
        
        # Simple heuristic: look for related keywords and concepts
        content_text = str(context).lower()
        
        # Coherent concept clusters
        concept_clusters = [
            ['space', 'navigate', 'direction', 'location', 'coordinate'],
            ['emotion', 'feel', 'mood', 'sentiment', 'experience'],
            ['memory', 'remember', 'recall', 'past', 'history'],
            ['learn', 'understand', 'knowledge', 'insight', 'wisdom'],
            ['create', 'build', 'make', 'generate', 'produce'],
            ['analyze', 'think', 'reflect', 'consider', 'contemplate']
        ]
        
        # Count concepts from each cluster
        cluster_matches = []
        for cluster in concept_clusters:
            matches = sum(1 for concept in cluster if concept in content_text)
            if len(cluster) > 0:
                cluster_strength = matches / len(cluster)
                cluster_matches.append(cluster_strength)
        
        # Higher coherence if concepts from same cluster appear together
        if cluster_matches:
            max_cluster_coherence = max(cluster_matches)
            avg_cluster_coherence = sum(cluster_matches) / len(cluster_matches)
            
            # Weighted combination
            coherence = 0.7 * max_cluster_coherence + 0.3 * avg_cluster_coherence
            return min(1.0, coherence)
        
        return 0.3  # Default moderate coherence
    
    def determine_optimal_gamma_frequency(self, binding_scope: str) -> Tuple[float, GammaFrequencyBand]:
        """
        Determine optimal gamma frequency based on binding scope.
        
        Local binding -> Low gamma (30-50Hz)
        Long-range binding -> High gamma (50-100Hz)
        Consciousness binding -> Ultra gamma (100-150Hz)
        """
        
        if binding_scope in ['local', 'immediate', 'context']:
            # Local binding within immediate context
            freq = 30 + (time.time() % 1) * 20  # 30-50Hz with variation
            return freq, GammaFrequencyBand.LOW_GAMMA
            
        elif binding_scope in ['cross_modal', 'integration', 'system']:
            # Long-range binding across different systems
            freq = 50 + (time.time() % 1) * 50  # 50-100Hz with variation
            return freq, GammaFrequencyBand.HIGH_GAMMA
            
        elif binding_scope in ['consciousness', 'global', 'awareness']:
            # Global consciousness-level binding
            freq = 100 + (time.time() % 1) * 50  # 100-150Hz with variation
            return freq, GammaFrequencyBand.ULTRA_GAMMA
            
        else:
            # Default to low gamma for unknown scopes
            freq = self.base_gamma_freq
            return freq, GammaFrequencyBand.LOW_GAMMA
    
    def generate_gamma_burst(self, duration: float = 0.1) -> float:
        """
        Generate a gamma burst for enhanced memory binding.
        
        Returns the burst strength (0-1).
        """
        
        # Simulate gamma burst with realistic parameters
        burst_amplitude = 0.8 + (math.sin(time.time() * 50) * 0.2)  # 0.6-1.0 range
        burst_strength = max(0.6, min(1.0, burst_amplitude))
        
        # Update gamma state
        self.current_state.binding_power = burst_strength
        self.current_state.timestamp = time.time()
        
        return burst_strength
    
    def create_synchronized_memory_anchor(self, direction_vector: 'DirectionalVector', 
                                        context: Dict[str, Any],
                                        binding_scope: str = "local") -> Tuple[str, float]:
        """
        Create a gamma-synchronized memory anchor with enhanced binding strength.
        
        Returns:
        - binding_id: Unique identifier for this binding
        - anchor_strength: Final strength of the memory anchor
        """
        
        # Calculate gamma synchrony strength
        gamma_synchrony = self.calculate_gamma_synchrony(context)
        
        # Determine optimal gamma frequency
        optimal_freq, gamma_band = self.determine_optimal_gamma_frequency(binding_scope)
        
        # Update gamma state
        self.current_state.frequency = optimal_freq
        self.current_state.band = gamma_band
        self.current_state.synchrony_strength = gamma_synchrony
        
        # Generate gamma burst for enhanced encoding
        gamma_burst_strength = self.generate_gamma_burst()
        
        # Calculate anchor strength with gamma enhancement
        base_strength = direction_vector.strength
        
        # Gamma synchrony multiplier (stronger synchrony = stronger anchor)
        synchrony_multiplier = 1.0 + (gamma_synchrony * 0.5)  # 1.0-1.5x multiplier
        
        # Gamma burst multiplier
        burst_multiplier = 1.0 + (gamma_burst_strength * 0.3)  # 1.0-1.3x multiplier
        
        # Final anchor strength
        anchor_strength = base_strength * synchrony_multiplier * burst_multiplier
        anchor_strength = min(2.0, anchor_strength)  # Cap at 2.0 for exceptional anchors
        
        # Create binding
        binding_id = f"gamma_binding_{int(time.time() * 1000)}"
        
        # Bind multiple contextual elements with gamma
        bound_elements = self.gamma_bind_context_elements(
            emotional_state=context.get('emotional_context', {}),
            spatial_state=context.get('current_state', 'unknown'),
            directional_state=direction_vector
        )
        
        # Store memory binding
        memory_binding = MemoryBinding(
            binding_id=binding_id,
            elements=bound_elements,
            synchrony_strength=gamma_synchrony,
            binding_timestamp=time.time(),
            gamma_burst_strength=gamma_burst_strength,
            anchor_strength=anchor_strength
        )
        
        self.memory_bindings[binding_id] = memory_binding
        self.binding_history.append(memory_binding)
        self.current_state.active_bindings.add(binding_id)
        
        print(f"Gamma-synchronized memory anchor created:")
        print(f"  Binding ID: {binding_id}")
        print(f"  Anchor Strength: {anchor_strength:.3f}")
        print(f"  Gamma Frequency: {optimal_freq:.1f}Hz ({gamma_band.value})")
        print(f"  Synchrony: {gamma_synchrony:.3f} | Burst: {gamma_burst_strength:.3f}")
        
        return binding_id, anchor_strength
    
    def gamma_bind_context_elements(self, emotional_state: Dict, 
                                  spatial_state: str,
                                  directional_state: 'DirectionalVector') -> List[Dict[str, Any]]:
        """
        Bind multiple contextual elements together using gamma synchronization.
        
        This creates integrated, multi-modal memory representations.
        """
        
        bound_elements = []
        
        # Emotional element
        if emotional_state:
            emotional_element = {
                "type": "emotional",
                "content": emotional_state,
                "binding_strength": self.current_state.synchrony_strength * 0.9,
                "gamma_frequency": self.current_state.frequency
            }
            bound_elements.append(emotional_element)
        
        # Spatial element
        spatial_element = {
            "type": "spatial",
            "content": spatial_state,
            "binding_strength": self.current_state.synchrony_strength * 0.8,
            "gamma_frequency": self.current_state.frequency
        }
        bound_elements.append(spatial_element)
        
        # Directional element
        directional_element = {
            "type": "directional", 
            "content": {
                "primary_direction": directional_state.primary_direction.value,
                "secondary_direction": directional_state.secondary_direction.value if directional_state.secondary_direction else None,
                "strength": directional_state.strength,
                "confidence": directional_state.confidence
            },
            "binding_strength": self.current_state.synchrony_strength,
            "gamma_frequency": self.current_state.frequency
        }
        bound_elements.append(directional_element)
        
        # Temporal element (when this binding occurred)
        temporal_element = {
            "type": "temporal",
            "content": {
                "timestamp": time.time(),
                "gamma_phase": (time.time() * self.current_state.frequency * 2 * math.pi) % (2 * math.pi)
            },
            "binding_strength": self.current_state.synchrony_strength * 0.7,
            "gamma_frequency": self.current_state.frequency
        }
        bound_elements.append(temporal_element)
        
        return bound_elements
    
    def retrieve_gamma_bound_memory(self, query_context: Dict[str, Any],
                                  similarity_threshold: float = 0.7) -> Optional[MemoryBinding]:
        """
        Retrieve memory binding based on gamma synchrony similarity.
        
        Uses gamma binding patterns to find most similar memory anchors.
        """
        
        if not self.memory_bindings:
            return None
        
        # Calculate query synchrony
        query_synchrony = self.calculate_gamma_synchrony(query_context)
        
        best_binding = None
        best_similarity = 0.0
        
        for binding in self.memory_bindings.values():
            # Calculate similarity based on:
            # 1. Synchrony strength similarity
            # 2. Content element overlap
            # 3. Gamma frequency compatibility
            
            synchrony_similarity = 1.0 - abs(binding.synchrony_strength - query_synchrony)
            
            # Simple content similarity (could be enhanced)
            content_similarity = 0.5  # Placeholder
            
            # Frequency compatibility
            freq_similarity = 1.0 - min(abs(binding.elements[0]["gamma_frequency"] - self.current_state.frequency) / 100.0, 1.0)
            
            # Weighted overall similarity
            overall_similarity = (synchrony_similarity * 0.4 + 
                                content_similarity * 0.4 + 
                                freq_similarity * 0.2)
            
            if overall_similarity > best_similarity and overall_similarity >= similarity_threshold:
                best_similarity = overall_similarity
                best_binding = binding
        
        if best_binding:
            print(f"Retrieved gamma-bound memory: {best_binding.binding_id} "
                  f"(similarity: {best_similarity:.3f})")
        
        return best_binding
    
    def get_gamma_coupling_with_theta(self, theta_frequency: float, theta_phase: float) -> float:
        """
        Calculate gamma-theta coupling strength for optimal information processing.
        
        Research shows gamma couples with theta for enhanced memory and navigation.
        """
        
        # Optimal gamma-theta coupling occurs when:
        # - Gamma bursts happen during theta peaks
        # - Gamma frequency is harmonically related to theta
        
        # Check harmonic relationship
        harmonic_ratio = self.current_state.frequency / theta_frequency
        optimal_ratios = [8, 10, 12, 15]  # Common gamma:theta ratios in brain
        
        harmonic_coupling = 0.0
        for ratio in optimal_ratios:
            if abs(harmonic_ratio - ratio) < 1.0:  # Within 1 Hz of optimal
                harmonic_coupling = 1.0 - abs(harmonic_ratio - ratio)
                break
        
        # Check phase coupling (gamma bursts during theta peaks)
        theta_peak_strength = max(0, math.cos(theta_phase))  # 0-1 range
        phase_coupling = theta_peak_strength
        
        # Combined coupling strength
        coupling_strength = (harmonic_coupling * 0.6 + phase_coupling * 0.4)
        
        return coupling_strength
    
    def enhance_binding_with_theta_coupling(self, theta_frequency: float, 
                                          theta_phase: float,
                                          base_binding_strength: float) -> float:
        """
        Enhance memory binding strength using gamma-theta coupling.
        """
        
        coupling_strength = self.get_gamma_coupling_with_theta(theta_frequency, theta_phase)
        
        # Enhancement factor based on coupling
        enhancement_factor = 1.0 + (coupling_strength * 0.4)  # Up to 40% enhancement
        
        enhanced_strength = base_binding_strength * enhancement_factor
        
        if coupling_strength > 0.7:
            print(f"Strong gamma-theta coupling detected: {coupling_strength:.3f}")
            print(f"Enhanced binding strength: {base_binding_strength:.3f} -> {enhanced_strength:.3f}")
        
        return enhanced_strength
    
    def get_current_gamma_state(self) -> GammaState:
        """Get current gamma oscillation state."""
        return self.current_state
    
    def get_binding_analytics(self) -> Dict[str, Any]:
        """Get analytics about gamma binding performance."""
        
        if not self.binding_history:
            return {
                "total_bindings": 0,
                "average_binding_strength": 0.0,
                "average_synchrony": 0.0,
                "gamma_frequency_distribution": {}
            }
        
        total_bindings = len(self.binding_history)
        avg_binding_strength = sum(b.anchor_strength for b in self.binding_history) / total_bindings
        avg_synchrony = sum(b.synchrony_strength for b in self.binding_history) / total_bindings
        
        # Frequency distribution
        freq_ranges = {"30-50Hz": 0, "50-100Hz": 0, "100-150Hz": 0}
        for binding in self.binding_history:
            freq = binding.elements[0]["gamma_frequency"]
            if 30 <= freq < 50:
                freq_ranges["30-50Hz"] += 1
            elif 50 <= freq < 100:
                freq_ranges["50-100Hz"] += 1
            elif 100 <= freq <= 150:
                freq_ranges["100-150Hz"] += 1
        
        return {
            "total_bindings": total_bindings,
            "active_bindings": len(self.current_state.active_bindings),
            "average_binding_strength": avg_binding_strength,
            "average_synchrony": avg_synchrony,
            "current_gamma_frequency": self.current_state.frequency,
            "current_gamma_band": self.current_state.band.value,
            "gamma_frequency_distribution": freq_ranges,
            "current_binding_power": self.current_state.binding_power
        }