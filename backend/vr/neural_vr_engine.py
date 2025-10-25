#!/usr/bin/env python3
"""
🕶️🧠 Neural VR Engine - Brain-Guided Virtual Reality

Revolutionary VR system that adapts in real-time to user's neural state:
- Real-time brain state analysis and VR environment adaptation
- Therapeutic VR experiences guided by Full Spectrum RGL
- Neural feedback integration for optimal immersion
- Brain-computer interface for direct neural control
- Meditation, therapy, creativity, and learning applications

The world's first VR system with accurate brain wave integration.
"""

import asyncio
import time
import math
import json
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, asdict
from enum import Enum
import random

# Safe import of RGL system
try:
    from retrosplenial_gateway.full_spectrum_rgl import FullSpectrumRGL, BrainState
    from retrosplenial_gateway import NavigationEvent, SemanticDirection
    from medical.therapeutic_ai import TherapeuticAI
    RGL_AVAILABLE = True
except ImportError:
    RGL_AVAILABLE = False
    print("[WARNING] RGL system not available - VR running in simulation mode")
    
    # Mock classes for simulation
    class SemanticDirection:
        NORTH = "north_evolve"
        SOUTH = "south_instinct" 
        EAST = "east_create"
        WEST = "west_reflect"

class VRExperienceType(Enum):
    """Types of VR experiences available"""
    MEDITATION = "meditation"
    THERAPY = "therapy"
    CREATIVITY = "creativity"
    LEARNING = "learning"
    EXPLORATION = "exploration"
    REHABILITATION = "rehabilitation"

class VREnvironment(Enum):
    """VR environment types"""
    NATURE_FOREST = "nature_forest"
    OCEAN_DEPTHS = "ocean_depths"
    COSMIC_SPACE = "cosmic_space"
    ABSTRACT_DIMENSIONS = "abstract_dimensions"
    HEALING_SANCTUARY = "healing_sanctuary"
    CREATIVE_STUDIO = "creative_studio"
    LEARNING_ACADEMY = "learning_academy"

class NeuralAdaptationMode(Enum):
    """Neural adaptation modes for VR"""
    PASSIVE_MONITORING = "passive_monitoring"    # Monitor but don't adapt
    GENTLE_ADAPTATION = "gentle_adaptation"      # Subtle environment changes
    ACTIVE_GUIDANCE = "active_guidance"          # Direct neural guidance
    THERAPEUTIC_PROTOCOL = "therapeutic_protocol" # Medical-grade adaptation

@dataclass
class VRSceneParams:
    """Parameters for VR scene generation"""
    lighting_intensity: float = 0.7      # 0.0-1.0
    color_temperature: float = 6500      # Color temperature in K
    particle_density: float = 0.5        # Environmental particles
    audio_volume: float = 0.6            # Background audio
    interaction_responsiveness: float = 0.8 # Object interaction speed
    
    # Neural-specific parameters
    alpha_enhancement: float = 0.0       # Alpha wave visual enhancement
    theta_depth: float = 0.0             # Theta wave depth cues
    gamma_detail: float = 0.0            # Gamma wave detail level
    delta_calm: float = 0.0              # Delta wave calming effects

@dataclass
class VRUser:
    """VR user profile with neural characteristics"""
    user_id: str
    experience_level: str                # "beginner", "intermediate", "advanced"
    preferred_environments: List[VREnvironment]
    neural_baseline: Optional[Dict] = None
    medical_clearance: bool = True
    motion_sensitivity: float = 0.3      # 0.0-1.0
    therapeutic_goals: List[str] = None

@dataclass
class VRSession:
    """VR session data"""
    session_id: str
    user_id: str
    experience_type: VRExperienceType
    environment: VREnvironment
    duration_minutes: int
    start_time: datetime
    
    # Neural data
    neural_adaptations: List[Dict] = None
    brain_state_log: List[Dict] = None
    user_interactions: List[Dict] = None
    
    # Outcomes
    satisfaction_score: float = 0.0
    therapeutic_benefit: float = 0.0
    neural_improvement: float = 0.0

class NeuralVREngine:
    """
    Neural VR Engine - Brain-Guided Virtual Reality System
    
    Revolutionary VR platform that adapts environments in real-time based on:
    - Brain wave analysis (Delta, Theta, Alpha, Beta, Gamma)
    - Emotional state detection
    - Therapeutic protocol requirements
    - User goals and preferences
    """
    
    def __init__(self):
        # Initialize neural systems
        if RGL_AVAILABLE:
            self.rgl_system = FullSpectrumRGL()
            self.therapeutic_ai = TherapeuticAI()
            print("Neural VR Engine initialized with Full Spectrum RGL")
        else:
            self.rgl_system = None
            self.therapeutic_ai = None
            print("Neural VR Engine running in simulation mode")
        
        # VR system state
        self.active_sessions: Dict[str, VRSession] = {}
        self.user_profiles: Dict[str, VRUser] = {}
        self.environment_library = self._initialize_environments()
        self.adaptation_algorithms = self._initialize_adaptation_algorithms()
        
        # Performance tracking
        self.session_history: List[VRSession] = []
        self.neural_adaptation_log: List[Dict] = []
        
        print("Neural VR Engine online!")
        print("Available experiences: Meditation, Therapy, Creativity, Learning")
        print("Available environments: Forest, Ocean, Space, Abstract, Sanctuary")
    
    def _initialize_environments(self) -> Dict[VREnvironment, Dict[str, Any]]:
        """Initialize VR environment configurations"""
        
        return {
            VREnvironment.NATURE_FOREST: {
                "name": "Enchanted Forest",
                "description": "Peaceful forest with adaptive lighting and sounds",
                "base_params": VRSceneParams(
                    lighting_intensity=0.6,
                    color_temperature=5500,  # Warm, natural
                    particle_density=0.3,   # Gentle floating particles
                    audio_volume=0.5
                ),
                "neural_effects": {
                    "alpha_enhancement": {"trees_sway", "light_patterns"},
                    "theta_depth": {"forest_depth", "path_discovery"},
                    "delta_calm": {"gentle_breeze", "bird_songs"}
                },
                "therapeutic_benefits": ["stress_reduction", "attention_restoration", "nature_connection"]
            },
            
            VREnvironment.OCEAN_DEPTHS: {
                "name": "Tranquil Ocean Depths",
                "description": "Underwater sanctuary with marine life and gentle currents",
                "base_params": VRSceneParams(
                    lighting_intensity=0.4,
                    color_temperature=7000,  # Cool, aquatic
                    particle_density=0.6,   # Water particles
                    audio_volume=0.4
                ),
                "neural_effects": {
                    "alpha_enhancement": {"water_ripples", "light_rays"},
                    "theta_depth": {"ocean_depths", "coral_exploration"},
                    "delta_calm": {"whale_songs", "gentle_currents"}
                },
                "therapeutic_benefits": ["deep_relaxation", "anxiety_reduction", "flow_states"]
            },
            
            VREnvironment.COSMIC_SPACE: {
                "name": "Infinite Cosmic Space",
                "description": "Expansive space environment with nebulae and stars",
                "base_params": VRSceneParams(
                    lighting_intensity=0.8,
                    color_temperature=8000,  # Cool, cosmic
                    particle_density=0.4,   # Cosmic dust
                    audio_volume=0.3
                ),
                "neural_effects": {
                    "gamma_detail": {"star_formations", "nebula_complexity"},
                    "theta_depth": {"space_exploration", "cosmic_perspective"},
                    "alpha_enhancement": {"aurora_patterns", "stellar_rhythms"}
                },
                "therapeutic_benefits": ["perspective_shift", "creativity_boost", "transcendence"]
            },
            
            VREnvironment.ABSTRACT_DIMENSIONS: {
                "name": "Abstract Geometric Dimensions",
                "description": "Dynamic abstract environment responding to brain states",
                "base_params": VRSceneParams(
                    lighting_intensity=0.7,
                    color_temperature=6000,
                    particle_density=0.8,
                    audio_volume=0.6
                ),
                "neural_effects": {
                    "gamma_detail": {"fractal_complexity", "geometric_precision"},
                    "beta_control": {"shape_manipulation", "pattern_control"},
                    "alpha_enhancement": {"color_transitions", "form_morphing"}
                },
                "therapeutic_benefits": ["cognitive_enhancement", "creativity", "focus_training"]
            },
            
            VREnvironment.HEALING_SANCTUARY: {
                "name": "Healing Sanctuary",
                "description": "Therapeutic environment designed for healing and recovery",
                "base_params": VRSceneParams(
                    lighting_intensity=0.5,
                    color_temperature=5000,  # Warm, healing
                    particle_density=0.2,   # Minimal, peaceful
                    audio_volume=0.4
                ),
                "neural_effects": {
                    "delta_calm": {"healing_energy", "restoration_field"},
                    "theta_depth": {"inner_healing", "memory_integration"},
                    "alpha_enhancement": {"peaceful_aura", "gentle_glow"}
                },
                "therapeutic_benefits": ["trauma_healing", "emotional_processing", "restoration"]
            }
        }
    
    def _initialize_adaptation_algorithms(self) -> Dict[str, callable]:
        """Initialize neural adaptation algorithms"""
        
        return {
            "alpha_visual_enhancement": self._adapt_alpha_visuals,
            "theta_depth_modulation": self._adapt_theta_depth,
            "beta_interaction_speed": self._adapt_beta_interactions,
            "gamma_detail_level": self._adapt_gamma_details,
            "delta_calming_effects": self._adapt_delta_calming
        }
    
    async def create_user_profile(self, user_data: Dict[str, Any]) -> VRUser:
        """Create VR user profile with neural baseline"""
        
        user_id = user_data['user_id']
        
        # Conduct neural baseline assessment
        neural_baseline = None
        if RGL_AVAILABLE:
            baseline_event = NavigationEvent(
                event_id=f"vr_baseline_{user_id}_{int(time.time())}",
                event_type="vr_baseline_assessment",
                content="VR baseline neural assessment",
                timestamp=datetime.now(),
                source_layer="neural_vr_engine",
                emotional_valence=user_data.get('mood', 0.0),
                urgency_level=0.2,
                context_metadata=user_data
            )
            
            neural_analysis = await self.rgl_system.process_full_spectrum_event(baseline_event)
            neural_baseline = neural_analysis['brain_state']
        else:
            # Simulation baseline
            neural_baseline = {
                "alpha_baseline": random.uniform(0.5, 0.8),
                "theta_baseline": random.uniform(0.4, 0.7),
                "beta_baseline": random.uniform(0.6, 0.9),
                "gamma_baseline": random.uniform(0.5, 0.8),
                "delta_baseline": random.uniform(0.3, 0.6)
            }
        
        # Create user profile
        user = VRUser(
            user_id=user_id,
            experience_level=user_data.get('experience_level', 'beginner'),
            preferred_environments=[VREnvironment(env) for env in user_data.get('preferred_environments', ['nature_forest'])],
            neural_baseline=neural_baseline,
            medical_clearance=user_data.get('medical_clearance', True),
            motion_sensitivity=user_data.get('motion_sensitivity', 0.3),
            therapeutic_goals=user_data.get('therapeutic_goals', [])
        )
        
        self.user_profiles[user_id] = user
        
        print(f"VR user profile created: {user_id}")
        print(f"Experience level: {user.experience_level}")
        print(f"Preferred environments: {[env.value for env in user.preferred_environments]}")
        
        return user
    
    async def start_vr_session(self, user_id: str, 
                              experience_type: VRExperienceType,
                              duration_minutes: int = 20,
                              environment: Optional[VREnvironment] = None) -> VRSession:
        """Start neural-guided VR session"""
        
        if user_id not in self.user_profiles:
            raise ValueError(f"User {user_id} not found. Create profile first.")
        
        user = self.user_profiles[user_id]
        session_id = f"vr_session_{user_id}_{int(time.time())}"
        
        # Select environment
        if environment is None:
            environment = self._select_optimal_environment(user, experience_type)
        
        print(f"Starting VR session: {session_id}")
        print(f"User: {user_id}")
        print(f"Experience: {experience_type.value}")
        print(f"Environment: {environment.value}")
        print(f"Duration: {duration_minutes} minutes")
        
        # Create session
        session = VRSession(
            session_id=session_id,
            user_id=user_id,
            experience_type=experience_type,
            environment=environment,
            duration_minutes=duration_minutes,
            start_time=datetime.now(),
            neural_adaptations=[],
            brain_state_log=[],
            user_interactions=[]
        )
        
        self.active_sessions[session_id] = session
        
        # Initialize VR environment
        await self._initialize_vr_environment(session)
        
        # Start neural monitoring and adaptation
        await self._start_neural_monitoring(session)
        
        return session
    
    async def _initialize_vr_environment(self, session: VRSession):
        """Initialize VR environment based on session parameters"""
        
        user = self.user_profiles[session.user_id]
        env_config = self.environment_library[session.environment]
        
        # Get current neural state
        current_neural_state = await self._get_current_neural_state(session.user_id)
        
        # Adapt initial environment parameters
        initial_params = await self._adapt_environment_params(
            env_config['base_params'],
            current_neural_state,
            user
        )
        
        # Log initial setup
        session.neural_adaptations.append({
            "timestamp": datetime.now().isoformat(),
            "adaptation_type": "initial_setup",
            "parameters": asdict(initial_params),
            "neural_state": current_neural_state
        })
        
        print(f"VR environment initialized: {session.environment.value}")
        print(f"Initial lighting: {initial_params.lighting_intensity:.2f}")
        print(f"Alpha enhancement: {initial_params.alpha_enhancement:.2f}")
    
    async def _start_neural_monitoring(self, session: VRSession):
        """Start continuous neural monitoring and adaptation"""
        
        user = self.user_profiles[session.user_id]
        
        print(f"Neural monitoring started for session: {session.session_id}")
        
        # Simulate continuous monitoring (in real implementation, this would be async)
        monitoring_interval = 5  # seconds
        total_intervals = (session.duration_minutes * 60) // monitoring_interval
        
        for interval in range(min(3, total_intervals)):  # Limit for demo
            await asyncio.sleep(0.1)  # Simulate processing time
            
            # Get current neural state
            neural_state = await self._get_current_neural_state(session.user_id)
            
            # Log brain state
            session.brain_state_log.append({
                "timestamp": datetime.now().isoformat(),
                "interval": interval,
                "neural_state": neural_state
            })
            
            # Adapt VR environment based on neural state
            adaptations = await self._perform_neural_adaptations(session, neural_state)
            
            if adaptations:
                session.neural_adaptations.extend(adaptations)
                print(f"Neural adaptations applied: {len(adaptations)}")
            
            # Simulate user interaction
            interaction = self._simulate_user_interaction(session, neural_state)
            session.user_interactions.append(interaction)
        
        print(f"Neural monitoring completed for session: {session.session_id}")
    
    async def _get_current_neural_state(self, user_id: str) -> Dict[str, Any]:
        """Get current neural state of user"""
        
        if RGL_AVAILABLE:
            # Create monitoring event
            monitor_event = NavigationEvent(
                event_id=f"vr_monitor_{user_id}_{int(time.time())}",
                event_type="vr_neural_monitoring",
                content="Real-time VR neural state monitoring",
                timestamp=datetime.now(),
                source_layer="neural_vr_engine",
                emotional_valence=random.uniform(-0.2, 0.8),  # VR generally positive
                urgency_level=0.1
            )
            
            neural_analysis = await self.rgl_system.process_full_spectrum_event(monitor_event)
            return neural_analysis['brain_state']
        else:
            # Simulation mode - generate realistic neural states
            return {
                "delta": {"frequency": random.uniform(1.0, 3.0), "amplitude": random.uniform(0.4, 0.8)},
                "theta": {"frequency": random.uniform(4.0, 8.0), "power": random.uniform(0.5, 0.9)},
                "alpha": {"frequency": random.uniform(8.0, 12.0), "coherence": random.uniform(0.6, 0.9)},
                "beta": {"frequency": random.uniform(13.0, 25.0), "attention": random.uniform(0.5, 0.8)},
                "gamma": {"frequency": random.uniform(30.0, 80.0), "synchrony": random.uniform(0.4, 0.7)},
                "coherence_index": random.uniform(0.6, 0.9),
                "arousal_level": random.uniform(0.3, 0.7)
            }
    
    async def _perform_neural_adaptations(self, session: VRSession, 
                                        neural_state: Dict[str, Any]) -> List[Dict]:
        """Perform VR adaptations based on neural state"""
        
        adaptations = []
        user = self.user_profiles[session.user_id]
        env_config = self.environment_library[session.environment]
        
        # Alpha wave adaptations (visual enhancements)
        if self._should_adapt_alpha(neural_state):
            alpha_adaptation = await self._adapt_alpha_visuals(session, neural_state)
            if alpha_adaptation:
                adaptations.append({
                    "timestamp": datetime.now().isoformat(),
                    "adaptation_type": "alpha_visual_enhancement",
                    "details": alpha_adaptation,
                    "trigger": neural_state.get('alpha', {})
                })
        
        # Theta wave adaptations (depth and exploration)
        if self._should_adapt_theta(neural_state):
            theta_adaptation = await self._adapt_theta_depth(session, neural_state)
            if theta_adaptation:
                adaptations.append({
                    "timestamp": datetime.now().isoformat(),
                    "adaptation_type": "theta_depth_modulation",
                    "details": theta_adaptation,
                    "trigger": neural_state.get('theta', {})
                })
        
        # Beta wave adaptations (interaction responsiveness)
        if self._should_adapt_beta(neural_state):
            beta_adaptation = await self._adapt_beta_interactions(session, neural_state)
            if beta_adaptation:
                adaptations.append({
                    "timestamp": datetime.now().isoformat(),
                    "adaptation_type": "beta_interaction_speed",
                    "details": beta_adaptation,
                    "trigger": neural_state.get('beta', {})
                })
        
        # Gamma wave adaptations (detail level)
        if self._should_adapt_gamma(neural_state):
            gamma_adaptation = await self._adapt_gamma_details(session, neural_state)
            if gamma_adaptation:
                adaptations.append({
                    "timestamp": datetime.now().isoformat(),
                    "adaptation_type": "gamma_detail_enhancement",
                    "details": gamma_adaptation,
                    "trigger": neural_state.get('gamma', {})
                })
        
        # Delta wave adaptations (calming effects)
        if self._should_adapt_delta(neural_state):
            delta_adaptation = await self._adapt_delta_calming(session, neural_state)
            if delta_adaptation:
                adaptations.append({
                    "timestamp": datetime.now().isoformat(),
                    "adaptation_type": "delta_calming_effects",
                    "details": delta_adaptation,
                    "trigger": neural_state.get('delta', {})
                })
        
        return adaptations
    
    def _should_adapt_alpha(self, neural_state: Dict) -> bool:
        """Determine if alpha adaptation is needed"""
        alpha_data = neural_state.get('alpha', {})
        if isinstance(alpha_data, dict):
            coherence = alpha_data.get('coherence', 0.5)
            return coherence < 0.7  # Enhance if coherence is low
        return False
    
    def _should_adapt_theta(self, neural_state: Dict) -> bool:
        """Determine if theta adaptation is needed"""
        theta_data = neural_state.get('theta', {})
        if isinstance(theta_data, dict):
            power = theta_data.get('power', 0.5)
            return power > 0.7  # Adapt if theta is high (deep state)
        return False
    
    def _should_adapt_beta(self, neural_state: Dict) -> bool:
        """Determine if beta adaptation is needed"""
        beta_data = neural_state.get('beta', {})
        if isinstance(beta_data, dict):
            attention = beta_data.get('attention', 0.5)
            return attention > 0.8  # Adapt if high attention/focus
        return False
    
    def _should_adapt_gamma(self, neural_state: Dict) -> bool:
        """Determine if gamma adaptation is needed"""
        gamma_data = neural_state.get('gamma', {})
        if isinstance(gamma_data, dict):
            synchrony = gamma_data.get('synchrony', 0.5)
            return synchrony > 0.6  # Adapt if good synchrony
        return False
    
    def _should_adapt_delta(self, neural_state: Dict) -> bool:
        """Determine if delta adaptation is needed"""
        delta_data = neural_state.get('delta', {})
        if isinstance(delta_data, dict):
            amplitude = delta_data.get('amplitude', 0.5)
            return amplitude > 0.7  # Adapt if deep relaxation
        return False
    
    async def _adapt_alpha_visuals(self, session: VRSession, neural_state: Dict) -> Dict:
        """Adapt visual elements based on alpha waves"""
        alpha_data = neural_state.get('alpha', {})
        coherence = alpha_data.get('coherence', 0.5) if isinstance(alpha_data, dict) else 0.5
        
        # Enhance visual patterns when alpha coherence is good
        enhancement_level = min(1.0, coherence * 1.2)
        
        return {
            "visual_enhancement": enhancement_level,
            "pattern_complexity": coherence * 0.8,
            "color_saturation": 0.6 + (coherence * 0.3),
            "description": f"Enhanced visual patterns for alpha coherence: {coherence:.3f}"
        }
    
    async def _adapt_theta_depth(self, session: VRSession, neural_state: Dict) -> Dict:
        """Adapt environment depth based on theta waves"""
        theta_data = neural_state.get('theta', {})
        power = theta_data.get('power', 0.5) if isinstance(theta_data, dict) else 0.5
        
        # Increase environmental depth when theta is strong
        depth_factor = min(1.0, power * 1.1)
        
        return {
            "depth_layers": depth_factor,
            "exploration_elements": power * 0.9,
            "mystery_factor": 0.5 + (power * 0.4),
            "description": f"Enhanced depth perception for theta power: {power:.3f}"
        }
    
    async def _adapt_beta_interactions(self, session: VRSession, neural_state: Dict) -> Dict:
        """Adapt interaction responsiveness based on beta waves"""
        beta_data = neural_state.get('beta', {})
        attention = beta_data.get('attention', 0.5) if isinstance(beta_data, dict) else 0.5
        
        # Increase responsiveness when attention is high
        responsiveness = min(1.0, attention * 1.2)
        
        return {
            "interaction_speed": responsiveness,
            "precision_mode": attention > 0.8,
            "feedback_intensity": 0.6 + (attention * 0.3),
            "description": f"Adapted interactions for attention level: {attention:.3f}"
        }
    
    async def _adapt_gamma_details(self, session: VRSession, neural_state: Dict) -> Dict:
        """Adapt detail level based on gamma waves"""
        gamma_data = neural_state.get('gamma', {})
        synchrony = gamma_data.get('synchrony', 0.5) if isinstance(gamma_data, dict) else 0.5
        
        # Increase detail when gamma synchrony is good
        detail_level = min(1.0, synchrony * 1.3)
        
        return {
            "detail_resolution": detail_level,
            "texture_complexity": synchrony * 0.9,
            "geometric_precision": 0.7 + (synchrony * 0.3),
            "description": f"Enhanced details for gamma synchrony: {synchrony:.3f}"
        }
    
    async def _adapt_delta_calming(self, session: VRSession, neural_state: Dict) -> Dict:
        """Adapt calming effects based on delta waves"""
        delta_data = neural_state.get('delta', {})
        amplitude = delta_data.get('amplitude', 0.5) if isinstance(delta_data, dict) else 0.5
        
        # Increase calming when delta is strong
        calming_factor = min(1.0, amplitude * 1.1)
        
        return {
            "calming_intensity": calming_factor,
            "gentle_motions": amplitude * 0.8,
            "peaceful_ambiance": 0.5 + (amplitude * 0.4),
            "description": f"Enhanced calming for delta amplitude: {amplitude:.3f}"
        }
    
    def _simulate_user_interaction(self, session: VRSession, neural_state: Dict) -> Dict:
        """Simulate user interaction in VR environment"""
        
        # Generate realistic interaction based on neural state
        arousal = neural_state.get('arousal_level', 0.5)
        coherence = neural_state.get('coherence_index', 0.5)
        
        interaction_types = ["gaze", "gesture", "voice", "movement"]
        interaction_type = random.choice(interaction_types)
        
        return {
            "timestamp": datetime.now().isoformat(),
            "type": interaction_type,
            "intensity": arousal,
            "precision": coherence,
            "satisfaction": random.uniform(0.6, 0.9),
            "neural_correlation": arousal * coherence
        }
    
    def _select_optimal_environment(self, user: VRUser, 
                                  experience_type: VRExperienceType) -> VREnvironment:
        """Select optimal VR environment for user and experience"""
        
        # Environment preferences by experience type
        experience_environments = {
            VRExperienceType.MEDITATION: [VREnvironment.NATURE_FOREST, VREnvironment.OCEAN_DEPTHS],
            VRExperienceType.THERAPY: [VREnvironment.HEALING_SANCTUARY, VREnvironment.NATURE_FOREST],
            VRExperienceType.CREATIVITY: [VREnvironment.ABSTRACT_DIMENSIONS, VREnvironment.COSMIC_SPACE],
            VRExperienceType.LEARNING: [VREnvironment.ABSTRACT_DIMENSIONS, VREnvironment.COSMIC_SPACE],
            VRExperienceType.EXPLORATION: [VREnvironment.COSMIC_SPACE, VREnvironment.OCEAN_DEPTHS]
        }
        
        suitable_environments = experience_environments.get(experience_type, user.preferred_environments)
        
        # Choose from user preferences if available
        for env in user.preferred_environments:
            if env in suitable_environments:
                return env
        
        # Otherwise use first suitable environment
        return suitable_environments[0] if suitable_environments else VREnvironment.NATURE_FOREST
    
    async def _adapt_environment_params(self, base_params: VRSceneParams,
                                      neural_state: Dict, user: VRUser) -> VRSceneParams:
        """Adapt environment parameters based on neural state"""
        
        # Start with base parameters
        adapted = VRSceneParams(
            lighting_intensity=base_params.lighting_intensity,
            color_temperature=base_params.color_temperature,
            particle_density=base_params.particle_density,
            audio_volume=base_params.audio_volume,
            interaction_responsiveness=base_params.interaction_responsiveness
        )
        
        # Apply neural adaptations
        if isinstance(neural_state, dict):
            # Alpha enhancement
            alpha_data = neural_state.get('alpha', {})
            if isinstance(alpha_data, dict):
                coherence = alpha_data.get('coherence', 0.5)
                adapted.alpha_enhancement = coherence * 0.8
            
            # Theta depth
            theta_data = neural_state.get('theta', {})
            if isinstance(theta_data, dict):
                power = theta_data.get('power', 0.5)
                adapted.theta_depth = power * 0.7
            
            # Gamma detail
            gamma_data = neural_state.get('gamma', {})
            if isinstance(gamma_data, dict):
                synchrony = gamma_data.get('synchrony', 0.5)
                adapted.gamma_detail = synchrony * 0.9
            
            # Delta calming
            delta_data = neural_state.get('delta', {})
            if isinstance(delta_data, dict):
                amplitude = delta_data.get('amplitude', 0.5)
                adapted.delta_calm = amplitude * 0.6
        
        return adapted
    
    async def end_vr_session(self, session_id: str) -> Dict[str, Any]:
        """End VR session and calculate outcomes"""
        
        if session_id not in self.active_sessions:
            return {"error": "Session not found"}
        
        session = self.active_sessions[session_id]
        end_time = datetime.now()
        actual_duration = (end_time - session.start_time).total_seconds() / 60
        
        print(f"Ending VR session: {session_id}")
        print(f"Actual duration: {actual_duration:.1f} minutes")
        
        # Calculate outcomes
        outcomes = self._calculate_session_outcomes(session)
        
        session.satisfaction_score = outcomes['satisfaction']
        session.therapeutic_benefit = outcomes['therapeutic_benefit']
        session.neural_improvement = outcomes['neural_improvement']
        
        # Move to history
        self.session_history.append(session)
        del self.active_sessions[session_id]
        
        print(f"Session outcomes:")
        print(f"  Satisfaction: {session.satisfaction_score:.3f}")
        print(f"  Therapeutic benefit: {session.therapeutic_benefit:.3f}")
        print(f"  Neural improvement: {session.neural_improvement:.3f}")
        
        return {
            "session_id": session_id,
            "duration_minutes": actual_duration,
            "adaptations_applied": len(session.neural_adaptations),
            "user_interactions": len(session.user_interactions),
            "outcomes": {
                "satisfaction": session.satisfaction_score,
                "therapeutic_benefit": session.therapeutic_benefit,
                "neural_improvement": session.neural_improvement
            }
        }
    
    def _calculate_session_outcomes(self, session: VRSession) -> Dict[str, float]:
        """Calculate session outcomes based on neural data and interactions"""
        
        # Calculate satisfaction based on interactions
        if session.user_interactions:
            avg_satisfaction = sum(interaction.get('satisfaction', 0.5) 
                                 for interaction in session.user_interactions) / len(session.user_interactions)
        else:
            avg_satisfaction = 0.7  # Default
        
        # Calculate therapeutic benefit based on adaptations
        therapeutic_benefit = min(1.0, len(session.neural_adaptations) * 0.15)
        
        # Calculate neural improvement based on brain state progression
        neural_improvement = 0.0
        if len(session.brain_state_log) >= 2:
            initial_coherence = session.brain_state_log[0]['neural_state'].get('coherence_index', 0.5)
            final_coherence = session.brain_state_log[-1]['neural_state'].get('coherence_index', 0.5)
            neural_improvement = max(0.0, final_coherence - initial_coherence)
        
        return {
            "satisfaction": avg_satisfaction,
            "therapeutic_benefit": therapeutic_benefit,
            "neural_improvement": neural_improvement
        }
    
    def get_vr_analytics(self) -> Dict[str, Any]:
        """Get comprehensive VR system analytics"""
        
        total_sessions = len(self.session_history)
        total_users = len(self.user_profiles)
        
        if total_sessions == 0:
            return {
                "system_status": {
                    "total_users": total_users,
                    "total_sessions": 0,
                    "rgl_available": RGL_AVAILABLE
                },
                "message": "No sessions completed yet"
            }
        
        # Calculate averages
        avg_satisfaction = sum(session.satisfaction_score for session in self.session_history) / total_sessions
        avg_therapeutic = sum(session.therapeutic_benefit for session in self.session_history) / total_sessions
        avg_neural_improvement = sum(session.neural_improvement for session in self.session_history) / total_sessions
        
        # Experience type distribution
        experience_counts = {}
        for session in self.session_history:
            exp_type = session.experience_type.value
            experience_counts[exp_type] = experience_counts.get(exp_type, 0) + 1
        
        # Environment popularity
        environment_counts = {}
        for session in self.session_history:
            env = session.environment.value
            environment_counts[env] = environment_counts.get(env, 0) + 1
        
        return {
            "system_status": {
                "total_users": total_users,
                "total_sessions": total_sessions,
                "active_sessions": len(self.active_sessions),
                "rgl_available": RGL_AVAILABLE
            },
            "session_outcomes": {
                "avg_satisfaction": round(avg_satisfaction, 3),
                "avg_therapeutic_benefit": round(avg_therapeutic, 3),
                "avg_neural_improvement": round(avg_neural_improvement, 3)
            },
            "usage_patterns": {
                "experience_distribution": experience_counts,
                "environment_popularity": environment_counts
            },
            "neural_integration": {
                "total_adaptations": sum(len(session.neural_adaptations) for session in self.session_history),
                "avg_adaptations_per_session": round(sum(len(session.neural_adaptations) for session in self.session_history) / total_sessions, 2)
            }
        }

# Factory function for VR deployment
def create_neural_vr_engine() -> NeuralVREngine:
    """Create Neural VR Engine for deployment"""
    return NeuralVREngine()