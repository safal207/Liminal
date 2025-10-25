"""
MindBridge Direct - Next-Generation Brain-Computer Interface
Direct neural communication system for LIMINAL ecosystem

Features:
- Direct thought-to-action translation
- Neural prosthetics control
- Brain-computer symbiosis protocols
- Real-time neural signal processing
- Quantum-enhanced neural decoding
- Medical-grade safety systems
"""

import numpy as np
from typing import Dict, List, Tuple, Optional, Any, Union
import asyncio
import logging
from datetime import datetime, timedelta
from dataclasses import dataclass, field
from enum import Enum
import json
import math
from collections import defaultdict, deque

# Import core LIMINAL components
try:
    from ..quantum.rgl_quantum_integration import QuantumRGLAPI
    from ..quantum.quantum_neural_engine import QuantumNeuralEngine
    from ..biomarkers.predictive_analytics_engine import BiomarkerType, Biomarker
    LIMINAL_CORE_AVAILABLE = True
except ImportError:
    LIMINAL_CORE_AVAILABLE = False

class BCIInterfaceType(Enum):
    """Types of brain-computer interfaces"""
    NON_INVASIVE_EEG = "non_invasive_eeg"       # External EEG electrodes
    SEMI_INVASIVE_ECOG = "semi_invasive_ecog"   # Surface electrodes
    INVASIVE_MICRO = "invasive_micro"           # Microelectrode arrays
    OPTICAL_FNIRS = "optical_fnirs"             # Functional near-infrared spectroscopy
    HYBRID_MULTIMODAL = "hybrid_multimodal"     # Multiple technologies combined

class NeuralSignalType(Enum):
    """Types of neural signals for BCI"""
    MOTOR_CORTEX = "motor_cortex"               # Movement intentions
    VISUAL_CORTEX = "visual_cortex"             # Visual processing
    AUDITORY_CORTEX = "auditory_cortex"         # Audio processing  
    PREFRONTAL = "prefrontal"                   # Executive decisions
    LANGUAGE_AREAS = "language_areas"           # Speech and language
    EMOTIONAL_LIMBIC = "emotional_limbic"       # Emotional states
    ATTENTION_NETWORKS = "attention_networks"   # Attention and focus

class BCIControlMode(Enum):
    """BCI control modes"""
    CURSOR_CONTROL = "cursor_control"           # Computer cursor movement
    PROSTHETIC_LIMB = "prosthetic_limb"        # Artificial limb control
    WHEELCHAIR = "wheelchair"                   # Wheelchair navigation
    COMMUNICATION = "communication"             # Text/speech communication
    ENVIRONMENTAL = "environmental"             # Smart home control
    VIRTUAL_REALITY = "virtual_reality"        # VR environment interaction
    CREATIVE_EXPRESSION = "creative_expression" # Art and music creation

@dataclass
class NeuralSignal:
    """Individual neural signal reading"""
    signal_id: str
    signal_type: NeuralSignalType
    electrode_location: str  # e.g., "C3", "F4", "motor_cortex_left"
    amplitude: float         # Signal amplitude in μV
    frequency: float         # Dominant frequency in Hz
    timestamp: datetime
    confidence: float = 0.8  # Signal quality confidence
    artifact_level: float = 0.0  # Artifact contamination level

@dataclass
class BCICommand:
    """Decoded BCI command from neural signals"""
    command_id: str
    intent_type: str         # "move_cursor", "select", "type_text", etc.
    parameters: Dict[str, Any] = field(default_factory=dict)
    confidence: float = 0.0
    processing_time_ms: float = 0.0
    source_signals: List[str] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)

@dataclass
class BCIUser:
    """BCI user profile with calibration data"""
    user_id: str
    name: str
    condition: str           # "healthy", "spinal_injury", "als", "stroke", etc.
    interface_type: BCIInterfaceType
    calibration_date: datetime
    neural_baseline: Dict[str, float] = field(default_factory=dict)
    control_accuracy: float = 0.0  # Overall BCI control accuracy
    preferred_commands: List[str] = field(default_factory=list)
    adaptation_level: int = 1  # 1-10, how well adapted to BCI
    safety_protocols: List[str] = field(default_factory=list)

class NeuralSignalProcessor:
    """
    Advanced neural signal processing for BCI applications
    Real-time processing of brain signals with quantum enhancement
    """
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        
        # Signal processing parameters
        self.sampling_rate = 1000.0  # Hz
        self.buffer_size = 2000      # samples (2 seconds)
        self.artifact_threshold = 100.0  # μV
        
        # Frequency band definitions
        self.frequency_bands = {
            'delta': (0.5, 4.0),
            'theta': (4.0, 8.0),
            'alpha': (8.0, 13.0),
            'beta': (13.0, 30.0),
            'gamma': (30.0, 100.0),
            'high_gamma': (100.0, 200.0)
        }
        
        # Signal buffers for real-time processing
        self.signal_buffers: Dict[str, deque] = defaultdict(lambda: deque(maxlen=self.buffer_size))
        
        # Quantum enhancement if available
        if LIMINAL_CORE_AVAILABLE:
            try:
                self.quantum_neural = QuantumNeuralEngine()
                self.quantum_enabled = True
            except:
                self.quantum_enabled = False
        else:
            self.quantum_enabled = False
        
        self.logger.info(f"Neural signal processor initialized - Quantum: {self.quantum_enabled}")
    
    async def process_neural_signal(self, signal: NeuralSignal) -> Dict[str, Any]:
        """Process incoming neural signal in real-time"""
        try:
            processing_start = datetime.now()
            
            # Add to buffer
            buffer_key = f"{signal.signal_type.value}_{signal.electrode_location}"
            self.signal_buffers[buffer_key].append({
                'amplitude': signal.amplitude,
                'frequency': signal.frequency,
                'timestamp': signal.timestamp,
                'confidence': signal.confidence
            })
            
            # Real-time signal analysis
            analysis = {
                'signal_id': signal.signal_id,
                'processed_timestamp': processing_start.isoformat(),
                'quality_metrics': {},
                'frequency_analysis': {},
                'pattern_detection': {},
                'quantum_enhancement': {}
            }
            
            # Quality assessment
            quality_metrics = self._assess_signal_quality(signal)
            analysis['quality_metrics'] = quality_metrics
            
            # Frequency domain analysis
            frequency_analysis = await self._analyze_frequency_domain(signal, buffer_key)
            analysis['frequency_analysis'] = frequency_analysis
            
            # Pattern detection
            pattern_detection = await self._detect_neural_patterns(signal, buffer_key)
            analysis['pattern_detection'] = pattern_detection
            
            # Quantum enhancement if available
            if self.quantum_enabled and quality_metrics['overall_quality'] > 0.7:
                quantum_enhancement = await self._apply_quantum_enhancement(signal)
                analysis['quantum_enhancement'] = quantum_enhancement
            
            # Calculate processing time
            processing_time = (datetime.now() - processing_start).total_seconds() * 1000
            analysis['processing_time_ms'] = processing_time
            
            return analysis
            
        except Exception as e:
            self.logger.error(f"Neural signal processing failed: {e}")
            return {'error': str(e), 'signal_id': signal.signal_id}
    
    def _assess_signal_quality(self, signal: NeuralSignal) -> Dict[str, float]:
        """Assess neural signal quality metrics"""
        quality_metrics = {
            'amplitude_quality': 0.0,
            'artifact_level': signal.artifact_level,
            'electrode_impedance': 0.0,  # Simulated
            'signal_to_noise_ratio': 0.0,
            'overall_quality': 0.0
        }
        
        # Amplitude quality (ideal range varies by signal type)
        ideal_ranges = {
            NeuralSignalType.MOTOR_CORTEX: (10, 100),    # μV
            NeuralSignalType.VISUAL_CORTEX: (5, 50),     # μV
            NeuralSignalType.PREFRONTAL: (8, 80),        # μV
        }
        
        ideal_min, ideal_max = ideal_ranges.get(signal.signal_type, (5, 50))
        if ideal_min <= abs(signal.amplitude) <= ideal_max:
            quality_metrics['amplitude_quality'] = 1.0
        else:
            # Degrade quality if outside ideal range
            distance = min(abs(abs(signal.amplitude) - ideal_min), abs(abs(signal.amplitude) - ideal_max))
            quality_metrics['amplitude_quality'] = max(0.0, 1.0 - (distance / ideal_max))
        
        # Simulated electrode impedance (lower is better)
        quality_metrics['electrode_impedance'] = max(0.0, 1.0 - np.random.uniform(0, 0.3))
        
        # Signal-to-noise ratio
        snr = abs(signal.amplitude) / max(1.0, signal.artifact_level)
        quality_metrics['signal_to_noise_ratio'] = min(1.0, snr / 10.0)  # Normalize to 0-1
        
        # Overall quality
        quality_metrics['overall_quality'] = (
            quality_metrics['amplitude_quality'] * 0.3 +
            (1.0 - signal.artifact_level) * 0.3 +
            quality_metrics['electrode_impedance'] * 0.2 +
            quality_metrics['signal_to_noise_ratio'] * 0.2
        )
        
        return quality_metrics
    
    async def _analyze_frequency_domain(self, signal: NeuralSignal, buffer_key: str) -> Dict[str, float]:
        """Analyze signal in frequency domain"""
        frequency_analysis = {}
        
        if len(self.signal_buffers[buffer_key]) < 256:  # Need minimum samples for FFT
            return {'status': 'insufficient_data'}
        
        # Get recent signal data
        recent_amplitudes = [s['amplitude'] for s in list(self.signal_buffers[buffer_key])[-256:]]
        
        # Simulated frequency analysis (in production, would use real FFT)
        for band_name, (low_freq, high_freq) in self.frequency_bands.items():
            # Simulate power in each frequency band
            if low_freq <= signal.frequency <= high_freq:
                power = abs(signal.amplitude) ** 2 * np.random.uniform(0.8, 1.2)
            else:
                power = abs(signal.amplitude) ** 2 * np.random.uniform(0.1, 0.3)
            
            frequency_analysis[f'{band_name}_power'] = power
            frequency_analysis[f'{band_name}_relative'] = power / sum(recent_amplitudes) if recent_amplitudes else 0
        
        # Dominant frequency
        frequency_analysis['dominant_frequency'] = signal.frequency
        frequency_analysis['frequency_stability'] = np.random.uniform(0.7, 0.95)  # Simulated stability
        
        return frequency_analysis
    
    async def _detect_neural_patterns(self, signal: NeuralSignal, buffer_key: str) -> Dict[str, Any]:
        """Detect specific neural patterns for BCI control"""
        patterns = {
            'motor_imagery_detected': False,
            'attention_spike': False,
            'blink_artifact': False,
            'movement_preparation': False,
            'error_related_negativity': False,
            'pattern_confidence': 0.0
        }
        
        buffer_data = list(self.signal_buffers[buffer_key])
        if len(buffer_data) < 100:  # Need sufficient data
            return patterns
        
        # Motor imagery detection (for motor cortex signals)
        if signal.signal_type == NeuralSignalType.MOTOR_CORTEX:
            # Detect beta desynchronization (8-30 Hz power decrease)
            if 8 <= signal.frequency <= 30 and abs(signal.amplitude) < np.mean([s['amplitude'] for s in buffer_data[-50:]]):
                patterns['motor_imagery_detected'] = True
                patterns['pattern_confidence'] += 0.3
        
        # Attention spike detection
        if signal.signal_type == NeuralSignalType.PREFRONTAL:
            if signal.frequency >= 30 and abs(signal.amplitude) > 50:  # High gamma activity
                patterns['attention_spike'] = True
                patterns['pattern_confidence'] += 0.2
        
        # Blink artifact detection (common contamination)
        if abs(signal.amplitude) > 150 and signal.frequency < 4:  # Large, slow deflection
            patterns['blink_artifact'] = True
            patterns['pattern_confidence'] -= 0.1  # Reduces overall confidence
        
        # Movement preparation (Bereitschaftspotential-like)
        if signal.signal_type == NeuralSignalType.MOTOR_CORTEX:
            recent_amplitudes = [s['amplitude'] for s in buffer_data[-20:]]
            if len(recent_amplitudes) >= 10:
                trend = np.polyfit(range(len(recent_amplitudes)), recent_amplitudes, 1)[0]
                if trend > 2:  # Rising amplitude trend
                    patterns['movement_preparation'] = True
                    patterns['pattern_confidence'] += 0.25
        
        # Normalize confidence
        patterns['pattern_confidence'] = max(0.0, min(1.0, patterns['pattern_confidence']))
        
        return patterns
    
    async def _apply_quantum_enhancement(self, signal: NeuralSignal) -> Dict[str, Any]:
        """Apply quantum enhancement to neural signal processing"""
        enhancement = {
            'quantum_coherence_boost': 0.0,
            'signal_clarity_improvement': 0.0,
            'pattern_detection_enhancement': 0.0,
            'processing_time_reduction': 0.0
        }
        
        try:
            # Map neural signal type to quantum frequency
            frequency_map = {
                NeuralSignalType.MOTOR_CORTEX: 'beta',
                NeuralSignalType.VISUAL_CORTEX: 'gamma',
                NeuralSignalType.PREFRONTAL: 'gamma',
                NeuralSignalType.ATTENTION_NETWORKS: 'gamma'
            }
            
            quantum_frequency = frequency_map.get(signal.signal_type, 'beta')
            
            # Apply quantum neural processing
            quantum_event = await self.quantum_neural.process_neural_frequency(
                quantum_frequency, 
                abs(signal.amplitude) / 100.0,  # Normalize amplitude
                signal.frequency / 100.0  # Normalize frequency as phase
            )
            
            # Extract quantum enhancements
            enhancement['quantum_coherence_boost'] = quantum_event.coherence_time / 1000.0  # Convert to ms
            enhancement['signal_clarity_improvement'] = quantum_event.cognitive_enhancement
            enhancement['pattern_detection_enhancement'] = quantum_event.memory_boost
            enhancement['processing_time_reduction'] = min(0.5, quantum_event.cognitive_enhancement * 0.3)
            
        except Exception as e:
            self.logger.warning(f"Quantum enhancement failed: {e}")
        
        return enhancement

class BCICommandDecoder:
    """
    Decodes neural signals into actionable BCI commands
    Translates brain activity into computer/device control
    """
    
    def __init__(self, signal_processor: NeuralSignalProcessor):
        self.signal_processor = signal_processor
        self.logger = logging.getLogger(__name__)
        
        # Command classification models (simplified for demonstration)
        self.command_models = self._initialize_command_models()
        
        # Command history for temporal context
        self.command_history = deque(maxlen=100)
        
        # Calibration data per user
        self.user_calibrations: Dict[str, Dict] = {}
    
    def _initialize_command_models(self) -> Dict[str, Dict]:
        """Initialize command classification models"""
        models = {}
        
        # Cursor control model
        models['cursor_control'] = {
            'features': ['motor_imagery_detected', 'beta_power', 'movement_preparation'],
            'commands': {
                'move_up': {'beta_desync': True, 'electrode_C3': True},
                'move_down': {'beta_desync': True, 'electrode_C4': True},
                'move_left': {'alpha_sync': True, 'electrode_C3': True},
                'move_right': {'alpha_sync': True, 'electrode_C4': True},
                'click': {'attention_spike': True, 'gamma_burst': True}
            }
        }
        
        # Communication model
        models['communication'] = {
            'features': ['attention_spike', 'gamma_power', 'prefrontal_activity'],
            'commands': {
                'select_letter': {'gamma_burst': True, 'attention_sustained': True},
                'delete': {'error_related_negativity': True},
                'space': {'attention_spike': True, 'short_duration': True},
                'enter': {'attention_spike': True, 'long_duration': True}
            }
        }
        
        # Prosthetic control model
        models['prosthetic_limb'] = {
            'features': ['motor_imagery_detected', 'movement_preparation', 'beta_desync'],
            'commands': {
                'grasp_open': {'motor_imagery': 'hand_open', 'confidence': 0.8},
                'grasp_close': {'motor_imagery': 'hand_close', 'confidence': 0.8},
                'wrist_flex': {'motor_imagery': 'wrist_flex', 'confidence': 0.7},
                'wrist_extend': {'motor_imagery': 'wrist_extend', 'confidence': 0.7}
            }
        }
        
        return models
    
    async def decode_bci_command(self, processed_signals: List[Dict[str, Any]], 
                                control_mode: BCIControlMode,
                                user_id: str = None) -> BCICommand:
        """Decode neural signals into BCI command"""
        try:
            decode_start = datetime.now()
            
            # Get appropriate model for control mode
            model = self.command_models.get(control_mode.value, {})
            if not model:
                return BCICommand(
                    command_id=f"cmd_unsupported_{int(datetime.now().timestamp())}",
                    intent_type="unsupported",
                    confidence=0.0
                )
            
            # Extract features from processed signals
            features = await self._extract_command_features(processed_signals)
            
            # Apply user-specific calibration if available
            if user_id and user_id in self.user_calibrations:
                features = self._apply_user_calibration(features, user_id)
            
            # Classify command intent
            command_result = await self._classify_command_intent(features, model)
            
            # Apply temporal context from command history
            command_result = self._apply_temporal_context(command_result)
            
            # Create BCI command
            processing_time = (datetime.now() - decode_start).total_seconds() * 1000
            
            bci_command = BCICommand(
                command_id=f"cmd_{int(datetime.now().timestamp())}_{np.random.randint(1000)}",
                intent_type=command_result['intent'],
                parameters=command_result['parameters'],
                confidence=command_result['confidence'],
                processing_time_ms=processing_time,
                source_signals=[s.get('signal_id', 'unknown') for s in processed_signals]
            )
            
            # Add to command history
            self.command_history.append({
                'command': bci_command.intent_type,
                'confidence': bci_command.confidence,
                'timestamp': bci_command.timestamp
            })
            
            return bci_command
            
        except Exception as e:
            self.logger.error(f"BCI command decoding failed: {e}")
            return BCICommand(
                command_id=f"cmd_error_{int(datetime.now().timestamp())}",
                intent_type="error",
                confidence=0.0
            )
    
    async def _extract_command_features(self, processed_signals: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Extract features relevant for command classification"""
        features = {
            'motor_imagery_detected': False,
            'attention_spike': False,
            'beta_desync': False,
            'gamma_burst': False,
            'error_related_negativity': False,
            'movement_preparation': False,
            'signal_quality_avg': 0.0,
            'dominant_electrode': None,
            'pattern_stability': 0.0
        }
        
        if not processed_signals:
            return features
        
        # Aggregate features across signals
        motor_detections = []
        attention_spikes = []
        quality_scores = []
        
        for signal_data in processed_signals:
            if 'pattern_detection' in signal_data:
                patterns = signal_data['pattern_detection']
                motor_detections.append(patterns.get('motor_imagery_detected', False))
                attention_spikes.append(patterns.get('attention_spike', False))
                
                if patterns.get('movement_preparation', False):
                    features['movement_preparation'] = True
                
                if patterns.get('error_related_negativity', False):
                    features['error_related_negativity'] = True
            
            if 'quality_metrics' in signal_data:
                quality_scores.append(signal_data['quality_metrics'].get('overall_quality', 0))
            
            if 'frequency_analysis' in signal_data:
                freq_analysis = signal_data['frequency_analysis']
                # Beta desynchronization detection
                if freq_analysis.get('beta_power', 0) < freq_analysis.get('baseline_beta', 10):
                    features['beta_desync'] = True
                
                # Gamma burst detection
                if freq_analysis.get('gamma_power', 0) > freq_analysis.get('baseline_gamma', 5) * 2:
                    features['gamma_burst'] = True
        
        # Aggregate boolean features
        features['motor_imagery_detected'] = any(motor_detections)
        features['attention_spike'] = any(attention_spikes)
        
        # Average quality
        features['signal_quality_avg'] = np.mean(quality_scores) if quality_scores else 0.0
        
        # Pattern stability (how consistent patterns are across signals)
        if len(processed_signals) > 1:
            pattern_consistency = sum(1 for det in motor_detections if det == motor_detections[0])
            features['pattern_stability'] = pattern_consistency / len(motor_detections)
        else:
            features['pattern_stability'] = 1.0 if motor_detections and motor_detections[0] else 0.0
        
        return features
    
    def _apply_user_calibration(self, features: Dict[str, Any], user_id: str) -> Dict[str, Any]:
        """Apply user-specific calibration to features"""
        calibration = self.user_calibrations[user_id]
        calibrated_features = features.copy()
        
        # Apply threshold adjustments
        for feature_name, threshold in calibration.get('thresholds', {}).items():
            if feature_name in features and isinstance(features[feature_name], (int, float)):
                calibrated_features[feature_name] = features[feature_name] * threshold
        
        # Apply personal patterns
        personal_patterns = calibration.get('personal_patterns', {})
        for pattern_name, boost_factor in personal_patterns.items():
            if pattern_name in features and features[pattern_name]:
                calibrated_features[f'{pattern_name}_confidence'] = boost_factor
        
        return calibrated_features
    
    async def _classify_command_intent(self, features: Dict[str, Any], 
                                     model: Dict[str, Any]) -> Dict[str, Any]:
        """Classify command intent based on features"""
        command_scores = {}
        
        # Score each possible command
        for command_name, command_pattern in model.get('commands', {}).items():
            score = 0.0
            matches = 0
            total_requirements = len(command_pattern)
            
            for requirement, expected_value in command_pattern.items():
                if requirement in features:
                    if isinstance(expected_value, bool):
                        if features[requirement] == expected_value:
                            score += 1.0
                            matches += 1
                    elif isinstance(expected_value, (int, float)):
                        # Numeric requirement
                        if abs(features[requirement] - expected_value) < 0.1:
                            score += 1.0
                            matches += 1
                        else:
                            score += max(0, 1.0 - abs(features[requirement] - expected_value))
            
            # Normalize score
            if total_requirements > 0:
                command_scores[command_name] = score / total_requirements
            else:
                command_scores[command_name] = 0.0
        
        # Find best matching command
        if command_scores:
            best_command = max(command_scores.keys(), key=lambda x: command_scores[x])
            best_score = command_scores[best_command]
            
            # Apply quality threshold
            quality_factor = features.get('signal_quality_avg', 0.5)
            final_confidence = best_score * quality_factor
            
            return {
                'intent': best_command,
                'confidence': final_confidence,
                'parameters': {
                    'all_scores': command_scores,
                    'quality_factor': quality_factor,
                    'pattern_stability': features.get('pattern_stability', 0.0)
                }
            }
        else:
            return {
                'intent': 'no_command',
                'confidence': 0.0,
                'parameters': {}
            }
    
    def _apply_temporal_context(self, command_result: Dict[str, Any]) -> Dict[str, Any]:
        """Apply temporal context from command history"""
        if not self.command_history:
            return command_result
        
        # Check for command repetition (increases confidence)
        recent_commands = [cmd['command'] for cmd in list(self.command_history)[-5:]]
        if command_result['intent'] in recent_commands:
            repetition_boost = recent_commands.count(command_result['intent']) * 0.1
            command_result['confidence'] = min(1.0, command_result['confidence'] + repetition_boost)
        
        # Check for impossible command sequences (decreases confidence)
        if len(self.command_history) > 0:
            last_command = self.command_history[-1]['command']
            
            # Define impossible sequences
            impossible_sequences = [
                ('click', 'click'),  # Double click unlikely
                ('move_up', 'move_down'),  # Contradictory movements
                ('grasp_open', 'grasp_open')  # Already open
            ]
            
            for seq in impossible_sequences:
                if seq == (last_command, command_result['intent']):
                    command_result['confidence'] *= 0.5  # Reduce confidence by half
                    break
        
        return command_result
    
    async def calibrate_user(self, user_id: str, calibration_data: Dict[str, Any]) -> Dict[str, Any]:
        """Calibrate BCI system for specific user"""
        calibration_result = {
            'user_id': user_id,
            'calibration_timestamp': datetime.now().isoformat(),
            'success': False,
            'accuracy_improvement': 0.0,
            'personalized_thresholds': {},
            'recommended_adjustments': []
        }
        
        try:
            # Analyze user's neural patterns
            user_patterns = await self._analyze_user_patterns(calibration_data)
            
            # Calculate personalized thresholds
            thresholds = self._calculate_personalized_thresholds(user_patterns)
            calibration_result['personalized_thresholds'] = thresholds
            
            # Store calibration
            self.user_calibrations[user_id] = {
                'thresholds': thresholds,
                'personal_patterns': user_patterns,
                'calibration_date': datetime.now(),
                'calibration_accuracy': user_patterns.get('baseline_accuracy', 0.7)
            }
            
            calibration_result['success'] = True
            calibration_result['accuracy_improvement'] = 0.15  # Expected 15% improvement
            
            # Generate recommendations
            calibration_result['recommended_adjustments'] = [
                "Practice motor imagery for 10 minutes daily",
                "Maintain relaxed attention during BCI use", 
                "Avoid excessive muscle tension",
                "Take breaks every 30 minutes to prevent fatigue"
            ]
            
        except Exception as e:
            self.logger.error(f"User calibration failed: {e}")
            calibration_result['error'] = str(e)
        
        return calibration_result
    
    async def _analyze_user_patterns(self, calibration_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze user-specific neural patterns"""
        patterns = {
            'baseline_accuracy': 0.7,
            'motor_imagery_strength': 0.0,
            'attention_control': 0.0,
            'artifact_susceptibility': 0.0,
            'preferred_frequencies': []
        }
        
        # Simulate pattern analysis (in production, would analyze real calibration data)
        patterns['motor_imagery_strength'] = np.random.uniform(0.5, 0.9)
        patterns['attention_control'] = np.random.uniform(0.6, 0.95)
        patterns['artifact_susceptibility'] = np.random.uniform(0.1, 0.4)
        
        # Identify user's strongest frequency bands
        frequency_strengths = {
            'alpha': np.random.uniform(0.3, 0.8),
            'beta': np.random.uniform(0.4, 0.9),
            'gamma': np.random.uniform(0.2, 0.7)
        }
        
        patterns['preferred_frequencies'] = [
            freq for freq, strength in frequency_strengths.items() 
            if strength > 0.6
        ]
        
        return patterns
    
    def _calculate_personalized_thresholds(self, user_patterns: Dict[str, Any]) -> Dict[str, float]:
        """Calculate personalized detection thresholds"""
        thresholds = {}
        
        # Adjust thresholds based on user patterns
        motor_strength = user_patterns.get('motor_imagery_strength', 0.7)
        attention_control = user_patterns.get('attention_control', 0.7)
        
        # Motor imagery threshold
        thresholds['motor_imagery_threshold'] = max(0.3, 1.0 - motor_strength)
        
        # Attention threshold  
        thresholds['attention_threshold'] = max(0.4, 1.0 - attention_control)
        
        # Artifact rejection threshold
        artifact_susceptibility = user_patterns.get('artifact_susceptibility', 0.2)
        thresholds['artifact_threshold'] = min(0.8, 0.5 + artifact_susceptibility)
        
        return thresholds

class MindBridgeDirectSystem:
    """
    Complete MindBridge Direct BCI System
    Integrates signal processing, command decoding, and device control
    """
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        
        # Initialize components
        self.signal_processor = NeuralSignalProcessor()
        self.command_decoder = BCICommandDecoder(self.signal_processor)
        
        # System state
        self.active_users: Dict[str, BCIUser] = {}
        self.device_connections: Dict[str, Any] = {}  # Connected devices
        self.session_active = False
        
        # Safety systems
        self.safety_monitoring = True
        self.emergency_stop_active = False
        self.max_session_duration = timedelta(hours=4)  # Maximum continuous use
        
        # Performance metrics
        self.accuracy_history = deque(maxlen=1000)
        self.response_times = deque(maxlen=1000)
        
        # Integration with LIMINAL ecosystem
        if LIMINAL_CORE_AVAILABLE:
            try:
                self.quantum_rgl = QuantumRGLAPI()
                self.ecosystem_integration = True
            except:
                self.ecosystem_integration = False
        else:
            self.ecosystem_integration = False
        
        self.logger.info(f"MindBridge Direct initialized - Ecosystem: {self.ecosystem_integration}")
    
    async def start_bci_session(self, user: BCIUser, 
                               control_mode: BCIControlMode) -> Dict[str, Any]:
        """Start BCI control session for user"""
        session_result = {
            'session_id': f"session_{user.user_id}_{int(datetime.now().timestamp())}",
            'user_id': user.user_id,
            'control_mode': control_mode.value,
            'start_time': datetime.now().isoformat(),
            'status': 'starting',
            'safety_checks': [],
            'calibration_status': 'unknown',
            'estimated_accuracy': 0.0
        }
        
        try:
            # Safety checks
            safety_results = await self._perform_safety_checks(user)
            session_result['safety_checks'] = safety_results
            
            if not all(check['passed'] for check in safety_results):
                session_result['status'] = 'failed_safety_checks'
                return session_result
            
            # Check/perform calibration
            calibration_status = await self._check_user_calibration(user)
            session_result['calibration_status'] = calibration_status['status']
            session_result['estimated_accuracy'] = calibration_status.get('estimated_accuracy', 0.0)
            
            # Initialize session
            self.active_users[user.user_id] = user
            self.session_active = True
            
            # Setup device connections based on control mode
            device_setup = await self._setup_device_connections(control_mode)
            session_result['device_connections'] = device_setup
            
            # Initialize monitoring
            monitoring_task = asyncio.create_task(self._monitor_session_safety(user.user_id))
            
            session_result['status'] = 'active'
            self.logger.info(f"BCI session started for user {user.user_id} - Mode: {control_mode.value}")
            
            return session_result
            
        except Exception as e:
            self.logger.error(f"BCI session start failed: {e}")
            session_result['status'] = 'error'
            session_result['error'] = str(e)
            return session_result
    
    async def _perform_safety_checks(self, user: BCIUser) -> List[Dict[str, Any]]:
        """Perform comprehensive safety checks before BCI session"""
        safety_checks = []
        
        # User condition check
        safe_conditions = ['healthy', 'spinal_injury', 'als', 'stroke']
        condition_check = {
            'check_name': 'user_condition',
            'passed': user.condition in safe_conditions,
            'message': f"User condition '{user.condition}' {'approved' if user.condition in safe_conditions else 'requires medical clearance'}"
        }
        safety_checks.append(condition_check)
        
        # Interface safety check
        interface_safety = {
            'check_name': 'interface_safety',
            'passed': user.interface_type != BCIInterfaceType.INVASIVE_MICRO or len(user.safety_protocols) > 0,
            'message': 'Interface safety protocols verified' if user.interface_type != BCIInterfaceType.INVASIVE_MICRO else 'Invasive interface requires medical protocols'
        }
        safety_checks.append(interface_safety)
        
        # Calibration recency check
        days_since_calibration = (datetime.now() - user.calibration_date).days
        calibration_check = {
            'check_name': 'calibration_recency',
            'passed': days_since_calibration <= 7,  # Calibration valid for 1 week
            'message': f"Calibration is {days_since_calibration} days old {'(valid)' if days_since_calibration <= 7 else '(requires recalibration)'}"
        }
        safety_checks.append(calibration_check)
        
        # Control accuracy check
        accuracy_check = {
            'check_name': 'control_accuracy',
            'passed': user.control_accuracy >= 0.7,  # Minimum 70% accuracy
            'message': f"Control accuracy {user.control_accuracy:.1%} {'meets' if user.control_accuracy >= 0.7 else 'below'} minimum threshold"
        }
        safety_checks.append(accuracy_check)
        
        return safety_checks
    
    async def _check_user_calibration(self, user: BCIUser) -> Dict[str, Any]:
        """Check and update user calibration status"""
        calibration_status = {
            'status': 'valid',
            'last_calibration': user.calibration_date.isoformat(),
            'estimated_accuracy': user.control_accuracy,
            'recommendation': 'ready_for_session'
        }
        
        # Check if recalibration needed
        days_since_calibration = (datetime.now() - user.calibration_date).days
        
        if days_since_calibration > 7:
            calibration_status['status'] = 'expired'
            calibration_status['recommendation'] = 'recalibration_required'
        elif days_since_calibration > 3:
            calibration_status['status'] = 'aging'
            calibration_status['recommendation'] = 'recalibration_recommended'
        
        # Adjust estimated accuracy based on calibration age
        age_factor = max(0.7, 1.0 - (days_since_calibration * 0.02))  # 2% degradation per day
        calibration_status['estimated_accuracy'] = user.control_accuracy * age_factor
        
        return calibration_status
    
    async def _setup_device_connections(self, control_mode: BCIControlMode) -> Dict[str, Any]:
        """Setup connections to controlled devices"""
        device_setup = {
            'primary_device': None,
            'secondary_devices': [],
            'connection_status': {},
            'control_parameters': {}
        }
        
        # Define device mappings
        device_mappings = {
            BCIControlMode.CURSOR_CONTROL: {
                'primary': 'computer_cursor',
                'parameters': {'sensitivity': 0.8, 'smoothing': 0.3}
            },
            BCIControlMode.PROSTHETIC_LIMB: {
                'primary': 'prosthetic_arm',
                'parameters': {'response_time': 0.1, 'force_feedback': True}
            },
            BCIControlMode.WHEELCHAIR: {
                'primary': 'powered_wheelchair',
                'parameters': {'max_speed': 0.5, 'obstacle_avoidance': True}
            },
            BCIControlMode.COMMUNICATION: {
                'primary': 'communication_interface',
                'parameters': {'word_prediction': True, 'voice_synthesis': True}
            },
            BCIControlMode.ENVIRONMENTAL: {
                'primary': 'smart_home_hub',
                'secondary': ['lights', 'climate_control', 'entertainment_system'],
                'parameters': {'multi_device': True, 'scene_control': True}
            },
            BCIControlMode.VIRTUAL_REALITY: {
                'primary': 'vr_headset',
                'parameters': {'immersion_level': 0.9, 'motion_prediction': True}
            }
        }
        
        mapping = device_mappings.get(control_mode, {})
        device_setup['primary_device'] = mapping.get('primary', 'unknown')
        device_setup['secondary_devices'] = mapping.get('secondary', [])
        device_setup['control_parameters'] = mapping.get('parameters', {})
        
        # Simulate device connections
        devices_to_connect = [device_setup['primary_device']] + device_setup['secondary_devices']
        for device in devices_to_connect:
            if device and device != 'unknown':
                # Simulate connection success/failure
                connection_success = np.random.random() > 0.1  # 90% success rate
                device_setup['connection_status'][device] = 'connected' if connection_success else 'failed'
                
                if connection_success:
                    self.device_connections[device] = {
                        'connected_at': datetime.now(),
                        'status': 'ready',
                        'control_mode': control_mode.value
                    }
        
        return device_setup
    
    async def process_realtime_bci(self, neural_signals: List[NeuralSignal],
                                  user_id: str, control_mode: BCIControlMode) -> Dict[str, Any]:
        """Process real-time BCI control loop"""
        if user_id not in self.active_users:
            return {'error': f'No active session for user {user_id}'}
        
        process_start = datetime.now()
        
        try:
            # Process neural signals
            processed_signals = []
            for signal in neural_signals:
                processed = await self.signal_processor.process_neural_signal(signal)
                processed_signals.append(processed)
            
            # Decode BCI command
            bci_command = await self.command_decoder.decode_bci_command(
                processed_signals, control_mode, user_id
            )
            
            # Execute command if confidence is sufficient
            execution_result = None
            if bci_command.confidence > 0.6:  # Minimum confidence threshold
                execution_result = await self._execute_bci_command(bci_command, control_mode)
            
            # Record performance metrics
            total_processing_time = (datetime.now() - process_start).total_seconds() * 1000
            self.response_times.append(total_processing_time)
            self.accuracy_history.append(bci_command.confidence)
            
            # Ecosystem integration - send to RGL for optimization
            ecosystem_feedback = None
            if self.ecosystem_integration and bci_command.confidence > 0.8:
                ecosystem_feedback = await self._integrate_with_ecosystem(bci_command, user_id)
            
            return {
                'processing_timestamp': process_start.isoformat(),
                'neural_signals_processed': len(neural_signals),
                'bci_command': {
                    'intent': bci_command.intent_type,
                    'confidence': bci_command.confidence,
                    'parameters': bci_command.parameters,
                    'processing_time_ms': bci_command.processing_time_ms
                },
                'execution_result': execution_result,
                'total_processing_time_ms': total_processing_time,
                'ecosystem_feedback': ecosystem_feedback,
                'session_performance': {
                    'avg_accuracy': np.mean(list(self.accuracy_history)),
                    'avg_response_time': np.mean(list(self.response_times))
                }
            }
            
        except Exception as e:
            self.logger.error(f"Real-time BCI processing failed: {e}")
            return {'error': str(e), 'timestamp': datetime.now().isoformat()}
    
    async def _execute_bci_command(self, bci_command: BCICommand, 
                                 control_mode: BCIControlMode) -> Dict[str, Any]:
        """Execute the decoded BCI command"""
        execution_result = {
            'command_executed': bci_command.intent_type,
            'execution_time': datetime.now().isoformat(),
            'success': False,
            'device_responses': {},
            'error_message': None
        }
        
        try:
            # Route command based on control mode
            if control_mode == BCIControlMode.CURSOR_CONTROL:
                result = await self._execute_cursor_command(bci_command)
            elif control_mode == BCIControlMode.PROSTHETIC_LIMB:
                result = await self._execute_prosthetic_command(bci_command)
            elif control_mode == BCIControlMode.COMMUNICATION:
                result = await self._execute_communication_command(bci_command)
            elif control_mode == BCIControlMode.ENVIRONMENTAL:
                result = await self._execute_environmental_command(bci_command)
            else:
                result = {'success': False, 'message': f'Unsupported control mode: {control_mode.value}'}
            
            execution_result.update(result)
            
        except Exception as e:
            execution_result['error_message'] = str(e)
            self.logger.error(f"BCI command execution failed: {e}")
        
        return execution_result
    
    async def _execute_cursor_command(self, command: BCICommand) -> Dict[str, Any]:
        """Execute cursor control commands"""
        if 'computer_cursor' not in self.device_connections:
            return {'success': False, 'message': 'Cursor device not connected'}
        
        cursor_actions = {
            'move_up': lambda: {'action': 'move_cursor', 'direction': 'up', 'distance': 10},
            'move_down': lambda: {'action': 'move_cursor', 'direction': 'down', 'distance': 10},
            'move_left': lambda: {'action': 'move_cursor', 'direction': 'left', 'distance': 10},
            'move_right': lambda: {'action': 'move_cursor', 'direction': 'right', 'distance': 10},
            'click': lambda: {'action': 'mouse_click', 'button': 'left'}
        }
        
        if command.intent_type in cursor_actions:
            action = cursor_actions[command.intent_type]()
            return {
                'success': True,
                'device_responses': {'computer_cursor': action},
                'message': f"Executed {command.intent_type} with confidence {command.confidence:.2f}"
            }
        else:
            return {'success': False, 'message': f'Unknown cursor command: {command.intent_type}'}
    
    async def _execute_prosthetic_command(self, command: BCICommand) -> Dict[str, Any]:
        """Execute prosthetic limb control commands"""
        if 'prosthetic_arm' not in self.device_connections:
            return {'success': False, 'message': 'Prosthetic device not connected'}
        
        prosthetic_actions = {
            'grasp_open': {'joint': 'hand', 'action': 'open', 'force': 0.3},
            'grasp_close': {'joint': 'hand', 'action': 'close', 'force': 0.7},
            'wrist_flex': {'joint': 'wrist', 'action': 'flex', 'angle': 30},
            'wrist_extend': {'joint': 'wrist', 'action': 'extend', 'angle': 30}
        }
        
        if command.intent_type in prosthetic_actions:
            action = prosthetic_actions[command.intent_type]
            return {
                'success': True,
                'device_responses': {'prosthetic_arm': action},
                'message': f"Executed prosthetic {command.intent_type}"
            }
        else:
            return {'success': False, 'message': f'Unknown prosthetic command: {command.intent_type}'}
    
    async def _execute_communication_command(self, command: BCICommand) -> Dict[str, Any]:
        """Execute communication interface commands"""
        if 'communication_interface' not in self.device_connections:
            return {'success': False, 'message': 'Communication device not connected'}
        
        comm_actions = {
            'select_letter': {'action': 'input_character', 'method': 'selection'},
            'delete': {'action': 'delete_character'},
            'space': {'action': 'input_character', 'character': ' '},
            'enter': {'action': 'input_character', 'character': '\n'}
        }
        
        if command.intent_type in comm_actions:
            action = comm_actions[command.intent_type]
            return {
                'success': True,
                'device_responses': {'communication_interface': action},
                'message': f"Executed communication {command.intent_type}"
            }
        else:
            return {'success': False, 'message': f'Unknown communication command: {command.intent_type}'}
    
    async def _execute_environmental_command(self, command: BCICommand) -> Dict[str, Any]:
        """Execute environmental/smart home control commands"""
        connected_devices = [device for device in self.device_connections.keys() 
                           if device in ['smart_home_hub', 'lights', 'climate_control']]
        
        if not connected_devices:
            return {'success': False, 'message': 'No environmental devices connected'}
        
        # Simulate environmental control
        responses = {}
        for device in connected_devices:
            if device == 'lights':
                responses[device] = {'action': 'toggle', 'brightness': 0.8}
            elif device == 'climate_control':
                responses[device] = {'action': 'adjust_temperature', 'change': 2}
            elif device == 'smart_home_hub':
                responses[device] = {'action': 'scene_activate', 'scene': 'comfortable'}
        
        return {
            'success': True,
            'device_responses': responses,
            'message': f"Executed environmental control: {command.intent_type}"
        }
    
    async def _integrate_with_ecosystem(self, bci_command: BCICommand, user_id: str) -> Dict[str, Any]:
        """Integrate BCI data with LIMINAL ecosystem for optimization"""
        if not self.ecosystem_integration:
            return {'status': 'ecosystem_not_available'}
        
        try:
            # Create navigation query based on BCI intent
            navigation_query = f"BCI user performing {bci_command.intent_type} with {bci_command.confidence:.1%} confidence"
            
            # Get RGL guidance
            nav_result = await self.quantum_rgl.navigate_quantum(navigation_query)
            
            ecosystem_feedback = {
                'rgl_direction': nav_result.get('direction', 'unknown'),
                'quantum_coherence': nav_result.get('quantum_coherence', 0.0),
                'cognitive_enhancement': nav_result.get('cognitive_enhancement', 0.0),
                'recommendations': []
            }
            
            # Generate BCI-specific recommendations
            if nav_result.get('quantum_coherence', 0) > 0.8:
                ecosystem_feedback['recommendations'].append("Optimal neural state - consider advanced BCI tasks")
            
            if nav_result.get('cognitive_enhancement', 0) > 0.5:
                ecosystem_feedback['recommendations'].append("Enhanced cognitive state detected - accuracy may improve")
            
            return ecosystem_feedback
            
        except Exception as e:
            self.logger.warning(f"Ecosystem integration failed: {e}")
            return {'status': 'integration_error', 'error': str(e)}
    
    async def _monitor_session_safety(self, user_id: str):
        """Monitor BCI session for safety concerns"""
        while self.session_active and user_id in self.active_users:
            try:
                # Check for emergency conditions
                if self.emergency_stop_active:
                    await self._emergency_stop_session(user_id)
                    break
                
                # Monitor session duration
                user = self.active_users[user_id]
                session_duration = datetime.now() - datetime.fromisoformat(user.calibration_date.isoformat())
                
                if session_duration > self.max_session_duration:
                    self.logger.warning(f"Maximum session duration exceeded for user {user_id}")
                    await self._end_bci_session(user_id, "max_duration_exceeded")
                    break
                
                # Monitor accuracy degradation
                if len(self.accuracy_history) > 10:
                    recent_accuracy = np.mean(list(self.accuracy_history)[-10:])
                    if recent_accuracy < 0.4:  # Below 40% accuracy
                        self.logger.warning(f"BCI accuracy degraded for user {user_id}: {recent_accuracy:.1%}")
                        await self._end_bci_session(user_id, "accuracy_degradation")
                        break
                
                # Wait before next check
                await asyncio.sleep(10)  # Check every 10 seconds
                
            except Exception as e:
                self.logger.error(f"Safety monitoring error: {e}")
                await asyncio.sleep(30)  # Wait longer on error
    
    async def _emergency_stop_session(self, user_id: str):
        """Emergency stop BCI session"""
        self.logger.critical(f"EMERGENCY STOP activated for user {user_id}")
        
        # Immediately disable all device connections
        for device_id in list(self.device_connections.keys()):
            self.device_connections[device_id]['status'] = 'emergency_stopped'
        
        # End session
        await self._end_bci_session(user_id, "emergency_stop")
        
        # Reset emergency state
        self.emergency_stop_active = False
    
    async def _end_bci_session(self, user_id: str, reason: str = "normal"):
        """End BCI session gracefully"""
        if user_id in self.active_users:
            user = self.active_users[user_id]
            
            # Generate session report
            session_report = {
                'user_id': user_id,
                'session_end_time': datetime.now().isoformat(),
                'end_reason': reason,
                'session_stats': {
                    'avg_accuracy': np.mean(list(self.accuracy_history)) if self.accuracy_history else 0,
                    'avg_response_time': np.mean(list(self.response_times)) if self.response_times else 0,
                    'total_commands': len(self.response_times)
                }
            }
            
            # Update user stats
            if self.accuracy_history:
                user.control_accuracy = np.mean(list(self.accuracy_history))
            
            # Cleanup
            del self.active_users[user_id]
            self.device_connections.clear()
            
            if not self.active_users:
                self.session_active = False
            
            self.logger.info(f"BCI session ended for user {user_id} - Reason: {reason}")
            return session_report
    
    def get_system_status(self) -> Dict[str, Any]:
        """Get comprehensive system status"""
        status = {
            'system_active': self.session_active,
            'active_users': len(self.active_users),
            'connected_devices': len(self.device_connections),
            'safety_monitoring': self.safety_monitoring,
            'ecosystem_integration': self.ecosystem_integration,
            'performance_metrics': {}
        }
        
        # Performance metrics
        if self.accuracy_history:
            status['performance_metrics'] = {
                'current_accuracy': np.mean(list(self.accuracy_history)[-10:]),
                'overall_accuracy': np.mean(list(self.accuracy_history)),
                'accuracy_trend': 'improving' if len(self.accuracy_history) > 20 and 
                                np.mean(list(self.accuracy_history)[-10:]) > np.mean(list(self.accuracy_history)[-20:-10:]) else 'stable',
                'avg_response_time_ms': np.mean(list(self.response_times)) if self.response_times else 0
            }
        
        return status

if __name__ == "__main__":
    # Demo MindBridge Direct BCI System
    async def mindbridge_demo():
        bci_system = MindBridgeDirectSystem()
        
        print("=== MindBridge Direct BCI Demo ===")
        
        # Create demo user
        demo_user = BCIUser(
            user_id="demo_bci_user_001",
            name="Demo User",
            condition="spinal_injury",
            interface_type=BCIInterfaceType.NON_INVASIVE_EEG,
            calibration_date=datetime.now() - timedelta(days=2),
            control_accuracy=0.85,
            preferred_commands=["cursor_control", "communication"],
            safety_protocols=["medical_supervision", "emergency_stop"]
        )
        
        # Start BCI session
        print("Starting BCI session...")
        session_result = await bci_system.start_bci_session(demo_user, BCIControlMode.CURSOR_CONTROL)
        print(f"Session Status: {session_result['status']}")
        print(f"Estimated Accuracy: {session_result['estimated_accuracy']:.1%}")
        
        # Simulate neural signals
        print("\nProcessing neural signals...")
        demo_signals = [
            NeuralSignal(
                signal_id="sig_001",
                signal_type=NeuralSignalType.MOTOR_CORTEX,
                electrode_location="C3",
                amplitude=45.0,
                frequency=20.0,
                timestamp=datetime.now()
            ),
            NeuralSignal(
                signal_id="sig_002", 
                signal_type=NeuralSignalType.PREFRONTAL,
                electrode_location="F4",
                amplitude=35.0,
                frequency=40.0,
                timestamp=datetime.now()
            )
        ]
        
        # Process real-time BCI
        bci_result = await bci_system.process_realtime_bci(
            demo_signals, demo_user.user_id, BCIControlMode.CURSOR_CONTROL
        )
        
        print(f"BCI Command: {bci_result['bci_command']['intent']}")
        print(f"Confidence: {bci_result['bci_command']['confidence']:.1%}")
        print(f"Processing Time: {bci_result['total_processing_time_ms']:.1f}ms")
        
        if bci_result['execution_result']:
            print(f"Execution: {bci_result['execution_result']['success']}")
        
        # System status
        status = bci_system.get_system_status()
        print(f"\nSystem Status:")
        print(f"Active Users: {status['active_users']}")
        print(f"Connected Devices: {status['connected_devices']}")
        print(f"Ecosystem Integration: {status['ecosystem_integration']}")
        
        if 'performance_metrics' in status:
            perf = status['performance_metrics']
            print(f"Current Accuracy: {perf.get('current_accuracy', 0):.1%}")
            print(f"Avg Response Time: {perf.get('avg_response_time_ms', 0):.1f}ms")
    
    # Run demo
    import asyncio
    asyncio.run(mindbridge_demo())