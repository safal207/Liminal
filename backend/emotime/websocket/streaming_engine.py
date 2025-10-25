"""
⚡🧠 Real-time Emotional Streaming Engine — OpenAI/DeepMind practices

World-class real-time emotional intelligence streaming:
- OpenAI: Safety-first real-time AI
- DeepMind: Multi-modal real-time processing  
- Stanford: Adaptive streaming algorithms
- Google: Scalable real-time systems
"""

import asyncio
import time
import json
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, asdict
from collections import deque
import uuid

from ..core import EmotimeEngine
from ..sensors import SensorData, SensorType
from ..modes import ModeType
from ..utils import safe_logger
from .connection_manager import EmotionalUpdate, get_connection_manager


@dataclass
class StreamingMetrics:
    """Метрики потокового вещания."""
    user_id: str
    session_id: str
    start_time: datetime
    updates_sent: int = 0
    bytes_streamed: int = 0
    average_latency: float = 0.0
    quality_score: float = 1.0
    dropped_frames: int = 0
    adaptive_adjustments: int = 0


@dataclass
class StreamingConfig:
    """Конфигурация потокового вещания."""
    update_frequency: float = 1.0  # updates per second
    quality_level: str = "high"  # high, medium, low
    adaptive_quality: bool = True
    safety_filtering: bool = True
    ml_insights_enabled: bool = True
    compression_enabled: bool = False


class AdaptiveQualityController:
    """
    Stanford-style adaptive quality controller.
    
    Автоматически адаптирует качество потока на основе
    производительности сети и системы.
    """
    
    def __init__(self):
        self.current_quality = "high"
        self.latency_history = deque(maxlen=50)
        self.bandwidth_history = deque(maxlen=50)
        self.adjustment_threshold = 100  # ms
        
    def record_performance(self, latency_ms: float, bandwidth_bps: float):
        """Записывает метрики производительности."""
        self.latency_history.append(latency_ms)
        self.bandwidth_history.append(bandwidth_bps)
    
    def should_adjust_quality(self) -> Optional[str]:
        """Определяет нужно ли изменить качество."""
        if len(self.latency_history) < 10:
            return None
            
        avg_latency = sum(self.latency_history) / len(self.latency_history)
        
        if avg_latency > 150 and self.current_quality != "low":
            # High latency - reduce quality
            if self.current_quality == "high":
                return "medium"
            elif self.current_quality == "medium":
                return "low"
        elif avg_latency < 50 and self.current_quality != "high":
            # Low latency - can increase quality
            if self.current_quality == "low":
                return "medium"
            elif self.current_quality == "medium":
                return "high"
        
        return None
    
    def get_quality_config(self, quality: str) -> Dict[str, Any]:
        """Возвращает конфигурацию для качества."""
        configs = {
            "high": {
                "update_frequency": 2.0,
                "feature_detail": "full",
                "ml_insights": True,
                "compression": False
            },
            "medium": {
                "update_frequency": 1.0,
                "feature_detail": "reduced",
                "ml_insights": True,
                "compression": True
            },
            "low": {
                "update_frequency": 0.5,
                "feature_detail": "minimal",
                "ml_insights": False,
                "compression": True
            }
        }
        return configs.get(quality, configs["medium"])


class SafetyFilter:
    """
    OpenAI-style safety filtering для эмоциональных данных.
    """
    
    def __init__(self):
        self.safety_rules = {
            "extreme_emotions": {
                "stress_threshold": 0.9,
                "trigger_action": "moderate"
            },
            "rapid_changes": {
                "change_threshold": 0.7,
                "time_window": 5.0,  # seconds
                "trigger_action": "smooth"
            },
            "privacy_protection": {
                "sensitive_features": ["personal_markers"],
                "trigger_action": "filter"
            }
        }
        
        self.emotion_history = deque(maxlen=20)
    
    def filter_update(self, update: EmotionalUpdate) -> EmotionalUpdate:
        """Применяет safety filtering к обновлению."""
        filtered_update = update
        
        # Check for extreme emotions
        if update.features.get("stress_intensity", 0) > 0.9:
            filtered_update.safety_status = "moderated"
            # Moderate extreme values
            if "stress" in update.features:
                update.features["stress"] = min(update.features["stress"], 0.8)
        
        # Check for rapid emotional changes
        if len(self.emotion_history) > 0:
            last_emotion = self.emotion_history[-1]
            valence_change = abs(update.features.get("valence", 0.5) - 
                               last_emotion.get("valence", 0.5))
            
            if valence_change > 0.7:
                filtered_update.safety_status = "smoothed"
                # Apply smoothing
                smooth_factor = 0.7
                for key in ["valence", "arousal", "dominance"]:
                    if key in update.features and key in last_emotion:
                        old_val = last_emotion[key]
                        new_val = update.features[key]
                        update.features[key] = old_val * smooth_factor + new_val * (1 - smooth_factor)
        
        # Store for history
        self.emotion_history.append(update.features.copy())
        
        return filtered_update


class RealTimeEmotionalStreamer:
    """
    World-class real-time emotional streaming engine.
    
    Best practices from:
    - OpenAI: Safety-first streaming
    - DeepMind: Multi-modal coordination
    - Stanford: Adaptive algorithms
    - Google: Scalable architecture
    """
    
    def __init__(self, user_id: str):
        self.user_id = user_id
        self.session_id = f"stream_{uuid.uuid4()}"
        
        # Core components
        self.emotime_engine: Optional[EmotimeEngine] = None
        self.connection_manager = get_connection_manager()
        
        # AI Lab features
        self.quality_controller = AdaptiveQualityController()
        self.safety_filter = SafetyFilter()
        
        # Configuration
        self.config = StreamingConfig()
        self.metrics = StreamingMetrics(
            user_id=user_id,
            session_id=self.session_id,
            start_time=datetime.now()
        )
        
        # Streaming state
        self.is_streaming = False
        self.stream_task: Optional[asyncio.Task] = None
        self.last_update = datetime.now()
        
        # Performance optimization
        self.update_queue = asyncio.Queue()
        self.batch_processor: Optional[asyncio.Task] = None
        
        safe_logger.info(f"Real-time streamer initialized for user {user_id}")
    
    async def start_streaming(self, engine: EmotimeEngine):
        """
        Запускает real-time streaming с world-class practices.
        """
        if self.is_streaming:
            safe_logger.warning("Streaming already active")
            return
        
        self.emotime_engine = engine
        self.is_streaming = True
        
        # Start streaming task
        self.stream_task = asyncio.create_task(self._streaming_loop())
        
        # Start batch processor for performance
        self.batch_processor = asyncio.create_task(self._batch_processing_loop())
        
        safe_logger.info(f"Real-time streaming started for {self.user_id}")
    
    async def stop_streaming(self):
        """Останавливает потоковое вещание."""
        self.is_streaming = False
        
        if self.stream_task:
            self.stream_task.cancel()
            try:
                await self.stream_task
            except asyncio.CancelledError:
                pass
        
        if self.batch_processor:
            self.batch_processor.cancel()
            try:
                await self.batch_processor
            except asyncio.CancelledError:
                pass
        
        safe_logger.info(f"Streaming stopped for {self.user_id}")
    
    async def _streaming_loop(self):
        """
        Основной цикл потокового вещания.
        Stanford-style adaptive streaming.
        """
        while self.is_streaming:
            try:
                start_time = time.time()
                
                # Get current emotional state
                if self.emotime_engine:
                    current_state = await self.emotime_engine.get_current_state()
                    
                    if current_state:
                        # Create streaming update
                        update = await self._create_emotional_update(current_state)
                        
                        # Apply safety filtering (OpenAI practice)
                        filtered_update = self.safety_filter.filter_update(update)
                        
                        # Queue for batch processing
                        await self.update_queue.put(filtered_update)
                        
                        # Performance tracking
                        processing_time = (time.time() - start_time) * 1000
                        self.quality_controller.record_performance(processing_time, 1000)
                        
                        # Adaptive quality adjustment (Stanford practice)
                        new_quality = self.quality_controller.should_adjust_quality()
                        if new_quality and new_quality != self.quality_controller.current_quality:
                            await self._adjust_streaming_quality(new_quality)
                
                # Sleep based on update frequency
                sleep_time = 1.0 / self.config.update_frequency
                await asyncio.sleep(sleep_time)
                
            except Exception as e:
                safe_logger.error(f"Streaming loop error: {e}")
                await asyncio.sleep(1.0)  # Error recovery
    
    async def _batch_processing_loop(self):
        """
        Google-style batch processing для оптимизации производительности.
        """
        batch = []
        batch_size = 10
        batch_timeout = 0.1  # seconds
        
        while self.is_streaming:
            try:
                # Collect batch
                try:
                    update = await asyncio.wait_for(
                        self.update_queue.get(), 
                        timeout=batch_timeout
                    )
                    batch.append(update)
                    
                    if len(batch) >= batch_size:
                        await self._process_update_batch(batch)
                        batch = []
                        
                except asyncio.TimeoutError:
                    # Process partial batch on timeout
                    if batch:
                        await self._process_update_batch(batch)
                        batch = []
                
            except Exception as e:
                safe_logger.error(f"Batch processing error: {e}")
    
    async def _process_update_batch(self, batch: List[EmotionalUpdate]):
        """Обрабатывает batch обновлений."""
        for update in batch:
            try:
                await self.connection_manager.broadcast_emotional_update(
                    self.user_id, update
                )
                
                self.metrics.updates_sent += 1
                self.metrics.bytes_streamed += len(json.dumps(asdict(update)))
                
            except Exception as e:
                safe_logger.error(f"Failed to broadcast update: {e}")
    
    async def _create_emotional_update(self, current_state) -> EmotionalUpdate:
        """Создает emotional update с ML insights."""
        # Get ML insights if available
        ml_insights = None
        if (self.config.ml_insights_enabled and 
            hasattr(self.emotime_engine, 'get_emotional_insights')):
            try:
                insights = await self.emotime_engine.get_emotional_insights()
                ml_insights = {
                    "adaptive_calibration": insights.get("adaptive_calibration", {}),
                    "ml_analytics": insights.get("ml_analytics", {}),
                    "deep_learning": insights.get("deep_learning", {})
                }
            except Exception as e:
                safe_logger.warning(f"Failed to get ML insights: {e}")
        
        # Create resonance trace
        resonance_trace = []
        if current_state.resonance_trace:
            for point in current_state.resonance_trace[-5:]:  # Last 5 points
                resonance_trace.append({
                    "timestamp": point.timestamp.isoformat(),
                    "valence": point.valence,
                    "arousal": point.arousal,
                    "intensity": point.intensity
                })
        
        return EmotionalUpdate(
            timestamp=current_state.timestamp,
            user_id=self.user_id,
            session_id=self.session_id,
            mode={
                "name": current_state.mode.name,
                "type": current_state.mode.type.value,
                "intensity": current_state.mode.intensity,
                "confidence": current_state.mode.confidence,
                "description": current_state.mode.description
            },
            features=current_state.features.__dict__,
            confidence=current_state.confidence,
            resonance_trace=resonance_trace,
            ml_insights=ml_insights
        )
    
    async def _adjust_streaming_quality(self, new_quality: str):
        """Адаптирует качество потока."""
        quality_config = self.quality_controller.get_quality_config(new_quality)
        
        self.config.update_frequency = quality_config["update_frequency"]
        self.config.ml_insights_enabled = quality_config["ml_insights"]
        self.config.compression_enabled = quality_config["compression"]
        
        self.quality_controller.current_quality = new_quality
        self.metrics.adaptive_adjustments += 1
        
        safe_logger.info(f"Streaming quality adjusted to {new_quality}")
        
        # Notify client about quality change
        quality_message = {
            "type": "quality_adjustment",
            "new_quality": new_quality,
            "config": quality_config,
            "timestamp": datetime.now().isoformat()
        }
        
        await self.connection_manager.pool.broadcast_to_user(
            self.user_id, quality_message
        )
    
    def get_streaming_analytics(self) -> Dict[str, Any]:
        """Возвращает аналитику потокового вещания."""
        runtime = (datetime.now() - self.metrics.start_time).total_seconds()
        
        return {
            "user_id": self.user_id,
            "session_id": self.session_id,
            "runtime_seconds": runtime,
            "metrics": asdict(self.metrics),
            "config": asdict(self.config),
            "quality": {
                "current_level": self.quality_controller.current_quality,
                "average_latency": (sum(self.quality_controller.latency_history) / 
                                  max(len(self.quality_controller.latency_history), 1))
            },
            "performance": {
                "updates_per_second": self.metrics.updates_sent / max(runtime, 1),
                "bytes_per_second": self.metrics.bytes_streamed / max(runtime, 1),
                "dropped_frame_rate": self.metrics.dropped_frames / max(self.metrics.updates_sent, 1)
            }
        }
    
    async def handle_client_message(self, message: Dict[str, Any]):
        """Обрабатывает сообщения от клиента."""
        message_type = message.get("type")
        
        if message_type == "quality_preference":
            # Client quality preference
            preferred_quality = message.get("quality", "medium")
            if preferred_quality in ["high", "medium", "low"]:
                await self._adjust_streaming_quality(preferred_quality)
        
        elif message_type == "sensor_data":
            # Real-time sensor data from client
            if self.emotime_engine:
                sensor_data = message.get("data")
                if sensor_data:
                    # Process sensor data in real-time
                    await self._process_client_sensor_data(sensor_data)
        
        elif message_type == "feedback":
            # Emotional feedback for learning
            if self.emotime_engine and hasattr(self.emotime_engine, 'learn_from_feedback'):
                feedback = message.get("feedback")
                if feedback:
                    await self.emotime_engine.learn_from_feedback(
                        feedback.get("actual_emotion"),
                        feedback.get("context", {})
                    )
    
    async def _process_client_sensor_data(self, sensor_data: Dict[str, Any]):
        """Обрабатывает сенсорные данные от клиента в реальном времени."""
        try:
            # Convert to SensorData object
            from ..sensors import TextData, TouchData, AudioData
            
            sensor_type = SensorType(sensor_data.get("type", "text"))
            timestamp = datetime.now()
            
            if sensor_type == SensorType.TEXT:
                text = sensor_data.get("text", "")
                raw_data = TextData(
                    text=text,
                    word_count=len(text.split()),
                    char_count=len(text),
                    typing_speed=sensor_data.get("typing_speed"),
                    pause_duration=sensor_data.get("pause_duration")
                )
            elif sensor_type == SensorType.TOUCH:
                raw_data = TouchData(
                    pressure=sensor_data.get("pressure", 0.5),
                    duration=sensor_data.get("duration", 1.0),
                    frequency=sensor_data.get("frequency", 1.0),
                    pattern=sensor_data.get("pattern", "tap"),
                    coordinates=sensor_data.get("coordinates")
                )
            elif sensor_type == SensorType.AUDIO:
                raw_data = AudioData(
                    pitch_mean=sensor_data.get("pitch_mean", 0.5),
                    pitch_variance=sensor_data.get("pitch_variance", 0.2),
                    speech_rate=sensor_data.get("speech_rate", 120),
                    volume_level=sensor_data.get("volume_level", 0.7),
                    pause_ratio=sensor_data.get("pause_ratio", 0.15),
                    emotion_markers=sensor_data.get("emotion_markers", [])
                )
            else:
                return
            
            # Create SensorData
            from ..sensors import SensorData
            sensor_obj = SensorData(
                sensor_type=sensor_type,
                timestamp=timestamp,
                raw_data=raw_data,
                metadata=sensor_data.get("metadata", {})
            )
            
            # Process in engine
            await self.emotime_engine.process_sensor_data(sensor_obj)
            
        except Exception as e:
            safe_logger.error(f"Failed to process client sensor data: {e}")


# Global streamers registry
_active_streamers: Dict[str, RealTimeEmotionalStreamer] = {}

def get_streamer(user_id: str) -> RealTimeEmotionalStreamer:
    """Возвращает streamer для пользователя."""
    if user_id not in _active_streamers:
        _active_streamers[user_id] = RealTimeEmotionalStreamer(user_id)
    return _active_streamers[user_id]

def remove_streamer(user_id: str):
    """Удаляет streamer пользователя."""
    if user_id in _active_streamers:
        del _active_streamers[user_id]