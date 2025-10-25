"""
🌿✨ Emotime Core Engine — сердцебиение эмоционального времени

Центральный движок Emotime, координирующий все компоненты:
- Сенсорный слой (текст, касания, аудио)  
- Фьюжн признаков (валентность, возбуждение, темп)
- Временные ряды и режимы (спокойствие, фокус, стресс)
"""

import asyncio
import time
import numpy as np
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, asdict

from .sensors import SensorData
from .fusion import FeatureFusion, EmotionalFeatures
from .timeseries import EmotionalTimeSeries, EmotionalPoint
from .modes import EmotionalModes, EmotionalMode
from .metrics_integration import emotime_metrics
from .neo4j_storage import EmotimeNeo4jStorage
from .utils import safe_logger

# MIT Advanced ML components
try:
    from .ml import (
        get_calibrator, AdaptiveCalibrator,
        get_adaptive_engine, AdaptiveEmotionalEngine,
        get_feature_learner, DeepFeatureLearner,
        ADAPTIVE_ENGINE_AVAILABLE, FEATURE_LEARNING_AVAILABLE
    )
    ML_ENHANCED = True
except ImportError:
    ML_ENHANCED = False
    ADAPTIVE_ENGINE_AVAILABLE = False
    FEATURE_LEARNING_AVAILABLE = False
    safe_logger.warning("ML enhancements not available - using classical algorithms")


@dataclass
class EmotimeState:
    """Текущее эмоциональное состояние системы."""
    timestamp: datetime
    features: EmotionalFeatures
    mode: EmotionalMode
    resonance_trace: List[EmotionalPoint]  # последние N точек
    confidence: float = 0.0


class EmotimeEngine:
    """
    Главный движок Emotime — сердце эмоционального времени.
    
    Этот класс координирует весь поток:
    сенсоры → фьюжн → временные ряды → режимы → резонансный след
    """
    
    def __init__(
        self,
        user_id: str = "default_user",
        session_id: str = None,
        trace_window: int = 100,  # размер окна резонансного следа
        update_interval: float = 1.0,  # интервал обновления в секундах
        enable_neo4j: bool = True,  # включить Neo4j хранение
        enable_ml: bool = True,  # включить MIT ML enhancements
    ):
        self.user_id = user_id
        self.session_id = session_id or f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        self.trace_window = trace_window
        self.update_interval = update_interval
        
        # Компоненты системы
        self.fusion = FeatureFusion()
        self.timeseries = EmotionalTimeSeries()
        self.modes = EmotionalModes()
        
        # Neo4j хранение
        self.storage = EmotimeNeo4jStorage() if enable_neo4j else None
        
        # MIT ML enhancements
        if enable_ml and ML_ENHANCED:
            # Калибратор всегда доступен (без sklearn)
            self.calibrator = get_calibrator(user_id)
            
            # Адаптивный движок если доступен sklearn
            if ADAPTIVE_ENGINE_AVAILABLE and get_adaptive_engine:
                self.adaptive_engine = get_adaptive_engine(user_id)
            else:
                self.adaptive_engine = None
                
            # Feature learner если доступен sklearn
            if FEATURE_LEARNING_AVAILABLE and get_feature_learner:
                self.feature_learner = get_feature_learner(user_id)
            else:
                self.feature_learner = None
            
            safe_logger.info(f"MIT ML enabled: calibrator=True, adaptive={self.adaptive_engine is not None}, features={self.feature_learner is not None}")
        else:
            self.calibrator = None
            self.adaptive_engine = None
            self.feature_learner = None
            if enable_ml:
                safe_logger.warning("ML requested but not available")
        
        # Текущее состояние
        self.current_state: Optional[EmotimeState] = None
        self.is_running = False
        
        # Буфер для накопления сенсорных данных
        self._sensor_buffer: List[SensorData] = []
        
        # Для связывания точек в последовательность
        self._last_point_id: Optional[str] = None
        
    async def start(self):
        """Запускает движок Emotime."""
        self.is_running = True
        safe_logger.info("Engine started - heartbeat begins")
        
        # Запускаем главный цикл обработки
        asyncio.create_task(self._heartbeat_loop())
    
    async def stop(self):
        """Останавливает движок Emotime."""
        self.is_running = False
        
        # Закрываем соединение с Neo4j
        if self.storage:
            self.storage.close()
            
        safe_logger.info("Engine stopped - heart stops")
    
    async def process_sensor_data(self, sensor_data: SensorData):
        """
        Обрабатывает новые данные с сенсоров.
        
        Args:
            sensor_data: Данные с сенсора (текст/касание/аудио)
        """
        self._sensor_buffer.append(sensor_data)
        
    async def get_current_state(self) -> Optional[EmotimeState]:
        """Возвращает текущее эмоциональное состояние."""
        return self.current_state
    
    async def get_resonance_trace(self, limit: int = None) -> List[EmotionalPoint]:
        """Возвращает резонансный след (историю эмоциональных точек)."""
        if not self.current_state:
            return []
        
        trace = self.current_state.resonance_trace
        if limit:
            return trace[-limit:]
        return trace
    
    async def _heartbeat_loop(self):
        """
        Главный цикл обработки — сердцебиение Emotime.
        
        Каждый удар:
        1. Собираем данные с сенсоров
        2. Выполняем фьюжн признаков  
        3. Обновляем временные ряды
        4. Определяем эмоциональный режим
        5. Обновляем резонансный след
        """
        while self.is_running:
            try:
                await self._process_heartbeat()
                await asyncio.sleep(self.update_interval)
                
            except Exception as e:
                safe_logger.error(f"Heartbeat error: {e}")
                await asyncio.sleep(self.update_interval)
    
    async def _process_heartbeat(self):
        """Обрабатывает один такт сердцебиения."""
        if not self._sensor_buffer:
            return
            
        # 1. Забираем данные из буфера
        sensor_data_batch = self._sensor_buffer.copy()
        self._sensor_buffer.clear()
        
        # 2. MIT Enhanced Feature Processing
        if self.feature_learner:
            # Deep feature learning
            deep_features, feature_metadata = await self.feature_learner.process_sensor_batch(sensor_data_batch)
            
            # Traditional fusion for compatibility
            features = await self.fusion.process_batch(sensor_data_batch)
            
            # Если deep learning не сработал, используем классический подход
            if features is None and len(deep_features) > 0:
                # Создаем EmotionalFeatures из deep features
                # Простое маппирование для совместимости
                features = EmotionalFeatures(
                    valence=float(np.clip(deep_features[0] if len(deep_features) > 0 else 0.5, 0, 1)),
                    arousal=float(np.clip(deep_features[1] if len(deep_features) > 1 else 0.5, 0, 1)),
                    dominance=float(np.clip(deep_features[2] if len(deep_features) > 2 else 0.5, 0, 1)),
                    tempo=float(np.clip(deep_features[3] if len(deep_features) > 3 else 0.5, 0, 1)),
                    intensity=float(np.clip(deep_features[4] if len(deep_features) > 4 else 0.5, 0, 1)),
                    confidence=float(feature_metadata.get('attention_weights', [0.8])[-1] if feature_metadata.get('attention_weights') else 0.8)
                )
        else:
            # Классический фьюжн признаков
            features = await self.fusion.process_batch(sensor_data_batch)
            
        if not features:
            emotime_metrics.record_heartbeat("no_data")
            return
            
        # Записываем метрики батчевой обработки
        emotime_metrics.record_batch_processing(sensor_data_batch, features)
            
        # 3. Добавляем точку во временные ряды
        timestamp = datetime.now()
        point = EmotionalPoint(
            timestamp=timestamp,
            valence=features.valence,
            arousal=features.arousal, 
            dominance=features.dominance,
            tempo=features.tempo,
            intensity=features.intensity
        )
        
        self.timeseries.add_point(point)
        
        # 4. MIT Enhanced Emotional Mode Classification
        if self.adaptive_engine:
            # Используем адаптивное машинное обучение
            mode, ml_confidence = await self.adaptive_engine.predict_emotional_mode(point)
        else:
            # Классическая классификация
            mode = await self.modes.classify_mode(point, self.timeseries)
        
        # Записываем метрики режима и пиков
        emotime_metrics.record_mode_transition(mode)
        emotime_metrics.record_peak_detection(point)
        emotime_metrics.record_timeseries_points(len(self.timeseries.points))
        
        # 5. Сохраняем в Neo4j если включено
        if self.storage:
            try:
                point_id = await self.storage.store_emotional_point(
                    user_id=self.user_id,
                    session_id=self.session_id,
                    point=point,
                    mode=mode,
                    previous_point_id=self._last_point_id
                )
                self._last_point_id = point_id
            except Exception as e:
                safe_logger.error(f"Failed to store point in Neo4j: {e}")
        
        # 6. Обновляем состояние и резонансный след
        await self._update_state(features, mode, point)
        
    async def _update_state(
        self, 
        features: EmotionalFeatures, 
        mode: EmotionalMode, 
        new_point: EmotionalPoint
    ):
        """Обновляет текущее состояние системы."""
        
        # Получаем последние точки для резонансного следа
        recent_points = self.timeseries.get_recent_points(self.trace_window)
        
        # Вычисляем confidence на основе консистентности режима
        confidence = await self._calculate_confidence(mode, recent_points)
        
        # Обновляем состояние
        self.current_state = EmotimeState(
            timestamp=new_point.timestamp,
            features=features,
            mode=mode,
            resonance_trace=recent_points,
            confidence=confidence
        )
        
        # Выводим сердцебиение
        await self._emit_heartbeat(mode, confidence)
    
    async def _calculate_confidence(
        self, 
        current_mode: EmotionalMode, 
        recent_points: List[EmotionalPoint]
    ) -> float:
        """Вычисляет уверенность в текущем режиме."""
        if len(recent_points) < 5:
            return 0.5
            
        # Простая эвристика: стабильность режима за последние точки
        stable_count = 0
        for point in recent_points[-10:]:  # последние 10 точек
            point_mode = await self.modes.classify_mode(point, self.timeseries)
            if point_mode.name == current_mode.name:
                stable_count += 1
                
        return min(stable_count / 10.0, 1.0)
    
    async def _emit_heartbeat(self, mode: EmotionalMode, confidence: float):
        """Выводит сердцебиение в консоль."""
        safe_logger.heartbeat(mode.name.lower(), confidence)
    
    async def get_emotional_insights(self) -> Dict[str, Any]:
        """Возвращает глубокие инсайты об эмоциональном состоянии."""
        insights = {
            "current_state": self.to_dict(),
            "timeseries_analysis": self.timeseries.to_dict(),
            "mode_statistics": self.modes.get_mode_statistics(),
            "mode_insights": self.modes.get_mode_insights(),
            "fusion_statistics": self.fusion.get_feature_statistics()
        }
        
        # MIT Advanced ML Insights
        if self.calibrator:
            insights["adaptive_calibration"] = self.calibrator.get_calibration_analytics()
            insights["calibrated_thresholds"] = self.calibrator.get_calibrated_thresholds()
            
        if self.adaptive_engine:
            insights["ml_analytics"] = self.adaptive_engine.get_learning_analytics()
            
        if self.feature_learner:
            insights["deep_learning"] = self.feature_learner.get_learned_representations()
        
        # Добавляем данные из Neo4j если доступны
        if self.storage:
            try:
                patterns = await self.storage.get_emotional_patterns(self.user_id)
                insights["historical_patterns"] = patterns
            except Exception as e:
                safe_logger.warning(f"Failed to get historical patterns: {e}")
                
        return insights
    
    def to_dict(self) -> Dict[str, Any]:
        """Преобразует состояние в словарь для API."""
        if not self.current_state:
            return {"status": "no_data"}
            
        return {
            "status": "active",
            "user_id": self.user_id,
            "session_id": self.session_id,
            "timestamp": self.current_state.timestamp.isoformat(),
            "mode": {
                "name": self.current_state.mode.name,
                "type": self.current_state.mode.type.value,
                "intensity": self.current_state.mode.intensity,
                "description": self.current_state.mode.description,
                "duration": self.current_state.mode.duration
            },
            "features": {
                "valence": self.current_state.features.valence,
                "arousal": self.current_state.features.arousal,
                "dominance": self.current_state.features.dominance,
                "tempo": self.current_state.features.tempo,
                "intensity": self.current_state.features.intensity,
                "confidence": self.current_state.features.confidence
            },
            "confidence": self.current_state.confidence,
            "trace_points": len(self.current_state.resonance_trace),
            "storage_enabled": self.storage is not None,
            "ml_enhanced": self.adaptive_engine is not None
        }
    
    async def learn_from_feedback(
        self, 
        actual_emotion: str, 
        context: Optional[Dict] = None
    ):
        """
        MIT Advanced Learning from User Feedback.
        
        Args:
            actual_emotion: Реальная эмоция от пользователя
            context: Дополнительный контекст для обучения
        """
        if not self.adaptive_engine or not self.current_state:
            safe_logger.warning("ML feedback not available - adaptive engine disabled")
            return
        
        try:
            # Преобразуем строку в ModeType
            from .modes import ModeType
            actual_mode = None
            
            for mode_type in ModeType:
                if mode_type.value == actual_emotion.lower():
                    actual_mode = mode_type
                    break
            
            if not actual_mode:
                safe_logger.warning(f"Unknown emotion type: {actual_emotion}")
                return
            
            # Обучаем адаптивный движок
            last_point = self.current_state.resonance_trace[-1] if self.current_state.resonance_trace else None
            if last_point:
                if self.adaptive_engine:
                    await self.adaptive_engine.learn_from_feedback(last_point, actual_mode, context)
                
                # MIT Calibration: обучение калибратора
                if self.calibrator:
                    # Получаем предсказанный режим
                    predicted_mode = self.current_state.mode.type
                    confidence = self.current_state.confidence
                    
                    self.calibrator.add_feedback(
                        emotional_point=last_point,
                        predicted_mode=predicted_mode,
                        actual_mode=actual_mode,
                        confidence=confidence
                    )
                
                safe_logger.info(f"Learned from feedback: {actual_emotion}")
            
        except Exception as e:
            safe_logger.error(f"Learning from feedback failed: {e}")
    
    async def trigger_adaptive_learning(self):
        """Запускает адаптивное обучение."""
        if self.feature_learner:
            await self.feature_learner.update_cross_modal_learning()
            safe_logger.info("Cross-modal learning updated")