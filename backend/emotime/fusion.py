"""
🌿✨ Emotime Feature Fusion — объединение признаков

Фьюжн слой, который объединяет данные с разных сенсоров в единые
эмоциональные признаки: валентность, возбуждение, доминирование, темп, интенсивность.

"Как чувства сливаются в одну мелодию души"
"""

import asyncio
import numpy as np
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from collections import defaultdict, deque

from .sensors import SensorData, SensorType


@dataclass 
class EmotionalFeatures:
    """Объединенные эмоциональные признаки."""
    valence: float      # -1.0 to 1.0 (негативное → позитивное)
    arousal: float      # 0.0 to 1.0 (спокойствие → возбуждение)  
    dominance: float    # 0.0 to 1.0 (подчинение → доминирование)
    tempo: float        # 0.0 to 1.0 (медленно → быстро)
    intensity: float    # 0.0 to 1.0 (слабо → сильно)
    
    # Метаданные
    timestamp: datetime = None
    confidence: float = 0.0
    sources: List[str] = None  # какие сенсоры участвовали


class FeatureFusion:
    """
    Система фьюжн признаков Emotime.
    
    Объединяет данные с текстовых, тактильных и аудио сенсоров
    в единые эмоциональные признаки с использованием экспоненциального
    сглаживания и весовых коэффициентов.
    """
    
    def __init__(
        self,
        text_weight: float = 0.4,
        touch_weight: float = 0.3, 
        audio_weight: float = 0.3,
        smoothing_alpha: float = 0.7,  # коэффициент EWMA
        history_window: int = 50       # размер окна истории
    ):
        self.weights = {
            SensorType.TEXT: text_weight,
            SensorType.TOUCH: touch_weight,
            SensorType.AUDIO: audio_weight
        }
        
        self.smoothing_alpha = smoothing_alpha
        self.history_window = history_window
        
        # История предыдущих признаков для сглаживания
        self.feature_history: deque = deque(maxlen=history_window)
        
        # Последние значения для EWMA
        self.last_features: Optional[EmotionalFeatures] = None
        
        # Буферы данных с сенсоров (для объединения в батчи)
        self.sensor_buffers: Dict[SensorType, deque] = {
            SensorType.TEXT: deque(maxlen=10),
            SensorType.TOUCH: deque(maxlen=20),
            SensorType.AUDIO: deque(maxlen=5)
        }
    
    async def process_batch(self, sensor_data_list: List[SensorData]) -> Optional[EmotionalFeatures]:
        """
        Обрабатывает батч данных с сенсоров и возвращает объединенные признаки.
        
        Args:
            sensor_data_list: Список данных с разных сенсоров
            
        Returns:
            Объединенные эмоциональные признаки или None если недостаточно данных
        """
        if not sensor_data_list:
            return None
            
        # Распределяем данные по типам сенсоров
        sensor_groups = defaultdict(list)
        for data in sensor_data_list:
            sensor_groups[data.sensor_type].append(data)
            
        # Извлекаем признаки с каждого типа сенсоров
        sensor_features = {}
        active_sources = []
        
        for sensor_type, data_list in sensor_groups.items():
            features = await self._extract_sensor_features(sensor_type, data_list)
            if features:
                sensor_features[sensor_type] = features
                active_sources.append(sensor_type.value)
        
        if not sensor_features:
            return None
            
        # Объединяем признаки
        fused_features = await self._fuse_features(sensor_features, active_sources)
        
        # Применяем сглаживание
        smoothed_features = await self._apply_smoothing(fused_features)
        
        # Сохраняем в историю
        self.feature_history.append(smoothed_features)
        self.last_features = smoothed_features
        
        return smoothed_features
    
    async def _extract_sensor_features(
        self, 
        sensor_type: SensorType, 
        data_list: List[SensorData]
    ) -> Optional[Dict[str, float]]:
        """Извлекает признаки из данных определенного типа сенсора."""
        
        if not data_list:
            return None
            
        # Объединяем метаданные всех данных этого типа
        all_metadata = {}
        for data in data_list:
            all_metadata.update(data.metadata)
            
        # Извлекаем базовые эмоциональные признаки
        base_features = {
            "valence": all_metadata.get("valence", 0.0),
            "arousal": all_metadata.get("arousal", 0.0), 
            "intensity": all_metadata.get("intensity", 0.0)
        }
        
        # Добавляем специфичные для сенсора признаки
        if sensor_type == SensorType.TEXT:
            return await self._process_text_features(data_list, base_features)
        elif sensor_type == SensorType.TOUCH:
            return await self._process_touch_features(data_list, base_features)
        elif sensor_type == SensorType.AUDIO:
            return await self._process_audio_features(data_list, base_features)
            
        return base_features
    
    async def _process_text_features(
        self, 
        data_list: List[SensorData], 
        base_features: Dict[str, float]
    ) -> Dict[str, float]:
        """Обрабатывает текстовые признаки."""
        
        # Анализируем темп печати
        total_chars = 0
        total_time = 0
        pause_times = []
        
        for data in data_list:
            text_data = data.raw_data
            total_chars += text_data.char_count
            
            if text_data.typing_speed:
                total_time += text_data.char_count / text_data.typing_speed
                
            if text_data.pause_duration:
                pause_times.append(text_data.pause_duration)
                
        # Вычисляем общий темп
        if total_time > 0:
            avg_typing_speed = total_chars / total_time
            tempo = min(avg_typing_speed / 10.0, 1.0)  # нормализуем к 10 символам/сек
        else:
            tempo = 0.5
            
        # Анализируем паузы (влияют на доминирование)
        if pause_times:
            avg_pause = np.mean(pause_times)
            # Короткие паузы = уверенность, длинные = неуверенность
            dominance = max(0.0, 1.0 - min(avg_pause / 10.0, 1.0))
        else:
            dominance = 0.5
            
        return {
            **base_features,
            "tempo": tempo,
            "dominance": dominance
        }
    
    async def _process_touch_features(
        self, 
        data_list: List[SensorData],
        base_features: Dict[str, float]
    ) -> Dict[str, float]:
        """Обрабатывает тактильные признаки."""
        
        pressures = []
        frequencies = []
        durations = []
        
        for data in data_list:
            touch_data = data.raw_data
            pressures.append(touch_data.pressure)
            frequencies.append(touch_data.frequency)
            durations.append(touch_data.duration)
            
        # Темп на основе частоты касаний
        avg_frequency = np.mean(frequencies) if frequencies else 0
        tempo = min(avg_frequency / 60.0, 1.0)  # нормализуем к 60 касаниям/мин
        
        # Доминирование на основе силы нажатий
        avg_pressure = np.mean(pressures) if pressures else 0.5
        dominance = min(avg_pressure, 1.0)
        
        return {
            **base_features,
            "tempo": tempo,
            "dominance": dominance
        }
    
    async def _process_audio_features(
        self,
        data_list: List[SensorData],
        base_features: Dict[str, float]  
    ) -> Dict[str, float]:
        """Обрабатывает аудио признаки."""
        
        speech_rates = []
        volumes = []
        pitch_variances = []
        
        for data in data_list:
            audio_data = data.raw_data
            speech_rates.append(audio_data.speech_rate)
            volumes.append(audio_data.volume_level)
            pitch_variances.append(audio_data.pitch_variance)
            
        # Темп на основе скорости речи
        avg_speech_rate = np.mean(speech_rates) if speech_rates else 150
        tempo = min(avg_speech_rate / 200.0, 1.0)  # нормализуем к 200 слов/мин
        
        # Доминирование на основе громкости и вариативности тона
        avg_volume = np.mean(volumes) if volumes else 0.5
        avg_variance = np.mean(pitch_variances) if pitch_variances else 25
        
        dominance = (avg_volume + min(avg_variance / 50.0, 1.0)) / 2.0
        
        return {
            **base_features,
            "tempo": tempo, 
            "dominance": dominance
        }
    
    async def _fuse_features(
        self, 
        sensor_features: Dict[SensorType, Dict[str, float]],
        active_sources: List[str]
    ) -> EmotionalFeatures:
        """Объединяет признаки с разных сенсоров."""
        
        # Вычисляем взвешенные суммы для каждого признака
        weighted_features = {
            "valence": 0.0,
            "arousal": 0.0,
            "dominance": 0.0,
            "tempo": 0.0,
            "intensity": 0.0
        }
        
        total_weight = 0.0
        
        for sensor_type, features in sensor_features.items():
            weight = self.weights[sensor_type]
            total_weight += weight
            
            for feature_name in weighted_features:
                if feature_name in features:
                    weighted_features[feature_name] += features[feature_name] * weight
                    
        # Нормализуем по общему весу
        if total_weight > 0:
            for feature_name in weighted_features:
                weighted_features[feature_name] /= total_weight
                
        # Вычисляем confidence на основе количества активных сенсоров
        confidence = len(sensor_features) / len(self.weights)
        
        return EmotionalFeatures(
            valence=max(-1.0, min(1.0, weighted_features["valence"])),
            arousal=max(0.0, min(1.0, weighted_features["arousal"])),
            dominance=max(0.0, min(1.0, weighted_features["dominance"])),
            tempo=max(0.0, min(1.0, weighted_features["tempo"])),
            intensity=max(0.0, min(1.0, weighted_features["intensity"])),
            timestamp=datetime.now(),
            confidence=confidence,
            sources=active_sources
        )
    
    async def _apply_smoothing(self, new_features: EmotionalFeatures) -> EmotionalFeatures:
        """Применяет экспоненциальное сглаживание к признакам."""
        
        if not self.last_features:
            return new_features
            
        # Применяем EWMA (Exponential Weighted Moving Average)
        alpha = self.smoothing_alpha
        
        smoothed = EmotionalFeatures(
            valence=alpha * new_features.valence + (1 - alpha) * self.last_features.valence,
            arousal=alpha * new_features.arousal + (1 - alpha) * self.last_features.arousal,
            dominance=alpha * new_features.dominance + (1 - alpha) * self.last_features.dominance,
            tempo=alpha * new_features.tempo + (1 - alpha) * self.last_features.tempo,
            intensity=alpha * new_features.intensity + (1 - alpha) * self.last_features.intensity,
            timestamp=new_features.timestamp,
            confidence=new_features.confidence,
            sources=new_features.sources
        )
        
        return smoothed
    
    def get_feature_statistics(self) -> Dict[str, Any]:
        """Возвращает статистику по историческим данным признаков."""
        if not self.feature_history:
            return {"status": "no_data"}
            
        history = list(self.feature_history)
        
        # Вычисляем статистики
        valences = [f.valence for f in history]
        arousals = [f.arousal for f in history]
        
        return {
            "total_points": len(history),
            "valence": {
                "mean": np.mean(valences),
                "std": np.std(valences),
                "min": np.min(valences),
                "max": np.max(valences)
            },
            "arousal": {
                "mean": np.mean(arousals),
                "std": np.std(arousals),
                "min": np.min(arousals),
                "max": np.max(arousals)
            },
            "recent_confidence": history[-1].confidence if history else 0.0,
            "active_sources": history[-1].sources if history else []
        }