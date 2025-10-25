"""
🌿✨ Emotime Time Series — временные ряды эмоций

Модуль для работы с эмоциональными временными рядами:
- Сглаживание EWMA
- Детекция пиков и трендов
- Фильтрация шумов
- Построение резонансного следа

"Время течет через эмоции, оставляя следы на карте души"
"""

import numpy as np
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Optional, Dict, Any, Tuple
from collections import deque
import asyncio


@dataclass
class EmotionalPoint:
    """Точка в эмоциональном временном ряду."""
    timestamp: datetime
    valence: float      # -1.0 to 1.0
    arousal: float      # 0.0 to 1.0  
    dominance: float    # 0.0 to 1.0
    tempo: float        # 0.0 to 1.0
    intensity: float    # 0.0 to 1.0
    
    # Метаданные
    smoothed_valence: float = None
    smoothed_arousal: float = None
    trend: str = None              # "rising", "falling", "stable"
    is_peak: bool = False          # пик эмоций
    confidence: float = 1.0


@dataclass 
class TrendAnalysis:
    """Анализ трендов во временном ряду."""
    direction: str      # "rising", "falling", "stable", "volatile"
    strength: float     # 0.0 to 1.0 - сила тренда
    duration: int       # количество точек в текущем тренде
    volatility: float   # 0.0 to 1.0 - волатильность


class EmotionalTimeSeries:
    """
    Система временных рядов для эмоций.
    
    Реализует:
    - EWMA сглаживание (аналог Mojo-ядра)
    - Детекцию пиков и трендов
    - Фильтрацию шумов
    - Анализ паттернов
    """
    
    def __init__(
        self,
        max_points: int = 1000,        # максимальное количество точек в памяти
        ewma_alpha: float = 0.3,       # коэффициент EWMA сглаживания
        peak_threshold: float = 0.15,   # порог для детекции пиков
        trend_window: int = 5          # окно для анализа трендов
    ):
        self.max_points = max_points
        self.ewma_alpha = ewma_alpha
        self.peak_threshold = peak_threshold
        self.trend_window = trend_window
        
        # Основное хранилище точек
        self.points: deque[EmotionalPoint] = deque(maxlen=max_points)
        
        # Сглаженные значения (EWMA)
        self.smoothed_valence: Optional[float] = None
        self.smoothed_arousal: Optional[float] = None
        
        # Анализ трендов
        self.current_trend: Optional[TrendAnalysis] = None
        
    def add_point(self, point: EmotionalPoint):
        """
        Добавляет новую точку во временной ряд.
        
        Выполняет:
        1. EWMA сглаживание
        2. Детекцию пиков
        3. Анализ трендов
        4. Обновление метаданных
        """
        
        # 1. Применяем EWMA сглаживание
        self._apply_ewma_smoothing(point)
        
        # 2. Детекция пиков
        self._detect_peaks(point)
        
        # 3. Анализ трендов
        self._analyze_trends(point)
        
        # 4. Добавляем в основное хранилище
        self.points.append(point)
        
        # 5. Обновляем глобальный анализ трендов
        self._update_trend_analysis()
    
    def _apply_ewma_smoothing(self, point: EmotionalPoint):
        """Применяет EWMA сглаживание (аналог Mojo-ядра)."""
        
        alpha = self.ewma_alpha
        
        if self.smoothed_valence is None:
            # Первая точка - инициализация
            self.smoothed_valence = point.valence
            self.smoothed_arousal = point.arousal
        else:
            # EWMA формула: S_t = α * X_t + (1-α) * S_{t-1}
            self.smoothed_valence = alpha * point.valence + (1 - alpha) * self.smoothed_valence
            self.smoothed_arousal = alpha * point.arousal + (1 - alpha) * self.smoothed_arousal
            
        # Сохраняем сглаженные значения в точку
        point.smoothed_valence = self.smoothed_valence
        point.smoothed_arousal = self.smoothed_arousal
    
    def _detect_peaks(self, point: EmotionalPoint):
        """Детекция пиков эмоций."""
        
        if len(self.points) < 2:
            return
            
        # Берем последние точки для анализа
        recent_points = list(self.points)[-3:] + [point]
        
        if len(recent_points) >= 3:
            # Проверяем, является ли текущая точка пиком
            values = [p.smoothed_valence or p.valence for p in recent_points]
            
            # Пик = значение выше/ниже соседних на threshold
            current_idx = len(values) - 1
            if current_idx > 0 and current_idx < len(values) - 1:
                prev_val = values[current_idx - 1]
                curr_val = values[current_idx]
                
                # Пик вверх или вниз
                if abs(curr_val - prev_val) > self.peak_threshold:
                    point.is_peak = True
    
    def _analyze_trends(self, point: EmotionalPoint):
        """Анализирует локальный тренд для точки."""
        
        if len(self.points) < self.trend_window:
            point.trend = "stable"
            return
            
        # Берем последние точки
        recent_points = list(self.points)[-(self.trend_window-1):] + [point]
        values = [p.smoothed_valence or p.valence for p in recent_points]
        
        # Простой анализ тренда через наклон
        if len(values) >= 3:
            # Линейная регрессия для определения наклона
            x = np.arange(len(values))
            slope = np.polyfit(x, values, 1)[0]
            
            if slope > 0.05:
                point.trend = "rising"
            elif slope < -0.05:
                point.trend = "falling"
            else:
                point.trend = "stable"
        else:
            point.trend = "stable"
    
    def _update_trend_analysis(self):
        """Обновляет общий анализ трендов."""
        
        if len(self.points) < self.trend_window * 2:
            return
            
        # Берем последние точки для анализа
        recent_points = list(self.points)[-self.trend_window * 2:]
        valences = [p.smoothed_valence or p.valence for p in recent_points]
        arousals = [p.smoothed_arousal or p.arousal for p in recent_points]
        
        # Анализируем тренд валентности
        x = np.arange(len(valences))
        slope = np.polyfit(x, valences, 1)[0]
        
        # Определяем направление
        if abs(slope) < 0.02:
            direction = "stable"
        elif slope > 0.02:
            direction = "rising"
        else:
            direction = "falling"
            
        # Вычисляем волатильность (стандартное отклонение)
        volatility = min(np.std(valences), 1.0)
        
        # Если волатильность высокая, возможно это "volatile"
        if volatility > 0.3:
            direction = "volatile"
            
        # Подсчитываем продолжительность текущего тренда
        duration = 1
        if self.current_trend and self.current_trend.direction == direction:
            duration = self.current_trend.duration + 1
            
        # Сила тренда = абсолютное значение наклона
        strength = min(abs(slope) * 10, 1.0)
        
        self.current_trend = TrendAnalysis(
            direction=direction,
            strength=strength,
            duration=duration,
            volatility=volatility
        )
    
    def get_recent_points(self, limit: int = None) -> List[EmotionalPoint]:
        """Возвращает последние точки."""
        points_list = list(self.points)
        if limit:
            return points_list[-limit:]
        return points_list
    
    def get_peaks(self, limit: int = 10) -> List[EmotionalPoint]:
        """Возвращает последние пики эмоций."""
        peaks = [p for p in self.points if p.is_peak]
        return peaks[-limit:] if limit else peaks
    
    def get_trend_analysis(self, window: int = None) -> Optional[TrendAnalysis]:
        """Возвращает анализ текущего тренда."""
        return self.current_trend
    
    def get_emotional_baseline(self) -> Dict[str, float]:
        """Вычисляет эмоциональный базис пользователя."""
        
        if len(self.points) < 10:
            return {
                "baseline_valence": 0.0,
                "baseline_arousal": 0.5,
                "baseline_intensity": 0.3
            }
            
        # Берем все точки для вычисления базиса
        valences = [p.valence for p in self.points]
        arousals = [p.arousal for p in self.points]
        intensities = [p.intensity for p in self.points]
        
        return {
            "baseline_valence": np.mean(valences),
            "baseline_arousal": np.mean(arousals), 
            "baseline_intensity": np.mean(intensities),
            "valence_std": np.std(valences),
            "arousal_std": np.std(arousals),
            "total_points": len(self.points)
        }
    
    def detect_emotional_patterns(self) -> List[Dict[str, Any]]:
        """Детекция эмоциональных паттернов."""
        
        patterns = []
        
        if len(self.points) < 20:
            return patterns
            
        # Паттерн 1: Цикличность (волны эмоций)
        valences = [p.valence for p in self.points[-50:]]  # последние 50 точек
        
        # Простая детекция циклов через автокорреляцию
        if len(valences) >= 20:
            # Ищем периодичность 
            autocorr = np.correlate(valences, valences, mode='full')
            autocorr = autocorr[autocorr.size // 2:]
            
            # Ищем пики в автокорреляции (указывают на цикличность)
            if len(autocorr) > 10:
                peak_indices = []
                for i in range(2, len(autocorr) - 2):
                    if (autocorr[i] > autocorr[i-1] and 
                        autocorr[i] > autocorr[i+1] and 
                        autocorr[i] > 0.3):
                        peak_indices.append(i)
                        
                if peak_indices:
                    patterns.append({
                        "type": "cyclical",
                        "period": peak_indices[0] if peak_indices else None,
                        "strength": max(autocorr[peak_indices]) if peak_indices else 0,
                        "description": "Обнаружена цикличность в эмоциях"
                    })
        
        # Паттерн 2: Градуальные изменения
        if self.current_trend and self.current_trend.duration > 10:
            patterns.append({
                "type": "gradual_change",
                "direction": self.current_trend.direction,
                "duration": self.current_trend.duration,
                "strength": self.current_trend.strength,
                "description": f"Долговременный тренд: {self.current_trend.direction}"
            })
            
        # Паттерн 3: Эмоциональная стабильность
        recent_valences = [p.valence for p in self.points[-20:]]
        if len(recent_valences) >= 10:
            stability = 1.0 - min(np.std(recent_valences), 1.0)
            if stability > 0.8:
                patterns.append({
                    "type": "stability",
                    "level": stability,
                    "description": "Высокая эмоциональная стабильность"
                })
        
        return patterns
    
    def to_dict(self) -> Dict[str, Any]:
        """Преобразует временной ряд в словарь для API."""
        
        recent_points = self.get_recent_points(10)
        peaks = self.get_peaks(5)
        baseline = self.get_emotional_baseline()
        patterns = self.detect_emotional_patterns()
        
        return {
            "total_points": len(self.points),
            "recent_points": [
                {
                    "timestamp": p.timestamp.isoformat(),
                    "valence": p.valence,
                    "arousal": p.arousal,
                    "smoothed_valence": p.smoothed_valence,
                    "trend": p.trend,
                    "is_peak": p.is_peak
                }
                for p in recent_points
            ],
            "peaks": [
                {
                    "timestamp": p.timestamp.isoformat(),
                    "valence": p.valence,
                    "arousal": p.arousal,
                    "intensity": p.intensity
                }
                for p in peaks
            ],
            "current_trend": {
                "direction": self.current_trend.direction,
                "strength": self.current_trend.strength,
                "duration": self.current_trend.duration,
                "volatility": self.current_trend.volatility
            } if self.current_trend else None,
            "baseline": baseline,
            "patterns": patterns,
            "smoothed_state": {
                "valence": self.smoothed_valence,
                "arousal": self.smoothed_arousal
            } if self.smoothed_valence is not None else None
        }