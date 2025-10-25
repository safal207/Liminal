"""
🌿✨ Emotime Emotional Modes — режимы эмоций

Классификация эмоциональных состояний во времени:
- Calm: спокойствие и умиротворение
- Focus: концентрация и погруженность  
- Stress: напряжение и возбуждение
- Joy: радость и вдохновение
- Contemplation: размышление и рефлексия

"Каждое состояние — это музыка души, играющая в своем ритме"
"""

import numpy as np
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Dict, Optional, Any
from enum import Enum

from .timeseries import EmotionalPoint, EmotionalTimeSeries


class ModeType(Enum):
    """Типы эмоциональных режимов."""
    CALM = "calm"
    FOCUS = "focus" 
    STRESS = "stress"
    JOY = "joy"
    CONTEMPLATION = "contemplation"
    NEUTRAL = "neutral"


@dataclass
class EmotionalMode:
    """Эмоциональный режим."""
    name: str
    type: ModeType
    intensity: float        # 0.0 to 1.0 - интенсивность режима
    confidence: float       # 0.0 to 1.0 - уверенность в классификации
    description: str
    duration: int = 1       # сколько циклов в этом режиме
    
    # Характеристики режима
    valence_range: tuple = None     # (min, max) валентность
    arousal_range: tuple = None     # (min, max) возбуждение
    tempo_range: tuple = None       # (min, max) темп


class EmotionalModes:
    """
    Система классификации эмоциональных режимов.
    
    Использует простую версию HMM (Hidden Markov Model) для определения
    эмоциональных состояний на основе временных рядов.
    """
    
    # Определения режимов с их характеристиками
    MODE_DEFINITIONS = {
        ModeType.CALM: {
            "name": "Calm",
            "description": "State of peace and balance",
            "valence_range": (-0.2, 0.4),   # слегка позитивное или нейтральное
            "arousal_range": (0.0, 0.3),    # низкое возбуждение
            "tempo_range": (0.0, 0.4),      # медленный темп
            "emoji": "🧘‍♀️"
        },
        ModeType.FOCUS: {
            "name": "Focus", 
            "description": "State of deep concentration",
            "valence_range": (-0.1, 0.3),   # нейтральное
            "arousal_range": (0.4, 0.7),    # среднее возбуждение
            "tempo_range": (0.3, 0.7),      # умеренный темп
            "emoji": "🎯"
        },
        ModeType.STRESS: {
            "name": "Stress",
            "description": "State of stress and anxiety",
            "valence_range": (-0.8, -0.1),  # негативное
            "arousal_range": (0.6, 1.0),    # высокое возбуждение
            "tempo_range": (0.5, 1.0),      # быстрый темп
            "emoji": "⚡"
        },
        ModeType.JOY: {
            "name": "Joy",
            "description": "State of happiness and inspiration",
            "valence_range": (0.4, 1.0),    # высоко позитивное
            "arousal_range": (0.3, 0.8),    # среднее-высокое возбуждение
            "tempo_range": (0.2, 0.8),      # варьируется
            "emoji": "✨"
        },
        ModeType.CONTEMPLATION: {
            "name": "Contemplation", 
            "description": "State of deep reflection",
            "valence_range": (-0.3, 0.2),   # слегка негативное или нейтральное
            "arousal_range": (0.2, 0.5),    # низкое-среднее возбуждение
            "tempo_range": (0.0, 0.3),      # медленный темп
            "emoji": "💭"
        },
        ModeType.NEUTRAL: {
            "name": "Neutral",
            "description": "Basic neutral state",
            "valence_range": (-0.2, 0.2),   # около нуля
            "arousal_range": (0.3, 0.6),    # среднее возбуждение
            "tempo_range": (0.3, 0.6),      # средний темп
            "emoji": "😐"
        }
    }
    
    def __init__(self, stability_threshold: float = 0.8):
        self.stability_threshold = stability_threshold
        self.current_mode: Optional[EmotionalMode] = None
        self.mode_history: List[EmotionalMode] = []
        
        # Переходные вероятности между режимами (упрощенная HMM)
        self.transition_probabilities = self._initialize_transitions()
    
    def _initialize_transitions(self) -> Dict[ModeType, Dict[ModeType, float]]:
        """Инициализирует матрицу переходных вероятностей."""
        
        # Базовые переходы - некоторые состояния переходят друг в друга чаще
        transitions = {
            ModeType.CALM: {
                ModeType.CALM: 0.7,          # стабильное состояние
                ModeType.FOCUS: 0.15,        # из спокойствия в фокус
                ModeType.CONTEMPLATION: 0.1,  # в размышления
                ModeType.NEUTRAL: 0.05
            },
            ModeType.FOCUS: {
                ModeType.FOCUS: 0.6,         # фокус держится
                ModeType.CALM: 0.2,          # расслабление после фокуса
                ModeType.STRESS: 0.1,        # перенапряжение
                ModeType.JOY: 0.05,          # радость от достижения
                ModeType.NEUTRAL: 0.05
            },
            ModeType.STRESS: {
                ModeType.STRESS: 0.4,        # стресс может продолжаться
                ModeType.CALM: 0.25,         # успокоение
                ModeType.FOCUS: 0.15,        # переключение на задачу
                ModeType.CONTEMPLATION: 0.1, # обдумывание проблемы
                ModeType.NEUTRAL: 0.1
            },
            ModeType.JOY: {
                ModeType.JOY: 0.5,           # радость может продолжаться
                ModeType.CALM: 0.3,          # удовлетворение
                ModeType.FOCUS: 0.1,         # энергия для работы
                ModeType.NEUTRAL: 0.1
            },
            ModeType.CONTEMPLATION: {
                ModeType.CONTEMPLATION: 0.6,  # размышления углубляются
                ModeType.CALM: 0.2,          # приходит покой
                ModeType.FOCUS: 0.1,         # понимание ведет к действию
                ModeType.NEUTRAL: 0.1
            },
            ModeType.NEUTRAL: {
                ModeType.NEUTRAL: 0.4,       # базовое состояние
                ModeType.CALM: 0.2,
                ModeType.FOCUS: 0.15,
                ModeType.CONTEMPLATION: 0.1,
                ModeType.STRESS: 0.1,
                ModeType.JOY: 0.05
            }
        }
        
        return transitions
    
    async def classify_mode(
        self, 
        point: EmotionalPoint, 
        timeseries: EmotionalTimeSeries
    ) -> EmotionalMode:
        """
        Классифицирует эмоциональный режим для данной точки.
        
        Учитывает:
        1. Текущие значения валентности/возбуждения/темпа
        2. История предыдущих режимов (HMM transitions)
        3. Стабильность состояния
        """
        
        # 1. Вычисляем соответствие каждому режиму
        mode_scores = {}
        for mode_type in ModeType:
            score = self._calculate_mode_score(point, mode_type)
            mode_scores[mode_type] = score
            
        # 2. Применяем переходные вероятности если есть история
        if self.current_mode:
            adjusted_scores = self._apply_transition_probabilities(mode_scores)
        else:
            adjusted_scores = mode_scores
            
        # 3. Выбираем режим с максимальным скором
        best_mode_type = max(adjusted_scores, key=adjusted_scores.get)
        confidence = adjusted_scores[best_mode_type]
        
        # 4. Создаем объект режима
        mode_def = self.MODE_DEFINITIONS[best_mode_type]
        
        # Вычисляем интенсивность режима
        intensity = self._calculate_intensity(point, best_mode_type)
        
        # Определяем продолжительность
        duration = 1
        if (self.current_mode and 
            self.current_mode.type == best_mode_type):
            duration = self.current_mode.duration + 1
            
        new_mode = EmotionalMode(
            name=mode_def["name"],
            type=best_mode_type,
            intensity=intensity,
            confidence=confidence,
            description=mode_def["description"],
            duration=duration,
            valence_range=mode_def["valence_range"],
            arousal_range=mode_def["arousal_range"], 
            tempo_range=mode_def["tempo_range"]
        )
        
        # 5. Обновляем историю
        self.current_mode = new_mode
        self.mode_history.append(new_mode)
        
        # Ограничиваем размер истории
        if len(self.mode_history) > 100:
            self.mode_history = self.mode_history[-50:]
            
        return new_mode
    
    def _calculate_mode_score(
        self, 
        point: EmotionalPoint, 
        mode_type: ModeType
    ) -> float:
        """Вычисляет соответствие точки определенному режиму."""
        
        mode_def = self.MODE_DEFINITIONS[mode_type]
        
        valence_range = mode_def["valence_range"]
        arousal_range = mode_def["arousal_range"] 
        tempo_range = mode_def["tempo_range"]
        
        # Функция принадлежности для каждого параметра
        valence_score = self._membership_function(
            point.valence, valence_range[0], valence_range[1]
        )
        arousal_score = self._membership_function(
            point.arousal, arousal_range[0], arousal_range[1]
        )
        tempo_score = self._membership_function(
            point.tempo, tempo_range[0], tempo_range[1]
        )
        
        # Дополнительные факторы
        intensity_factor = 1.0
        if mode_type in [ModeType.JOY, ModeType.STRESS]:
            # Радость и стресс требуют высокой интенсивности
            intensity_factor = point.intensity
        elif mode_type == ModeType.CALM:
            # Спокойствие требует низкой интенсивности
            intensity_factor = 1.0 - point.intensity
            
        # Взвешенная сумма
        total_score = (
            valence_score * 0.4 +
            arousal_score * 0.35 +
            tempo_score * 0.15 +
            intensity_factor * 0.1
        )
        
        return total_score
    
    def _membership_function(self, value: float, range_min: float, range_max: float) -> float:
        """
        Функция принадлежности (membership function) для нечеткой логики.
        
        Возвращает значение от 0 до 1 - насколько value принадлежит диапазону.
        """
        
        if range_min <= value <= range_max:
            return 1.0
            
        # Плавное снижение за границами диапазона
        range_width = range_max - range_min
        tolerance = range_width * 0.5  # допуск 50% от ширины диапазона
        
        if value < range_min:
            distance = range_min - value
            return max(0.0, 1.0 - distance / tolerance)
        else:  # value > range_max
            distance = value - range_max  
            return max(0.0, 1.0 - distance / tolerance)
    
    def _apply_transition_probabilities(self, mode_scores: Dict[ModeType, float]) -> Dict[ModeType, float]:
        """Применяет переходные вероятности HMM."""
        
        if not self.current_mode:
            return mode_scores
            
        current_type = self.current_mode.type
        transitions = self.transition_probabilities.get(current_type, {})
        
        # Корректируем скоры на основе переходных вероятностей
        adjusted_scores = {}
        for mode_type, score in mode_scores.items():
            transition_prob = transitions.get(mode_type, 0.1)  # базовая вероятность
            
            # Комбинируем исходный скор с переходной вероятностью
            adjusted_score = score * 0.7 + transition_prob * 0.3
            adjusted_scores[mode_type] = adjusted_score
            
        return adjusted_scores
    
    def _calculate_intensity(self, point: EmotionalPoint, mode_type: ModeType) -> float:
        """Вычисляет интенсивность режима."""
        
        # Базовая интенсивность на основе общей интенсивности точки
        base_intensity = point.intensity
        
        # Модификаторы для разных режимов
        if mode_type == ModeType.STRESS:
            # Стресс усиливается высоким возбуждением
            return min(base_intensity + point.arousal * 0.3, 1.0)
        elif mode_type == ModeType.JOY:
            # Радость усиливается позитивной валентностью
            return min(base_intensity + max(0, point.valence) * 0.3, 1.0)
        elif mode_type == ModeType.CALM:
            # Спокойствие обратно пропорционально возбуждению
            return max(base_intensity - point.arousal * 0.2, 0.0)
        elif mode_type == ModeType.FOCUS:
            # Фокус связан со средним возбуждением
            optimal_arousal = 0.55
            arousal_distance = abs(point.arousal - optimal_arousal)
            focus_bonus = max(0, 0.3 - arousal_distance)
            return min(base_intensity + focus_bonus, 1.0)
            
        return base_intensity
    
    def get_mode_statistics(self) -> Dict[str, Any]:
        """Возвращает статистику по режимам."""
        
        if not self.mode_history:
            return {"status": "no_data"}
            
        # Подсчитываем время в каждом режиме
        mode_durations = {}
        for mode in self.mode_history:
            mode_name = mode.name
            if mode_name not in mode_durations:
                mode_durations[mode_name] = 0
            mode_durations[mode_name] += 1
            
        # Вычисляем проценты
        total_points = len(self.mode_history)
        mode_percentages = {
            mode: (duration / total_points) * 100
            for mode, duration in mode_durations.items()
        }
        
        # Анализируем переходы между режимами
        transitions = {}
        for i in range(1, len(self.mode_history)):
            prev_mode = self.mode_history[i-1].name
            curr_mode = self.mode_history[i].name
            
            transition_key = f"{prev_mode} → {curr_mode}"
            if transition_key not in transitions:
                transitions[transition_key] = 0
            transitions[transition_key] += 1
            
        # Самые частые переходы
        frequent_transitions = sorted(
            transitions.items(), 
            key=lambda x: x[1], 
            reverse=True
        )[:5]
        
        return {
            "total_points": total_points,
            "current_mode": {
                "name": self.current_mode.name,
                "type": self.current_mode.type.value,
                "intensity": self.current_mode.intensity,
                "duration": self.current_mode.duration,
                "confidence": self.current_mode.confidence
            } if self.current_mode else None,
            "mode_distribution": mode_percentages,
            "mode_durations": mode_durations,
            "frequent_transitions": [
                {"transition": trans, "count": count}
                for trans, count in frequent_transitions
            ],
            "stability": self._calculate_mode_stability()
        }
    
    def _calculate_mode_stability(self) -> float:
        """Вычисляет стабильность эмоциональных режимов."""
        
        if len(self.mode_history) < 5:
            return 0.5
            
        # Считаем количество переключений режимов
        switches = 0
        for i in range(1, len(self.mode_history)):
            if self.mode_history[i].type != self.mode_history[i-1].type:
                switches += 1
                
        # Стабильность = 1 - (переключения / возможные_переключения)
        possible_switches = len(self.mode_history) - 1
        stability = 1.0 - (switches / possible_switches)
        
        return stability
    
    def get_mode_insights(self) -> List[str]:
        """Возвращает инсайты о паттернах эмоциональных режимов."""
        
        insights = []
        
        if not self.mode_history or len(self.mode_history) < 10:
            return ["Недостаточно данных для анализа паттернов"]
            
        stats = self.get_mode_statistics()
        
        # Анализ доминирующих режимов
        mode_dist = stats["mode_distribution"]
        dominant_mode = max(mode_dist, key=mode_dist.get)
        dominant_percentage = mode_dist[dominant_mode]
        
        if dominant_percentage > 60:
            insights.append(f"Преобладает режим '{dominant_mode}' ({dominant_percentage:.1f}%)")
        
        # Анализ стабильности
        stability = stats["stability"]
        if stability > 0.8:
            insights.append("Высокая эмоциональная стабильность")
        elif stability < 0.4:
            insights.append("Частые смены эмоциональных режимов")
            
        # Анализ текущего режима
        if self.current_mode and self.current_mode.duration > 5:
            insights.append(f"Длительное пребывание в режиме '{self.current_mode.name}'")
            
        # Анализ интенсивности
        recent_intensities = [mode.intensity for mode in self.mode_history[-10:]]
        avg_intensity = sum(recent_intensities) / len(recent_intensities)
        
        if avg_intensity > 0.8:
            insights.append("Высокая эмоциональная интенсивность")
        elif avg_intensity < 0.3:
            insights.append("Низкая эмоциональная активность")
            
        return insights if insights else ["Эмоциональное состояние в пределах нормы"]