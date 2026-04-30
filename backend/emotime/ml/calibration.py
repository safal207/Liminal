"""
🎯⚡ Adaptive Calibration System — MIT-style parameter optimization

Smart calibration without heavy ML dependencies:
- Evolutionary algorithms for parameter tuning
- Bayesian-inspired optimization
- Performance-based adaptive thresholds
- Real-time system calibration

Based on MIT optimization research.
"""

import json
import random
from collections import deque
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Tuple

import numpy as np

from ..modes import EmotionalMode, ModeType
from ..timeseries import EmotionalPoint
from ..utils import safe_logger


@dataclass
class ParameterSet:
    """Набор параметров для калибровки."""

    valence_thresholds: Dict[str, float]
    arousal_thresholds: Dict[str, float]
    confidence_boost: float
    temporal_weight: float
    performance_score: float = 0.0
    generation: int = 0
    usage_count: int = 0


@dataclass
class CalibrationEvent:
    """Событие калибровки."""

    timestamp: datetime
    old_parameters: Dict[str, Any]
    new_parameters: Dict[str, Any]
    performance_improvement: float
    trigger_reason: str


class EvolutionaryOptimizer:
    """
    Эволюционный оптимизатор параметров.

    Использует генетический алгоритм для поиска
    оптимальных параметров классификации.
    """

    def __init__(self, population_size: int = 20, mutation_rate: float = 0.1):
        self.population_size = population_size
        self.mutation_rate = mutation_rate
        self.population: List[ParameterSet] = []
        self.generation = 0
        self.best_performer: Optional[ParameterSet] = None

    def initialize_population(self):
        """Инициализирует популяцию параметров."""
        for _ in range(self.population_size):
            params = ParameterSet(
                valence_thresholds={
                    "calm_max": random.uniform(0.2, 0.6),
                    "joy_min": random.uniform(0.4, 0.8),
                    "stress_max": random.uniform(0.1, 0.5),
                },
                arousal_thresholds={
                    "calm_max": random.uniform(0.2, 0.4),
                    "stress_min": random.uniform(0.6, 0.9),
                    "focus_range": (random.uniform(0.3, 0.5), random.uniform(0.6, 0.8)),
                },
                confidence_boost=random.uniform(0.8, 1.2),
                temporal_weight=random.uniform(0.1, 0.9),
                generation=self.generation,
            )
            self.population.append(params)

        safe_logger.info(
            f"Initialized population of {self.population_size} parameter sets"
        )

    def evaluate_fitness(
        self, params: ParameterSet, feedback_data: List[Dict]
    ) -> float:
        """Оценивает качество параметров на основе обратной связи."""
        if not feedback_data:
            return 0.5

        correct_predictions = 0
        total_predictions = len(feedback_data)

        for data_point in feedback_data:
            point = data_point.get("emotional_point")
            actual_mode = data_point.get("actual_mode")

            if not point or not actual_mode:
                continue

            # Предсказываем режим с текущими параметрами
            predicted_mode = self._classify_with_params(point, params)

            if predicted_mode == actual_mode:
                correct_predictions += 1

        accuracy = correct_predictions / max(total_predictions, 1)

        # Бонус за стабильность (меньше экстремальных значений)
        stability_bonus = (
            1.0
            - (abs(params.confidence_boost - 1.0) + abs(params.temporal_weight - 0.5))
            * 0.1
        )

        return accuracy * stability_bonus

    def _classify_with_params(
        self, point: EmotionalPoint, params: ParameterSet
    ) -> ModeType:
        """Классифицирует эмоцию с заданными параметрами."""
        valence = point.valence
        arousal = point.arousal

        # Применяем пороги из параметров
        if (
            valence <= params.valence_thresholds["calm_max"]
            and arousal <= params.arousal_thresholds["calm_max"]
        ):
            return ModeType.CALM

        elif valence >= params.valence_thresholds["joy_min"]:
            return ModeType.JOY

        elif (
            valence <= params.valence_thresholds["stress_max"]
            and arousal >= params.arousal_thresholds["stress_min"]
        ):
            return ModeType.STRESS

        elif (
            params.arousal_thresholds["focus_range"][0]
            <= arousal
            <= params.arousal_thresholds["focus_range"][1]
        ):
            return ModeType.FOCUS

        elif arousal < params.arousal_thresholds["calm_max"]:
            return ModeType.CONTEMPLATION

        else:
            return ModeType.NEUTRAL

    def evolve_generation(self, feedback_data: List[Dict]) -> ParameterSet:
        """Эволюционирует поколение параметров."""
        if not self.population:
            self.initialize_population()

        # Оценка приспособленности
        for params in self.population:
            params.performance_score = self.evaluate_fitness(params, feedback_data)

        # Сортировка по приспособленности
        self.population.sort(key=lambda p: p.performance_score, reverse=True)

        # Обновление лучшего исполнителя
        if (
            not self.best_performer
            or self.population[0].performance_score
            > self.best_performer.performance_score
        ):
            self.best_performer = self.population[0]

        # Селекция (оставляем лучшую половину)
        survivors = self.population[: self.population_size // 2]

        # Кроссовер и мутация
        new_generation = survivors.copy()

        while len(new_generation) < self.population_size:
            parent1, parent2 = random.choices(survivors, k=2)
            child = self._crossover(parent1, parent2)
            child = self._mutate(child)
            child.generation = self.generation + 1
            new_generation.append(child)

        self.population = new_generation
        self.generation += 1

        safe_logger.info(
            f"Generation {self.generation}: best score = {self.best_performer.performance_score:.3f}"
        )

        return self.best_performer

    def _crossover(self, parent1: ParameterSet, parent2: ParameterSet) -> ParameterSet:
        """Создает потомка от двух родителей."""
        child = ParameterSet(
            valence_thresholds={},
            arousal_thresholds={},
            confidence_boost=0.0,
            temporal_weight=0.0,
        )

        # Смешиваем пороги валентности
        for key in parent1.valence_thresholds:
            if random.random() < 0.5:
                child.valence_thresholds[key] = parent1.valence_thresholds[key]
            else:
                child.valence_thresholds[key] = parent2.valence_thresholds[key]

        # Смешиваем пороги возбуждения
        for key in parent1.arousal_thresholds:
            if random.random() < 0.5:
                child.arousal_thresholds[key] = parent1.arousal_thresholds[key]
            else:
                child.arousal_thresholds[key] = parent2.arousal_thresholds[key]

        # Усредняем скалярные параметры
        child.confidence_boost = (
            parent1.confidence_boost + parent2.confidence_boost
        ) / 2
        child.temporal_weight = (parent1.temporal_weight + parent2.temporal_weight) / 2

        return child

    def _mutate(self, params: ParameterSet) -> ParameterSet:
        """Мутирует параметры."""
        if random.random() > self.mutation_rate:
            return params

        # Мутация порогов валентности
        for key in params.valence_thresholds:
            if random.random() < 0.3:  # 30% шанс мутации каждого порога
                noise = random.uniform(-0.1, 0.1)
                params.valence_thresholds[key] = np.clip(
                    params.valence_thresholds[key] + noise, 0.0, 1.0
                )

        # Мутация порогов возбуждения
        for key in params.arousal_thresholds:
            if random.random() < 0.3:
                if isinstance(params.arousal_thresholds[key], tuple):
                    # Диапазон
                    low, high = params.arousal_thresholds[key]
                    noise_low = random.uniform(-0.05, 0.05)
                    noise_high = random.uniform(-0.05, 0.05)
                    new_low = np.clip(low + noise_low, 0.0, 1.0)
                    new_high = np.clip(high + noise_high, 0.0, 1.0)
                    if new_low < new_high:
                        params.arousal_thresholds[key] = (new_low, new_high)
                else:
                    # Одиночное значение
                    noise = random.uniform(-0.1, 0.1)
                    params.arousal_thresholds[key] = np.clip(
                        params.arousal_thresholds[key] + noise, 0.0, 1.0
                    )

        # Мутация скалярных параметров
        if random.random() < 0.3:
            params.confidence_boost += random.uniform(-0.1, 0.1)
            params.confidence_boost = np.clip(params.confidence_boost, 0.5, 1.5)

        if random.random() < 0.3:
            params.temporal_weight += random.uniform(-0.1, 0.1)
            params.temporal_weight = np.clip(params.temporal_weight, 0.0, 1.0)

        return params


class AdaptiveCalibrator:
    """
    Адаптивный калибратор системы.

    MIT-level features:
    - Evolutionary parameter optimization
    - Real-time performance monitoring
    - Adaptive threshold adjustment
    - Multi-objective optimization
    """

    def __init__(self, user_id: str):
        self.user_id = user_id
        self.optimizer = EvolutionaryOptimizer()

        # Performance tracking
        self.feedback_buffer = deque(maxlen=100)
        self.performance_history = deque(maxlen=50)
        self.calibration_events = deque(maxlen=20)

        # Current parameters
        self.current_parameters: Optional[ParameterSet] = None
        self.baseline_performance = 0.5

        # Calibration settings
        self.min_feedback_for_calibration = 10
        self.calibration_interval = timedelta(hours=1)
        self.last_calibration = datetime.now()

        safe_logger.info(f"Adaptive calibrator initialized for user {user_id}")

    def add_feedback(
        self,
        emotional_point: EmotionalPoint,
        predicted_mode: ModeType,
        actual_mode: ModeType,
        confidence: float,
    ):
        """Добавляет обратную связь для калибровки."""
        feedback = {
            "timestamp": datetime.now(),
            "emotional_point": emotional_point,
            "predicted_mode": predicted_mode,
            "actual_mode": actual_mode,
            "confidence": confidence,
            "correct": predicted_mode == actual_mode,
        }

        self.feedback_buffer.append(feedback)

        # Проверяем нужна ли калибровка
        if self._should_calibrate():
            self._trigger_calibration()

    def _should_calibrate(self) -> bool:
        """Определяет нужна ли калибровка."""
        if len(self.feedback_buffer) < self.min_feedback_for_calibration:
            return False

        if datetime.now() - self.last_calibration < self.calibration_interval:
            return False

        # Проверяем падение производительности
        recent_feedback = list(self.feedback_buffer)[-10:]
        recent_accuracy = sum(1 for f in recent_feedback if f["correct"]) / len(
            recent_feedback
        )

        if recent_accuracy < self.baseline_performance - 0.1:  # Падение на 10%
            safe_logger.info(
                f"Performance drop detected: {recent_accuracy:.3f} vs {self.baseline_performance:.3f}"
            )
            return True

        return False

    def _trigger_calibration(self):
        """Запускает процесс калибровки."""
        safe_logger.info("Starting adaptive calibration...")

        try:
            # Подготовка данных обратной связи
            feedback_data = list(self.feedback_buffer)

            # Эволюционная оптимизация
            old_params = self.current_parameters
            new_params = self.optimizer.evolve_generation(feedback_data)

            # Оценка улучшения
            old_score = (
                old_params.performance_score
                if old_params
                else self.baseline_performance
            )
            improvement = new_params.performance_score - old_score

            if improvement > 0.01:  # Минимальное улучшение 1%
                self.current_parameters = new_params
                self.baseline_performance = new_params.performance_score

                # Записываем событие калибровки
                calibration_event = CalibrationEvent(
                    timestamp=datetime.now(),
                    old_parameters=asdict(old_params) if old_params else {},
                    new_parameters=asdict(new_params),
                    performance_improvement=improvement,
                    trigger_reason="performance_optimization",
                )

                self.calibration_events.append(calibration_event)
                self.last_calibration = datetime.now()

                safe_logger.info(f"Calibration complete: {improvement:.3f} improvement")
            else:
                safe_logger.info("No significant improvement found")

        except Exception as e:
            safe_logger.error(f"Calibration failed: {e}")

    def get_calibrated_thresholds(self) -> Dict[str, Any]:
        """Возвращает откалиброванные пороги."""
        if not self.current_parameters:
            # Возвращаем стандартные пороги
            return {
                "valence_thresholds": {
                    "calm_max": 0.4,
                    "joy_min": 0.6,
                    "stress_max": 0.3,
                },
                "arousal_thresholds": {
                    "calm_max": 0.3,
                    "stress_min": 0.7,
                    "focus_range": (0.4, 0.7),
                },
                "confidence_boost": 1.0,
                "temporal_weight": 0.5,
                "calibration_status": "default",
            }

        return {
            "valence_thresholds": self.current_parameters.valence_thresholds,
            "arousal_thresholds": self.current_parameters.arousal_thresholds,
            "confidence_boost": self.current_parameters.confidence_boost,
            "temporal_weight": self.current_parameters.temporal_weight,
            "calibration_status": "optimized",
            "performance_score": self.current_parameters.performance_score,
            "generation": self.current_parameters.generation,
        }

    def get_calibration_analytics(self) -> Dict[str, Any]:
        """Возвращает аналитику калибровки."""
        recent_feedback = list(self.feedback_buffer)[-20:]
        recent_accuracy = sum(1 for f in recent_feedback if f["correct"]) / max(
            len(recent_feedback), 1
        )

        return {
            "user_id": self.user_id,
            "current_performance": recent_accuracy,
            "baseline_performance": self.baseline_performance,
            "total_feedback": len(self.feedback_buffer),
            "calibration_events": len(self.calibration_events),
            "last_calibration": self.last_calibration.isoformat(),
            "optimizer_generation": self.optimizer.generation,
            "best_performer_score": (
                self.optimizer.best_performer.performance_score
                if self.optimizer.best_performer
                else None
            ),
            "recent_events": [
                asdict(event) for event in list(self.calibration_events)[-5:]
            ],
        }

    def save_calibration(self, filepath: str):
        """Сохраняет состояние калибровки."""
        try:
            state = {
                "user_id": self.user_id,
                "current_parameters": (
                    asdict(self.current_parameters) if self.current_parameters else None
                ),
                "baseline_performance": self.baseline_performance,
                "optimizer_generation": self.optimizer.generation,
                "calibration_events": [
                    asdict(event) for event in self.calibration_events
                ],
            }

            with open(filepath, "w") as f:
                json.dump(state, f, indent=2, default=str)

            safe_logger.info(f"Calibration state saved to {filepath}")

        except Exception as e:
            safe_logger.error(f"Failed to save calibration: {e}")

    def load_calibration(self, filepath: str):
        """Загружает состояние калибровки."""
        try:
            with open(filepath, "r") as f:
                state = json.load(f)

            self.baseline_performance = state.get("baseline_performance", 0.5)
            self.optimizer.generation = state.get("optimizer_generation", 0)

            if state.get("current_parameters"):
                params_dict = state["current_parameters"]
                self.current_parameters = ParameterSet(**params_dict)

            safe_logger.info(f"Calibration state loaded from {filepath}")

        except Exception as e:
            safe_logger.warning(f"Failed to load calibration: {e}")


# Global calibrator instances
_calibrators = {}


def get_calibrator(user_id: str) -> AdaptiveCalibrator:
    """Возвращает калибратор для пользователя."""
    if user_id not in _calibrators:
        _calibrators[user_id] = AdaptiveCalibrator(user_id)
    return _calibrators[user_id]
