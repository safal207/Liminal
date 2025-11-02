"""
🚀🛡️ BurnoutGuard Core Engine — ядро защиты от выгорания

Центральный движок BurnoutGuard, интегрированный с Emotime:
- Анализ риска выгорания в реальном времени
- Система скоринга на основе ML и confidence
- Персонализированные алгоритмы детекции
- Интеграция с существующей архитектурой

"Превращаем эмоциональный анализ в защиту от выгорания" ✨
"""

import asyncio
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, asdict

try:  # Support both `backend.*` and direct package imports
    from backend.emotime.core import EmotimeEngine, EmotimeState
    from backend.emotime.modes import EmotionalMode
    from backend.emotime.fusion import EmotionalFeatures
    from backend.emotime.timeseries import EmotionalPoint
except ImportError:  # pragma: no cover - fallback for local runs
    from emotime.core import EmotimeEngine, EmotimeState
    from emotime.modes import EmotionalMode
    from emotime.fusion import EmotionalFeatures
    from emotime.timeseries import EmotionalPoint
from .modes import BurnoutModeMapper, BurnoutMode, BurnoutRiskLevel
from .utils import safe_logger

try:
    from backend.emotime.ml import AdaptiveCalibrator, AdaptiveEmotionalEngine
    ML_AVAILABLE = True
except ImportError:
    try:
        from emotime.ml import AdaptiveCalibrator, AdaptiveEmotionalEngine  # type: ignore
        ML_AVAILABLE = True
    except ImportError:
        ML_AVAILABLE = False


@dataclass
class BurnoutRisk:
    """Оценка риска выгорания."""
    score: float                    # 0.0-1.0 общий скор риска
    level: BurnoutRiskLevel        # уровень риска
    factors: Dict[str, float]      # факторы риска и их веса
    confidence: float              # уверенность в оценке
    timestamp: datetime
    
    # Подробная аналитика
    emotional_indicators: List[str]    # эмоциональные индикаторы
    behavioral_patterns: List[str]     # поведенческие паттерны
    duration_risk: float              # риск от длительности состояний
    trend_risk: float                 # риск от негативных трендов
    
    
@dataclass 
class BurnoutState:
    """Полное состояние BurnoutGuard."""
    timestamp: datetime
    emotime_state: EmotimeState        # базовое эмоциональное состояние
    burnout_mode: BurnoutMode          # режим выгорания
    risk_assessment: BurnoutRisk       # оценка риска
    
    # История и тренды
    risk_history: List[float]          # история скоров риска (последние N)
    mode_stability: float              # стабильность режима (0.0-1.0)
    intervention_needed: bool          # нужно ли вмешательство
    
    
class BurnoutRiskScorer:
    """
    Система скоринга риска выгорания.
    
    Использует многофакторный анализ:
    1. Эмоциональные факторы (валентность, возбуждение, режимы)
    2. Поведенческие факторы (паттерны активности, время)
    3. Временные факторы (длительность, тренды, циклы)
    4. ML-факторы (confidence, адаптивные модели)
    """
    
    # Веса факторов риска
    RISK_FACTOR_WEIGHTS = {
        "emotional_state": 0.35,      # текущее эмоциональное состояние
        "behavioral_patterns": 0.25,   # поведенческие паттерны
        "temporal_analysis": 0.25,     # временной анализ и тренды
        "ml_confidence": 0.15         # ML уверенность и адаптация
    }
    
    # Критерии для различных факторов риска
    EMOTIONAL_RISK_CRITERIA = {
        "high_stress": {"weight": 0.4, "threshold": 0.7},
        "low_joy": {"weight": 0.3, "threshold": 0.2},
        "negative_valence": {"weight": 0.3, "threshold": -0.3},
        "high_arousal": {"weight": 0.2, "threshold": 0.8},
        "emotional_flatness": {"weight": 0.25, "threshold": 0.1}
    }
    
    def __init__(self, user_id: str):
        self.user_id = user_id
        self.risk_history: List[Tuple[float, datetime]] = []
        self.max_history = 100  # максимум записей в истории
        
    async def calculate_risk(
        self, 
        emotime_state: EmotimeState,
        burnout_mode: BurnoutMode,
        additional_context: Optional[Dict[str, Any]] = None
    ) -> BurnoutRisk:
        """
        Вычисляет комплексную оценку риска выгорания.
        
        Args:
            emotime_state: Текущее эмоциональное состояние
            burnout_mode: Текущий режим выгорания
            additional_context: Дополнительный контекст (время работы, активность и т.д.)
            
        Returns:
            Комплексная оценка риска выгорания
        """
        
        context = additional_context or {}
        
        # 1. Анализ эмоциональных факторов
        emotional_score, emotional_indicators = await self._analyze_emotional_factors(
            emotime_state.features, emotime_state.mode
        )
        
        # 2. Анализ поведенческих паттернов
        behavioral_score, behavioral_patterns = await self._analyze_behavioral_patterns(
            emotime_state, context
        )
        
        # 3. Временной анализ
        temporal_score = await self._analyze_temporal_factors(
            emotime_state, burnout_mode
        )
        
        # 4. ML confidence анализ
        ml_score = await self._analyze_ml_factors(emotime_state)
        
        # 5. Комбинируем факторы
        total_score = (
            emotional_score * self.RISK_FACTOR_WEIGHTS["emotional_state"] +
            behavioral_score * self.RISK_FACTOR_WEIGHTS["behavioral_patterns"] +
            temporal_score * self.RISK_FACTOR_WEIGHTS["temporal_analysis"] +
            ml_score * self.RISK_FACTOR_WEIGHTS["ml_confidence"]
        )
        
        # 6. Определяем уровень риска
        risk_level = self._determine_risk_level(total_score)
        
        # 7. Вычисляем уверенность
        confidence = await self._calculate_confidence(
            emotime_state, [emotional_score, behavioral_score, temporal_score, ml_score]
        )
        
        # 8. Собираем факторы
        risk_factors = {
            "emotional": emotional_score,
            "behavioral": behavioral_score, 
            "temporal": temporal_score,
            "ml_confidence": ml_score
        }
        
        # Сохраняем в историю
        self.risk_history.append((total_score, datetime.now()))
        if len(self.risk_history) > self.max_history:
            self.risk_history = self.risk_history[-self.max_history:]
        
        return BurnoutRisk(
            score=total_score,
            level=risk_level,
            factors=risk_factors,
            confidence=confidence,
            timestamp=datetime.now(),
            emotional_indicators=emotional_indicators,
            behavioral_patterns=behavioral_patterns,
            duration_risk=temporal_score,
            trend_risk=await self._calculate_trend_risk()
        )
    
    async def _analyze_emotional_factors(
        self, 
        features: EmotionalFeatures,
        mode: EmotionalMode
    ) -> Tuple[float, List[str]]:
        """Анализирует эмоциональные факторы риска."""
        
        indicators = []
        total_risk = 0.0
        
        # Проверяем критерии риска
        if features.valence < self.EMOTIONAL_RISK_CRITERIA["negative_valence"]["threshold"]:
            risk = self.EMOTIONAL_RISK_CRITERIA["negative_valence"]["weight"]
            total_risk += risk
            indicators.append(f"Негативная валентность ({features.valence:.2f})")
            
        if features.arousal > self.EMOTIONAL_RISK_CRITERIA["high_arousal"]["threshold"]:
            risk = self.EMOTIONAL_RISK_CRITERIA["high_arousal"]["weight"]
            total_risk += risk
            indicators.append(f"Высокое возбуждение ({features.arousal:.2f})")
            
        if features.intensity < self.EMOTIONAL_RISK_CRITERIA["emotional_flatness"]["threshold"]:
            risk = self.EMOTIONAL_RISK_CRITERIA["emotional_flatness"]["weight"]
            total_risk += risk
            indicators.append(f"Эмоциональная притупленность ({features.intensity:.2f})")
        
        # Анализ режима
        try:
            from backend.emotime.modes import ModeType
        except ImportError:  # pragma: no cover
            from emotime.modes import ModeType  # type: ignore
        if mode.type == ModeType.STRESS:
            total_risk += self.EMOTIONAL_RISK_CRITERIA["high_stress"]["weight"]
            indicators.append(f"Режим стресса (интенсивность: {mode.intensity:.2f})")
            
        if mode.type == ModeType.JOY and mode.intensity < self.EMOTIONAL_RISK_CRITERIA["low_joy"]["threshold"]:
            total_risk += self.EMOTIONAL_RISK_CRITERIA["low_joy"]["weight"]
            indicators.append("Недостаток радости")
        
        # Нормализуем к [0, 1]
        normalized_risk = min(1.0, total_risk)
        
        return normalized_risk, indicators
    
    async def _analyze_behavioral_patterns(
        self, 
        emotime_state: EmotimeState,
        context: Dict[str, Any]
    ) -> Tuple[float, List[str]]:
        """Анализирует поведенческие паттерны."""
        
        patterns = []
        risk_score = 0.0
        
        # Анализ времени активности
        current_hour = datetime.now().hour
        
        if current_hour < 6 or current_hour > 22:
            risk_score += 0.3
            patterns.append(f"Активность в нерабочее время ({current_hour}:00)")
        
        # Анализ продолжительности сессии
        session_duration = context.get("session_duration_hours", 0)
        if session_duration > 8:
            risk_score += 0.4
            patterns.append(f"Длительная сессия ({session_duration:.1f} часов)")
        elif session_duration > 10:
            risk_score += 0.6
            patterns.append(f"Критически длительная сессия ({session_duration:.1f} часов)")
        
        # Анализ частоты переключений режимов
        if len(emotime_state.resonance_trace) > 5:
            recent_modes = [point.mode for point in emotime_state.resonance_trace[-10:]]
            mode_changes = sum(1 for i in range(1, len(recent_modes)) 
                             if recent_modes[i] != recent_modes[i-1])
            
            if mode_changes > 6:  # слишком частые переключения
                risk_score += 0.2
                patterns.append(f"Частые смены эмоционального состояния ({mode_changes} за 10 циклов)")
        
        # Анализ качества данных (confidence)
        avg_confidence = emotime_state.confidence
        if avg_confidence < 0.3:
            risk_score += 0.2
            patterns.append(f"Низкая уверенность в анализе ({avg_confidence:.2f})")
        
        return min(1.0, risk_score), patterns
    
    async def _analyze_temporal_factors(
        self, 
        emotime_state: EmotimeState,
        burnout_mode: BurnoutMode
    ) -> float:
        """Анализирует временные факторы риска."""
        
        risk_score = 0.0
        
        # Длительность текущего режима выгорания
        if burnout_mode.duration_hours > 4:
            risk_score += 0.3
        elif burnout_mode.duration_hours > 8:
            risk_score += 0.5
        elif burnout_mode.duration_hours > 12:
            risk_score += 0.7
        
        # Анализ трендов в истории
        if len(self.risk_history) >= 5:
            recent_scores = [score for score, _ in self.risk_history[-5:]]
            trend = np.polyfit(range(len(recent_scores)), recent_scores, 1)[0]
            
            if trend > 0.1:  # возрастающий тренд
                risk_score += min(0.4, trend * 2.0)
        
        # Анализ стабильности эмоционального состояния
        if len(emotime_state.resonance_trace) > 5:
            recent_valences = [point.valence for point in emotime_state.resonance_trace[-10:]]
            if recent_valences:
                valence_std = np.std(recent_valences)
                if valence_std > 0.4:  # высокая вариативность = нестабильность
                    risk_score += 0.2
        
        return min(1.0, risk_score)
    
    async def _analyze_ml_factors(self, emotime_state: EmotimeState) -> float:
        """Анализирует ML-факторы (confidence, адаптивность)."""
        
        if not ML_AVAILABLE:
            return 0.5  # нейтральный скор если ML недоступен
        
        # Базовый анализ confidence
        confidence_factor = 1.0 - emotime_state.confidence  # низкий confidence = высокий риск
        
        # Если есть дополнительные ML метрики, добавляем их
        ml_risk = confidence_factor * 0.8  # confidence составляет 80% ML фактора
        
        return min(1.0, ml_risk)
    
    def _determine_risk_level(self, score: float) -> BurnoutRiskLevel:
        """Определяет уровень риска по скору."""
        
        if score <= 0.2:
            return BurnoutRiskLevel.VERY_LOW
        elif score <= 0.4:
            return BurnoutRiskLevel.LOW
        elif score <= 0.6:
            return BurnoutRiskLevel.MEDIUM
        elif score <= 0.8:
            return BurnoutRiskLevel.HIGH
        else:
            return BurnoutRiskLevel.CRITICAL
    
    async def _calculate_confidence(
        self, 
        emotime_state: EmotimeState,
        factor_scores: List[float]
    ) -> float:
        """Вычисляет уверенность в оценке риска."""
        
        # Базовая уверенность от Emotime
        base_confidence = emotime_state.confidence
        
        # Уверенность на основе согласованности факторов
        factor_std = np.std(factor_scores)
        consistency_confidence = max(0.0, 1.0 - factor_std)  # низкая вариативность = высокая уверенность
        
        # Уверенность на основе количества данных
        data_confidence = min(1.0, len(emotime_state.resonance_trace) / 10.0)
        
        # Комбинируем
        total_confidence = (
            base_confidence * 0.5 +
            consistency_confidence * 0.3 +
            data_confidence * 0.2
        )
        
        return min(0.95, max(0.1, total_confidence))
    
    async def _calculate_trend_risk(self) -> float:
        """Вычисляет риск на основе трендов."""
        
        if len(self.risk_history) < 3:
            return 0.0
            
        # Берем последние 10 значений
        recent_scores = [score for score, _ in self.risk_history[-10:]]
        
        if len(recent_scores) < 3:
            return 0.0
            
        # Вычисляем тренд
        trend = np.polyfit(range(len(recent_scores)), recent_scores, 1)[0]
        
        # Положительный тренд = рост риска
        return max(0.0, min(1.0, trend * 3.0))


class BurnoutGuardEngine:
    """
    Главный движок BurnoutGuard.
    
    Интегрируется с EmotimeEngine для анализа выгорания в реальном времени.
    """
    
    def __init__(
        self,
        user_id: str,
        emotime_engine: EmotimeEngine,
        update_interval: float = 60.0,  # обновление каждую минуту
        lookback_hours: int = 24        # анализ за последние 24 часа
    ):
        self.user_id = user_id
        self.emotime_engine = emotime_engine
        self.update_interval = update_interval
        
        # Компоненты системы
        self.mode_mapper = BurnoutModeMapper(lookback_hours=lookback_hours)
        self.risk_scorer = BurnoutRiskScorer(user_id=user_id)
        
        # Текущее состояние
        self.current_state: Optional[BurnoutState] = None
        self.is_running = False
        
        # История состояний
        self.state_history: List[BurnoutState] = []
        self.max_history = 1000
        
    async def start(self):
        """Запускает BurnoutGuard движок."""
        self.is_running = True
        safe_logger.info(f"BurnoutGuard engine started for user {self.user_id}")
        
        # Запускаем главный цикл анализа
        asyncio.create_task(self._analysis_loop())
    
    async def stop(self):
        """Останавливает BurnoutGuard движок."""
        self.is_running = False
        safe_logger.info(f"BurnoutGuard engine stopped for user {self.user_id}")
    
    async def get_current_state(self) -> Optional[BurnoutState]:
        """Возвращает текущее состояние анализа выгорания."""
        return self.current_state
    
    async def get_risk_assessment(self) -> Optional[BurnoutRisk]:
        """Возвращает текущую оценку риска."""
        if self.current_state:
            return self.current_state.risk_assessment
        return None
    
    async def analyze_now(self, additional_context: Optional[Dict[str, Any]] = None) -> Optional[BurnoutState]:
        """Выполняет немедленный анализ состояния."""
        return await self._perform_analysis(additional_context)
    
    async def _analysis_loop(self):
        """Главный цикл анализа выгорания."""
        while self.is_running:
            try:
                await self._perform_analysis()
                await asyncio.sleep(self.update_interval)
                
            except Exception as e:
                safe_logger.error(f"BurnoutGuard analysis error: {e}")
                await asyncio.sleep(self.update_interval)
    
    async def _perform_analysis(self, additional_context: Optional[Dict[str, Any]] = None) -> Optional[BurnoutState]:
        """Выполняет анализ риска выгорания."""
        
        # Получаем текущее эмоциональное состояние
        emotime_state = await self.emotime_engine.get_current_state()
        if not emotime_state:
            return None
        
        # Собираем историю эмоциональных режимов
        emotional_history = []
        for point in emotime_state.resonance_trace:
            if hasattr(point, 'mode') and hasattr(point, 'timestamp'):
                emotional_history.append((point.mode, point.timestamp))
        
        # Анализируем режим выгорания
        burnout_mode = await self.mode_mapper.analyze_burnout_risk(emotional_history)
        
        # Вычисляем оценку риска
        risk_assessment = await self.risk_scorer.calculate_risk(
            emotime_state, burnout_mode, additional_context
        )
        
        # Анализируем стабильность и тренды
        mode_stability = await self._calculate_mode_stability()
        intervention_needed = await self._determine_intervention_need(risk_assessment)
        
        # Собираем историю рисков
        risk_history = [score for score, _ in self.risk_scorer.risk_history[-20:]]
        
        # Создаем новое состояние
        new_state = BurnoutState(
            timestamp=datetime.now(),
            emotime_state=emotime_state,
            burnout_mode=burnout_mode,
            risk_assessment=risk_assessment,
            risk_history=risk_history,
            mode_stability=mode_stability,
            intervention_needed=intervention_needed
        )
        
        # Обновляем текущее состояние
        self.current_state = new_state
        
        # Сохраняем в историю
        self.state_history.append(new_state)
        if len(self.state_history) > self.max_history:
            self.state_history = self.state_history[-self.max_history:]
        
        # Логируем важные события
        if risk_assessment.level.value in ["high", "critical"]:
            safe_logger.warning(
                f"High burnout risk detected for user {self.user_id}: "
                f"{risk_assessment.level.value} ({risk_assessment.score:.2f})"
            )
        
        return new_state
    
    async def _calculate_mode_stability(self) -> float:
        """Вычисляет стабильность режима выгорания."""
        
        if len(self.state_history) < 3:
            return 0.5  # нейтральная стабильность при недостатке данных
        
        # Анализируем последние 10 состояний
        recent_modes = [state.burnout_mode.type for state in self.state_history[-10:]]
        
        if not recent_modes:
            return 0.5
        
        # Вычисляем процент времени в текущем режиме
        current_mode = recent_modes[-1]
        same_mode_count = sum(1 for mode in recent_modes if mode == current_mode)
        stability = same_mode_count / len(recent_modes)
        
        return stability
    
    async def _determine_intervention_need(self, risk_assessment: BurnoutRisk) -> bool:
        """Определяет необходимость вмешательства."""
        
        # Критерии для вмешательства
        if risk_assessment.level in [BurnoutRiskLevel.HIGH, BurnoutRiskLevel.CRITICAL]:
            return True
        
        # Если средний риск, но с возрастающим трендом
        if (risk_assessment.level == BurnoutRiskLevel.MEDIUM and 
            risk_assessment.trend_risk > 0.3):
            return True
        
        # Если низкая уверенность в анализе при высоком риске
        if (risk_assessment.score > 0.6 and risk_assessment.confidence < 0.4):
            return True
        
        return False