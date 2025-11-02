"""
🚀🛡️ BurnoutGuard Real-time Streaming — мониторинг выгорания в реальном времени

Система real-time мониторинга выгорания, интегрированная с существующей
архитектурой WebSocket streaming:
- Расширение EmotionalUpdate для данных о выгорании
- Адаптивные уведомления о риске
- Безопасная передача критических алертов
- Интеграция с командной аналитикой

"Мгновенная защита от выгорания через умные уведомления" ⚡
"""

import asyncio
import time
import json
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, asdict
from collections import deque
import uuid

try:  # Allow module to work whether imported via `backend` namespace or locally
    from backend.emotime.websocket.streaming_engine import (
        RealTimeEmotionalStreamer, StreamingConfig, StreamingMetrics,
        AdaptiveQualityController, SafetyFilter,
    )
except ImportError:  # pragma: no cover - fallback for local runs
    from emotime.websocket.streaming_engine import (
        RealTimeEmotionalStreamer, StreamingConfig, StreamingMetrics,
        AdaptiveQualityController, SafetyFilter,
    )

try:
    from backend.websocket.connection_manager import EmotionalUpdate, get_connection_manager
except ImportError:  # pragma: no cover
    from websocket.connection_manager import EmotionalUpdate, get_connection_manager  # type: ignore
from .core import BurnoutGuardEngine, BurnoutState, BurnoutRisk
from .modes import BurnoutMode, BurnoutRiskLevel
from .utils import safe_logger, format_risk_score, create_alert_message


@dataclass
class BurnoutUpdate(EmotionalUpdate):
    """Расширение EmotionalUpdate для данных о выгорании."""
    
    # Добавляем специфические для выгорания поля
    burnout_risk_score: float = 0.0
    burnout_risk_level: str = "low"
    burnout_mode: str = "healthy"
    intervention_needed: bool = False
    
    # Индикаторы и рекомендации
    risk_indicators: List[str] = None
    recommendations: List[str] = None
    
    # Тренды и аналитика
    risk_trend: str = "stable"  # rising, falling, stable
    mode_stability: float = 1.0
    time_since_last_break: Optional[float] = None
    
    # Командная информация (для HR)
    team_average_risk: Optional[float] = None
    team_alert_level: Optional[str] = None
    
    def __post_init__(self):
        super().__post_init__()
        if self.risk_indicators is None:
            self.risk_indicators = []
        if self.recommendations is None:
            self.recommendations = []


@dataclass
class BurnoutStreamingConfig(StreamingConfig):
    """Конфигурация потокового вещания для выгорания."""
    
    # Специфичные настройки для выгорания
    risk_alert_threshold: float = 0.6      # порог для алертов
    critical_alert_threshold: float = 0.8  # порог для критических алертов
    
    # Частота уведомлений
    alert_frequency_minutes: float = 5.0   # минимальный интервал между алертами
    critical_alert_immediate: bool = True  # критические алерты сразу
    
    # Командные настройки
    team_monitoring_enabled: bool = False
    hr_notifications_enabled: bool = False
    
    # Персонализация
    user_notification_preferences: Dict[str, bool] = None
    
    def __post_init__(self):
        if self.user_notification_preferences is None:
            self.user_notification_preferences = {
                "instant_alerts": True,
                "daily_summary": True,
                "weekly_report": True,
                "break_reminders": True
            }


class BurnoutSafetyFilter(SafetyFilter):
    """
    Безопасная фильтрация данных о выгорании.
    
    Защищает от:
    - Ложных алармов при низкой уверенности
    - Слишком частых уведомлений
    - Паники от критических значений
    - Утечки приватных данных
    """
    
    def __init__(self):
        super().__init__()
        self.last_alert_time: Dict[str, datetime] = {}
        self.alert_cooldown = timedelta(minutes=5)
        
    def filter_burnout_update(self, update: BurnoutUpdate) -> BurnoutUpdate:
        """Применяет безопасную фильтрацию к обновлению выгорания."""
        
        filtered_update = update
        
        # 1. Фильтрация ложных алармов
        if update.confidence < 0.4 and update.burnout_risk_score > 0.7:
            # Низкая уверенность + высокий риск = снижаем риск
            filtered_update.burnout_risk_score *= 0.7
            filtered_update.safety_status = "confidence_adjusted"
            safe_logger.debug(f"Adjusted high risk due to low confidence: {update.confidence}")
        
        # 2. Контроль частоты алертов
        user_id = getattr(update, 'user_id', 'unknown')
        now = datetime.now()
        
        if user_id in self.last_alert_time:
            time_since_last = now - self.last_alert_time[user_id]
            if (time_since_last < self.alert_cooldown and 
                update.burnout_risk_score > 0.6 and 
                update.burnout_risk_score < 0.8):  # не критический
                
                # Подавляем не-критические алерты в cooldown периоде
                filtered_update.intervention_needed = False
                filtered_update.safety_status = "alert_cooldown"
        
        # 3. Сглаживание критических значений
        if update.burnout_risk_score > 0.95:
            # Очень высокие значения сглаживаем для предотвращения паники
            filtered_update.burnout_risk_score = 0.9
            filtered_update.safety_status = "panic_prevention"
        
        # 4. Защита приватности в командных данных
        if hasattr(update, 'team_average_risk') and update.team_average_risk:
            # Не передаем точные командные данные если пользователь не HR
            if not getattr(update, 'is_hr_user', False):
                filtered_update.team_average_risk = None
                filtered_update.team_alert_level = None
        
        # 5. Обновляем время последнего алерта
        if filtered_update.intervention_needed:
            self.last_alert_time[user_id] = now
        
        return filtered_update


class BurnoutAdaptiveController(AdaptiveQualityController):
    """
    Адаптивный контроллер качества для мониторинга выгорания.
    
    Автоматически адаптирует частоту и детализацию мониторинга
    на основе уровня риска и активности пользователя.
    """
    
    def __init__(self):
        super().__init__()
        self.risk_based_quality = True
        
    def get_adaptive_config(self, risk_score: float, activity_level: str = "normal") -> Dict[str, Any]:
        """Получает адаптивную конфигурацию на основе риска."""
        
        base_config = self.get_quality_config(self.current_quality)
        
        # Адаптация на основе риска выгорания
        if risk_score > 0.8:  # критический риск
            base_config.update({
                "update_frequency": 3.0,  # увеличиваем частоту
                "feature_detail": "full",
                "ml_insights": True,
                "alert_priority": "high"
            })
        elif risk_score > 0.6:  # высокий риск
            base_config.update({
                "update_frequency": 2.0,
                "feature_detail": "enhanced",
                "ml_insights": True,
                "alert_priority": "medium"
            })
        elif risk_score < 0.2:  # низкий риск
            base_config.update({
                "update_frequency": 0.5,  # снижаем частоту
                "feature_detail": "minimal",
                "ml_insights": False,
                "alert_priority": "low"
            })
        
        # Адаптация на основе активности
        if activity_level == "high":
            base_config["update_frequency"] *= 1.5
        elif activity_level == "low":
            base_config["update_frequency"] *= 0.7
        
        return base_config


class BurnoutStreamingEngine:
    """
    Движок real-time мониторинга выгорания.
    
    Интегрируется с существующей системой WebSocket streaming
    для предоставления мгновенных уведомлений о риске выгорания.
    """
    
    def __init__(
        self, 
        user_id: str,
        burnout_engine: BurnoutGuardEngine,
        team_id: Optional[str] = None
    ):
        self.user_id = user_id
        self.team_id = team_id
        self.burnout_engine = burnout_engine
        
        # Компоненты streaming
        self.connection_manager = get_connection_manager()
        self.safety_filter = BurnoutSafetyFilter()
        self.adaptive_controller = BurnoutAdaptiveController()
        
        # Конфигурация
        self.config = BurnoutStreamingConfig()
        
        # Состояние и метрики
        self.is_streaming = False
        self.last_update_time: Optional[datetime] = None
        self.update_count = 0
        
        # Буферы для адаптивности
        self.risk_history = deque(maxlen=20)
        self.update_buffer = deque(maxlen=5)
        
    async def start_streaming(self):
        """Запускает real-time мониторинг выгорания."""
        
        if self.is_streaming:
            safe_logger.warning(f"Streaming already active for user {self.user_id}")
            return
            
        self.is_streaming = True
        safe_logger.info(f"Starting burnout streaming for user {self.user_id}")
        
        # Подписываемся на каналы
        await self._subscribe_to_channels()
        
        # Запускаем основной цикл мониторинга
        asyncio.create_task(self._monitoring_loop())
        
    async def stop_streaming(self):
        """Останавливает real-time мониторинг."""
        
        self.is_streaming = False
        safe_logger.info(f"Stopped burnout streaming for user {self.user_id}")
        
        # Отписываемся от каналов
        await self._unsubscribe_from_channels()
    
    async def _subscribe_to_channels(self):
        """Подписывается на необходимые каналы."""
        
        # Персональный канал
        personal_channel = f"burnout:{self.user_id}"
        await self.connection_manager.subscribe_to_channel(self.user_id, personal_channel)
        
        # Командный канал (если есть команда)
        if self.team_id:
            team_channel = f"burnout:team:{self.team_id}"
            await self.connection_manager.subscribe_to_channel(self.user_id, team_channel)
        
        # Общие алерты
        global_channel = "burnout:alerts"
        await self.connection_manager.subscribe_to_channel(self.user_id, global_channel)
    
    async def _unsubscribe_from_channels(self):
        """Отписывается от каналов."""
        
        channels = [
            f"burnout:{self.user_id}",
            "burnout:alerts"
        ]
        
        if self.team_id:
            channels.append(f"burnout:team:{self.team_id}")
        
        for channel in channels:
            await self.connection_manager.unsubscribe_from_channel(self.user_id, channel)
    
    async def _monitoring_loop(self):
        """Основной цикл мониторинга выгорания."""
        
        while self.is_streaming:
            try:
                # Получаем текущее состояние выгорания
                burnout_state = await self.burnout_engine.get_current_state()
                
                if burnout_state:
                    # Создаем обновление
                    update = await self._create_burnout_update(burnout_state)
                    
                    # Применяем безопасную фильтрацию
                    filtered_update = self.safety_filter.filter_burnout_update(update)
                    
                    # Определяем нужно ли отправить обновление
                    should_send = await self._should_send_update(filtered_update)
                    
                    if should_send:
                        await self._send_burnout_update(filtered_update)
                        
                    # Обрабатываем критические ситуации
                    if filtered_update.burnout_risk_score > 0.8:
                        await self._handle_critical_risk(filtered_update)
                
                # Адаптивная задержка
                delay = await self._calculate_adaptive_delay(burnout_state)
                await asyncio.sleep(delay)
                
            except Exception as e:
                safe_logger.error(f"Error in burnout monitoring loop: {e}")
                await asyncio.sleep(5.0)  # задержка при ошибке
    
    async def _create_burnout_update(self, burnout_state: BurnoutState) -> BurnoutUpdate:
        """Создает BurnoutUpdate из BurnoutState."""
        
        # Базовые данные от Emotime
        emotime_state = burnout_state.emotime_state
        
        # Создаем BurnoutUpdate
        update = BurnoutUpdate(
            user_id=self.user_id,
            session_id=getattr(burnout_state, 'session_id', f'burnout_{uuid.uuid4()}'),
            timestamp=burnout_state.timestamp,
            
            # Эмоциональные данные
            mode=emotime_state.mode.name,
            features=asdict(emotime_state.features),
            confidence=emotime_state.confidence,
            resonance_trace=[asdict(point) for point in emotime_state.resonance_trace[-5:]],
            
            # Данные о выгорании
            burnout_risk_score=burnout_state.risk_assessment.score,
            burnout_risk_level=burnout_state.risk_assessment.level.value,
            burnout_mode=burnout_state.burnout_mode.type.value,
            intervention_needed=burnout_state.intervention_needed,
            
            # Индикаторы и рекомендации
            risk_indicators=burnout_state.risk_assessment.emotional_indicators,
            recommendations=await self._generate_recommendations(burnout_state),
            
            # Тренды
            risk_trend=await self._calculate_risk_trend(burnout_state),
            mode_stability=burnout_state.mode_stability,
            
            # Метаданные
            ml_insights={"burnout_confidence": burnout_state.burnout_mode.confidence},
            safety_status="normal"
        )
        
        # Добавляем командные данные если доступны
        if self.team_id and await self._user_has_team_access():
            team_data = await self._get_team_analytics()
            if team_data:
                update.team_average_risk = team_data.get("average_risk")
                update.team_alert_level = team_data.get("alert_level")
        
        return update
    
    async def _should_send_update(self, update: BurnoutUpdate) -> bool:
        """Определяет, нужно ли отправить обновление."""
        
        # Всегда отправляем критические алерты
        if update.burnout_risk_score > 0.8:
            return True
        
        # Отправляем если изменился уровень риска
        if len(self.risk_history) > 0:
            last_risk = self.risk_history[-1]
            current_level = update.burnout_risk_level
            last_level = self._get_risk_level_from_score(last_risk)
            
            if current_level != last_level:
                return True
        
        # Отправляем если нужно вмешательство
        if update.intervention_needed:
            return True
        
        # Отправляем по расписанию (каждые N секунд)
        if not self.last_update_time:
            return True
            
        time_since_last = datetime.now() - self.last_update_time
        min_interval = self._get_min_update_interval(update.burnout_risk_score)
        
        return time_since_last.total_seconds() >= min_interval
    
    async def _send_burnout_update(self, update: BurnoutUpdate):
        """Отправляет обновление о выгорании через WebSocket."""
        
        try:
            # Конвертируем в словарь для отправки
            update_data = asdict(update)
            update_data["type"] = "burnout_update"
            
            # Отправляем персональное обновление
            await self.connection_manager.send_to_user(
                self.user_id, 
                update_data,
                require_ack=update.intervention_needed  # требуем подтверждение для важных сообщений
            )
            
            # Отправляем командное обновление (если нужно)
            if self.team_id and update.burnout_risk_score > 0.6:
                await self._send_team_alert(update)
            
            # Обновляем статистику
            self.last_update_time = datetime.now()
            self.update_count += 1
            self.risk_history.append(update.burnout_risk_score)
            
            safe_logger.debug(f"Sent burnout update to user {self.user_id}: risk={update.burnout_risk_score:.2f}")
            
        except Exception as e:
            safe_logger.error(f"Failed to send burnout update: {e}")
    
    async def _handle_critical_risk(self, update: BurnoutUpdate):
        """Обрабатывает критические ситуации с риском выгорания."""
        
        # Создаем критический алерт
        alert_data = {
            "type": "critical_burnout_alert",
            "user_id": self.user_id,
            "risk_score": update.burnout_risk_score,
            "risk_level": update.burnout_risk_level,
            "indicators": update.risk_indicators,
            "recommendations": update.recommendations,
            "timestamp": update.timestamp.isoformat(),
            "message": create_alert_message(update.burnout_risk_score, update.risk_indicators)
        }
        
        # Отправляем немедленно с высоким приоритетом
        await self.connection_manager.send_to_user(
            self.user_id,
            alert_data,
            require_ack=True,
            priority="high"
        )
        
        # Уведомляем команду/HR (если настроено)
        if self.team_id and self.config.hr_notifications_enabled:
            await self._notify_team_about_critical_risk(update)
        
        safe_logger.warning(
            f"Critical burnout risk for user {self.user_id}: "
            f"{update.burnout_risk_score:.2f} ({update.burnout_risk_level})"
        )
    
    async def _calculate_adaptive_delay(self, burnout_state: Optional[BurnoutState]) -> float:
        """Вычисляет адаптивную задержку между обновлениями."""
        
        if not burnout_state:
            return 30.0  # базовая задержка
        
        risk_score = burnout_state.risk_assessment.score
        
        # Высокий риск = чаще проверки
        if risk_score > 0.8:
            return 15.0
        elif risk_score > 0.6:
            return 30.0
        elif risk_score > 0.4:
            return 60.0
        else:
            return 120.0  # низкий риск = реже проверки
    
    async def _generate_recommendations(self, burnout_state: BurnoutState) -> List[str]:
        """Генерирует персонализированные рекомендации."""
        
        recommendations = []
        risk_score = burnout_state.risk_assessment.score
        burnout_mode = burnout_state.burnout_mode.type.value
        
        # Базовые рекомендации по уровню риска
        if risk_score > 0.8:
            recommendations.extend([
                "🚨 Немедленно сделайте перерыв на 15-20 минут",
                "💧 Выпейте воды и сделайте несколько глубоких вдохов", 
                "🚶‍♂️ Прогуляйтесь на свежем воздухе"
            ])
        elif risk_score > 0.6:
            recommendations.extend([
                "⏰ Запланируйте перерыв в ближайшие 30 минут",
                "🎯 Сосредоточьтесь на одной задаче за раз",
                "📱 Отключите уведомления на 1 час"
            ])
        elif risk_score > 0.4:
            recommendations.extend([
                "⚖️ Оцените текущую нагрузку и приоритеты",
                "🧘‍♀️ Попробуйте 5-минутную медитацию",
                "📝 Запишите 3 вещи, за которые вы благодарны"
            ])
        
        # Специфичные рекомендации по типу выгорания
        if burnout_mode == "overwork":
            recommendations.append("📅 Пересмотрите расписание и делегируйте задачи")
        elif burnout_mode == "emotional_exhaustion":
            recommendations.append("🤗 Поговорите с коллегой или близким человеком")
        elif burnout_mode == "cynicism":
            recommendations.append("💡 Найдите позитивный аспект в текущей работе")
        
        return recommendations[:5]  # максимум 5 рекомендаций
    
    async def _calculate_risk_trend(self, burnout_state: BurnoutState) -> str:
        """Вычисляет тренд изменения риска."""
        
        if len(self.risk_history) < 3:
            return "stable"
        
        recent_risks = list(self.risk_history)[-3:]
        current_risk = burnout_state.risk_assessment.score
        
        # Простой анализ тренда
        if current_risk > recent_risks[-1] + 0.1:
            return "rising"
        elif current_risk < recent_risks[-1] - 0.1:
            return "falling"
        else:
            return "stable"
    
    def _get_risk_level_from_score(self, score: float) -> str:
        """Получает уровень риска из скора."""
        if score <= 0.2:
            return "very_low"
        elif score <= 0.4:
            return "low"
        elif score <= 0.6:
            return "medium"
        elif score <= 0.8:
            return "high"
        else:
            return "critical"
    
    def _get_min_update_interval(self, risk_score: float) -> float:
        """Получает минимальный интервал обновлений."""
        if risk_score > 0.8:
            return 10.0  # критический - каждые 10 секунд
        elif risk_score > 0.6:
            return 30.0  # высокий - каждые 30 секунд
        elif risk_score > 0.4:
            return 60.0  # средний - каждую минуту
        else:
            return 120.0  # низкий - каждые 2 минуты
    
    async def _user_has_team_access(self) -> bool:
        """Проверяет, есть ли у пользователя доступ к командным данным."""
        # Здесь должна быть проверка прав доступа
        # Пока возвращаем True если есть team_id
        return self.team_id is not None
    
    async def _get_team_analytics(self) -> Optional[Dict[str, Any]]:
        """Получает аналитику команды."""
        # Заглушка для командной аналитики
        # В реальной реализации здесь будет запрос к базе данных
        return {
            "average_risk": 0.45,
            "alert_level": "medium"
        }
    
    async def _send_team_alert(self, update: BurnoutUpdate):
        """Отправляет алерт команде."""
        if not self.team_id:
            return
            
        team_alert = {
            "type": "team_burnout_alert",
            "team_id": self.team_id,
            "user_id": self.user_id,  # может быть анонимизирован
            "risk_level": update.burnout_risk_level,
            "timestamp": update.timestamp.isoformat(),
            "message": f"Член команды имеет {update.burnout_risk_level} риск выгорания"
        }
        
        # Отправляем на командный канал
        team_channel = f"burnout:team:{self.team_id}"
        await self.connection_manager.broadcast_to_channel(team_channel, team_alert)
    
    async def _notify_team_about_critical_risk(self, update: BurnoutUpdate):
        """Уведомляет команду о критическом риске."""
        if not self.team_id:
            return
            
        critical_notification = {
            "type": "critical_team_alert",
            "team_id": self.team_id,
            "risk_level": "critical",
            "timestamp": update.timestamp.isoformat(),
            "message": "Обнаружен критический риск выгорания в команде",
            "action_required": True
        }
        
        # Отправляем HR и руководителям
        hr_channel = f"burnout:hr:{self.team_id}"
        await self.connection_manager.broadcast_to_channel(hr_channel, critical_notification)


# Фабричная функция для создания BurnoutStreamingEngine
async def create_burnout_streaming_engine(
    user_id: str,
    burnout_engine: BurnoutGuardEngine,
    team_id: Optional[str] = None
) -> BurnoutStreamingEngine:
    """Создает и настраивает BurnoutStreamingEngine."""
    
    engine = BurnoutStreamingEngine(user_id, burnout_engine, team_id)
    return engine