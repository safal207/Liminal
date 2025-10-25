"""
🚀🛡️ BurnoutGuard Team Analytics — командная аналитика для HR

Система аналитики выгорания для команд и HR отделов:
- Агрегированная аналитика по командам
- Раннее предупреждение о рисках команды
- Детализированные отчеты для HR
- Тренды и прогнозы выгорания
- Интеграция с существующей data architecture

"Превращаем данные о выгорании в стратегические решения для HR" 📊
"""

import asyncio
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, asdict
from enum import Enum
import statistics
from collections import defaultdict, deque

from .core import BurnoutState, BurnoutRisk
from .modes import BurnoutRiskLevel, BurnoutModeType
from .utils import safe_logger, format_risk_score, calculate_percentage_change


class TeamAlertLevel(Enum):
    """Уровни алертов для команды."""
    GREEN = "green"          # Все в порядке
    YELLOW = "yellow"        # Требует внимания
    ORANGE = "orange"        # Высокий риск
    RED = "red"             # Критическая ситуация


class TrendDirection(Enum):
    """Направление тренда."""
    IMPROVING = "improving"
    STABLE = "stable"
    DETERIORATING = "deteriorating"


@dataclass
class TeamMember:
    """Информация о члене команды."""
    user_id: str
    name: str
    role: str
    department: str
    current_risk_score: float
    risk_level: BurnoutRiskLevel
    last_update: datetime
    days_since_break: int = 0
    
    # Privacy settings
    anonymized: bool = False
    share_details: bool = True


@dataclass
class TeamBurnoutTrend:
    """Тренд выгорания команды."""
    timeframe: str                  # "24h", "7d", "30d"
    direction: TrendDirection
    change_percentage: float        # % изменения
    affected_members: int           # количество затронутых
    risk_distribution: Dict[BurnoutRiskLevel, int]
    key_factors: List[str]         # основные факторы
    
    
@dataclass
class TeamAnalytics:
    """Аналитика команды по выгоранию."""
    team_id: str
    team_name: str
    analysis_timestamp: datetime
    
    # Основные метрики
    total_members: int
    active_members: int              # активных за последние 24ч
    average_risk_score: float
    team_alert_level: TeamAlertLevel
    
    # Распределение рисков
    risk_distribution: Dict[BurnoutRiskLevel, int]
    high_risk_members: List[str]     # anonymized IDs
    
    # Тренды
    trend_24h: TeamBurnoutTrend
    trend_7d: TeamBurnoutTrend
    trend_30d: TeamBurnoutTrend
    
    # Индикаторы команды
    team_indicators: List[str]
    workload_metrics: Dict[str, float]
    
    # Рекомендации для HR
    hr_recommendations: List[str]
    intervention_priority: int       # 1-5 (5 = срочно)


@dataclass
class DepartmentAnalytics:
    """Аналитика отдела."""
    department_name: str
    teams: List[TeamAnalytics]
    total_employees: int
    average_department_risk: float
    critical_teams: List[str]        # team IDs with high risk
    
    # Сравнительная аналитика
    benchmark_comparison: Dict[str, float]  # сравнение с бенчмарками
    best_practices: List[str]
    

class TeamBurnoutAnalyzer:
    """
    Анализатор выгорания команд.
    
    Агрегирует индивидуальные данные о выгорании в командную аналитику
    для принятия управленческих решений.
    """
    
    def __init__(self, data_retention_days: int = 90):
        self.data_retention_days = data_retention_days
        
        # Хранилища данных (в production будет DB)
        self.team_members: Dict[str, List[TeamMember]] = {}  # team_id -> members
        self.historical_data: Dict[str, deque] = {}          # team_id -> historical analytics
        
        # Кэш для оптимизации
        self.analytics_cache: Dict[str, Tuple[TeamAnalytics, datetime]] = {}
        self.cache_ttl = timedelta(minutes=5)
        
    async def add_team_member(
        self, 
        team_id: str, 
        member: TeamMember
    ) -> bool:
        """Добавляет или обновляет участника команды."""
        
        if team_id not in self.team_members:
            self.team_members[team_id] = []
        
        # Обновляем существующего участника или добавляем нового
        for i, existing_member in enumerate(self.team_members[team_id]):
            if existing_member.user_id == member.user_id:
                self.team_members[team_id][i] = member
                safe_logger.debug(f"Updated team member {member.user_id} in team {team_id}")
                return True
        
        # Добавляем нового участника
        self.team_members[team_id].append(member)
        safe_logger.info(f"Added new team member {member.user_id} to team {team_id}")
        return True
    
    async def analyze_team(
        self, 
        team_id: str,
        team_name: str = None,
        include_trends: bool = True
    ) -> Optional[TeamAnalytics]:
        """Анализирует состояние команды по выгоранию."""
        
        # Проверяем кэш
        if team_id in self.analytics_cache:
            cached_analytics, cache_time = self.analytics_cache[team_id]
            if datetime.now() - cache_time < self.cache_ttl:
                return cached_analytics
        
        # Получаем участников команды
        members = self.team_members.get(team_id, [])
        if not members:
            safe_logger.warning(f"No members found for team {team_id}")
            return None
        
        # Фильтруем активных участников (обновления за последние 24ч)
        cutoff_time = datetime.now() - timedelta(hours=24)
        active_members = [m for m in members if m.last_update >= cutoff_time]
        
        if not active_members:
            safe_logger.warning(f"No active members in team {team_id}")
            return None
        
        # Вычисляем основные метрики
        risk_scores = [m.current_risk_score for m in active_members]
        average_risk = statistics.mean(risk_scores)
        
        # Распределение по уровням риска
        risk_distribution = {level: 0 for level in BurnoutRiskLevel}
        for member in active_members:
            risk_distribution[member.risk_level] += 1
        
        # Определяем уровень алерта команды
        team_alert_level = self._determine_team_alert_level(
            active_members, average_risk, risk_distribution
        )
        
        # Высокорисковые участники (анонимизированные)
        high_risk_members = [
            f"member_{i+1}" for i, m in enumerate(active_members)
            if m.risk_level in [BurnoutRiskLevel.HIGH, BurnoutRiskLevel.CRITICAL]
        ]
        
        # Анализируем тренды
        trends = {}
        if include_trends:
            trends = await self._analyze_team_trends(team_id, active_members)
        
        # Генерируем индикаторы и рекомендации
        team_indicators = self._generate_team_indicators(active_members, risk_distribution)
        workload_metrics = self._calculate_workload_metrics(active_members)
        hr_recommendations = await self._generate_hr_recommendations(
            active_members, team_alert_level, risk_distribution
        )
        
        # Определяем приоритет вмешательства
        intervention_priority = self._calculate_intervention_priority(
            team_alert_level, average_risk, len(high_risk_members)
        )
        
        # Создаем аналитику
        analytics = TeamAnalytics(
            team_id=team_id,
            team_name=team_name or f"Team {team_id}",
            analysis_timestamp=datetime.now(),
            total_members=len(members),
            active_members=len(active_members),
            average_risk_score=average_risk,
            team_alert_level=team_alert_level,
            risk_distribution=risk_distribution,
            high_risk_members=high_risk_members,
            trend_24h=trends.get("24h"),
            trend_7d=trends.get("7d"), 
            trend_30d=trends.get("30d"),
            team_indicators=team_indicators,
            workload_metrics=workload_metrics,
            hr_recommendations=hr_recommendations,
            intervention_priority=intervention_priority
        )
        
        # Сохраняем в кэш
        self.analytics_cache[team_id] = (analytics, datetime.now())
        
        # Сохраняем в историю
        await self._save_to_history(team_id, analytics)
        
        return analytics
    
    def _determine_team_alert_level(
        self,
        members: List[TeamMember],
        average_risk: float,
        risk_distribution: Dict[BurnoutRiskLevel, int]
    ) -> TeamAlertLevel:
        """Определяет уровень алерта для команды."""
        
        total_members = len(members)
        critical_count = risk_distribution.get(BurnoutRiskLevel.CRITICAL, 0)
        high_count = risk_distribution.get(BurnoutRiskLevel.HIGH, 0)
        
        # Критический уровень
        if critical_count > 0 or (high_count / total_members) > 0.5:
            return TeamAlertLevel.RED
        
        # Высокий риск
        if average_risk > 0.7 or (high_count / total_members) > 0.3:
            return TeamAlertLevel.ORANGE
        
        # Требует внимания
        if average_risk > 0.5 or (high_count / total_members) > 0.1:
            return TeamAlertLevel.YELLOW
        
        # Все в порядке
        return TeamAlertLevel.GREEN
    
    async def _analyze_team_trends(
        self, 
        team_id: str, 
        current_members: List[TeamMember]
    ) -> Dict[str, TeamBurnoutTrend]:
        """Анализирует тренды команды."""
        
        trends = {}
        timeframes = ["24h", "7d", "30d"]
        
        for timeframe in timeframes:
            trend = await self._calculate_trend_for_timeframe(
                team_id, current_members, timeframe
            )
            if trend:
                trends[timeframe] = trend
        
        return trends
    
    async def _calculate_trend_for_timeframe(
        self,
        team_id: str,
        current_members: List[TeamMember],
        timeframe: str
    ) -> Optional[TeamBurnoutTrend]:
        """Вычисляет тренд для определенного периода."""
        
        # Получаем историческую аналитику
        if team_id not in self.historical_data:
            return None
        
        # Определяем период для сравнения
        hours_map = {"24h": 24, "7d": 168, "30d": 720}
        hours_back = hours_map.get(timeframe, 24)
        
        cutoff_time = datetime.now() - timedelta(hours=hours_back)
        
        # Находим ближайшую историческую точку
        historical_analytics = None
        for analytics in reversed(self.historical_data[team_id]):
            if analytics.analysis_timestamp <= cutoff_time:
                historical_analytics = analytics
                break
        
        if not historical_analytics:
            return None
        
        # Вычисляем изменения
        current_avg_risk = statistics.mean([m.current_risk_score for m in current_members])
        historical_avg_risk = historical_analytics.average_risk_score
        
        change_percentage = calculate_percentage_change(
            historical_avg_risk, current_avg_risk
        )
        
        # Определяем направление тренда
        if abs(change_percentage) < 5:
            direction = TrendDirection.STABLE
        elif change_percentage > 0:
            direction = TrendDirection.DETERIORATING
        else:
            direction = TrendDirection.IMPROVING
        
        # Подсчитываем затронутых участников
        current_high_risk = sum(1 for m in current_members 
                               if m.risk_level in [BurnoutRiskLevel.HIGH, BurnoutRiskLevel.CRITICAL])
        historical_high_risk = sum(historical_analytics.risk_distribution.get(level, 0) 
                                  for level in [BurnoutRiskLevel.HIGH, BurnoutRiskLevel.CRITICAL])
        
        affected_members = abs(current_high_risk - historical_high_risk)
        
        # Текущее распределение рисков
        risk_distribution = {level: 0 for level in BurnoutRiskLevel}
        for member in current_members:
            risk_distribution[member.risk_level] += 1
        
        # Генерируем ключевые факторы
        key_factors = self._identify_trend_factors(
            current_members, historical_analytics, direction
        )
        
        return TeamBurnoutTrend(
            timeframe=timeframe,
            direction=direction,
            change_percentage=change_percentage,
            affected_members=affected_members,
            risk_distribution=risk_distribution,
            key_factors=key_factors
        )
    
    def _generate_team_indicators(
        self,
        members: List[TeamMember],
        risk_distribution: Dict[BurnoutRiskLevel, int]
    ) -> List[str]:
        """Генерирует индикаторы для команды."""
        
        indicators = []
        total_members = len(members)
        
        # Анализ распределения рисков
        critical_ratio = risk_distribution.get(BurnoutRiskLevel.CRITICAL, 0) / total_members
        high_ratio = risk_distribution.get(BurnoutRiskLevel.HIGH, 0) / total_members
        
        if critical_ratio > 0:
            indicators.append(f"🚨 {int(critical_ratio * 100)}% команды в критическом состоянии")
        
        if high_ratio > 0.3:
            indicators.append(f"⚠️ {int(high_ratio * 100)}% команды с высоким риском")
        
        # Анализ паттернов ролей
        role_risks = defaultdict(list)
        for member in members:
            role_risks[member.role].append(member.current_risk_score)
        
        for role, risks in role_risks.items():
            if len(risks) > 1 and statistics.mean(risks) > 0.6:
                indicators.append(f"📋 Высокий риск в роли: {role}")
        
        # Анализ отсутствия перерывов
        no_break_count = sum(1 for m in members if m.days_since_break > 7)
        if no_break_count > total_members * 0.3:
            indicators.append(f"⏰ {no_break_count} участников без перерыва >7 дней")
        
        return indicators if indicators else ["✅ Основные показатели в норме"]
    
    def _calculate_workload_metrics(self, members: List[TeamMember]) -> Dict[str, float]:
        """Вычисляет метрики нагрузки команды."""
        
        if not members:
            return {}
        
        risk_scores = [m.current_risk_score for m in members]
        break_days = [m.days_since_break for m in members]
        
        return {
            "average_workload_pressure": statistics.mean(risk_scores),
            "workload_variance": statistics.stdev(risk_scores) if len(risk_scores) > 1 else 0.0,
            "average_days_since_break": statistics.mean(break_days),
            "overworked_percentage": sum(1 for score in risk_scores if score > 0.7) / len(risk_scores),
            "burnout_distribution_index": len(set(m.risk_level for m in members)) / len(BurnoutRiskLevel)
        }
    
    async def _generate_hr_recommendations(
        self,
        members: List[TeamMember],
        alert_level: TeamAlertLevel,
        risk_distribution: Dict[BurnoutRiskLevel, int]
    ) -> List[str]:
        """Генерирует рекомендации для HR."""
        
        recommendations = []
        total_members = len(members)
        
        # Рекомендации по уровню алерта
        if alert_level == TeamAlertLevel.RED:
            recommendations.extend([
                "🚨 КРИТИЧНО: Немедленное вмешательство руководства",
                "📞 Провести индивидуальные встречи с сотрудниками",
                "⏰ Перераспределить нагрузку и отложить несрочные задачи",
                "🏥 Предложить консультации специалистов по ментальному здоровью"
            ])
        elif alert_level == TeamAlertLevel.ORANGE:
            recommendations.extend([
                "⚠️ Запланировать командное обсуждение нагрузки",
                "📅 Пересмотреть дедлайны и приоритеты проектов",
                "🎯 Внедрить обязательные перерывы в рабочий процесс",
                "👥 Рассмотреть возможность привлечения дополнительных ресурсов"
            ])
        elif alert_level == TeamAlertLevel.YELLOW:
            recommendations.extend([
                "📊 Мониторить ситуацию еженедельно",
                "💬 Провести опрос по удовлетворенности работой",
                "🏃‍♀️ Организовать активности для снятия стресса",
                "📚 Предложить тренинги по управлению стрессом"
            ])
        else:  # GREEN
            recommendations.extend([
                "✅ Поддерживать текущий уровень благополучия",
                "🎉 Отметить достижения команды",
                "📈 Изучить успешные практики для тиражирования",
                "🤝 Предложить команде помочь другим отделам"
            ])
        
        # Специфичные рекомендации по рискам
        critical_count = risk_distribution.get(BurnoutRiskLevel.CRITICAL, 0)
        if critical_count > 0:
            recommendations.append(f"🔥 {critical_count} сотрудников требуют немедленного отпуска")
        
        high_count = risk_distribution.get(BurnoutRiskLevel.HIGH, 0)
        if high_count > total_members * 0.2:
            recommendations.append("📋 Пересмотреть процессы распределения задач")
        
        return recommendations[:6]  # максимум 6 рекомендаций
    
    def _calculate_intervention_priority(
        self,
        alert_level: TeamAlertLevel,
        average_risk: float,
        high_risk_count: int
    ) -> int:
        """Вычисляет приоритет вмешательства (1-5)."""
        
        priority = 1
        
        # Базовый приоритет по уровню алерта
        if alert_level == TeamAlertLevel.RED:
            priority = 5
        elif alert_level == TeamAlertLevel.ORANGE:
            priority = 4
        elif alert_level == TeamAlertLevel.YELLOW:
            priority = 3
        else:
            priority = 2
        
        # Корректировки по метрикам
        if average_risk > 0.8:
            priority = min(5, priority + 1)
        
        if high_risk_count > 3:
            priority = min(5, priority + 1)
        
        return priority
    
    def _identify_trend_factors(
        self,
        current_members: List[TeamMember],
        historical_analytics: TeamAnalytics,
        direction: TrendDirection
    ) -> List[str]:
        """Определяет ключевые факторы тренда."""
        
        factors = []
        
        if direction == TrendDirection.DETERIORATING:
            factors.extend([
                "Увеличение рабочей нагрузки",
                "Сокращение времени на отдых",
                "Возможные организационные изменения"
            ])
        elif direction == TrendDirection.IMPROVING:
            factors.extend([
                "Эффективные меры по снижению стресса",
                "Улучшение рабочих процессов",
                "Положительная динамика в команде"
            ])
        else:
            factors.append("Стабильная рабочая обстановка")
        
        return factors
    
    async def _save_to_history(self, team_id: str, analytics: TeamAnalytics):
        """Сохраняет аналитику в историю."""
        
        if team_id not in self.historical_data:
            self.historical_data[team_id] = deque(maxlen=100)  # последние 100 записей
        
        self.historical_data[team_id].append(analytics)
        
        # Очистка старых данных
        cutoff_date = datetime.now() - timedelta(days=self.data_retention_days)
        while (self.historical_data[team_id] and 
               self.historical_data[team_id][0].analysis_timestamp < cutoff_date):
            self.historical_data[team_id].popleft()


class DepartmentAnalyzer:
    """Анализатор уровня отдела."""
    
    def __init__(self, team_analyzer: TeamBurnoutAnalyzer):
        self.team_analyzer = team_analyzer
        
    async def analyze_department(
        self,
        department_name: str,
        team_ids: List[str]
    ) -> Optional[DepartmentAnalytics]:
        """Анализирует отдел по командам."""
        
        team_analytics = []
        total_employees = 0
        critical_teams = []
        
        # Анализируем каждую команду
        for team_id in team_ids:
            analytics = await self.team_analyzer.analyze_team(team_id)
            if analytics:
                team_analytics.append(analytics)
                total_employees += analytics.active_members
                
                if analytics.team_alert_level in [TeamAlertLevel.RED, TeamAlertLevel.ORANGE]:
                    critical_teams.append(team_id)
        
        if not team_analytics:
            return None
        
        # Вычисляем средний риск отдела
        department_risk = statistics.mean([ta.average_risk_score for ta in team_analytics])
        
        # Бенчмарки (в production будут из DB)
        benchmark_comparison = {
            "industry_average": 0.35,
            "company_average": 0.40,
            "department_score": department_risk
        }
        
        # Лучшие практики
        best_practices = self._generate_best_practices(team_analytics)
        
        return DepartmentAnalytics(
            department_name=department_name,
            teams=team_analytics,
            total_employees=total_employees,
            average_department_risk=department_risk,
            critical_teams=critical_teams,
            benchmark_comparison=benchmark_comparison,
            best_practices=best_practices
        )
    
    def _generate_best_practices(self, team_analytics: List[TeamAnalytics]) -> List[str]:
        """Генерирует лучшие практики на основе анализа команд."""
        
        practices = []
        
        # Найти команды с низким риском
        low_risk_teams = [ta for ta in team_analytics if ta.average_risk_score < 0.3]
        
        if low_risk_teams:
            practices.append("🏆 Изучить практики команд с низким риском выгорания")
            practices.append("📋 Тиражировать успешные процессы управления нагрузкой")
        
        # Общие рекомендации
        practices.extend([
            "⏰ Внедрить единые стандарты рабочего времени",
            "📊 Регулярный мониторинг показателей благополучия",
            "🎯 Обучение менеджеров признакам выгорания",
            "💬 Создание культуры открытого обсуждения стресса"
        ])
        
        return practices[:5]