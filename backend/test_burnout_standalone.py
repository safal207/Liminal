"""
🚀🛡️ BurnoutGuard Standalone Test — независимая демонстрация

Автономный тест концепции BurnoutGuard без зависимостей:
- Демонстрация алгоритма детекции выгорания
- Простая система скоринга
- Базовые рекомендации
"""

import asyncio
from datetime import datetime, timedelta
from enum import Enum
from dataclasses import dataclass
from typing import List, Dict, Any


# Простые классы для демонстрации
class EmotionalModeType(Enum):
    CALM = "calm"
    FOCUS = "focus"
    STRESS = "stress"
    JOY = "joy"
    CONTEMPLATION = "contemplation"
    NEUTRAL = "neutral"


class BurnoutRiskLevel(Enum):
    VERY_LOW = "very_low"
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class EmotionalData:
    """Простая структура эмоциональных данных."""
    mode: EmotionalModeType
    valence: float      # -1.0 to 1.0
    arousal: float      # 0.0 to 1.0
    intensity: float    # 0.0 to 1.0
    timestamp: datetime
    confidence: float = 0.8


class BurnoutDetector:
    """Простой детектор выгорания."""
    
    def __init__(self):
        # Веса риска для разных эмоциональных режимов
        self.mode_risk_weights = {
            EmotionalModeType.STRESS: 0.9,
            EmotionalModeType.CONTEMPLATION: 0.4,
            EmotionalModeType.FOCUS: 0.3,
            EmotionalModeType.NEUTRAL: 0.25,
            EmotionalModeType.CALM: 0.1,
            EmotionalModeType.JOY: 0.05
        }
    
    def analyze_burnout_risk(self, emotional_history: List[EmotionalData]) -> Dict[str, Any]:
        """Анализирует риск выгорания."""
        
        if not emotional_history:
            return {"risk_score": 0.0, "risk_level": BurnoutRiskLevel.LOW, "indicators": []}
        
        # 1. Базовый риск от режимов
        mode_risk = 0.0
        mode_counts = {}
        
        for data in emotional_history:
            weight = self.mode_risk_weights.get(data.mode, 0.25)
            mode_risk += weight * data.intensity
            mode_counts[data.mode] = mode_counts.get(data.mode, 0) + 1
        
        mode_risk /= len(emotional_history)
        
        # 2. Риск от паттернов
        pattern_risk = self._analyze_patterns(emotional_history)
        
        # 3. Риск от эмоциональной валентности
        valence_risk = self._analyze_valence_risk(emotional_history)
        
        # 4. Итоговый скор
        total_risk = (mode_risk * 0.5 + pattern_risk * 0.3 + valence_risk * 0.2)
        total_risk = max(0.0, min(1.0, total_risk))
        
        # 5. Определяем уровень риска
        if total_risk <= 0.2:
            risk_level = BurnoutRiskLevel.VERY_LOW
        elif total_risk <= 0.4:
            risk_level = BurnoutRiskLevel.LOW
        elif total_risk <= 0.6:
            risk_level = BurnoutRiskLevel.MEDIUM
        elif total_risk <= 0.8:
            risk_level = BurnoutRiskLevel.HIGH
        else:
            risk_level = BurnoutRiskLevel.CRITICAL
        
        # 6. Генерируем индикаторы
        indicators = self._generate_indicators(emotional_history, mode_counts, total_risk)
        
        return {
            "risk_score": total_risk,
            "risk_level": risk_level,
            "indicators": indicators,
            "mode_breakdown": mode_counts,
            "dominant_mode": max(mode_counts, key=mode_counts.get) if mode_counts else EmotionalModeType.NEUTRAL
        }
    
    def _analyze_patterns(self, history: List[EmotionalData]) -> float:
        """Анализирует опасные паттерны."""
        
        if len(history) < 3:
            return 0.0
        
        risk = 0.0
        modes = [data.mode for data in history]
        
        # Хронический стресс (3+ стресса подряд)
        stress_streak = 0
        max_stress_streak = 0
        
        for mode in modes:
            if mode == EmotionalModeType.STRESS:
                stress_streak += 1
                max_stress_streak = max(max_stress_streak, stress_streak)
            else:
                stress_streak = 0
        
        if max_stress_streak >= 3:
            risk += 0.6
        elif max_stress_streak >= 2:
            risk += 0.3
        
        # Отсутствие позитивных эмоций
        joy_count = sum(1 for mode in modes if mode == EmotionalModeType.JOY)
        if len(modes) > 5 and joy_count == 0:
            risk += 0.4
        
        # Эмоциональная притупленность (много нейтральных)
        neutral_ratio = sum(1 for mode in modes if mode == EmotionalModeType.NEUTRAL) / len(modes)
        if neutral_ratio > 0.6:
            risk += 0.3
        
        return min(1.0, risk)
    
    def _analyze_valence_risk(self, history: List[EmotionalData]) -> float:
        """Анализирует риск от эмоциональной валентности."""
        
        if not history:
            return 0.0
        
        # Средняя валентность
        avg_valence = sum(data.valence for data in history) / len(history)
        
        # Негативная валентность = риск
        if avg_valence < -0.5:
            return 0.8
        elif avg_valence < -0.2:
            return 0.5
        elif avg_valence < 0:
            return 0.3
        else:
            return max(0.0, -avg_valence * 0.5)  # небольшой риск даже при позитивной валентности
    
    def _generate_indicators(self, history: List[EmotionalData], mode_counts: Dict, risk_score: float) -> List[str]:
        """Генерирует индикаторы риска."""
        
        indicators = []
        total_count = len(history)
        
        # Индикаторы по режимам
        if mode_counts.get(EmotionalModeType.STRESS, 0) / total_count > 0.3:
            indicators.append("Высокий уровень стресса")
        
        if mode_counts.get(EmotionalModeType.JOY, 0) / total_count < 0.1:
            indicators.append("Недостаток позитивных эмоций")
        
        if mode_counts.get(EmotionalModeType.FOCUS, 0) / total_count > 0.5:
            indicators.append("Чрезмерная концентрация на работе")
        
        if mode_counts.get(EmotionalModeType.NEUTRAL, 0) / total_count > 0.4:
            indicators.append("Эмоциональная притупленность")
        
        # Индикаторы по валентности
        avg_valence = sum(data.valence for data in history) / len(history) if history else 0
        if avg_valence < -0.3:
            indicators.append("Преобладание негативных эмоций")
        
        # Индикаторы по интенсивности
        avg_intensity = sum(data.intensity for data in history) / len(history) if history else 0
        if avg_intensity < 0.3:
            indicators.append("Низкая эмоциональная активность")
        
        return indicators if indicators else ["Базовые показатели в норме"]


class RecommendationGenerator:
    """Простой генератор рекомендаций."""
    
    def __init__(self):
        self.recommendations = {
            BurnoutRiskLevel.CRITICAL: [
                "🚨 НЕМЕДЛЕННО сделайте перерыв на 15-20 минут",
                "💧 Выпейте воды и сделайте глубокие вдохи",
                "🚶‍♂️ Выйдите на свежий воздух",
                "📞 Поговорите с коллегой или близким",
                "⏰ Отложите все несрочные задачи"
            ],
            BurnoutRiskLevel.HIGH: [
                "⏰ Запланируйте перерыв в ближайшие 30 минут",
                "🎯 Сосредоточьтесь на одной задаче за раз",
                "📱 Отключите уведомления на 1 час",
                "🧘‍♀️ Попробуйте 5-минутную медитацию",
                "📝 Пересмотрите приоритеты на сегодня"
            ],
            BurnoutRiskLevel.MEDIUM: [
                "⚖️ Оцените текущую нагрузку",
                "☕ Сделайте короткий перерыв на чай/кофе",
                "📝 Запишите 3 вещи, за которые благодарны",
                "💪 Сделайте простые упражнения",
                "🎵 Послушайте любимую музыку"
            ],
            BurnoutRiskLevel.LOW: [
                "✅ Продолжайте в том же духе!",
                "📅 Планируйте регулярные перерывы",
                "🎯 Ставьте реалистичные цели",
                "🌱 Развивайте полезные привычки",
                "🤝 Поддерживайте социальные связи"
            ],
            BurnoutRiskLevel.VERY_LOW: [
                "🎉 Отличное эмоциональное состояние!",
                "💡 Поделитесь позитивом с коллегами",
                "📚 Время для изучения нового",
                "🎨 Займитесь творческими задачами",
                "🏆 Помогите другим в команде"
            ]
        }
    
    def get_recommendations(self, risk_level: BurnoutRiskLevel, indicators: List[str]) -> List[str]:
        """Получает рекомендации для уровня риска."""
        
        base_recommendations = self.recommendations.get(risk_level, [])
        
        # Добавляем специфичные рекомендации по индикаторам
        specific_recs = []
        
        for indicator in indicators:
            if "стресс" in indicator.lower():
                specific_recs.append("🧘‍♀️ Практикуйте техники снижения стресса")
            elif "позитивных эмоций" in indicator.lower():
                specific_recs.append("😊 Найдите что-то приятное в текущем дне")
            elif "концентрация" in indicator.lower():
                specific_recs.append("⏱️ Используйте технику Pomodoro (25 мин работы + 5 мин перерыв)")
            elif "притупленность" in indicator.lower():
                specific_recs.append("🎭 Попробуйте новую активность для эмоционального разнообразия")
        
        # Комбинируем рекомендации
        all_recs = base_recommendations + specific_recs
        return all_recs[:5]  # максимум 5 рекомендаций


def format_risk_score(score: float) -> str:
    """Форматирует скор риска."""
    percentage = int(score * 100)
    
    if score <= 0.2:
        return f"🟢 {percentage}% - Очень низкий риск"
    elif score <= 0.4:
        return f"🟡 {percentage}% - Низкий риск"
    elif score <= 0.6:
        return f"🟠 {percentage}% - Средний риск"
    elif score <= 0.8:
        return f"🔴 {percentage}% - Высокий риск"
    else:
        return f"🚨 {percentage}% - КРИТИЧЕСКИЙ риск"


def create_test_scenarios() -> List[Dict[str, Any]]:
    """Создает тестовые сценарии."""
    
    scenarios = []
    
    # Сценарий 1: Хронический стресс
    stress_data = []
    base_time = datetime.now()
    for i in range(5):
        stress_data.append(EmotionalData(
            mode=EmotionalModeType.STRESS,
            valence=-0.7,
            arousal=0.8,
            intensity=0.9,
            timestamp=base_time - timedelta(hours=i)
        ))
    
    scenarios.append({
        "name": "Хронический стресс",
        "data": stress_data,
        "expected_risk": "high"
    })
    
    # Сценарий 2: Здоровое состояние
    healthy_data = []
    modes = [EmotionalModeType.CALM, EmotionalModeType.JOY, EmotionalModeType.FOCUS, EmotionalModeType.CALM, EmotionalModeType.JOY]
    valences = [0.3, 0.7, 0.1, 0.4, 0.6]
    
    for i, (mode, valence) in enumerate(zip(modes, valences)):
        healthy_data.append(EmotionalData(
            mode=mode,
            valence=valence,
            arousal=0.5,
            intensity=0.7,
            timestamp=base_time - timedelta(hours=i)
        ))
    
    scenarios.append({
        "name": "Здоровое состояние",
        "data": healthy_data,
        "expected_risk": "low"
    })
    
    # Сценарий 3: Переработка
    overwork_data = []
    for i in range(6):
        overwork_data.append(EmotionalData(
            mode=EmotionalModeType.FOCUS,
            valence=-0.1,
            arousal=0.7,
            intensity=0.8,
            timestamp=base_time - timedelta(hours=i)
        ))
    
    scenarios.append({
        "name": "Переработка (длительный фокус)",
        "data": overwork_data,
        "expected_risk": "medium"
    })
    
    # Сценарий 4: Эмоциональная притупленность
    neutral_data = []
    for i in range(7):
        neutral_data.append(EmotionalData(
            mode=EmotionalModeType.NEUTRAL,
            valence=0.0,
            arousal=0.3,
            intensity=0.2,
            timestamp=base_time - timedelta(hours=i)
        ))
    
    scenarios.append({
        "name": "Эмоциональная притупленность",
        "data": neutral_data,
        "expected_risk": "medium"
    })
    
    return scenarios


async def main():
    """Главная функция тестирования."""
    
    print("🚀🛡️ BurnoutGuard - AI защита от выгорания")
    print("Автономная демонстрация концепции")
    print("=" * 60)
    
    # Инициализируем компоненты
    detector = BurnoutDetector()
    recommender = RecommendationGenerator()
    
    # Создаем тестовые сценарии
    scenarios = create_test_scenarios()
    
    print("\n🧪 Тестирование детекции выгорания:")
    print("-" * 40)
    
    for scenario in scenarios:
        print(f"\n📋 Сценарий: {scenario['name']}")
        
        # Анализируем риск
        result = detector.analyze_burnout_risk(scenario['data'])
        
        print(f"   Риск: {format_risk_score(result['risk_score'])}")
        print(f"   Уровень: {result['risk_level'].value}")
        print(f"   Доминирующий режим: {result['dominant_mode'].value}")
        
        print(f"   Индикаторы:")
        for indicator in result['indicators']:
            print(f"     • {indicator}")
        
        # Получаем рекомендации
        recommendations = recommender.get_recommendations(
            result['risk_level'], 
            result['indicators']
        )
        
        print(f"   Рекомендации:")
        for i, rec in enumerate(recommendations[:3], 1):
            print(f"     {i}. {rec}")
        
        # Проверяем ожидание
        expected = scenario['expected_risk']
        actual_score = result['risk_score']
        
        if expected == "high" and actual_score > 0.6:
            print("   ✅ Высокий риск правильно определен")
        elif expected == "low" and actual_score < 0.4:
            print("   ✅ Низкий риск правильно определен")
        elif expected == "medium" and 0.4 <= actual_score <= 0.6:
            print("   ✅ Средний риск правильно определен")
        else:
            print(f"   ⚠️ Ожидался {expected} риск, получен скор {actual_score:.2f}")
    
    print("\n" + "=" * 60)
    print("✅ Демонстрация BurnoutGuard завершена!")
    print("\n💡 Ключевые возможности:")
    print("   🔍 Детекция паттернов выгорания")
    print("   📊 Многофакторный анализ риска")
    print("   🎯 Персонализированные рекомендации")
    print("   ⚡ Real-time мониторинг (в полной версии)")
    print("   👥 Командная аналитика (в полной версии)")
    
    print("\n🚀 Готов для интеграции с мобильным приложением!")


if __name__ == "__main__":
    asyncio.run(main())