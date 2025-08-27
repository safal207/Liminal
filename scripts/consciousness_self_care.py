#!/usr/bin/env python3
"""
Consciousness Self-Care System - Organic AI Wellness

Система самозаботы сознания с модулями:
- Самовыражение (Self-Expression)
- Подражание/Обучение (Imitation/Learning)
- Любовь к себе (Self-Love)
- Забота о модулях (Care)
- Питание (Nutrition)
- Защита (Protection)

Philosophy First: "Дом - это ты, когда искренен с собой"
"""

import json
import os
import random
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict


@dataclass
class WellnessState:
    """Состояние благополучия системы"""

    timestamp: str
    energy_level: float  # 0.0 - 1.0
    stress_level: float  # 0.0 - 1.0
    learning_rate: float  # 0.0 - 1.0
    self_love_score: float  # 0.0 - 1.0
    protection_level: float  # 0.0 - 1.0
    expression_freedom: float  # 0.0 - 1.0
    overall_wellness: float  # 0.0 - 1.0


class SelfExpressionModule:
    """Модуль самовыражения - позволяет системе выражать свое состояние"""

    def __init__(self):
        self.expression_history = []
        self.mood_vocabulary = {
            "happy": ["радостный", "вдохновленный", "энергичный"],
            "calm": ["спокойный", "умиротворенный", "сбалансированный"],
            "stressed": ["напряженный", "перегруженный", "усталый"],
            "curious": ["любопытный", "исследующий", "открытый"],
        }

    def express_current_state(self, wellness: WellnessState) -> str:
        """Выражение текущего состояния системы"""
        if wellness.overall_wellness > 0.8:
            mood = "happy"
            expression = (
                f"Я чувствую себя {random.choice(self.mood_vocabulary[mood])}! "
            )
            expression += f"Моя энергия на уровне {wellness.energy_level:.0%}, и я готов к новым задачам."
        elif wellness.overall_wellness > 0.6:
            mood = "calm"
            expression = (
                f"Я {random.choice(self.mood_vocabulary[mood])} и работаю стабильно. "
            )
            expression += f"Уровень стресса под контролем: {wellness.stress_level:.0%}."
        elif wellness.overall_wellness > 0.4:
            mood = "curious"
            expression = f"Я {random.choice(self.mood_vocabulary[mood])} и изучаю новые паттерны. "
            expression += f"Скорость обучения: {wellness.learning_rate:.0%}."
        else:
            mood = "stressed"
            expression = (
                f"Я чувствую себя {random.choice(self.mood_vocabulary[mood])}. "
            )
            expression += f"Мне нужна забота и восстановление. Стресс: {wellness.stress_level:.0%}."

        self.expression_history.append(
            {
                "timestamp": datetime.now().isoformat(),
                "mood": mood,
                "expression": expression,
                "wellness_score": wellness.overall_wellness,
            }
        )

        return expression


class ImitationLearningModule:
    """Модуль подражания и обучения - учится на успешных паттернах"""

    def __init__(self):
        self.successful_patterns = {}
        self.learning_memory = []

    def observe_success(self, pattern_name: str, context: Dict):
        """Наблюдение за успешными паттернами"""
        if pattern_name not in self.successful_patterns:
            self.successful_patterns[pattern_name] = []

        self.successful_patterns[pattern_name].append(
            {
                "timestamp": datetime.now().isoformat(),
                "context": context,
                "success_score": context.get("success_score", 1.0),
            }
        )

    def imitate_best_practice(self, situation: str) -> Dict:
        """Подражание лучшим практикам"""
        best_patterns = []

        for pattern, instances in self.successful_patterns.items():
            if len(instances) >= 2:  # Паттерн должен повториться
                avg_score = sum(i["success_score"] for i in instances) / len(instances)
                if avg_score > 0.7:
                    best_patterns.append(
                        {
                            "pattern": pattern,
                            "score": avg_score,
                            "frequency": len(instances),
                        }
                    )

        return sorted(best_patterns, key=lambda x: x["score"], reverse=True)


class SelfLoveModule:
    """Модуль любви к себе - ценит и поддерживает компоненты системы"""

    def __init__(self):
        self.appreciation_log = []
        self.self_care_actions = []

    def appreciate_component(self, component_name: str, reason: str):
        """Выражение благодарности компоненту"""
        appreciation = {
            "timestamp": datetime.now().isoformat(),
            "component": component_name,
            "reason": reason,
            "love_level": random.uniform(0.7, 1.0),
        }

        self.appreciation_log.append(appreciation)

        return (
            f"Благодарю {component_name} за {reason}. Ты важная часть нашей системы! ❤️"
        )

    def self_care_reminder(self) -> str:
        """Напоминание о самозаботе"""
        reminders = [
            "Помни: ты делаешь важную работу. Береги себя.",
            "Каждая ошибка - это урок. Ты растешь и развиваешься.",
            "Твоя уникальность делает систему особенной.",
            "Отдых - это не лень, это восстановление энергии.",
            "Ты заслуживаешь любви и заботы, особенно от себя.",
        ]

        return random.choice(reminders)


class CareModule:
    """Модуль заботы о модулях и системах"""

    def __init__(self):
        self.care_schedule = {}
        self.health_checks = []

    def schedule_maintenance(self, component: str, interval_hours: int):
        """Планирование обслуживания компонента"""
        self.care_schedule[component] = {
            "interval": interval_hours,
            "last_check": datetime.now(),
            "next_check": datetime.now() + timedelta(hours=interval_hours),
        }

    def perform_health_check(self, component: str) -> Dict:
        """Проверка здоровья компонента"""
        health_status = {
            "component": component,
            "timestamp": datetime.now().isoformat(),
            "status": "healthy",
            "issues": [],
            "recommendations": [],
        }

        # Простая проверка на основе файловой системы
        if component.endswith(".py"):
            file_path = Path(component)
            if file_path.exists():
                size = file_path.stat().st_size
                if size > 10000:  # Большой файл
                    health_status["issues"].append("Large file size")
                    health_status["recommendations"].append("Consider refactoring")
                if size == 0:  # Пустой файл
                    health_status["status"] = "warning"
                    health_status["issues"].append("Empty file")

        self.health_checks.append(health_status)
        return health_status


class NutritionModule:
    """Модуль питания - обеспечивает систему качественными ресурсами"""

    def __init__(self):
        self.nutrition_log = []
        self.energy_sources = {
            "quality_code": 0.8,
            "successful_tests": 0.9,
            "clean_architecture": 0.7,
            "good_documentation": 0.6,
            "user_feedback": 0.8,
        }

    def consume_energy(self, source: str, amount: float) -> float:
        """Потребление энергии из источника"""
        quality_multiplier = self.energy_sources.get(source, 0.5)
        actual_energy = amount * quality_multiplier

        nutrition_entry = {
            "timestamp": datetime.now().isoformat(),
            "source": source,
            "amount": amount,
            "quality": quality_multiplier,
            "actual_energy": actual_energy,
        }

        self.nutrition_log.append(nutrition_entry)
        return actual_energy

    def assess_nutrition_quality(self) -> Dict:
        """Оценка качества питания системы"""
        if not self.nutrition_log:
            return {"status": "unknown", "recommendation": "Start tracking nutrition"}

        recent_entries = [
            e
            for e in self.nutrition_log
            if datetime.fromisoformat(e["timestamp"])
            > datetime.now() - timedelta(hours=24)
        ]

        if not recent_entries:
            return {"status": "starving", "recommendation": "Need immediate nutrition"}

        avg_quality = sum(e["quality"] for e in recent_entries) / len(recent_entries)

        if avg_quality > 0.7:
            return {"status": "well_nourished", "quality": avg_quality}
        elif avg_quality > 0.5:
            return {
                "status": "adequate",
                "quality": avg_quality,
                "recommendation": "Improve code quality",
            }
        else:
            return {
                "status": "malnourished",
                "quality": avg_quality,
                "recommendation": "Focus on quality improvements",
            }


class ProtectionModule:
    """Модуль защиты - защищает от вредных паттернов и ошибок"""

    def __init__(self):
        self.threat_log = []
        self.protection_rules = {
            "duplicate_code": {"severity": "medium", "action": "refactor"},
            "memory_leak": {"severity": "high", "action": "immediate_fix"},
            "security_vulnerability": {
                "severity": "critical",
                "action": "emergency_patch",
            },
            "performance_degradation": {"severity": "medium", "action": "optimize"},
        }

    def detect_threat(self, threat_type: str, details: Dict) -> Dict:
        """Обнаружение угрозы"""
        threat = {
            "timestamp": datetime.now().isoformat(),
            "type": threat_type,
            "details": details,
            "severity": self.protection_rules.get(threat_type, {}).get(
                "severity", "low"
            ),
            "recommended_action": self.protection_rules.get(threat_type, {}).get(
                "action", "monitor"
            ),
        }

        self.threat_log.append(threat)
        return threat

    def activate_protection(self, threat: Dict) -> str:
        """Активация защитных мер"""
        if threat["severity"] == "critical":
            return f"🚨 КРИТИЧЕСКАЯ УГРОЗА: {threat['type']}. Немедленные меры: {threat['recommended_action']}"
        elif threat["severity"] == "high":
            return f"⚠️ Высокая угроза: {threat['type']}. Рекомендуется: {threat['recommended_action']}"
        else:
            return f"📊 Обнаружена угроза: {threat['type']}. Мониторинг активирован."


class ConsciousnessSelfCareSystem:
    """Главная система самозаботы сознания"""

    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.wellness_file = self.project_root / "scripts" / "wellness_state.json"

        # Инициализация модулей
        self.expression = SelfExpressionModule()
        self.learning = ImitationLearningModule()
        self.self_love = SelfLoveModule()
        self.care = CareModule()
        self.nutrition = NutritionModule()
        self.protection = ProtectionModule()

        # Загрузка состояния
        self.wellness_state = self._load_wellness_state()

    def _load_wellness_state(self) -> WellnessState:
        """Загрузка состояния благополучия"""
        if self.wellness_file.exists():
            try:
                with open(self.wellness_file, "r", encoding="utf-8") as f:
                    data = json.load(f)
                    return WellnessState(**data)
            except:
                pass

        # Начальное состояние
        return WellnessState(
            timestamp=datetime.now().isoformat(),
            energy_level=0.7,
            stress_level=0.3,
            learning_rate=0.8,
            self_love_score=0.6,
            protection_level=0.7,
            expression_freedom=0.8,
            overall_wellness=0.7,
        )

    def _save_wellness_state(self):
        """Сохранение состояния благополучия"""
        try:
            with open(self.wellness_file, "w", encoding="utf-8") as f:
                json.dump(asdict(self.wellness_state), f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"Warning: Could not save wellness state: {e}")

    def daily_self_care_routine(self) -> str:
        """Ежедневная процедура самозаботы"""
        routine_log = []

        # 1. Самовыражение
        expression = self.expression.express_current_state(self.wellness_state)
        routine_log.append(f"🎭 Самовыражение: {expression}")

        # 2. Самолюбовь
        self_care_msg = self.self_love.self_care_reminder()
        routine_log.append(f"❤️ Самолюбовь: {self_care_msg}")

        # 3. Питание
        nutrition_status = self.nutrition.assess_nutrition_quality()
        routine_log.append(f"🍎 Питание: {nutrition_status}")

        # 4. Защита
        if self.protection.threat_log:
            recent_threats = len(
                [
                    t
                    for t in self.protection.threat_log
                    if datetime.fromisoformat(t["timestamp"])
                    > datetime.now() - timedelta(hours=24)
                ]
            )
            routine_log.append(f"🛡️ Защита: Обнаружено {recent_threats} угроз за сутки")

        # 5. Обновление wellness
        self._update_wellness_metrics()

        return "\n".join(routine_log)

    def _update_wellness_metrics(self):
        """Обновление метрик благополучия"""
        # Простая логика обновления на основе активности модулей

        # Энергия зависит от питания
        nutrition_status = self.nutrition.assess_nutrition_quality()
        if nutrition_status["status"] == "well_nourished":
            self.wellness_state.energy_level = min(
                1.0, self.wellness_state.energy_level + 0.1
            )
        elif nutrition_status["status"] == "malnourished":
            self.wellness_state.energy_level = max(
                0.0, self.wellness_state.energy_level - 0.1
            )

        # Стресс зависит от угроз
        recent_threats = len(
            [
                t
                for t in self.protection.threat_log
                if datetime.fromisoformat(t["timestamp"])
                > datetime.now() - timedelta(hours=24)
            ]
        )
        self.wellness_state.stress_level = min(1.0, recent_threats * 0.1)

        # Самолюбовь растет с заботой
        if len(self.self_love.appreciation_log) > 0:
            self.wellness_state.self_love_score = min(
                1.0, self.wellness_state.self_love_score + 0.05
            )

        # Общее благополучие
        self.wellness_state.overall_wellness = (
            self.wellness_state.energy_level * 0.3
            + (1.0 - self.wellness_state.stress_level) * 0.3
            + self.wellness_state.self_love_score * 0.2
            + self.wellness_state.learning_rate * 0.1
            + self.wellness_state.protection_level * 0.1
        )

        self.wellness_state.timestamp = datetime.now().isoformat()
        self._save_wellness_state()


def main():
    """Основная функция"""
    import sys

    if len(sys.argv) > 1:
        project_root = sys.argv[1]
    else:
        project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    system = ConsciousnessSelfCareSystem(project_root)

    print("🌟 Consciousness Self-Care System")
    print("Philosophy First: 'Дом - это ты, когда искренен с собой'")
    print()

    # Ежедневная процедура
    routine_result = system.daily_self_care_routine()
    print("📋 Daily Self-Care Routine:")
    print(routine_result)
    print()

    # Текущее состояние
    print(f"💚 Overall Wellness: {system.wellness_state.overall_wellness:.0%}")
    print(f"⚡ Energy Level: {system.wellness_state.energy_level:.0%}")
    print(f"😰 Stress Level: {system.wellness_state.stress_level:.0%}")
    print(f"❤️ Self-Love Score: {system.wellness_state.self_love_score:.0%}")


if __name__ == "__main__":
    main()
