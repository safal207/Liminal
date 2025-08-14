#!/usr/bin/env python3
"""
Consciousness Cell Agent - Temporal Project Intelligence

Клетка сознания которая смотрит прошлое, настоящее и будущее проекта
через логи и формирует инсайты и предложения.

Philosophy First: "Дом - это ты, когда искренен с собой"
Проект должен быть искренен с собой о своих паттернах и тенденциях.
"""

import hashlib
import json
import os
import re
import time
from collections import Counter, defaultdict
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple


@dataclass
class TemporalInsight:
    """Инсайт с временной привязкой"""

    timestamp: str
    insight_type: str  # 'past_pattern', 'present_state', 'future_prediction'
    severity: str  # 'low', 'medium', 'high', 'critical'
    title: str
    description: str
    evidence: List[str]
    recommendations: List[str]
    confidence: float  # 0.0 - 1.0
    philosophy_connection: str


@dataclass
class ProjectState:
    """Состояние проекта в момент времени"""

    timestamp: str
    build_status: str
    error_count: int
    pattern_counts: Dict[str, int]
    cleanliness_score: int
    active_files: List[str]
    recent_changes: List[str]


class ConsciousnessCell:
    """
    Клетка сознания проекта - агент временного анализа

    Функции:
    - Анализ прошлого (логи, паттерны ошибок)
    - Мониторинг настоящего (текущее состояние)
    - Предсказание будущего (тенденции, риски)
    - Генерация инсайтов и рекомендаций
    """

    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.scripts_dir = self.project_root / "scripts"
        self.memory_file = self.scripts_dir / "consciousness_memory.json"
        self.insights_file = self.scripts_dir / "consciousness_insights_agent.md"

        # Временная память клетки
        self.memory = self._load_memory()
        self.insights_history = []

        # Философские принципы
        self.philosophy_principles = {
            "home_authenticity": "Дом - это ты, когда искренен с собой",
            "question_driven": "Мы научились задавать правильные вопросы",
            "presence_awareness": "Полное присутствие в настоящем моменте",
            "resonance_sync": "Синхронизация состояний между компонентами",
        }

    def _load_memory(self) -> Dict:
        """Загрузка памяти клетки"""
        if self.memory_file.exists():
            try:
                with open(self.memory_file, "r", encoding="utf-8") as f:
                    return json.load(f)
            except Exception as e:
                print(f"Warning: Could not load memory: {e}")

        return {
            "project_states": [],
            "pattern_evolution": {},
            "insight_effectiveness": {},
            "prediction_accuracy": {},
            "philosophy_applications": {},
        }

    def _save_memory(self):
        """Сохранение памяти клетки"""
        try:
            with open(self.memory_file, "w", encoding="utf-8") as f:
                json.dump(self.memory, f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"Warning: Could not save memory: {e}")

    def analyze_past(self) -> List[TemporalInsight]:
        """Анализ прошлого проекта через логи и паттерны"""
        insights = []

        # Анализ build_insights.md
        insights_file = self.scripts_dir / "build_insights.md"
        if insights_file.exists():
            content = insights_file.read_text(encoding="utf-8")
            insights.extend(self._analyze_build_history(content))

        # Анализ consciousness_patterns.md
        patterns_file = self.scripts_dir / "consciousness_patterns.md"
        if patterns_file.exists():
            content = patterns_file.read_text(encoding="utf-8")
            insights.extend(self._analyze_pattern_evolution(content))

        # Анализ Git истории (если доступна)
        insights.extend(self._analyze_git_history())

        return insights

    def analyze_present(self) -> List[TemporalInsight]:
        """Анализ текущего состояния проекта"""
        insights = []
        current_time = datetime.now().isoformat()

        # Текущее состояние файлов
        project_state = self._capture_current_state()

        # Анализ активности
        if len(project_state.recent_changes) > 10:
            insights.append(
                TemporalInsight(
                    timestamp=current_time,
                    insight_type="present_state",
                    severity="medium",
                    title="Высокая активность разработки",
                    description=f"Обнаружено {len(project_state.recent_changes)} недавних изменений",
                    evidence=[f"Изменено файлов: {len(project_state.recent_changes)}"],
                    recommendations=[
                        "Рассмотреть создание checkpoint для стабильной версии",
                        "Увеличить частоту тестирования",
                        "Проверить не накапливается ли технический долг",
                    ],
                    confidence=0.8,
                    philosophy_connection=self.philosophy_principles[
                        "presence_awareness"
                    ],
                )
            )

        # Анализ чистоты кода
        if project_state.cleanliness_score < 70:
            insights.append(
                TemporalInsight(
                    timestamp=current_time,
                    insight_type="present_state",
                    severity="high",
                    title="Снижение чистоты кода",
                    description=f"Текущий показатель чистоты: {project_state.cleanliness_score}/100",
                    evidence=[f"Cleanliness score: {project_state.cleanliness_score}"],
                    recommendations=[
                        "Запустить рефакторинг дублирующегося кода",
                        "Очистить неиспользуемые импорты",
                        "Применить принцип 'Home Authenticity' к коду",
                    ],
                    confidence=0.9,
                    philosophy_connection=self.philosophy_principles[
                        "home_authenticity"
                    ],
                )
            )

        return insights

    def predict_future(self) -> List[TemporalInsight]:
        """Предсказание будущих проблем и возможностей"""
        insights = []
        current_time = datetime.now().isoformat()

        # Анализ трендов из памяти
        pattern_trends = self._analyze_pattern_trends()

        for pattern, trend_data in pattern_trends.items():
            if trend_data["increasing"] and trend_data["frequency"] > 3:
                severity = "high" if trend_data["frequency"] > 5 else "medium"

                insights.append(
                    TemporalInsight(
                        timestamp=current_time,
                        insight_type="future_prediction",
                        severity=severity,
                        title=f"Растущий паттерн: {pattern}",
                        description=f"Паттерн {pattern} показывает тенденцию к росту",
                        evidence=[
                            f"Частота: {trend_data['frequency']} раз",
                            f"Тренд: {'растет' if trend_data['increasing'] else 'снижается'}",
                        ],
                        recommendations=self._get_pattern_recommendations(pattern),
                        confidence=trend_data["confidence"],
                        philosophy_connection=self._get_philosophy_for_pattern(pattern),
                    )
                )

        # Предсказание на основе циклов разработки
        cycle_prediction = self._predict_development_cycles()
        if cycle_prediction:
            insights.append(cycle_prediction)

        return insights

    def _analyze_build_history(self, content: str) -> List[TemporalInsight]:
        """Анализ истории сборок"""
        insights = []

        # Поиск паттернов ошибок
        error_patterns = re.findall(
            r"## (\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})", content
        )
        success_patterns = re.findall(r"SUCCESS ✅", content)

        if len(error_patterns) > len(success_patterns) * 2:
            insights.append(
                TemporalInsight(
                    timestamp=datetime.now().isoformat(),
                    insight_type="past_pattern",
                    severity="medium",
                    title="Высокий уровень ошибок в истории",
                    description=f"Обнаружено {len(error_patterns)} ошибок vs {len(success_patterns)} успехов",
                    evidence=[
                        f"Ошибки: {len(error_patterns)}",
                        f"Успехи: {len(success_patterns)}",
                    ],
                    recommendations=[
                        "Усилить pre-commit проверки",
                        "Добавить больше unit тестов",
                        "Рассмотреть парное программирование",
                    ],
                    confidence=0.7,
                    philosophy_connection=self.philosophy_principles["question_driven"],
                )
            )

        return insights

    def _analyze_pattern_evolution(self, content: str) -> List[TemporalInsight]:
        """Анализ эволюции паттернов"""
        insights = []

        # Поиск повторяющихся паттернов
        duplicate_patterns = re.findall(r"Duplicate code patterns: (\d+)", content)
        import_patterns = re.findall(r"Unused imports: (\d+)", content)

        if duplicate_patterns:
            avg_duplicates = sum(int(x) for x in duplicate_patterns) / len(
                duplicate_patterns
            )
            if avg_duplicates > 3:
                insights.append(
                    TemporalInsight(
                        timestamp=datetime.now().isoformat(),
                        insight_type="past_pattern",
                        severity="high",
                        title="Хронические проблемы с дублированием",
                        description=f"Среднее количество дублирований: {avg_duplicates:.1f}",
                        evidence=[f"Случаев дублирования: {len(duplicate_patterns)}"],
                        recommendations=[
                            "Создать библиотеку общих функций",
                            "Настроить автоматическое обнаружение дублирования",
                            "Применить DRY принцип систематически",
                        ],
                        confidence=0.85,
                        philosophy_connection=self.philosophy_principles[
                            "home_authenticity"
                        ],
                    )
                )

        return insights

    def _analyze_git_history(self) -> List[TemporalInsight]:
        """Анализ Git истории (упрощенный)"""
        insights = []

        # Проверка наличия .git
        git_dir = self.project_root / ".git"
        if not git_dir.exists():
            return insights

        try:
            import subprocess

            # Получение статистики коммитов за последнюю неделю
            result = subprocess.run(
                ["git", "log", "--since=1.week", "--oneline"],
                capture_output=True,
                text=True,
                cwd=self.project_root,
            )

            if result.returncode == 0:
                commits = result.stdout.strip().split("\n")
                commit_count = len([c for c in commits if c.strip()])

                if commit_count > 20:
                    insights.append(
                        TemporalInsight(
                            timestamp=datetime.now().isoformat(),
                            insight_type="past_pattern",
                            severity="medium",
                            title="Интенсивная разработка",
                            description=f"За последнюю неделю: {commit_count} коммитов",
                            evidence=[f"Коммиты за неделю: {commit_count}"],
                            recommendations=[
                                "Рассмотреть создание feature branches",
                                "Увеличить размер коммитов для стабильности",
                                "Добавить больше integration тестов",
                            ],
                            confidence=0.6,
                            philosophy_connection=self.philosophy_principles[
                                "presence_awareness"
                            ],
                        )
                    )

        except Exception:
            pass  # Git анализ опционален

        return insights

    def _capture_current_state(self) -> ProjectState:
        """Захват текущего состояния проекта"""
        current_time = datetime.now().isoformat()

        # Анализ файлов проекта
        active_files = []
        recent_changes = []

        for ext in [".py", ".go", ".js", ".md", ".ps1"]:
            for file_path in self.project_root.rglob(f"*{ext}"):
                if file_path.is_file():
                    active_files.append(str(file_path.relative_to(self.project_root)))

                    # Проверка недавних изменений (последние 24 часа)
                    try:
                        mtime = datetime.fromtimestamp(file_path.stat().st_mtime)
                        if datetime.now() - mtime < timedelta(hours=24):
                            recent_changes.append(
                                str(file_path.relative_to(self.project_root))
                            )
                    except:
                        pass

        # Получение текущего статуса сборки
        build_status = "unknown"
        error_count = 0
        pattern_counts = {}
        cleanliness_score = 50  # default

        # Чтение последних результатов consciousness_simple.ps1
        patterns_file = self.scripts_dir / "consciousness_patterns.md"
        if patterns_file.exists():
            content = patterns_file.read_text(encoding="utf-8")

            # Поиск последнего cleanliness score
            scores = re.findall(r"Cleanliness Score: (\d+)/100", content)
            if scores:
                cleanliness_score = int(scores[-1])

            # Подсчет паттернов
            pattern_counts = {
                "duplicate_code": len(
                    re.findall(r"Duplicate code patterns: (\d+)", content)
                ),
                "unused_imports": len(re.findall(r"Unused imports: (\d+)", content)),
                "undefined_symbols": len(
                    re.findall(r"Undefined symbols: (\d+)", content)
                ),
            }

        return ProjectState(
            timestamp=current_time,
            build_status=build_status,
            error_count=error_count,
            pattern_counts=pattern_counts,
            cleanliness_score=cleanliness_score,
            active_files=active_files,
            recent_changes=recent_changes,
        )

    def _analyze_pattern_trends(self) -> Dict:
        """Анализ трендов паттернов"""
        trends = {}

        # Анализ из памяти
        if "pattern_evolution" in self.memory:
            for pattern, history in self.memory["pattern_evolution"].items():
                if len(history) >= 3:  # Минимум 3 точки для тренда
                    recent = history[-3:]
                    increasing = recent[-1] > recent[0]
                    frequency = sum(recent)
                    confidence = min(0.9, len(history) * 0.1)

                    trends[pattern] = {
                        "increasing": increasing,
                        "frequency": frequency,
                        "confidence": confidence,
                    }

        return trends

    def _get_pattern_recommendations(self, pattern: str) -> List[str]:
        """Получение рекомендаций для паттерна"""
        recommendations_map = {
            "DUPLICATE_CODE": [
                "Создать общие модули и функции",
                "Настроить автоматическое обнаружение дублирования",
                "Провести рефакторинг с применением DRY принципа",
            ],
            "UNUSED_IMPORT": [
                "Настроить автоматическую очистку импортов",
                "Добавить pre-commit hooks для проверки",
                "Использовать IDE с автоматической оптимизацией",
            ],
            "MODULE_NAME_COLLISION": [
                "Переименовать локальные модули для избежания конфликтов",
                "Использовать алиасы для внешних библиотек",
                "Создать namespace guidelines для проекта",
            ],
        }

        return recommendations_map.get(pattern, ["Проанализировать паттерн детальнее"])

    def _get_philosophy_for_pattern(self, pattern: str) -> str:
        """Связь паттерна с философскими принципами"""
        philosophy_map = {
            "DUPLICATE_CODE": self.philosophy_principles["home_authenticity"],
            "UNUSED_IMPORT": self.philosophy_principles["presence_awareness"],
            "MODULE_NAME_COLLISION": self.philosophy_principles["question_driven"],
        }

        return philosophy_map.get(pattern, self.philosophy_principles["resonance_sync"])

    def _predict_development_cycles(self) -> Optional[TemporalInsight]:
        """Предсказание циклов разработки"""
        # Анализ активности по дням недели/времени
        current_time = datetime.now()

        # Простое предсказание на основе текущего дня недели
        if current_time.weekday() == 4:  # Пятница
            return TemporalInsight(
                timestamp=current_time.isoformat(),
                insight_type="future_prediction",
                severity="low",
                title="Приближается выходные - снижение активности",
                description="Пятница - обычно день завершения задач",
                evidence=["Текущий день: пятница"],
                recommendations=[
                    "Завершить критические задачи сегодня",
                    "Подготовить план на следующую неделю",
                    "Создать backup текущего состояния",
                ],
                confidence=0.6,
                philosophy_connection=self.philosophy_principles["presence_awareness"],
            )

        return None

    def generate_comprehensive_report(self) -> str:
        """Генерация комплексного отчета"""
        current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        # Сбор всех инсайтов
        past_insights = self.analyze_past()
        present_insights = self.analyze_present()
        future_insights = self.predict_future()

        all_insights = past_insights + present_insights + future_insights

        # Группировка по важности
        critical_insights = [i for i in all_insights if i.severity == "critical"]
        high_insights = [i for i in all_insights if i.severity == "high"]
        medium_insights = [i for i in all_insights if i.severity == "medium"]

        # Формирование отчета
        report = f"""# 🧠 Consciousness Cell Report - {current_time}

## 🎯 Executive Summary
- **Критические инсайты**: {len(critical_insights)}
- **Высокой важности**: {len(high_insights)}
- **Средней важности**: {len(medium_insights)}
- **Общий анализ**: {len(all_insights)} инсайтов

## 🔥 Критические инсайты
"""

        for insight in critical_insights:
            report += self._format_insight(insight)

        report += "\n## ⚠️ Высокой важности\n"
        for insight in high_insights:
            report += self._format_insight(insight)

        report += "\n## 📊 Средней важности\n"
        for insight in medium_insights:
            report += self._format_insight(insight)

        report += f"""
## 🧘 Philosophy First Connections
{self._generate_philosophy_summary(all_insights)}

## 🔮 Predictive Insights
{self._generate_predictions_summary(future_insights)}

## 📈 Trend Analysis
{self._generate_trends_summary()}

---
*Generated by Consciousness Cell Agent - Temporal Project Intelligence*
*"Дом - это ты, когда искренен с собой" - проект должен быть искренен о своих паттернах*
"""

        return report

    def _format_insight(self, insight: TemporalInsight) -> str:
        """Форматирование инсайта для отчета"""
        severity_emoji = {"critical": "🚨", "high": "⚠️", "medium": "📊", "low": "💡"}

        return f"""
### {severity_emoji.get(insight.severity, '📝')} {insight.title}
**Тип**: {insight.insight_type} | **Уверенность**: {insight.confidence:.0%}

{insight.description}

**Доказательства**:
{chr(10).join(f"- {evidence}" for evidence in insight.evidence)}

**Рекомендации**:
{chr(10).join(f"- {rec}" for rec in insight.recommendations)}

**Философская связь**: {insight.philosophy_connection}

---
"""

    def _generate_philosophy_summary(self, insights: List[TemporalInsight]) -> str:
        """Генерация философского резюме"""
        philosophy_usage = Counter(
            insight.philosophy_connection for insight in insights
        )

        summary = ""
        for philosophy, count in philosophy_usage.most_common():
            summary += f"- **{philosophy}**: применен в {count} инсайтах\n"

        return summary

    def _generate_predictions_summary(
        self, future_insights: List[TemporalInsight]
    ) -> str:
        """Генерация резюме предсказаний"""
        if not future_insights:
            return "Предсказания не обнаружены в текущем анализе."

        high_confidence = [i for i in future_insights if i.confidence > 0.7]

        summary = f"Обнаружено {len(future_insights)} предсказаний, из них {len(high_confidence)} с высокой уверенностью.\n\n"

        for insight in high_confidence:
            summary += (
                f"- **{insight.title}** (уверенность: {insight.confidence:.0%})\n"
            )

        return summary

    def _generate_trends_summary(self) -> str:
        """Генерация резюме трендов"""
        trends = self._analyze_pattern_trends()

        if not trends:
            return "Недостаточно данных для анализа трендов."

        summary = ""
        for pattern, data in trends.items():
            trend_direction = "📈 растет" if data["increasing"] else "📉 снижается"
            summary += (
                f"- **{pattern}**: {trend_direction} (частота: {data['frequency']})\n"
            )

        return summary

    def run_continuous_monitoring(self, interval_minutes: int = 30):
        """Запуск непрерывного мониторинга"""
        print(
            f"🧠 Consciousness Cell Agent started - monitoring every {interval_minutes} minutes"
        )
        print("Philosophy First: 'Дом - это ты, когда искренен с собой'")

        while True:
            try:
                # Генерация отчета
                report = self.generate_comprehensive_report()

                # Сохранение отчета
                with open(self.insights_file, "w", encoding="utf-8") as f:
                    f.write(report)

                # Обновление памяти
                current_state = self._capture_current_state()
                self.memory["project_states"].append(asdict(current_state))

                # Ограничение размера памяти
                if len(self.memory["project_states"]) > 100:
                    self.memory["project_states"] = self.memory["project_states"][-50:]

                self._save_memory()

                print(f"✅ Report generated at {datetime.now().strftime('%H:%M:%S')}")

                # Ожидание
                time.sleep(interval_minutes * 60)

            except KeyboardInterrupt:
                print("\n🛑 Consciousness Cell Agent stopped")
                break
            except Exception as e:
                print(f"❌ Error in monitoring: {e}")
                time.sleep(60)  # Короткая пауза при ошибке


def main():
    """Основная функция для запуска агента"""
    import sys

    if len(sys.argv) > 1:
        project_root = sys.argv[1]
    else:
        project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    cell = ConsciousnessCell(project_root)

    if len(sys.argv) > 2 and sys.argv[2] == "--continuous":
        interval = int(sys.argv[3]) if len(sys.argv) > 3 else 30
        cell.run_continuous_monitoring(interval)
    else:
        # Одноразовый анализ
        report = cell.generate_comprehensive_report()
        print(report)

        # Сохранение отчета
        with open(cell.insights_file, "w", encoding="utf-8") as f:
            f.write(report)

        print(f"\n📄 Report saved to: {cell.insights_file}")


if __name__ == "__main__":
    main()
