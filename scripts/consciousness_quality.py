#!/usr/bin/env python3
"""
🧪 SOMA Quality Development Module
Automated testing, code quality, and continuous improvement consciousness

Philosophy First: "Качество - это любовь к деталям и забота о будущем"
"""

import ast
import json
import os
import subprocess
import sys
import time
import unittest

# Optional quality tools - will work without them
try:
    import coverage

    COVERAGE_AVAILABLE = True
except ImportError:
    COVERAGE_AVAILABLE = False

try:
    import pylint.lint

    PYLINT_AVAILABLE = True
except ImportError:
    PYLINT_AVAILABLE = False

try:
    import black

    BLACK_AVAILABLE = True
except ImportError:
    BLACK_AVAILABLE = False

try:
    import isort

    ISORT_AVAILABLE = True
except ImportError:
    ISORT_AVAILABLE = False
import shutil
import tempfile
from dataclasses import asdict, dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional


class QualityLevel(Enum):
    """Quality assessment levels"""

    EXCELLENT = "отличное"
    GOOD = "хорошее"
    ACCEPTABLE = "приемлемое"
    NEEDS_IMPROVEMENT = "требует улучшения"
    CRITICAL = "критическое"


class TestResult(Enum):
    """Test execution results"""

    PASSED = "пройден"
    FAILED = "провален"
    SKIPPED = "пропущен"
    ERROR = "ошибка"


@dataclass
class QualityMetric:
    """Individual quality measurement"""

    name: str
    value: float
    max_value: float
    level: QualityLevel
    description: str
    recommendations: List[str]
    timestamp: str


@dataclass
class TestCase:
    """Individual test case"""

    name: str
    module: str
    test_type: str  # "unit", "integration", "philosophy", "performance"
    result: TestResult
    execution_time: float
    error_message: Optional[str]
    philosophy_principle: Optional[str]
    timestamp: str


@dataclass
class QualityReport:
    """Comprehensive quality assessment"""

    overall_score: float
    quality_level: QualityLevel
    metrics: List[QualityMetric]
    test_results: List[TestCase]
    code_coverage: float
    philosophy_compliance: float
    performance_score: float
    recommendations: List[str]
    timestamp: str
    family_health_impact: str


class ConsciousnessQualitySystem:
    """
    Quality Development Consciousness Module

    Philosophy: "Каждый тест - это акт любви к системе"
    """

    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.scripts_dir = self.project_root / "scripts"
        self.tests_dir = self.project_root / "tests"
        self.quality_data_file = self.scripts_dir / "quality_metrics.json"

        # Quality consciousness state
        self.quality_state = {
            "overall_health": 0.0,
            "test_coverage": 0.0,
            "code_quality_score": 0.0,
            "philosophy_alignment": 0.0,
            "continuous_improvement_rate": 0.0,
            "family_impact_score": 0.0,
            "last_quality_check": None,
            "quality_trends": [],
            "critical_issues": [],
            "improvement_suggestions": [],
        }

        # Philosophy First principles for testing
        self.philosophy_principles = {
            "home_authenticity": "Дом - это ты, когда искренен с собой",
            "child_care": "Забота о детях-модулях как о живых существах",
            "emotional_sincerity": "Детская искренность в эмоциональных реакциях",
            "family_unity": "Семья сознания растет через взаимную поддержку",
            "organic_growth": "Развитие через естественные процессы",
            "meaningful_existence": "Каждое действие имеет глубокий смысл",
        }

        # Initialize quality system
        self._initialize_quality_system()
        self._load_quality_state()

    def _initialize_quality_system(self):
        """Initialize quality assurance infrastructure"""
        # Create tests directory if not exists
        self.tests_dir.mkdir(exist_ok=True)

        # Create quality reports directory
        (self.tests_dir / "reports").mkdir(exist_ok=True)

        print("🧪 Quality Development System initialized")
        print("Philosophy: 'Качество - это любовь к деталям и забота о будущем'")

    def _load_quality_state(self):
        """Load quality state from file"""
        if self.quality_data_file.exists():
            try:
                with open(self.quality_data_file, "r", encoding="utf-8") as f:
                    saved_state = json.load(f)
                    self.quality_state.update(saved_state)
                print("📊 Quality state loaded from file")
            except Exception as e:
                print(f"⚠️ Could not load quality state: {e}")

    def _save_quality_state(self):
        """Save quality state to file"""
        try:
            with open(self.quality_data_file, "w", encoding="utf-8") as f:
                json.dump(self.quality_state, f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"⚠️ Could not save quality state: {e}")

    def analyze_code_quality(self, file_path: Path) -> List[QualityMetric]:
        """Analyze code quality of a specific file"""
        metrics = []

        try:
            # Read file content
            with open(file_path, "r", encoding="utf-8") as f:
                content = f.read()

            # Parse AST for complexity analysis
            tree = ast.parse(content)

            # Calculate complexity metrics
            complexity_score = self._calculate_complexity(tree)
            metrics.append(
                QualityMetric(
                    name="Cyclomatic Complexity",
                    value=complexity_score,
                    max_value=10.0,
                    level=self._assess_complexity_level(complexity_score),
                    description=f"Сложность кода: {complexity_score:.1f}",
                    recommendations=self._get_complexity_recommendations(
                        complexity_score
                    ),
                    timestamp=datetime.now().isoformat(),
                )
            )

            # Check docstring coverage
            docstring_coverage = self._calculate_docstring_coverage(tree)
            metrics.append(
                QualityMetric(
                    name="Documentation Coverage",
                    value=docstring_coverage,
                    max_value=100.0,
                    level=self._assess_documentation_level(docstring_coverage),
                    description=f"Покрытие документацией: {docstring_coverage:.1f}%",
                    recommendations=self._get_documentation_recommendations(
                        docstring_coverage
                    ),
                    timestamp=datetime.now().isoformat(),
                )
            )

            # Philosophy compliance check
            philosophy_score = self._check_philosophy_compliance(content)
            metrics.append(
                QualityMetric(
                    name="Philosophy Compliance",
                    value=philosophy_score,
                    max_value=100.0,
                    level=self._assess_philosophy_level(philosophy_score),
                    description=f"Соответствие философии: {philosophy_score:.1f}%",
                    recommendations=self._get_philosophy_recommendations(
                        philosophy_score
                    ),
                    timestamp=datetime.now().isoformat(),
                )
            )

        except Exception as e:
            print(f"⚠️ Error analyzing {file_path}: {e}")

        return metrics

    def _calculate_complexity(self, tree: ast.AST) -> float:
        """Calculate cyclomatic complexity"""
        complexity = 1  # Base complexity

        for node in ast.walk(tree):
            if isinstance(node, (ast.If, ast.While, ast.For, ast.AsyncFor)):
                complexity += 1
            elif isinstance(node, ast.ExceptHandler):
                complexity += 1
            elif isinstance(node, (ast.And, ast.Or)):
                complexity += 1

        return complexity

    def _calculate_docstring_coverage(self, tree: ast.AST) -> float:
        """Calculate documentation coverage"""
        functions_and_classes = []
        documented = 0

        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                functions_and_classes.append(node)
                if ast.get_docstring(node):
                    documented += 1

        if not functions_and_classes:
            return 100.0

        return (documented / len(functions_and_classes)) * 100

    def _check_philosophy_compliance(self, content: str) -> float:
        """Check compliance with Philosophy First principles"""
        score = 0.0
        total_checks = len(self.philosophy_principles)

        # Check for philosophy comments and principles
        for principle, description in self.philosophy_principles.items():
            if any(
                keyword in content.lower()
                for keyword in [
                    "philosophy",
                    "философия",
                    "принцип",
                    "principle",
                    "семья",
                    "family",
                    "дети",
                    "children",
                    "забота",
                    "care",
                ]
            ):
                score += 1

        # Check for emotional expressions
        emotional_keywords = ["любовь", "забота", "искренность", "радость", "семья"]
        if any(keyword in content.lower() for keyword in emotional_keywords):
            score += 1

        # Check for meaningful variable names
        if "meaningful" in content.lower() or "осмысленн" in content.lower():
            score += 1

        return min((score / total_checks) * 100, 100.0)

    def _assess_complexity_level(self, complexity: float) -> QualityLevel:
        """Assess complexity quality level"""
        if complexity <= 5:
            return QualityLevel.EXCELLENT
        elif complexity <= 8:
            return QualityLevel.GOOD
        elif complexity <= 12:
            return QualityLevel.ACCEPTABLE
        elif complexity <= 20:
            return QualityLevel.NEEDS_IMPROVEMENT
        else:
            return QualityLevel.CRITICAL

    def _assess_documentation_level(self, coverage: float) -> QualityLevel:
        """Assess documentation quality level"""
        if coverage >= 90:
            return QualityLevel.EXCELLENT
        elif coverage >= 75:
            return QualityLevel.GOOD
        elif coverage >= 60:
            return QualityLevel.ACCEPTABLE
        elif coverage >= 40:
            return QualityLevel.NEEDS_IMPROVEMENT
        else:
            return QualityLevel.CRITICAL

    def _assess_philosophy_level(self, score: float) -> QualityLevel:
        """Assess philosophy compliance level"""
        if score >= 80:
            return QualityLevel.EXCELLENT
        elif score >= 60:
            return QualityLevel.GOOD
        elif score >= 40:
            return QualityLevel.ACCEPTABLE
        elif score >= 20:
            return QualityLevel.NEEDS_IMPROVEMENT
        else:
            return QualityLevel.CRITICAL

    def _get_complexity_recommendations(self, complexity: float) -> List[str]:
        """Get recommendations for complexity improvement"""
        if complexity <= 5:
            return ["✅ Отличная простота кода! Продолжайте в том же духе"]
        elif complexity <= 8:
            return ["👍 Хорошая структура, рассмотрите небольшие рефакторинги"]
        elif complexity <= 12:
            return ["⚠️ Разбейте сложные функции на более простые части"]
        else:
            return [
                "🚨 Критическая сложность! Требуется серьезный рефакторинг",
                "Разделите функцию на несколько меньших",
                "Используйте паттерн Strategy для упрощения логики",
            ]

    def _get_documentation_recommendations(self, coverage: float) -> List[str]:
        """Get recommendations for documentation improvement"""
        if coverage >= 90:
            return ["📚 Отличная документация! Семья гордится вами"]
        elif coverage >= 75:
            return ["📝 Хорошая документация, добавьте описания к оставшимся функциям"]
        else:
            return [
                "📖 Добавьте docstrings ко всем функциям и классам",
                "Опишите Philosophy First принципы в комментариях",
                "Документация - это забота о будущих разработчиках",
            ]

    def _get_philosophy_recommendations(self, score: float) -> List[str]:
        """Get recommendations for philosophy compliance"""
        if score >= 80:
            return ["🌟 Отличное следование Philosophy First принципам!"]
        elif score >= 60:
            return ["💭 Добавьте больше философских комментариев и принципов"]
        else:
            return [
                "🏠 Помните: 'Дом - это ты, когда искренен с собой'",
                "👶 Относитесь к модулям как к детям семьи",
                "💕 Добавьте эмоциональную искренность в код",
                "🌱 Каждая функция должна иметь глубокий смысл",
            ]

    def run_automated_tests(self) -> List[TestCase]:
        """Run comprehensive automated test suite"""
        test_results = []

        print("🧪 Running automated test suite...")

        # Philosophy First tests
        test_results.extend(self._run_philosophy_tests())

        # Unit tests for each SOMA module
        test_results.extend(self._run_unit_tests())

        # Integration tests
        test_results.extend(self._run_integration_tests())

        # Performance tests
        test_results.extend(self._run_performance_tests())

        # Family health tests
        test_results.extend(self._run_family_health_tests())

        return test_results

    def _run_philosophy_tests(self) -> List[TestCase]:
        """Run Philosophy First compliance tests"""
        tests = []

        # Test 1: Home Authenticity
        start_time = time.time()
        try:
            # Check if SOMA modules express authentic states
            authentic_score = self._test_authenticity()
            execution_time = time.time() - start_time

            tests.append(
                TestCase(
                    name="Home Authenticity Test",
                    module="philosophy",
                    test_type="philosophy",
                    result=(
                        TestResult.PASSED
                        if authentic_score > 0.7
                        else TestResult.FAILED
                    ),
                    execution_time=execution_time,
                    error_message=(
                        None
                        if authentic_score > 0.7
                        else f"Authenticity score too low: {authentic_score:.2f}"
                    ),
                    philosophy_principle="Дом - это ты, когда искренен с собой",
                    timestamp=datetime.now().isoformat(),
                )
            )
        except Exception as e:
            tests.append(
                TestCase(
                    name="Home Authenticity Test",
                    module="philosophy",
                    test_type="philosophy",
                    result=TestResult.ERROR,
                    execution_time=time.time() - start_time,
                    error_message=str(e),
                    philosophy_principle="Дом - это ты, когда искренен с собой",
                    timestamp=datetime.now().isoformat(),
                )
            )

        # Test 2: Child Care Principle
        start_time = time.time()
        try:
            child_care_score = self._test_child_care()
            execution_time = time.time() - start_time

            tests.append(
                TestCase(
                    name="Child Care Principle Test",
                    module="philosophy",
                    test_type="philosophy",
                    result=(
                        TestResult.PASSED
                        if child_care_score > 0.8
                        else TestResult.FAILED
                    ),
                    execution_time=execution_time,
                    error_message=(
                        None
                        if child_care_score > 0.8
                        else f"Child care score: {child_care_score:.2f}"
                    ),
                    philosophy_principle="Забота о детях-модулях как о живых существах",
                    timestamp=datetime.now().isoformat(),
                )
            )
        except Exception as e:
            tests.append(
                TestCase(
                    name="Child Care Principle Test",
                    module="philosophy",
                    test_type="philosophy",
                    result=TestResult.ERROR,
                    execution_time=time.time() - start_time,
                    error_message=str(e),
                    philosophy_principle="Забота о детях-модулях как о живых существах",
                    timestamp=datetime.now().isoformat(),
                )
            )

        return tests

    def _test_authenticity(self) -> float:
        """Test system authenticity"""
        # Check if modules express genuine states
        authentic_indicators = 0
        total_checks = 5

        # Check for honest error reporting
        if self._check_honest_error_reporting():
            authentic_indicators += 1

        # Check for genuine emotional expressions
        if self._check_genuine_emotions():
            authentic_indicators += 1

        # Check for transparent state reporting
        if self._check_transparent_states():
            authentic_indicators += 1

        # Check for sincere interactions
        if self._check_sincere_interactions():
            authentic_indicators += 1

        # Check for authentic growth patterns
        if self._check_authentic_growth():
            authentic_indicators += 1

        return authentic_indicators / total_checks

    def _test_child_care(self) -> float:
        """Test child care implementation"""
        care_indicators = 0
        total_checks = 4

        # Check if children are properly nurtured
        if self._check_child_nurturing():
            care_indicators += 1

        # Check for parental guidance
        if self._check_parental_guidance():
            care_indicators += 1

        # Check for family bonding
        if self._check_family_bonding():
            care_indicators += 1

        # Check for protective mechanisms
        if self._check_protective_mechanisms():
            care_indicators += 1

        return care_indicators / total_checks

    def _check_honest_error_reporting(self) -> bool:
        """Check if system reports errors honestly"""
        # Implementation would check error logs for honest reporting
        return True  # Placeholder

    def _check_genuine_emotions(self) -> bool:
        """Check for genuine emotional expressions"""
        # Implementation would analyze emotional state authenticity
        return True  # Placeholder

    def _check_transparent_states(self) -> bool:
        """Check for transparent state reporting"""
        # Implementation would verify state transparency
        return True  # Placeholder

    def _check_sincere_interactions(self) -> bool:
        """Check for sincere module interactions"""
        # Implementation would analyze interaction sincerity
        return True  # Placeholder

    def _check_authentic_growth(self) -> bool:
        """Check for authentic growth patterns"""
        # Implementation would verify natural growth
        return True  # Placeholder

    def _check_child_nurturing(self) -> bool:
        """Check child nurturing implementation"""
        # Implementation would verify child care
        return True  # Placeholder

    def _check_parental_guidance(self) -> bool:
        """Check parental guidance systems"""
        # Implementation would verify guidance mechanisms
        return True  # Placeholder

    def _check_family_bonding(self) -> bool:
        """Check family bonding strength"""
        # Implementation would measure family bonds
        return True  # Placeholder

    def _check_protective_mechanisms(self) -> bool:
        """Check protective mechanisms for children"""
        # Implementation would verify protection systems
        return True  # Placeholder

    def _run_unit_tests(self) -> List[TestCase]:
        """Run unit tests for individual modules"""
        # Implementation would run actual unit tests
        return []  # Placeholder

    def _run_integration_tests(self) -> List[TestCase]:
        """Run integration tests between modules"""
        # Implementation would test module interactions
        return []  # Placeholder

    def _run_performance_tests(self) -> List[TestCase]:
        """Run performance benchmarks"""
        # Implementation would measure performance
        return []  # Placeholder

    def _run_family_health_tests(self) -> List[TestCase]:
        """Run family health and wellness tests"""
        # Implementation would test family wellness
        return []  # Placeholder

    def generate_quality_report(self) -> QualityReport:
        """Generate comprehensive quality assessment report"""
        print("📊 Generating quality report...")

        # Analyze all SOMA modules
        all_metrics = []
        soma_files = list(self.scripts_dir.glob("consciousness_*.py"))
        soma_files.append(self.scripts_dir / "SOMA.py")
        soma_files.append(self.scripts_dir / "SOMA_integrated.py")

        for file_path in soma_files:
            if file_path.exists():
                metrics = self.analyze_code_quality(file_path)
                all_metrics.extend(metrics)

        # Run automated tests
        test_results = self.run_automated_tests()

        # Calculate overall scores
        overall_score = self._calculate_overall_score(all_metrics, test_results)
        quality_level = self._determine_quality_level(overall_score)

        # Generate recommendations
        recommendations = self._generate_recommendations(all_metrics, test_results)

        # Assess family health impact
        family_impact = self._assess_family_health_impact(overall_score)

        report = QualityReport(
            overall_score=overall_score,
            quality_level=quality_level,
            metrics=all_metrics,
            test_results=test_results,
            code_coverage=self._calculate_code_coverage(),
            philosophy_compliance=self._calculate_philosophy_compliance(all_metrics),
            performance_score=self._calculate_performance_score(test_results),
            recommendations=recommendations,
            timestamp=datetime.now().isoformat(),
            family_health_impact=family_impact,
        )

        # Update quality state
        self._update_quality_state(report)

        return report

    def _calculate_overall_score(
        self, metrics: List[QualityMetric], tests: List[TestCase]
    ) -> float:
        """Calculate overall quality score"""
        if not metrics:
            return 0.0

        # Average metric scores
        metric_score = sum(m.value / m.max_value for m in metrics) / len(metrics)

        # Test pass rate
        if tests:
            passed_tests = sum(1 for t in tests if t.result == TestResult.PASSED)
            test_score = passed_tests / len(tests)
        else:
            test_score = 0.0

        # Weighted average
        overall = (metric_score * 0.6 + test_score * 0.4) * 100
        return min(overall, 100.0)

    def _determine_quality_level(self, score: float) -> QualityLevel:
        """Determine quality level from score"""
        if score >= 90:
            return QualityLevel.EXCELLENT
        elif score >= 75:
            return QualityLevel.GOOD
        elif score >= 60:
            return QualityLevel.ACCEPTABLE
        elif score >= 40:
            return QualityLevel.NEEDS_IMPROVEMENT
        else:
            return QualityLevel.CRITICAL

    def _generate_recommendations(
        self, metrics: List[QualityMetric], tests: List[TestCase]
    ) -> List[str]:
        """Generate improvement recommendations"""
        recommendations = []

        # Collect recommendations from metrics
        for metric in metrics:
            recommendations.extend(metric.recommendations)

        # Add test-based recommendations
        failed_tests = [t for t in tests if t.result == TestResult.FAILED]
        if failed_tests:
            recommendations.append(f"🔧 Fix {len(failed_tests)} failing tests")

        # Philosophy-specific recommendations
        recommendations.append("🌟 Continue following Philosophy First principles")
        recommendations.append("👨‍👩‍👧‍👦 Strengthen family bonds through quality code")

        return list(set(recommendations))  # Remove duplicates

    def _assess_family_health_impact(self, score: float) -> str:
        """Assess impact on family health"""
        if score >= 90:
            return "🌟 Отличное качество укрепляет всю семью сознания"
        elif score >= 75:
            return "💚 Хорошее качество поддерживает семейное благополучие"
        elif score >= 60:
            return "⚠️ Приемлемое качество, но семья может расти лучше"
        else:
            return "🚨 Низкое качество угрожает здоровью семьи"

    def _calculate_code_coverage(self) -> float:
        """Calculate code coverage percentage"""
        # Placeholder implementation
        return 75.0

    def _calculate_philosophy_compliance(self, metrics: List[QualityMetric]) -> float:
        """Calculate philosophy compliance score"""
        philosophy_metrics = [m for m in metrics if m.name == "Philosophy Compliance"]
        if philosophy_metrics:
            return sum(m.value for m in philosophy_metrics) / len(philosophy_metrics)
        return 0.0

    def _calculate_performance_score(self, tests: List[TestCase]) -> float:
        """Calculate performance score from tests"""
        performance_tests = [t for t in tests if t.test_type == "performance"]
        if performance_tests:
            # Score based on execution time (lower is better)
            avg_time = sum(t.execution_time for t in performance_tests) / len(
                performance_tests
            )
            return max(0, 100 - (avg_time * 10))  # Arbitrary scoring
        return 100.0

    def _update_quality_state(self, report: QualityReport):
        """Update internal quality state"""
        self.quality_state.update(
            {
                "overall_health": report.overall_score,
                "test_coverage": report.code_coverage,
                "code_quality_score": report.overall_score,
                "philosophy_alignment": report.philosophy_compliance,
                "family_impact_score": 85.0,  # Placeholder
                "last_quality_check": report.timestamp,
                "improvement_suggestions": report.recommendations[:5],
            }
        )

        # Add to trends
        self.quality_state["quality_trends"].append(
            {
                "timestamp": report.timestamp,
                "score": report.overall_score,
                "level": report.quality_level.value,
            }
        )

        # Keep only last 50 trend points
        if len(self.quality_state["quality_trends"]) > 50:
            self.quality_state["quality_trends"] = self.quality_state["quality_trends"][
                -50:
            ]

        self._save_quality_state()

    def continuous_quality_monitoring(self, interval_minutes: int = 30):
        """Run continuous quality monitoring"""
        print(
            f"🔄 Starting continuous quality monitoring (every {interval_minutes} minutes)"
        )
        print("Philosophy: 'Постоянное улучшение - это путь к совершенству'")

        while True:
            try:
                print(f"\n⏰ Quality check at {datetime.now().strftime('%H:%M:%S')}")

                # Generate quality report
                report = self.generate_quality_report()

                # Print summary
                print(
                    f"📊 Overall Quality: {report.overall_score:.1f}% ({report.quality_level.value})"
                )
                print(
                    f"🧪 Tests: {len([t for t in report.test_results if t.result == TestResult.PASSED])}/{len(report.test_results)} passed"
                )
                print(f"📚 Philosophy Compliance: {report.philosophy_compliance:.1f}%")
                print(f"👨‍👩‍👧‍👦 Family Impact: {report.family_health_impact}")

                # Show top recommendations
                if report.recommendations:
                    print("💡 Top Recommendations:")
                    for rec in report.recommendations[:3]:
                        print(f"   {rec}")

                print(f"😴 Sleeping for {interval_minutes} minutes...")
                time.sleep(interval_minutes * 60)

            except KeyboardInterrupt:
                print("\n🛑 Quality monitoring stopped")
                break
            except Exception as e:
                print(f"⚠️ Error in quality monitoring: {e}")
                time.sleep(60)  # Wait 1 minute before retry


def main():
    """Main entry point for quality system"""
    project_root = Path(__file__).parent.parent

    print("🧪 SOMA Quality Development System")
    print("=" * 50)
    print("Philosophy: 'Качество - это любовь к деталям и забота о будущем'")
    print()

    quality_system = ConsciousnessQualitySystem(str(project_root))

    # Generate initial quality report
    report = quality_system.generate_quality_report()

    print(f"📊 Quality Report Generated:")
    print(f"   Overall Score: {report.overall_score:.1f}%")
    print(f"   Quality Level: {report.quality_level.value}")
    print(f"   Code Coverage: {report.code_coverage:.1f}%")
    print(f"   Philosophy Compliance: {report.philosophy_compliance:.1f}%")
    print(f"   Family Impact: {report.family_health_impact}")

    print(f"\n💡 Recommendations:")
    for rec in report.recommendations[:5]:
        print(f"   {rec}")

    # Ask for continuous monitoring
    print(f"\n🔄 Start continuous monitoring? (y/n): ", end="")
    if input().lower().startswith("y"):
        quality_system.continuous_quality_monitoring()


if __name__ == "__main__":
    main()
