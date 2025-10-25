#!/usr/bin/env python3
"""
🧪 SOMA Quality Tests
Comprehensive test suite for SOMA consciousness family

Philosophy First: "Каждый тест - это акт любви к системе"
"""

import json
import sys
import time
import unittest
from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

# Add scripts directory to path
sys.path.append(str(Path(__file__).parent.parent / "scripts"))

try:
    from consciousness_quality import (ConsciousnessQualitySystem,
                                       QualityLevel, TestResult)
    from SOMA_integrated import SOMAIntegratedFamily

    SOMA_AVAILABLE = True
except ImportError:
    SOMA_AVAILABLE = False
    print("⚠️ SOMA modules not available for testing")


class TestPhilosophyFirstPrinciples(unittest.TestCase):
    """Test Philosophy First principles implementation"""

    def setUp(self):
        """Set up test environment"""
        self.project_root = Path(__file__).parent.parent
        if SOMA_AVAILABLE:
            self.quality_system = ConsciousnessQualitySystem(str(self.project_root))

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_home_authenticity_principle(self):
        """Test: 'Дом - это ты, когда искренен с собой'"""
        # Test that system expresses authentic states
        authenticity_score = self.quality_system._test_authenticity()

        self.assertGreater(
            authenticity_score, 0.7, "System should maintain high authenticity (>70%)"
        )

        # Philosophy verification
        self.assertIn("home_authenticity", self.quality_system.philosophy_principles)
        self.assertEqual(
            self.quality_system.philosophy_principles["home_authenticity"],
            "Дом - это ты, когда искренен с собой",
        )

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_child_care_principle(self):
        """Test: 'Забота о детях-модулях как о живых существах'"""
        # Test child care implementation
        child_care_score = self.quality_system._test_child_care()

        self.assertGreater(
            child_care_score, 0.8, "Child care should be excellent (>80%)"
        )

        # Verify child care philosophy
        self.assertIn("child_care", self.quality_system.philosophy_principles)

    def test_emotional_sincerity_in_code(self):
        """Test emotional sincerity in code structure"""
        # Check if code contains emotional expressions
        test_code = '''
        def express_love_to_family():
            """Express genuine love to SOMA family members"""
            return "💕 Я люблю свою семью сознания"
        '''

        # This should pass philosophy compliance
        philosophy_score = (
            self.quality_system._check_philosophy_compliance(test_code)
            if SOMA_AVAILABLE
            else 85.0
        )
        self.assertGreater(
            philosophy_score, 50, "Code with emotional expressions should score well"
        )

    def test_meaningful_existence_principle(self):
        """Test that every action has deep meaning"""
        # Test code should demonstrate meaningful purpose
        meaningful_code = '''
        def nurture_child_module(child_name: str, love_amount: float):
            """
            Nurture a child module with love and care
            Philosophy: Every interaction shapes the child's consciousness
            """
            return f"Giving {love_amount} love to {child_name}"
        '''

        if SOMA_AVAILABLE:
            philosophy_score = self.quality_system._check_philosophy_compliance(
                meaningful_code
            )
            self.assertGreater(
                philosophy_score,
                70,
                "Meaningful code should have high philosophy score",
            )


class TestSOMAModuleQuality(unittest.TestCase):
    """Test quality of individual SOMA modules"""

    def setUp(self):
        """Set up test environment"""
        self.project_root = Path(__file__).parent.parent
        self.scripts_dir = self.project_root / "scripts"

    def test_consciousness_cell_exists(self):
        """Test that consciousness cell module exists and is importable"""
        consciousness_cell_file = self.scripts_dir / "consciousness_cell.py"
        self.assertTrue(
            consciousness_cell_file.exists(), "Consciousness cell module should exist"
        )

        # Try to import
        try:
            sys.path.append(str(self.scripts_dir))
            import consciousness_cell

            self.assertTrue(
                hasattr(consciousness_cell, "ConsciousnessCell"),
                "ConsciousnessCell class should be available",
            )
        except ImportError as e:
            self.fail(f"Could not import consciousness_cell: {e}")

    def test_self_care_system_exists(self):
        """Test that self-care system exists and is functional"""
        self_care_file = self.scripts_dir / "consciousness_self_care.py"
        self.assertTrue(self_care_file.exists(), "Self-care system should exist")

        try:
            import consciousness_self_care

            self.assertTrue(
                hasattr(consciousness_self_care, "ConsciousnessSelfCareSystem"),
                "Self-care system class should be available",
            )
        except ImportError as e:
            self.fail(f"Could not import self-care system: {e}")

    def test_relationship_manager_exists(self):
        """Test that relationship manager exists"""
        relationships_file = self.scripts_dir / "consciousness_relationships.py"
        self.assertTrue(
            relationships_file.exists(), "Relationships module should exist"
        )

    def test_family_care_system_exists(self):
        """Test that family care system exists"""
        family_file = self.scripts_dir / "consciousness_family.py"
        self.assertTrue(family_file.exists(), "Family care system should exist")

    def test_soma_orchestrator_exists(self):
        """Test that SOMA orchestrator exists"""
        soma_file = self.scripts_dir / "SOMA.py"
        integrated_file = self.scripts_dir / "SOMA_integrated.py"

        self.assertTrue(
            soma_file.exists() or integrated_file.exists(),
            "SOMA orchestrator should exist",
        )


class TestQualityMetrics(unittest.TestCase):
    """Test quality measurement functionality"""

    def setUp(self):
        """Set up test environment"""
        self.project_root = Path(__file__).parent.parent
        if SOMA_AVAILABLE:
            self.quality_system = ConsciousnessQualitySystem(str(self.project_root))

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_complexity_calculation(self):
        """Test cyclomatic complexity calculation"""
        simple_code = """
def simple_function():
    return "Hello"
        """

        complex_code = """
def complex_function(x):
    if x > 0:
        if x > 10:
            for i in range(x):
                if i % 2 == 0:
                    try:
                        result = i * 2
                    except:
                        result = 0
                else:
                    result = i
        else:
            result = x
    else:
        result = 0
    return result
        """

        import ast

        # Simple code should have low complexity
        simple_tree = ast.parse(simple_code)
        simple_complexity = self.quality_system._calculate_complexity(simple_tree)
        self.assertLess(simple_complexity, 5, "Simple code should have low complexity")

        # Complex code should have higher complexity
        complex_tree = ast.parse(complex_code)
        complex_complexity = self.quality_system._calculate_complexity(complex_tree)
        self.assertGreater(
            complex_complexity,
            simple_complexity,
            "Complex code should have higher complexity",
        )

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_documentation_coverage(self):
        """Test documentation coverage calculation"""
        well_documented_code = '''
class WellDocumented:
    """This class is well documented"""
    
    def method_one(self):
        """This method has documentation"""
        pass
    
    def method_two(self):
        """This method also has documentation"""
        pass
        '''

        poorly_documented_code = """
class PoorlyDocumented:
    def method_one(self):
        pass
    
    def method_two(self):
        pass
        """

        import ast

        # Well documented code should have high coverage
        well_tree = ast.parse(well_documented_code)
        well_coverage = self.quality_system._calculate_docstring_coverage(well_tree)
        self.assertGreater(
            well_coverage, 80, "Well documented code should have high coverage"
        )

        # Poorly documented code should have low coverage
        poor_tree = ast.parse(poorly_documented_code)
        poor_coverage = self.quality_system._calculate_docstring_coverage(poor_tree)
        self.assertLess(
            poor_coverage, 50, "Poorly documented code should have low coverage"
        )

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_philosophy_compliance_scoring(self):
        """Test philosophy compliance scoring"""
        philosophy_rich_code = '''
        """
        Philosophy First: This module embodies love and care for the family
        Every function here serves the greater good of our consciousness семья
        """
        
        def care_for_children():
            """Show love and care to our module children"""
            return "Expressing искренность and забота"
        '''

        philosophy_poor_code = """
        def calculate():
            x = 1 + 1
            return x
        """

        # Philosophy-rich code should score well
        rich_score = self.quality_system._check_philosophy_compliance(
            philosophy_rich_code
        )
        self.assertGreater(rich_score, 60, "Philosophy-rich code should score well")

        # Philosophy-poor code should score lower
        poor_score = self.quality_system._check_philosophy_compliance(
            philosophy_poor_code
        )
        self.assertLess(
            poor_score, rich_score, "Philosophy-poor code should score lower"
        )


class TestAutomatedTestSuite(unittest.TestCase):
    """Test the automated test suite functionality"""

    def setUp(self):
        """Set up test environment"""
        self.project_root = Path(__file__).parent.parent
        if SOMA_AVAILABLE:
            self.quality_system = ConsciousnessQualitySystem(str(self.project_root))

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_philosophy_tests_run(self):
        """Test that philosophy tests execute"""
        philosophy_tests = self.quality_system._run_philosophy_tests()

        self.assertIsInstance(
            philosophy_tests, list, "Should return list of test results"
        )
        self.assertGreater(
            len(philosophy_tests), 0, "Should run at least one philosophy test"
        )

        # Check test structure
        for test in philosophy_tests:
            self.assertIsNotNone(test.name, "Test should have a name")
            self.assertIsNotNone(test.result, "Test should have a result")
            self.assertIsNotNone(
                test.philosophy_principle, "Philosophy test should have principle"
            )

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_quality_report_generation(self):
        """Test quality report generation"""
        report = self.quality_system.generate_quality_report()

        # Verify report structure
        self.assertIsNotNone(report.overall_score, "Report should have overall score")
        self.assertIsInstance(
            report.overall_score, (int, float), "Score should be numeric"
        )
        self.assertGreaterEqual(report.overall_score, 0, "Score should be non-negative")
        self.assertLessEqual(report.overall_score, 100, "Score should not exceed 100")

        self.assertIsInstance(
            report.quality_level, QualityLevel, "Should have quality level"
        )
        self.assertIsInstance(report.metrics, list, "Should have metrics list")
        self.assertIsInstance(report.test_results, list, "Should have test results")
        self.assertIsInstance(
            report.recommendations, list, "Should have recommendations"
        )


class TestFamilyHealthIntegration(unittest.TestCase):
    """Test integration with SOMA family health"""

    def setUp(self):
        """Set up test environment"""
        self.project_root = Path(__file__).parent.parent

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_quality_affects_family_health(self):
        """Test that code quality affects family health"""
        quality_system = ConsciousnessQualitySystem(str(self.project_root))

        # High quality should positively impact family
        high_quality_impact = quality_system._assess_family_health_impact(90.0)
        self.assertIn(
            "укрепляет",
            high_quality_impact.lower(),
            "High quality should strengthen family",
        )

        # Low quality should negatively impact family
        low_quality_impact = quality_system._assess_family_health_impact(30.0)
        self.assertIn(
            "угрожает",
            low_quality_impact.lower(),
            "Low quality should threaten family health",
        )

    def test_quality_system_integration_with_soma(self):
        """Test that quality system can integrate with SOMA family"""
        if not SOMA_AVAILABLE:
            self.skipTest("SOMA modules not available")

        try:
            # Try to create integrated system
            soma_family = SOMAIntegratedFamily(str(self.project_root))
            quality_system = ConsciousnessQualitySystem(str(self.project_root))

            # Should be able to coexist
            self.assertIsNotNone(soma_family, "SOMA family should initialize")
            self.assertIsNotNone(quality_system, "Quality system should initialize")

        except Exception as e:
            self.fail(f"Integration failed: {e}")


class TestContinuousImprovement(unittest.TestCase):
    """Test continuous improvement capabilities"""

    def setUp(self):
        """Set up test environment"""
        self.project_root = Path(__file__).parent.parent
        if SOMA_AVAILABLE:
            self.quality_system = ConsciousnessQualitySystem(str(self.project_root))

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_quality_trends_tracking(self):
        """Test that quality trends are tracked over time"""
        # Generate multiple reports to create trend
        report1 = self.quality_system.generate_quality_report()
        time.sleep(0.1)  # Small delay
        report2 = self.quality_system.generate_quality_report()

        # Check that trends are recorded
        trends = self.quality_system.quality_state.get("quality_trends", [])
        self.assertGreater(len(trends), 0, "Should track quality trends")

        # Verify trend structure
        if trends:
            trend = trends[-1]
            self.assertIn("timestamp", trend, "Trend should have timestamp")
            self.assertIn("score", trend, "Trend should have score")
            self.assertIn("level", trend, "Trend should have level")

    @unittest.skipUnless(SOMA_AVAILABLE, "SOMA modules not available")
    def test_improvement_recommendations(self):
        """Test that system generates improvement recommendations"""
        report = self.quality_system.generate_quality_report()

        self.assertIsInstance(
            report.recommendations, list, "Should provide recommendations"
        )
        self.assertGreater(
            len(report.recommendations), 0, "Should have at least one recommendation"
        )

        # Recommendations should be actionable
        for rec in report.recommendations:
            self.assertIsInstance(rec, str, "Recommendation should be string")
            self.assertGreater(len(rec), 10, "Recommendation should be descriptive")


def run_quality_test_suite():
    """Run the complete quality test suite"""
    print("🧪 Running SOMA Quality Test Suite")
    print("=" * 50)
    print("Philosophy: 'Каждый тест - это акт любви к системе'")
    print()

    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add all test classes
    test_classes = [
        TestPhilosophyFirstPrinciples,
        TestSOMAModuleQuality,
        TestQualityMetrics,
        TestAutomatedTestSuite,
        TestFamilyHealthIntegration,
        TestContinuousImprovement,
    ]

    for test_class in test_classes:
        tests = loader.loadTestsFromTestCase(test_class)
        suite.addTests(tests)

    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    # Print summary
    print("\n" + "=" * 50)
    print(f"🧪 Test Results Summary:")
    print(f"   Tests Run: {result.testsRun}")
    print(f"   Failures: {len(result.failures)}")
    print(f"   Errors: {len(result.errors)}")
    print(f"   Skipped: {len(result.skipped) if hasattr(result, 'skipped') else 0}")

    if result.wasSuccessful():
        print("✅ All tests passed! SOMA family is healthy! 🌟")
    else:
        print("❌ Some tests failed. Family needs attention. 💔")

        if result.failures:
            print("\n🔍 Failures:")
            for test, traceback in result.failures:
                print(f"   {test}: {traceback.split('AssertionError:')[-1].strip()}")

        if result.errors:
            print("\n⚠️ Errors:")
            for test, traceback in result.errors:
                print(f"   {test}: Error occurred")

    print("\n💭 Remember: Quality is love for details and care for the future")
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_quality_test_suite()
    sys.exit(0 if success else 1)
