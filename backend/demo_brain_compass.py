#!/usr/bin/env python3
"""
🧠🧭 BRAIN COMPASS INTERACTIVE DEMO

Интерактивное демо для демонстрации возможностей мозгового компаса
в режиме реального времени с визуализацией нейронных осцилляций.
"""

import asyncio
import time
import math
from datetime import datetime
from typing import Dict, List, Any

try:
    from retrosplenial_gateway import (
        RetrosplenialGateway,
        NavigationEvent,
        NavigationContext,
    )

    SYSTEM_AVAILABLE = True
except ImportError:
    SYSTEM_AVAILABLE = False


class BrainCompassDemo:
    """Интерактивное демо мозгового компаса."""

    def __init__(self):
        if SYSTEM_AVAILABLE:
            self.gateway = RetrosplenialGateway()
            self._setup_demo_context()

        self.demo_events = self._create_demo_events()
        self.visualization_active = False

    def _setup_demo_context(self):
        """Настройка контекста для демо."""
        context = NavigationContext(
            current_state="interactive_demo",
            target_state="showcase_neural_capabilities",
            emotional_context={"curiosity": 0.9, "excitement": 0.8, "focus": 0.7},
        )
        self.gateway.set_navigation_context(context)

    def _create_demo_events(self) -> List[Dict[str, Any]]:
        """Создание разнообразных демо событий."""
        return [
            {
                "name": "Creative Inspiration",
                "description": "Sudden creative inspiration for a new project",
                "emotional_valence": 0.8,
                "urgency": 0.4,
                "expected_direction": "east_create",
                "demo_notes": "Should trigger EAST (Create) with high-gamma binding",
            },
            {
                "name": "Learning Challenge",
                "description": "Encountering a new complex concept in neuroscience",
                "emotional_valence": 0.3,
                "urgency": 0.6,
                "expected_direction": "north_evolve",
                "demo_notes": "Should trigger NORTH (Evolve) with theta enhancement",
            },
            {
                "name": "Emotional Overwhelm",
                "description": "Feeling emotional overwhelm from multiple tasks",
                "emotional_valence": -0.6,
                "urgency": 0.9,
                "expected_direction": "south_instinct",
                "demo_notes": "Should trigger SOUTH (Instinct) with survival focus",
            },
            {
                "name": "Deep Reflection",
                "description": "Reflecting on the meaning of life and my values",
                "emotional_valence": 0.1,
                "urgency": 0.2,
                "expected_direction": "west_reflect",
                "demo_notes": "Should trigger WEST (Reflect) with contemplative theta",
            },
            {
                "name": "Breakthrough Discovery",
                "description": "Discovering fundamental connections between ideas",
                "emotional_valence": 0.9,
                "urgency": 0.3,
                "expected_direction": "north_evolve",
                "demo_notes": "Should trigger NORTH (Evolve) with strong coupling",
            },
        ]

    async def run_interactive_demo(self):
        """Запуск интерактивного демо."""

        print("BRAIN COMPASS INTERACTIVE DEMO")
        print("=" * 50)
        print()
        print("This demo showcases:")
        print("* Real-time theta oscillations (4-8Hz)")
        print("* Gamma synchrony memory binding (30-100Hz)")
        print("* Cross-frequency coupling")
        print("* Semantic directional navigation")
        print()

        if not SYSTEM_AVAILABLE:
            print("[WARNING] RGL system not available - running conceptual demo")
            await self._run_conceptual_demo()
            return

        print("Choose demo mode:")
        print("1. [AUTO] Automated showcase")
        print("2. [MONITOR] Real-time oscillation monitoring")
        print("3. [TEST] Interactive event testing")
        print()

        # В демо режиме автоматически запускаем все
        await self._run_automated_showcase()
        await asyncio.sleep(1)
        await self._run_oscillation_monitoring()
        await asyncio.sleep(1)
        await self._run_interactive_testing()

    async def _run_automated_showcase(self):
        """Автоматическая демонстрация возможностей."""

        print("\n[AUTO] AUTOMATED SHOWCASE")
        print("-" * 30)
        print("Processing diverse events to show neural navigation...")
        print()

        for i, event_data in enumerate(self.demo_events, 1):
            print(f"[{i}/{len(self.demo_events)}] {event_data['name']}")
            print(f"Description: {event_data['description']}")
            print(f"Expected: {event_data['expected_direction']} direction")
            print(f"Demo Note: {event_data['demo_notes']}")
            print()

            # Создание навигационного события
            event = NavigationEvent(
                event_id=f"demo_{i}_{int(time.time())}",
                event_type="demo_event",
                content=event_data["description"],
                timestamp=datetime.now(),
                source_layer="interactive_demo",
                emotional_valence=event_data["emotional_valence"],
                urgency_level=event_data["urgency"],
            )

            # Обработка события
            direction = await self.gateway.process_navigation_event(event)
            analytics = self.gateway.get_navigation_analytics()

            # Отображение результатов
            self._display_navigation_result(direction, analytics, event_data)

            await asyncio.sleep(1)

        print("[OK] Automated showcase completed!")

    async def _run_oscillation_monitoring(self):
        """Мониторинг нейронных осцилляций в реальном времени."""

        print("\n[MONITOR] REAL-TIME OSCILLATION MONITORING")
        print("-" * 40)
        print("Monitoring theta and gamma oscillations for 10 seconds...")
        print("Watch how oscillations evolve and couple together!")
        print()

        monitoring_duration = 10
        start_time = time.time()

        while time.time() - start_time < monitoring_duration:
            analytics = self.gateway.get_navigation_analytics()

            theta = analytics["theta_oscillations"]
            gamma = analytics["gamma_synchrony"]

            # Визуализация текущих осцилляций
            elapsed = time.time() - start_time
            print(
                f"[{elapsed:.1f}s] Theta: {theta['theta_type']} @ {theta['current_frequency']:.1f}Hz (power: {theta['current_power']:.2f}) | "
                f"Gamma: {gamma['gamma_band']} @ {gamma['current_frequency']:.1f}Hz (sync: {gamma['synchrony_strength']:.2f}) | "
                f"Coupling: {gamma['gamma_theta_coupling']:.3f}"
            )

            # Индикатор силы связи
            coupling = gamma["gamma_theta_coupling"]
            if coupling > 0.7:
                print("         [STRONG] STRONG COUPLING!")
            elif coupling > 0.4:
                print("         [OK] MODERATE COUPLING")
            else:
                print("         [WEAK] WEAK COUPLING")

            await asyncio.sleep(1)

        print("\n[MONITOR] Oscillation monitoring completed!")

    async def _run_interactive_testing(self):
        """Интерактивное тестирование событий."""

        print("\n[TEST] INTERACTIVE EVENT TESTING")
        print("-" * 35)
        print("Testing how different types of events affect neural navigation...")
        print()

        test_scenarios = [
            {
                "category": "SPATIAL PROCESSING",
                "description": "Navigate through complex 3D coordinate system",
                "expected_theta": "fast_spatial (8Hz)",
                "expected_direction": "east_create",
            },
            {
                "category": "EMOTIONAL PROCESSING",
                "description": "Process deep emotional memories and feelings",
                "expected_theta": "slow_conceptual (3Hz)",
                "expected_direction": "west_reflect",
            },
            {
                "category": "LEARNING PROCESS",
                "description": "Master advanced quantum mechanics concepts",
                "expected_theta": "adaptive_human (4Hz)",
                "expected_direction": "north_evolve",
            },
            {
                "category": "CRISIS RESPONSE",
                "description": "Handle immediate safety threat and danger",
                "expected_theta": "adaptive_human (4Hz)",
                "expected_direction": "south_instinct",
            },
        ]

        for scenario in test_scenarios:
            print(f"[TEST] Testing: {scenario['category']}")
            print(f"Event: {scenario['description']}")
            print(f"Expected Theta: {scenario['expected_theta']}")
            print(f"Expected Direction: {scenario['expected_direction']}")
            print()

            # Создание и обработка события
            event = NavigationEvent(
                event_id=f"test_{scenario['category'].lower()}_{int(time.time())}",
                event_type=scenario["category"].lower().replace(" ", "_"),
                content=scenario["description"],
                timestamp=datetime.now(),
                source_layer="interactive_testing",
                emotional_valence=0.5,
                urgency_level=0.5 if "CRISIS" not in scenario["category"] else 0.9,
            )

            direction = await self.gateway.process_navigation_event(event)
            analytics = self.gateway.get_navigation_analytics()

            # Проверка предсказаний
            theta_info = analytics["theta_oscillations"]
            actual_direction = direction.primary_direction.value

            print(f"[RESULTS]:")
            print(
                f"   Actual Theta: {theta_info['theta_type']} @ {theta_info['current_frequency']:.1f}Hz"
            )
            print(f"   Actual Direction: {actual_direction}")
            print(f"   Direction Strength: {direction.strength:.2f}")
            print(
                f"   Gamma Coupling: {analytics['gamma_synchrony']['gamma_theta_coupling']:.3f}"
            )

            # Валидация предсказаний
            if scenario["expected_direction"] in actual_direction:
                print(f"   [OK] Direction prediction CORRECT!")
            else:
                print(f"   [NOTE] Direction unexpected (but system is adaptive)")

            print()
            await asyncio.sleep(1)

        print("[TEST] Interactive testing completed!")

    def _display_navigation_result(self, direction, analytics, event_data):
        """Отображение результатов навигации."""

        theta = analytics["theta_oscillations"]
        gamma = analytics["gamma_synchrony"]

        print(f"[BRAIN] NEURAL PROCESSING RESULT:")
        print(
            f"   Direction: {direction.primary_direction.value} (strength: {direction.strength:.2f})"
        )
        print(f"   Confidence: {direction.confidence:.2f}")
        print(f"   Theta: {theta['theta_type']} @ {theta['current_frequency']:.1f}Hz")
        print(f"   Gamma: {gamma['gamma_band']} @ {gamma['current_frequency']:.1f}Hz")
        print(f"   Coupling: {gamma['gamma_theta_coupling']:.3f}")
        print(f"   Memory Anchors: {analytics['memory_anchors']}")

        # Проверка ожидаемого направления
        if event_data["expected_direction"] in direction.primary_direction.value:
            print(f"   [OK] Expected direction achieved!")
        else:
            print(f"   [ADAPT] Adaptive direction (system responding to context)")

        # Анализ силы направления
        if direction.strength > 2.0:
            print(f"   [BOOST] EXCEPTIONAL strength - neural enhancement active!")
        elif direction.strength > 1.5:
            print(f"   [STRONG] STRONG direction - good neural coordination")
        elif direction.strength > 1.0:
            print(f"   [OK] SOLID direction - moderate enhancement")

        print()

    async def _run_conceptual_demo(self):
        """Концептуальное демо без RGL системы."""

        print("[CONCEPT] CONCEPTUAL DEMONSTRATION")
        print("Showing how Brain Compass would process events...")
        print()

        for event_data in self.demo_events:
            print(f"[EVENT] Event: {event_data['name']}")
            print(f"   Description: {event_data['description']}")
            print(f"   Expected Direction: {event_data['expected_direction']}")
            print(f"   Demo Notes: {event_data['demo_notes']}")

            # Симуляция обработки
            print(f"   [BRAIN] Simulated Processing:")
            print(f"      -> Direction: {event_data['expected_direction']}")
            print(f"      -> Theta frequency adaptation based on content")
            print(f"      -> Gamma synchrony for memory binding")
            print(f"      -> Cross-frequency coupling enhancement")
            print()

            await asyncio.sleep(0.5)

        print("[COMPLETE] Conceptual demo complete! Install RGL for full experience.")


async def main():
    """Главная функция демо."""
    demo = BrainCompassDemo()
    await demo.run_interactive_demo()


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("\n[STOP] Demo interrupted")
    except Exception as e:
        print(f"\nDemo error: {e}")
        import traceback

        traceback.print_exc()
