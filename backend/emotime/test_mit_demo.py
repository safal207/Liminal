"""
🚀🧠 MIT Emotime Advanced Demo — демонстрация MIT-level возможностей

Демонстрирует:
- Адаптивную калибровку с эволюционными алгоритмами
- Обучение с обратной связью
- Продвинутую аналитику
- Multi-modal sensor fusion (где доступно)
"""

import asyncio
import random
from datetime import datetime, timedelta

from .core import EmotimeEngine
from .modes import ModeType
from .sensors import AudioData, SensorData, SensorType, TextData, TouchData
from .utils import safe_logger


async def mit_emotional_journey_with_learning():
    """MIT демо с адаптивным обучением."""
    safe_logger.info("=== MIT EMOTIME ADVANCED DEMO ===")

    # Создаем MIT-enhanced движок
    engine = EmotimeEngine(
        user_id="mit_demo_user",
        session_id="mit_advanced_session",
        update_interval=0.5,  # Быстрое обновление для демо
        enable_neo4j=False,
        enable_ml=True,  # Включаем все MIT возможности
    )

    try:
        await engine.start()
        safe_logger.info("MIT Engine started with advanced capabilities")

        # Фаза 1: Обучающие данные с обратной связью
        safe_logger.info("Phase 1: Learning Phase with Feedback...")

        training_scenarios = [
            # Сценарии с известными эмоциональными состояниями
            {
                "text": "I'm feeling completely zen and peaceful today",
                "touch": {
                    "pressure": 0.2,
                    "duration": 3.0,
                    "frequency": 1.0,
                    "pattern": "gentle",
                },
                "actual_emotion": "calm",
                "context": {"time_of_day": 0.3, "session_length": 0.2},
            },
            {
                "text": "URGENT DEADLINE! Need to finish this NOW!!!",
                "touch": {
                    "pressure": 0.9,
                    "duration": 0.1,
                    "frequency": 10.0,
                    "pattern": "rapid_taps",
                },
                "actual_emotion": "stress",
                "context": {"time_of_day": 0.8, "session_length": 0.9},
            },
            {
                "text": "YES! I finally solved this problem! Amazing!",
                "touch": {
                    "pressure": 0.7,
                    "duration": 2.0,
                    "frequency": 5.0,
                    "pattern": "rhythmic",
                },
                "actual_emotion": "joy",
                "context": {"time_of_day": 0.6, "session_length": 0.5},
            },
            {
                "text": "Need to focus deeply on this complex task",
                "touch": {
                    "pressure": 0.5,
                    "duration": 4.0,
                    "frequency": 2.0,
                    "pattern": "hold",
                },
                "actual_emotion": "focus",
                "context": {"time_of_day": 0.5, "session_length": 0.7},
            },
            {
                "text": "Hmm, let me think about this philosophical question",
                "touch": {
                    "pressure": 0.3,
                    "duration": 5.0,
                    "frequency": 0.5,
                    "pattern": "gentle",
                },
                "actual_emotion": "contemplation",
                "context": {"time_of_day": 0.2, "session_length": 0.3},
            },
        ]

        # Обучаем систему
        for i, scenario in enumerate(
            training_scenarios * 2
        ):  # Повторяем каждый сценарий дважды
            safe_logger.info(f"Training scenario {i+1}: {scenario['actual_emotion']}")

            # Текстовые данные
            text_data = TextData(
                text=scenario["text"],
                word_count=len(scenario["text"].split()),
                char_count=len(scenario["text"]),
                typing_speed=random.uniform(50, 150),
                pause_duration=random.uniform(0.5, 3.0),
            )

            # Тактильные данные
            touch_data = TouchData(
                pressure=scenario["touch"]["pressure"],
                duration=scenario["touch"]["duration"],
                frequency=scenario["touch"]["frequency"],
                pattern=scenario["touch"]["pattern"],
            )

            # Аудио данные (симулируем)
            audio_data = AudioData(
                pitch_mean=0.6 + random.uniform(-0.2, 0.2),
                pitch_variance=0.3 + random.uniform(-0.1, 0.1),
                speech_rate=120 + random.uniform(-30, 30),
                volume_level=0.7 + random.uniform(-0.2, 0.2),
                pause_ratio=0.15 + random.uniform(-0.05, 0.05),
                emotion_markers=[scenario["actual_emotion"]],
            )

            # Отправляем данные
            sensor_data_batch = [
                SensorData(
                    sensor_type=SensorType.TEXT,
                    timestamp=datetime.now(),
                    raw_data=text_data,
                    metadata=scenario["context"],
                ),
                SensorData(
                    sensor_type=SensorType.TOUCH,
                    timestamp=datetime.now(),
                    raw_data=touch_data,
                    metadata=scenario["context"],
                ),
                SensorData(
                    sensor_type=SensorType.AUDIO,
                    timestamp=datetime.now(),
                    raw_data=audio_data,
                    metadata=scenario["context"],
                ),
            ]

            for data in sensor_data_batch:
                await engine.process_sensor_data(data)

            # Ждем обработки
            await asyncio.sleep(1)

            # Получаем предсказание и обучаем на обратной связи
            current_state = await engine.get_current_state()
            if current_state:
                predicted = current_state.mode.name.lower()
                actual = scenario["actual_emotion"]
                confidence = current_state.confidence

                safe_logger.info(
                    f"  Predicted: {predicted} (conf: {confidence:.2f}) | "
                    f"Actual: {actual} | "
                    f"Correct: {predicted == actual or predicted in actual}"
                )

                # Обучение с обратной связью
                await engine.learn_from_feedback(actual, scenario["context"])

        # Фаза 2: Демонстрация калибровки
        safe_logger.info("\nPhase 2: Calibration Analysis...")

        insights = await engine.get_emotional_insights()

        if "adaptive_calibration" in insights:
            calibration = insights["adaptive_calibration"]
            safe_logger.info(
                f"Current Performance: {calibration['current_performance']:.3f}"
            )
            safe_logger.info(
                f"Baseline Performance: {calibration['baseline_performance']:.3f}"
            )
            safe_logger.info(f"Total Feedback: {calibration['total_feedback']}")
            safe_logger.info(f"Calibration Events: {calibration['calibration_events']}")
            safe_logger.info(
                f"Optimizer Generation: {calibration['optimizer_generation']}"
            )

        if "calibrated_thresholds" in insights:
            thresholds = insights["calibrated_thresholds"]
            safe_logger.info(f"Calibration Status: {thresholds['calibration_status']}")
            if thresholds["calibration_status"] == "optimized":
                safe_logger.info(
                    f"Performance Score: {thresholds['performance_score']:.3f}"
                )
                safe_logger.info(f"Evolution Generation: {thresholds['generation']}")

        # Фаза 3: Тест производительности
        safe_logger.info("\nPhase 3: Performance Test...")

        test_scenarios = [
            {"text": "I feel so relaxed and at peace", "expected": "calm"},
            {"text": "This is URGENT and CRITICAL!", "expected": "stress"},
            {"text": "I'm so excited about this breakthrough!", "expected": "joy"},
            {"text": "I need to concentrate on this problem", "expected": "focus"},
        ]

        correct_predictions = 0
        total_predictions = len(test_scenarios)

        for scenario in test_scenarios:
            text_data = TextData(
                text=scenario["text"],
                word_count=len(scenario["text"].split()),
                char_count=len(scenario["text"]),
            )

            sensor_data = SensorData(
                sensor_type=SensorType.TEXT,
                timestamp=datetime.now(),
                raw_data=text_data,
                metadata={},
            )

            await engine.process_sensor_data(sensor_data)
            await asyncio.sleep(0.6)

            current_state = await engine.get_current_state()
            if current_state:
                predicted = current_state.mode.name.lower()
                expected = scenario["expected"]
                confidence = current_state.confidence

                is_correct = (
                    predicted == expected
                    or predicted in expected
                    or expected in predicted
                )

                if is_correct:
                    correct_predictions += 1

                safe_logger.info(
                    f"Test: '{scenario['text'][:40]}...' | "
                    f"Predicted: {predicted} | Expected: {expected} | "
                    f"Confidence: {confidence:.2f} | {'✓' if is_correct else '✗'}"
                )

        final_accuracy = correct_predictions / total_predictions
        safe_logger.info(
            f"\nFinal Test Accuracy: {final_accuracy:.1%} ({correct_predictions}/{total_predictions})"
        )

        # Фаза 4: Advanced Analytics
        safe_logger.info("\nPhase 4: Advanced MIT Analytics...")

        final_insights = await engine.get_emotional_insights()

        safe_logger.info("=== MIT ANALYTICS SUMMARY ===")
        safe_logger.info(f"Current State: {final_insights['current_state']['status']}")

        if final_insights["current_state"]["status"] == "active":
            current = final_insights["current_state"]
            safe_logger.info(f"Final Mode: {current['mode']['name']}")
            safe_logger.info(f"ML Enhanced: {current['ml_enhanced']}")
            safe_logger.info(f"Confidence: {current['confidence']:.3f}")
            safe_logger.info(f"Trace Points: {current['trace_points']}")

        # ML Analytics
        if "ml_analytics" in final_insights:
            ml_data = final_insights["ml_analytics"]
            safe_logger.info(f"ML Algorithm: {ml_data.get('current_algorithm', 'N/A')}")
            safe_logger.info(f"Training Samples: {ml_data.get('training_samples', 0)}")
            safe_logger.info(f"ML Available: {ml_data.get('ml_available', False)}")

        # Deep Learning
        if "deep_learning" in final_insights:
            dl_data = final_insights["deep_learning"]
            safe_logger.info(
                f"Temporal Buffer: {dl_data.get('temporal_buffer_size', 0)}"
            )
            safe_logger.info(
                f"Attention Mechanisms: {len(dl_data.get('attention_mechanisms', {}).get('last_weights', []))}"
            )

        # Triggers adaptive learning update
        await engine.trigger_adaptive_learning()

        safe_logger.info("\n=== MIT EMOTIME DEMO COMPLETED ===")
        safe_logger.info(
            "Advanced emotional intelligence with adaptive learning demonstrated!"
        )

    except Exception as e:
        safe_logger.error(f"MIT Demo error: {e}")
        import traceback

        traceback.print_exc()

    finally:
        await engine.stop()
        safe_logger.info("MIT Engine stopped")


async def main():
    """Главная функция MIT демо."""
    try:
        await mit_emotional_journey_with_learning()
    except KeyboardInterrupt:
        safe_logger.info("MIT Demo interrupted by user")
    except Exception as e:
        safe_logger.error(f"MIT Demo failed: {e}")


if __name__ == "__main__":
    print("[rocket][brain] MIT Emotime Advanced Demo Starting...")
    print("Features: Adaptive Calibration, Deep Learning, Multi-modal Fusion")
    print("Press Ctrl+C to stop\n")

    asyncio.run(main())
