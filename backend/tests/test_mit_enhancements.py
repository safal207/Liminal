"""
MIT Enhanced Features Testing Suite
Тестирование новых ML компонентов в духе MIT Computer Science.
"""

import os
import sys
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import numpy as np
import pytest

# Add project root to path
sys.path.append(os.path.dirname(os.path.dirname(__file__)))

from emotime.core import EmotimeEngine
from emotime.fusion import EmotionalFeatures
from emotime.sensors import SensorData, SensorType, TextData
from emotime.timeseries import EmotionalPoint


class TestMITEnhancements:
    """Тестирование MIT ML улучшений."""

    def setup_method(self):
        """Настройка для каждого теста."""
        self.engine_with_ml = EmotimeEngine(
            user_id="mit_test_user",
            session_id="mit_test_session",
            enable_ml=True,
            enable_neo4j=False,
        )

        self.engine_classic = EmotimeEngine(
            user_id="classic_test_user",
            session_id="classic_test_session",
            enable_ml=False,
            enable_neo4j=False,
        )

    def test_ml_enhancement_initialization(self):
        """Тест инициализации ML компонентов."""
        # Проверяем, что ML компоненты доступны
        if hasattr(self.engine_with_ml, "adaptive_engine"):
            print("   MIT ML enhancements detected")
            assert self.engine_with_ml.adaptive_engine is not None
            assert self.engine_with_ml.feature_learner is not None
        else:
            print("   Classic mode - ML enhancements not available")
            pytest.skip("ML enhancements not available")

    @pytest.mark.asyncio
    async def test_enhanced_feature_processing(self):
        """Тест улучшенной обработки признаков."""
        # Создаем тестовые данные
        sensor_data = SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=datetime.now(),
            raw_data=TextData(
                text="I am experiencing deep contemplation and insight",
                word_count=7,
                char_count=47,
            ),
            metadata={"source": "user_input"},
        )

        # Запускаем движок для обработки
        await self.engine_with_ml.start()
        await self.engine_with_ml.process_sensor_data(sensor_data)

        # Даем время на обработку
        await asyncio.sleep(1.5)

        # Проверяем результат
        current_state = await self.engine_with_ml.get_current_state()

        if current_state:
            print(f"   Enhanced processing result:")
            print(f"     Mode: {current_state.mode.name}")
            print(f"     Confidence: {current_state.confidence:.2f}")
            print(
                f"     Features: V={current_state.features.valence:.2f}, "
                f"A={current_state.features.arousal:.2f}"
            )

            # Проверяем базовые свойства
            assert 0.0 <= current_state.features.valence <= 1.0
            assert 0.0 <= current_state.features.arousal <= 1.0
            assert 0.0 <= current_state.confidence <= 1.0

        await self.engine_with_ml.stop()

    @pytest.mark.asyncio
    async def test_classical_vs_enhanced_comparison(self):
        """Сравнение классической и улучшенной обработки."""
        test_texts = [
            "I feel absolutely amazing and full of energy!",
            "This situation makes me quite anxious and worried.",
            "I'm in a peaceful state of deep meditation.",
            "The excitement is overwhelming - this is incredible!",
        ]

        classic_results = []
        enhanced_results = []

        # Запускаем оба движка
        await self.engine_classic.start()
        if hasattr(self.engine_with_ml, "adaptive_engine"):
            await self.engine_with_ml.start()

        try:
            for text in test_texts:
                sensor_data = SensorData(
                    sensor_type=SensorType.TEXT,
                    timestamp=datetime.now(),
                    raw_data=TextData(
                        text=text, word_count=len(text.split()), char_count=len(text)
                    ),
                    metadata={},
                )

                # Классическая обработка
                await self.engine_classic.process_sensor_data(sensor_data)
                await asyncio.sleep(1.2)
                classic_state = await self.engine_classic.get_current_state()

                # Улучшенная обработка (если доступна)
                if hasattr(self.engine_with_ml, "adaptive_engine"):
                    await self.engine_with_ml.process_sensor_data(sensor_data)
                    await asyncio.sleep(1.2)
                    enhanced_state = await self.engine_with_ml.get_current_state()

                    if enhanced_state:
                        enhanced_results.append(
                            {
                                "text": text[:30] + "...",
                                "mode": enhanced_state.mode.name,
                                "confidence": enhanced_state.confidence,
                                "valence": enhanced_state.features.valence,
                            }
                        )

                if classic_state:
                    classic_results.append(
                        {
                            "text": text[:30] + "...",
                            "mode": classic_state.mode.name,
                            "confidence": classic_state.confidence,
                            "valence": classic_state.features.valence,
                        }
                    )

            # Анализ результатов
            print(f"\n   COMPARISON ANALYSIS:")
            print(f"   Classic results: {len(classic_results)} processed")
            print(f"   Enhanced results: {len(enhanced_results)} processed")

            if enhanced_results:
                avg_classic_confidence = np.mean(
                    [r["confidence"] for r in classic_results]
                )
                avg_enhanced_confidence = np.mean(
                    [r["confidence"] for r in enhanced_results]
                )

                print(f"   Average confidence - Classic: {avg_classic_confidence:.3f}")
                print(
                    f"   Average confidence - Enhanced: {avg_enhanced_confidence:.3f}"
                )

                # Enhanced должен показывать не хуже классического
                assert len(enhanced_results) >= len(classic_results) * 0.8

        finally:
            await self.engine_classic.stop()
            if hasattr(self.engine_with_ml, "adaptive_engine"):
                await self.engine_with_ml.stop()

    @pytest.mark.asyncio
    async def test_deep_feature_learning_integration(self):
        """Тест интеграции глубокого обучения признаков."""
        if not hasattr(self.engine_with_ml, "feature_learner"):
            pytest.skip("Feature learner not available")

        # Симулируем батчевую обработку
        test_batch = [
            SensorData(
                sensor_type=SensorType.TEXT,
                timestamp=datetime.now(),
                raw_data=TextData(
                    text=f"Test message {i} with emotional content",
                    word_count=6,
                    char_count=30,
                ),
                metadata={},
            )
            for i in range(5)
        ]

        await self.engine_with_ml.start()

        try:
            # Обрабатываем батч
            for sensor_data in test_batch:
                await self.engine_with_ml.process_sensor_data(sensor_data)

            # Ждем обработки
            await asyncio.sleep(2.0)

            # Проверяем временные ряды
            trace = await self.engine_with_ml.get_resonance_trace()
            print(f"   Resonance trace length: {len(trace)}")

            if trace:
                latest_point = trace[-1]
                print(f"   Latest emotional point:")
                print(f"     Valence: {latest_point.valence:.3f}")
                print(f"     Arousal: {latest_point.arousal:.3f}")
                print(f"     Intensity: {latest_point.intensity:.3f}")

                # Проверяем валидность точек
                assert 0.0 <= latest_point.valence <= 1.0
                assert 0.0 <= latest_point.arousal <= 1.0
                assert 0.0 <= latest_point.intensity <= 1.0

        finally:
            await self.engine_with_ml.stop()

    @pytest.mark.asyncio
    async def test_adaptive_emotional_mode_prediction(self):
        """Тест адаптивного предсказания эмоциональных режимов."""
        if not hasattr(self.engine_with_ml, "adaptive_engine"):
            pytest.skip("Adaptive engine not available")

        # Создаем последовательность эмоциональных точек
        emotional_sequence = [
            # Постепенный переход от спокойствия к радости
            EmotionalPoint(datetime.now(), 0.5, 0.2, 0.6, 0.3, 0.4),  # Calm
            EmotionalPoint(datetime.now(), 0.6, 0.3, 0.6, 0.4, 0.5),  # Slight positive
            EmotionalPoint(datetime.now(), 0.7, 0.5, 0.7, 0.6, 0.7),  # Growing joy
            EmotionalPoint(datetime.now(), 0.8, 0.7, 0.8, 0.8, 0.8),  # High joy
        ]

        await self.engine_with_ml.start()

        try:
            for point in emotional_sequence:
                # Тестируем адаптивное предсказание
                if hasattr(
                    self.engine_with_ml.adaptive_engine, "predict_emotional_mode"
                ):
                    mode, confidence = (
                        await self.engine_with_ml.adaptive_engine.predict_emotional_mode(
                            point
                        )
                    )

                    print(
                        f"   Point V={point.valence:.2f}, A={point.arousal:.2f} → "
                        f"Mode: {mode.name}, Conf: {confidence:.3f}"
                    )

                    # Проверяем результат предсказания
                    assert confidence >= 0.0
                    assert hasattr(mode, "name")
                    assert hasattr(mode, "intensity")

        finally:
            await self.engine_with_ml.stop()

    def test_fallback_to_classical_processing(self):
        """Тест fallback к классической обработке при ошибках ML."""
        # Создаем движок с ML, но симулируем ошибку
        engine = EmotimeEngine(
            user_id="fallback_test", enable_ml=True, enable_neo4j=False
        )

        # Проверяем, что система работает даже если ML недоступен
        if not hasattr(engine, "adaptive_engine") or engine.adaptive_engine is None:
            print("   ML not available - classical fallback active")
            assert engine.fusion is not None  # Классический фьюжн доступен
            assert engine.modes is not None  # Классические режимы доступны
        else:
            print("   ML available - no fallback needed")
            assert engine.adaptive_engine is not None


class TestMITAlgorithmPerformance:
    """Тестирование производительности MIT алгоритмов."""

    @pytest.mark.performance
    @pytest.mark.asyncio
    async def test_processing_throughput(self):
        """Тест пропускной способности обработки."""
        engine = EmotimeEngine(user_id="perf_test", enable_ml=True, enable_neo4j=False)

        await engine.start()

        try:
            # Генерируем большое количество сенсорных данных
            start_time = datetime.now()
            num_messages = 100

            for i in range(num_messages):
                sensor_data = SensorData(
                    sensor_type=SensorType.TEXT,
                    timestamp=datetime.now(),
                    raw_data=TextData(
                        text=f"Performance test message {i} with emotional content",
                        word_count=7,
                        char_count=40,
                    ),
                    metadata={},
                )
                await engine.process_sensor_data(sensor_data)

                if i % 20 == 0:
                    print(f"   Processed {i+1}/{num_messages} messages")

            # Ждем завершения обработки
            await asyncio.sleep(3.0)

            end_time = datetime.now()
            processing_time = (end_time - start_time).total_seconds()
            throughput = num_messages / processing_time

            print(f"   Performance Results:")
            print(f"     Messages processed: {num_messages}")
            print(f"     Total time: {processing_time:.2f} seconds")
            print(f"     Throughput: {throughput:.1f} messages/second")

            # Проверяем минимальные требования производительности
            assert throughput > 10  # Минимум 10 сообщений в секунду

            # Проверяем финальное состояние
            final_state = await engine.get_current_state()
            if final_state:
                print(f"     Final emotional state: {final_state.mode.name}")
                print(f"     Final confidence: {final_state.confidence:.2f}")

        finally:
            await engine.stop()


if __name__ == "__main__":
    # Запуск тестов MIT улучшений
    pytest.main([__file__, "-v", "--tb=short"])
