"""
🌿✨ Emotime Demo Test — демонстрация работы системы

Простой демонстрационный скрипт для тестирования Emotime.
"""

import asyncio
from datetime import datetime

from .core import EmotimeEngine
from .sensors import SensorData, SensorType, TextData, TouchData, AudioData
from .utils import safe_logger


async def demo_emotional_journey():
    """Демонстрация эмоционального путешествия."""
    safe_logger.info("Starting Emotime demo...")
    
    # Создаем движок
    engine = EmotimeEngine(
        user_id="demo_user",
        session_id="demo_session",
        update_interval=1.0,  # 1 секунда между обработками
        enable_neo4j=False,   # Отключаем Neo4j для демо
        trace_window=10
    )
    
    try:
        # Запускаем движок
        await engine.start()
        safe_logger.info("Engine started - beginning emotional journey...")
        
        # Фаза 1: Спокойное утро
        safe_logger.info("Phase 1: Calm morning...")
        
        calm_texts = [
            "Good morning, feeling peaceful today",
            "Starting with meditation and tea",
            "The world feels quiet and serene"
        ]
        
        for text in calm_texts:
            text_data = TextData(
                text=text,
                word_count=len(text.split()),
                char_count=len(text)
            )
            
            sensor_data = SensorData(
                sensor_type=SensorType.TEXT,
                timestamp=datetime.now(),
                raw_data=text_data,
                metadata={"phase": "calm"}
            )
            
            await engine.process_sensor_data(sensor_data)
            await asyncio.sleep(2)
            
            # Показываем текущее состояние
            state = await engine.get_current_state()
            if state:
                safe_logger.info(f"Current mode: {state.mode.name} (confidence: {state.confidence:.2f})")
        
        # Фаза 2: Рабочий фокус
        safe_logger.info("Phase 2: Work focus...")
        
        focus_data = [
            ("I need to concentrate on this important project", SensorType.TEXT),
            ("Deep work session starting now", SensorType.TEXT),
        ]
        
        for text, sensor_type in focus_data:
            if sensor_type == SensorType.TEXT:
                text_data = TextData(
                    text=text,
                    word_count=len(text.split()),
                    char_count=len(text)
                )
                raw_data = text_data
            
            sensor_data = SensorData(
                sensor_type=sensor_type,
                timestamp=datetime.now(),
                raw_data=raw_data,
                metadata={"phase": "focus"}
            )
            
            await engine.process_sensor_data(sensor_data)
            await asyncio.sleep(2)
            
            state = await engine.get_current_state()
            if state:
                safe_logger.info(f"Current mode: {state.mode.name} (confidence: {state.confidence:.2f})")
        
        # Фаза 3: Стрессовый пик
        safe_logger.info("Phase 3: Stress peak...")
        
        stress_inputs = [
            ("URGENT DEADLINE!!! So much pressure!", SensorType.TEXT, None),
            (None, SensorType.TOUCH, TouchData(pressure=0.9, duration=0.2, frequency=8.0, pattern="rapid_taps")),
            (None, SensorType.AUDIO, AudioData(
                pitch_mean=0.8, pitch_variance=0.4, speech_rate=180.0,
                volume_level=0.9, pause_ratio=0.05, emotion_markers=["stress", "urgency"]
            ))
        ]
        
        for text, sensor_type, touch_audio_data in stress_inputs:
            if sensor_type == SensorType.TEXT:
                raw_data = TextData(text=text, word_count=len(text.split()), char_count=len(text))
            else:
                raw_data = touch_audio_data
            
            sensor_data = SensorData(
                sensor_type=sensor_type,
                timestamp=datetime.now(),
                raw_data=raw_data,
                metadata={"phase": "stress"}
            )
            
            await engine.process_sensor_data(sensor_data)
            await asyncio.sleep(2)
            
            state = await engine.get_current_state()
            if state:
                safe_logger.info(f"Current mode: {state.mode.name} (confidence: {state.confidence:.2f})")
        
        # Фаза 4: Радостный прорыв
        safe_logger.info("Phase 4: Joy breakthrough...")
        
        joy_texts = [
            "YES! Finally solved it! This is amazing!",
            "I feel so happy and accomplished right now",
            "What a wonderful feeling of success!"
        ]
        
        for text in joy_texts:
            text_data = TextData(
                text=text,
                word_count=len(text.split()),
                char_count=len(text)
            )
            
            sensor_data = SensorData(
                sensor_type=SensorType.TEXT,
                timestamp=datetime.now(),
                raw_data=text_data,
                metadata={"phase": "joy"}
            )
            
            await engine.process_sensor_data(sensor_data)
            await asyncio.sleep(2)
            
            state = await engine.get_current_state()
            if state:
                safe_logger.info(f"Current mode: {state.mode.name} (confidence: {state.confidence:.2f})")
        
        # Показываем финальную статистику
        safe_logger.info("Getting emotional insights...")
        insights = await engine.get_emotional_insights()
        
        safe_logger.info("=== EMOTIONAL JOURNEY COMPLETE ===")
        safe_logger.info(f"Final state: {insights['current_state']['status']}")
        
        if insights['current_state']['status'] == 'active':
            current = insights['current_state']
            safe_logger.info(f"Final mode: {current['mode']['name']}")
            safe_logger.info(f"Final confidence: {current['confidence']:.2f}")
            safe_logger.info(f"Trace points collected: {current['trace_points']}")
        
        # Показываем резонансный след
        trace = await engine.get_resonance_trace()
        safe_logger.info(f"Resonance trace length: {len(trace)} points")
        
        if trace:
            safe_logger.info("Recent emotional trajectory:")
            for i, point in enumerate(trace[-5:]):  # Последние 5 точек
                safe_logger.info(f"  Point {i+1}: valence={point.valence:.2f}, arousal={point.arousal:.2f}")
        
    except Exception as e:
        safe_logger.error(f"Demo error: {e}")
        
    finally:
        # Останавливаем движок
        await engine.stop()
        safe_logger.info("Demo completed - engine stopped")


async def main():
    """Главная функция демо."""
    try:
        await demo_emotional_journey()
    except KeyboardInterrupt:
        safe_logger.info("Demo interrupted by user")
    except Exception as e:
        safe_logger.error(f"Demo failed: {e}")


if __name__ == "__main__":
    print("[leaf][sparkle] Emotime Demo Starting...")
    print("Press Ctrl+C to stop\n")
    
    asyncio.run(main())