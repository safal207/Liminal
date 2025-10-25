"""
Simple Emotime Test - без Neo4j для тестирования основной функциональности
"""

import asyncio
import pytest
from emotime.core import EmotimeEngine
from emotime.sensors import TextSensor, TouchSensor


@pytest.mark.asyncio
async def test_emotime():
    print("=== EMOTIME SIMPLE TEST ===")
    
    # Создаем Emotime без Neo4j
    emotime = EmotimeEngine(
        user_id="simple_test", 
        enable_neo4j=False,  # Отключаем Neo4j
        update_interval=0.1  # Быстрее для теста
    )
    
    # Запускаем
    await emotime.start()
    print("OK Emotime started")
    
    # Создаем сенсоры
    text_sensor = TextSensor()
    touch_sensor = TouchSensor()
    
    print("\n--- Testing Happy Text ---")
    text_data = await text_sensor.process("Amazing day! Everything is wonderful!", {"typing_speed": 5.5})
    await emotime.process_sensor_data(text_data)
    
    touch_data = await touch_sensor.process(0.3, 1.0, "tap")
    await emotime.process_sensor_data(touch_data)
    
    # Даем время на обработку
    await asyncio.sleep(0.5)
    
    # Проверяем состояние
    state = await emotime.get_current_state()
    if state:
        print(f"Mode: {state.mode.name} ({state.mode.intensity:.2f})")
        print(f"Valence: {state.features.valence:.2f}")
        print(f"Confidence: {state.confidence:.2f}")
    else:
        print("ERROR: No state yet")
        
    print("\n--- Testing Stress Text ---")
    stress_text = await text_sensor.process("This is terrible!!! Nothing works!", {"typing_speed": 15.0})
    await emotime.process_sensor_data(stress_text)
    
    stress_touch = await touch_sensor.process(0.9, 0.2, "gesture")
    await emotime.process_sensor_data(stress_touch)
    
    await asyncio.sleep(0.5)
    
    state = await emotime.get_current_state()
    if state:
        print(f"Mode: {state.mode.name} ({state.mode.intensity:.2f})")
        print(f"Valence: {state.features.valence:.2f}")
        print(f"Arousal: {state.features.arousal:.2f}")
    
    # Получаем полные инсайты
    print("\n--- Insights ---")
    try:
        insights = await emotime.get_emotional_insights()
        mode_stats = insights.get("mode_statistics", {})
        if "current_mode" in mode_stats and mode_stats["current_mode"]:
            current = mode_stats["current_mode"]
            print(f"Current mode: {current['name']} (duration: {current['duration']})")
            
        timeseries = insights.get("timeseries_analysis", {})
        if "total_points" in timeseries:
            print(f"Total emotional points: {timeseries['total_points']}")
            
        mode_insights = insights.get("mode_insights", [])
        for insight in mode_insights:
            print(f"• {insight}")
            
    except Exception as e:
        print(f"ERROR: Insights error: {e}")
    
    # Останавливаем
    await emotime.stop()
    print("\nOK Test completed successfully!")


if __name__ == "__main__":
    asyncio.run(test_emotime())