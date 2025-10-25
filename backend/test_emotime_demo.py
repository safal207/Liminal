"""
🌿✨ Emotime Demo Script — демонстрация работы эмоциональных временных рядов

Тестирует все компоненты Emotime:
- Сенсоры (текст, касания, аудио)
- Фьюжн признаков 
- Временные ряды и режимы
- Метрики и Neo4j интеграция

"Путешествие по эмоциональному ландшафту души"
"""

import asyncio
import time
import random
from datetime import datetime, timedelta

# Импорты Emotime
from emotime.core import EmotimeEngine
from emotime.sensors import TextSensor, TouchSensor, AudioSensor


class EmotimeDemo:
    """Демонстратор возможностей Emotime."""
    
    def __init__(self):
        self.emotime = None
        self.text_sensor = TextSensor()
        self.touch_sensor = TouchSensor()
        self.audio_sensor = AudioSensor()
    
    async def run_full_demo(self):
        """Запускает полную демонстрацию Emotime."""
        print("🌿✨ EMOTIME DEMO STARTED — Путешествие по эмоциональному ландшафту")
        print("=" * 70)
        
        # Инициализация
        await self._initialize_emotime()
        
        # Демо сценарии
        await self._demo_morning_calm()
        await asyncio.sleep(2)
        
        await self._demo_work_focus()  
        await asyncio.sleep(2)
        
        await self._demo_stress_spike()
        await asyncio.sleep(2)
        
        await self._demo_joy_burst()
        await asyncio.sleep(2)
        
        await self._demo_contemplation()
        await asyncio.sleep(2)
        
        # Анализ результатов
        await self._show_insights()
        
        # Завершение
        await self._cleanup()
        
        print("\n🌿 EMOTIME DEMO COMPLETED — Сердце замирает...")
        print("=" * 70)
    
    async def _initialize_emotime(self):
        """Инициализирует Emotime с демо пользователем."""
        print("\n💓 Инициализация Emotime...")
        
        self.emotime = EmotimeEngine(
            user_id="demo_user",
            session_id=f"demo_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
            update_interval=0.5,  # Быстрее для демо
            enable_neo4j=True
        )
        
        await self.emotime.start()
        await asyncio.sleep(1)  # Даем время на запуск
        
        print("✅ Emotime инициализирован и запущен")
    
    async def _demo_morning_calm(self):
        """Демонстрирует спокойное утреннее состояние."""
        print("\n🌅 СЦЕНАРИЙ 1: Спокойное утро")
        print("-" * 40)
        
        # Медленный текст с позитивными эмоциями
        texts = [
            "Доброе утро! Сегодня прекрасный день.",
            "Солнце мягко светит в окно.",
            "Чувствую внутренний покой и гармонию.",
            "Время для глубокого размышления."
        ]
        
        for text in texts:
            sensor_data = await self.text_sensor.process(text, {"typing_speed": 3.5})
            await self.emotime.process_sensor_data(sensor_data)
            
            # Мягкие касания
            touch_data = await self.touch_sensor.process(
                pressure=0.2 + random.uniform(0, 0.2),
                duration=1.5 + random.uniform(0, 1.0),
                pattern="tap"
            )
            await self.emotime.process_sensor_data(touch_data)
            
            await asyncio.sleep(0.8)
        
        # Показываем состояние
        state = await self.emotime.get_current_state()
        if state:
            print(f"💭 Режим: {state.mode.name} ({state.mode.intensity:.2f})")
            print(f"💫 Валентность: {state.features.valence:.2f}")
            print(f"⚡ Возбуждение: {state.features.arousal:.2f}")
    
    async def _demo_work_focus(self):
        """Демонстрирует рабочее сосредоточение."""
        print("\n🎯 СЦЕНАРИЙ 2: Рабочий фокус")
        print("-" * 40)
        
        texts = [
            "Нужно сосредоточиться на задаче.",
            "Концентрируемся на важном проекте.", 
            "Анализирую данные шаг за шагом.",
            "Почти готово, еще немного усилий."
        ]
        
        for i, text in enumerate(texts):
            # Ускоряющаяся печать
            typing_speed = 6.0 + i * 1.5
            sensor_data = await self.text_sensor.process(text, {"typing_speed": typing_speed})
            await self.emotime.process_sensor_data(sensor_data)
            
            # Ритмичные касания 
            for _ in range(3):
                touch_data = await self.touch_sensor.process(
                    pressure=0.6 + random.uniform(0, 0.2),
                    duration=0.4 + random.uniform(0, 0.3),
                    pattern="tap"
                )
                await self.emotime.process_sensor_data(touch_data)
                await asyncio.sleep(0.3)
                
            await asyncio.sleep(0.5)
        
        state = await self.emotime.get_current_state()
        if state:
            print(f"🎯 Режим: {state.mode.name} ({state.mode.intensity:.2f})")
            print(f"⚡ Темп: {state.features.tempo:.2f}")
            print(f"💪 Доминирование: {state.features.dominance:.2f}")
    
    async def _demo_stress_spike(self):
        """Демонстрирует стрессовую ситуацию."""
        print("\n⚡ СЦЕНАРИЙ 3: Стрессовый пик")
        print("-" * 40)
        
        texts = [
            "Что происходит?! Система не отвечает!",
            "Это катастрофа! Нужно срочно исправить!!!",
            "Время уходит, а решения нет...",
            "Паника не поможет, но как тяжело сохранять спокойствие."
        ]
        
        for i, text in enumerate(texts):
            # Очень быстрая печать с ошибками
            typing_speed = 12.0 + i * 2.0
            sensor_data = await self.text_sensor.process(text, {"typing_speed": typing_speed})
            await self.emotime.process_sensor_data(sensor_data)
            
            # Резкие частые касания
            for _ in range(5):
                touch_data = await self.touch_sensor.process(
                    pressure=0.8 + random.uniform(0, 0.2),
                    duration=0.1 + random.uniform(0, 0.1),
                    pattern="gesture" if i > 2 else "tap"
                )
                await self.emotime.process_sensor_data(touch_data)
                await asyncio.sleep(0.1)
                
            # Добавляем аудио стресс-маркеры
            if i > 1:
                audio_data = await self.audio_sensor.process(
                    pitch_mean=180 + random.uniform(0, 50),  # Высокий тон
                    pitch_variance=60 + random.uniform(0, 20),  # Большая вариативность
                    speech_rate=220 + random.uniform(0, 30),  # Быстрая речь
                    volume_level=0.8 + random.uniform(0, 0.2),
                    pause_ratio=0.05,  # Мало пауз
                    emotion_markers=["stress", "anxiety", "tension"]
                )
                await self.emotime.process_sensor_data(audio_data)
                
            await asyncio.sleep(0.3)
        
        state = await self.emotime.get_current_state()
        if state:
            print(f"⚡ Режим: {state.mode.name} ({state.mode.intensity:.2f})")
            print(f"💔 Валентность: {state.features.valence:.2f}")
            print(f"🔥 Интенсивность: {state.features.intensity:.2f}")
    
    async def _demo_joy_burst(self):
        """Демонстрирует взрыв радости."""
        print("\n✨ СЦЕНАРИЙ 4: Взрыв радости")
        print("-" * 40)
        
        texts = [
            "Ура! Получилось!!!",
            "Я так счастлива! Все работает отлично! ✨",
            "Невероятно! Этот успех превзошел все ожидания!",
            "Хочется танцевать от радости! 🌟"
        ]
        
        for i, text in enumerate(texts):
            # Энергичная печать с восклицаниями
            typing_speed = 8.0 + random.uniform(-2, 4)
            sensor_data = await self.text_sensor.process(text, {"typing_speed": typing_speed})
            await self.emotime.process_sensor_data(sensor_data)
            
            # Игривые касания
            patterns = ["swipe", "tap", "gesture"]
            for _ in range(3):
                touch_data = await self.touch_sensor.process(
                    pressure=0.4 + random.uniform(0, 0.4),
                    duration=0.6 + random.uniform(0, 0.6),
                    pattern=random.choice(patterns)
                )
                await self.emotime.process_sensor_data(touch_data)
                await asyncio.sleep(0.2)
                
            # Радостные аудио маркеры
            if i > 0:
                audio_data = await self.audio_sensor.process(
                    pitch_mean=160 + random.uniform(0, 30),  # Приподнятый тон
                    pitch_variance=45 + random.uniform(0, 15),
                    speech_rate=180 + random.uniform(0, 20),  # Живая речь
                    volume_level=0.7 + random.uniform(0, 0.2),
                    pause_ratio=0.15,
                    emotion_markers=["joy", "excitement", "happiness", "confidence"]
                )
                await self.emotime.process_sensor_data(audio_data)
                
            await asyncio.sleep(0.6)
        
        state = await self.emotime.get_current_state()
        if state:
            print(f"✨ Режим: {state.mode.name} ({state.mode.intensity:.2f})")
            print(f"💖 Валентность: {state.features.valence:.2f}")
            print(f"🎉 Возбуждение: {state.features.arousal:.2f}")
    
    async def _demo_contemplation(self):
        """Демонстрирует глубокое размышление."""
        print("\n💭 СЦЕНАРИЙ 5: Глубокое размышление")
        print("-" * 40)
        
        texts = [
            "Задумываюсь о смысле происходящего...",
            "Что если посмотреть на это под другим углом?",
            "Интересная философская дилемма.",
            "Нужно время, чтобы все осмыслить."
        ]
        
        for text in texts:
            # Медленная вдумчивая печать с паузами
            typing_speed = 2.5 + random.uniform(0, 1)
            sensor_data = await self.text_sensor.process(text, {"typing_speed": typing_speed})
            await self.emotime.process_sensor_data(sensor_data)
            
            # Долгие вдумчивые касания
            touch_data = await self.touch_sensor.process(
                pressure=0.3 + random.uniform(0, 0.2),
                duration=2.0 + random.uniform(0, 1.5),
                pattern="hold"
            )
            await self.emotime.process_sensor_data(touch_data)
            
            # Долгая пауза для размышлений
            await asyncio.sleep(1.5)
        
        state = await self.emotime.get_current_state()
        if state:
            print(f"💭 Режим: {state.mode.name} ({state.mode.intensity:.2f})")
            print(f"🧠 Возбуждение: {state.features.arousal:.2f}")
            print(f"⏰ Темп: {state.features.tempo:.2f}")
    
    async def _show_insights(self):
        """Показывает глубокие инсайты о сессии."""
        print("\n🔍 АНАЛИЗ ЭМОЦИОНАЛЬНОЙ СЕССИИ")
        print("=" * 50)
        
        try:
            insights = await self.emotime.get_emotional_insights()
            
            # Текущее состояние
            current = insights["current_state"]
            print(f"💫 Текущий режим: {current.get('mode', {}).get('name', 'неизвестно')}")
            print(f"🎯 Уверенность: {current.get('confidence', 0):.1%}")
            
            # Статистика режимов
            mode_stats = insights["mode_statistics"]
            if "mode_distribution" in mode_stats:
                print(f"\n📊 Распределение режимов:")
                for mode, percentage in mode_stats["mode_distribution"].items():
                    print(f"   {mode}: {percentage:.1f}%")
            
            # Инсайты
            mode_insights = insights["mode_insights"]
            print(f"\n💡 Инсайты:")
            for insight in mode_insights:
                print(f"   • {insight}")
            
            # Анализ временных рядов
            timeseries = insights["timeseries_analysis"]
            if "patterns" in timeseries:
                patterns = timeseries["patterns"]
                if patterns:
                    print(f"\n🔄 Обнаруженные паттерны:")
                    for pattern in patterns:
                        print(f"   • {pattern['description']}")
            
            # Исторические данные из Neo4j
            if "historical_patterns" in insights:
                hist = insights["historical_patterns"]
                if hist.get("total_points", 0) > 0:
                    print(f"\n📈 Исторические данные:")
                    print(f"   Всего точек: {hist['total_points']}")
                    if hist.get("emotional_baseline"):
                        baseline = hist["emotional_baseline"]
                        print(f"   Базовая валентность: {baseline['valence']:.2f}")
                        print(f"   Базовое возбуждение: {baseline['arousal']:.2f}")
            
        except Exception as e:
            print(f"❌ Ошибка анализа: {e}")
    
    async def _cleanup(self):
        """Очищает ресурсы."""
        print("\n🧹 Завершение сессии...")
        if self.emotime:
            await self.emotime.stop()
        print("✅ Ресурсы освобождены")


async def main():
    """Основная функция демо."""
    demo = EmotimeDemo()
    await demo.run_full_demo()


if __name__ == "__main__":
    print("🌿✨ Запуск Emotime Demo...")
    asyncio.run(main())