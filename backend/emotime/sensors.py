"""
🌿✨ Emotime Sensors — глаза, уши и кожа системы

Сенсорный слой для захвата эмоциональных данных:
- TextSensor: анализ текстовых сообщений на эмоции
- TouchSensor: паттерны касаний и тактильные данные
- AudioSensor: анализ голосовых характеристик
"""

import re
import time
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Union


class SensorType(Enum):
    """Типы сенсоров."""

    TEXT = "text"
    TOUCH = "touch"
    AUDIO = "audio"


@dataclass
class SensorData:
    """Базовые данные с сенсора."""

    sensor_type: SensorType
    timestamp: datetime
    raw_data: Any
    metadata: Dict[str, Any]


@dataclass
class TextData:
    """Данные текстового сенсора."""

    text: str
    word_count: int
    char_count: int
    typing_speed: Optional[float] = None  # символов в секунду
    pause_duration: Optional[float] = None  # секунды до отправки


@dataclass
class TouchData:
    """Данные сенсора касаний."""

    pressure: float  # 0.0-1.0
    duration: float  # секунды
    frequency: float  # касаний в минуту
    pattern: str  # "tap", "swipe", "hold", "gesture"
    coordinates: Optional[tuple] = None  # (x, y) если доступно


@dataclass
class AudioData:
    """Данные аудио сенсора."""

    pitch_mean: float  # средняя частота голоса
    pitch_variance: float  # вариативность тона
    speech_rate: float  # слов в минуту
    volume_level: float  # 0.0-1.0
    pause_ratio: float  # доля пауз в речи
    emotion_markers: List[str]  # ["stress", "joy", "fatigue"]


class TextSensor:
    """
    Сенсор для анализа текстовых данных.

    Анализирует эмоциональные маркеры в тексте:
    - Эмоциональные слова и фразы
    - Паттерны печати (скорость, паузы)
    - Структуру предложений
    """

    # Словари эмоциональных маркеров
    POSITIVE_WORDS = {
        "радость",
        "счастье",
        "восторг",
        "любовь",
        "вдохновение",
        "благодарность",
        "success",
        "joy",
        "happy",
        "love",
        "amazing",
        "wonderful",
        "great",
        "огонь",
        "супер",
        "класс",
        "круто",
        "отлично",
        "прекрасно",
    }

    NEGATIVE_WORDS = {
        "грусть",
        "печаль",
        "боль",
        "страх",
        "тревога",
        "стресс",
        "усталость",
        "sad",
        "pain",
        "fear",
        "stress",
        "tired",
        "anxious",
        "worried",
        "плохо",
        "ужасно",
        "кошмар",
        "беда",
        "проблема",
        "сложно",
    }

    CALM_WORDS = {
        "спокойствие",
        "мир",
        "тишина",
        "баланс",
        "гармония",
        "медитация",
        "peace",
        "calm",
        "quiet",
        "balance",
        "meditation",
        "relax",
        "расслабленно",
        "тихо",
        "умиротворенно",
    }

    INTENSITY_MARKERS = {
        "очень",
        "крайне",
        "невероятно",
        "супер",
        "мега",
        "офигительно",
        "extremely",
        "incredibly",
        "totally",
        "absolutely",
        "fucking",
        "!!!",
        "???",
        "CAPS",
    }

    def __init__(self):
        self.last_message_time: Optional[datetime] = None

    async def process(self, text: str, metadata: Dict = None) -> SensorData:
        """Обрабатывает текстовое сообщение."""
        if metadata is None:
            metadata = {}

        timestamp = datetime.now()

        # Вычисляем паузу между сообщениями
        pause_duration = None
        if self.last_message_time:
            pause_duration = (timestamp - self.last_message_time).total_seconds()
        self.last_message_time = timestamp

        # Анализируем текст
        text_data = TextData(
            text=text,
            word_count=len(text.split()),
            char_count=len(text),
            typing_speed=metadata.get("typing_speed"),
            pause_duration=pause_duration,
        )

        # Добавляем эмоциональный анализ в метаданные
        emotion_analysis = self._analyze_emotions(text)
        metadata.update(emotion_analysis)

        return SensorData(
            sensor_type=SensorType.TEXT,
            timestamp=timestamp,
            raw_data=text_data,
            metadata=metadata,
        )

    def _analyze_emotions(self, text: str) -> Dict[str, float]:
        """Анализирует эмоциональные маркеры в тексте."""
        text_lower = text.lower()
        words = re.findall(r"\b\w+\b", text_lower)

        if not words:
            return {"valence": 0.0, "arousal": 0.0, "intensity": 0.0}

        # Подсчитываем эмоциональные маркеры
        positive_count = sum(1 for word in words if word in self.POSITIVE_WORDS)
        negative_count = sum(1 for word in words if word in self.NEGATIVE_WORDS)
        calm_count = sum(1 for word in words if word in self.CALM_WORDS)
        intensity_count = sum(1 for word in words if word in self.INTENSITY_MARKERS)

        # Добавляем анализ пунктуации
        if "!" in text:
            intensity_count += text.count("!")
        if text.isupper() and len(text) > 5:
            intensity_count += 2
        if "?" in text:
            intensity_count += text.count("?") * 0.5

        total_words = len(words)

        # Вычисляем валентность (-1.0 to 1.0)
        if positive_count + negative_count > 0:
            valence = (positive_count - negative_count) / (
                positive_count + negative_count
            )
        else:
            valence = 0.0

        # Корректируем валентность через спокойствие
        if calm_count > 0:
            calm_influence = min(calm_count / total_words * 2, 0.5)
            valence = valence * (1 - calm_influence) + 0.3 * calm_influence

        # Вычисляем возбуждение (0.0 to 1.0)
        arousal = min(
            (positive_count + negative_count + intensity_count) / total_words * 2, 1.0
        )

        # Вычисляем интенсивность (0.0 to 1.0)
        intensity = min(intensity_count / total_words * 3, 1.0)

        return {
            "valence": max(-1.0, min(1.0, valence)),
            "arousal": max(0.0, min(1.0, arousal)),
            "intensity": max(0.0, min(1.0, intensity)),
        }


class TouchSensor:
    """
    Сенсор для анализа паттернов касаний.

    Анализирует тактильные данные:
    - Давление и продолжительность касаний
    - Частоту и ритм взаимодействий
    - Паттерны жестов
    """

    def __init__(self, baseline_pressure: float = 0.5):
        self.baseline_pressure = baseline_pressure
        self.touch_history: List[TouchData] = []

    async def process(
        self,
        pressure: float,
        duration: float,
        pattern: str = "tap",
        coordinates: tuple = None,
        metadata: Dict = None,
    ) -> SensorData:
        """Обрабатывает данные касания."""
        if metadata is None:
            metadata = {}

        timestamp = datetime.now()

        # Вычисляем частоту касаний (за последнюю минуту)
        one_minute_ago = timestamp.timestamp() - 60
        recent_touches = [
            touch for touch in self.touch_history if touch.duration > one_minute_ago
        ]
        frequency = len(recent_touches)

        touch_data = TouchData(
            pressure=pressure,
            duration=duration,
            frequency=frequency,
            pattern=pattern,
            coordinates=coordinates,
        )

        # Сохраняем в историю (ограничиваем размер)
        self.touch_history.append(touch_data)
        if len(self.touch_history) > 100:
            self.touch_history = self.touch_history[-50:]

        # Анализируем эмоциональные характеристики касания
        emotion_analysis = self._analyze_touch_emotions(touch_data)
        metadata.update(emotion_analysis)

        return SensorData(
            sensor_type=SensorType.TOUCH,
            timestamp=timestamp,
            raw_data=touch_data,
            metadata=metadata,
        )

    def _analyze_touch_emotions(self, touch: TouchData) -> Dict[str, float]:
        """Анализирует эмоциональные характеристики касания."""

        # Валентность: мягкие касания = позитив, резкие = негатив
        if touch.pressure < self.baseline_pressure * 0.7:
            valence = 0.3  # мягкие касания
        elif touch.pressure > self.baseline_pressure * 1.5:
            valence = -0.2  # резкие касания
        else:
            valence = 0.0

        # Возбуждение: частота касаний и интенсивность
        arousal = min(touch.frequency / 30.0, 1.0)  # нормализуем к 30 касаниям в минуту
        arousal += min(touch.pressure, 1.0) * 0.3

        # Интенсивность: давление + длительность
        intensity = (touch.pressure + min(touch.duration, 5.0) / 5.0) / 2.0

        # Паттерн касаний влияет на эмоции
        pattern_modifiers = {
            "tap": {"valence": 0.1, "arousal": 0.1},
            "swipe": {"valence": 0.0, "arousal": 0.3},
            "hold": {"valence": 0.2, "arousal": -0.1},
            "gesture": {"valence": 0.3, "arousal": 0.2},
        }

        if touch.pattern in pattern_modifiers:
            mod = pattern_modifiers[touch.pattern]
            valence += mod["valence"]
            arousal += mod["arousal"]

        return {
            "valence": max(-1.0, min(1.0, valence)),
            "arousal": max(0.0, min(1.0, arousal)),
            "intensity": max(0.0, min(1.0, intensity)),
        }


class AudioSensor:
    """
    Сенсор для анализа голосовых характеристик.

    Анализирует аудио данные:
    - Тон и интонацию
    - Скорость речи
    - Паузы и ритм
    - Эмоциональные маркеры в голосе
    """

    def __init__(self):
        self.baseline_pitch = 150.0  # Hz
        self.baseline_rate = 150.0  # слов в минуту

    async def process(
        self,
        pitch_mean: float,
        pitch_variance: float,
        speech_rate: float,
        volume_level: float,
        pause_ratio: float,
        emotion_markers: List[str] = None,
        metadata: Dict = None,
    ) -> SensorData:
        """Обрабатывает аудио данные."""
        if metadata is None:
            metadata = {}
        if emotion_markers is None:
            emotion_markers = []

        timestamp = datetime.now()

        audio_data = AudioData(
            pitch_mean=pitch_mean,
            pitch_variance=pitch_variance,
            speech_rate=speech_rate,
            volume_level=volume_level,
            pause_ratio=pause_ratio,
            emotion_markers=emotion_markers,
        )

        # Анализируем эмоциональные характеристики голоса
        emotion_analysis = self._analyze_voice_emotions(audio_data)
        metadata.update(emotion_analysis)

        return SensorData(
            sensor_type=SensorType.AUDIO,
            timestamp=timestamp,
            raw_data=audio_data,
            metadata=metadata,
        )

    def _analyze_voice_emotions(self, audio: AudioData) -> Dict[str, float]:
        """Анализирует эмоциональные характеристики голоса."""

        # Валентность на основе тона и маркеров
        valence = 0.0
        if audio.pitch_mean > self.baseline_pitch * 1.1:
            valence += 0.2  # высокий тон = позитив
        elif audio.pitch_mean < self.baseline_pitch * 0.9:
            valence -= 0.1  # низкий тон = негатив

        # Эмоциональные маркеры
        positive_markers = {"joy", "happiness", "excitement", "confidence"}
        negative_markers = {"stress", "sadness", "anger", "fatigue"}

        for marker in audio.emotion_markers:
            if marker in positive_markers:
                valence += 0.3
            elif marker in negative_markers:
                valence -= 0.3

        # Возбуждение на основе скорости речи и вариативности тона
        arousal = 0.0
        if audio.speech_rate > self.baseline_rate * 1.2:
            arousal += 0.4  # быстрая речь = высокое возбуждение
        elif audio.speech_rate < self.baseline_rate * 0.8:
            arousal -= 0.2  # медленная речь = низкое возбуждение

        # Вариативность тона
        arousal += min(audio.pitch_variance / 50.0, 0.5)

        # Интенсивность на основе громкости и пауз
        intensity = audio.volume_level
        if audio.pause_ratio < 0.1:
            intensity += 0.2  # мало пауз = высокая интенсивность
        elif audio.pause_ratio > 0.3:
            intensity -= 0.1  # много пауз = низкая интенсивность

        return {
            "valence": max(-1.0, min(1.0, valence)),
            "arousal": max(0.0, min(1.0, arousal)),
            "intensity": max(0.0, min(1.0, intensity)),
        }
