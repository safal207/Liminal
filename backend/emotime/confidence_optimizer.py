"""
Emotime Confidence Optimization
Оптимизация уверенности эмоциональных предсказаний.
"""

import numpy as np
from typing import Dict, List, Optional, Any
from datetime import datetime
from dataclasses import dataclass

@dataclass
class ConfidenceCalibration:
    """Калибровка уверенности для эмоционального анализа."""
    text_quality_score: float = 0.0
    emotional_clarity: float = 0.0
    context_consistency: float = 0.0
    linguistic_confidence: float = 0.0
    temporal_stability: float = 0.0


class EmotimeConfidenceOptimizer:
    """Оптимизатор уверенности Emotime предсказаний."""
    
    def __init__(self):
        self.calibration_history = []
        self.emotional_keywords = {
            'high_positive': ['счастлив', 'радост', 'восторг', 'отлично', 'прекрасно', 'замечательно', 'великолепно'],
            'positive': ['хорошо', 'приятно', 'нравится', 'удовольствие', 'симпатично'],
            'high_negative': ['ужасно', 'кошмар', 'катастрофа', 'ненавижу', 'отвратительно', 'мучение'],
            'negative': ['плохо', 'грустно', 'неприятно', 'расстроен', 'тоскливо'],
            'stress': ['стресс', 'нервничаю', 'волнуюсь', 'тревожно', 'паника', 'переживаю'],
            'calm': ['спокойно', 'умиротворенно', 'расслабленно', 'тихо', 'мирно'],
            'focus': ['концентрируюсь', 'сосредоточен', 'внимателен', 'сфокусирован'],
            'energy': ['энергично', 'активно', 'бодро', 'динамично', 'живо']
        }
        
    def analyze_text_confidence(self, text: str) -> ConfidenceCalibration:
        """Анализирует уверенность на основе текстовых характеристик."""
        if not text or len(text.strip()) == 0:
            return ConfidenceCalibration()
        
        text_lower = text.lower()
        
        # 1. Text Quality Score - качество текста
        text_quality = self._calculate_text_quality(text)
        
        # 2. Emotional Clarity - ясность эмоциональных маркеров
        emotional_clarity = self._calculate_emotional_clarity(text_lower)
        
        # 3. Context Consistency - последовательность контекста
        context_consistency = self._calculate_context_consistency(text)
        
        # 4. Linguistic Confidence - лингвистическая уверенность
        linguistic_confidence = self._calculate_linguistic_confidence(text)
        
        # 5. Temporal Stability - временная стабильность (заглушка)
        temporal_stability = 0.7  # Базовое значение
        
        return ConfidenceCalibration(
            text_quality_score=text_quality,
            emotional_clarity=emotional_clarity,
            context_consistency=context_consistency,
            linguistic_confidence=linguistic_confidence,
            temporal_stability=temporal_stability
        )
    
    def _calculate_text_quality(self, text: str) -> float:
        """Вычисляет качество текста для анализа."""
        if len(text) < 3:
            return 0.1
        
        quality_score = 0.5  # Базовый балл
        
        # Длина текста (оптимум 10-200 символов)
        length = len(text)
        if 10 <= length <= 200:
            quality_score += 0.2
        elif length > 200:
            quality_score += 0.1
        
        # Наличие знаков препинания
        if any(punct in text for punct in ',.!?;:'):
            quality_score += 0.1
        
        # Количество слов
        word_count = len(text.split())
        if 3 <= word_count <= 50:
            quality_score += 0.2
        
        return min(quality_score, 1.0)
    
    def _calculate_emotional_clarity(self, text_lower: str) -> float:
        """Вычисляет ясность эмоциональных маркеров."""
        emotional_matches = 0
        total_keywords = 0
        
        for category, keywords in self.emotional_keywords.items():
            total_keywords += len(keywords)
            for keyword in keywords:
                if keyword in text_lower:
                    emotional_matches += 1
        
        if total_keywords == 0:
            return 0.5
        
        # Базовая ясность от количества найденных маркеров
        base_clarity = min(emotional_matches / 3.0, 1.0)  # До 3 маркеров = 100%
        
        # Бонус за интенсивность эмоций
        intensity_bonus = 0.0
        high_intensity_words = ['очень', 'крайне', 'невероятно', 'чрезвычайно', '!!!', 'ужасно', 'потрясающе']
        for word in high_intensity_words:
            if word in text_lower:
                intensity_bonus += 0.1
        
        return min(base_clarity + intensity_bonus, 1.0)
    
    def _calculate_context_consistency(self, text: str) -> float:
        """Вычисляет последовательность контекста."""
        # Простая эвристика: отсутствие противоречивых эмоций
        text_lower = text.lower()
        
        positive_count = 0
        negative_count = 0
        
        # Подсчет позитивных и негативных маркеров
        for keyword in self.emotional_keywords['high_positive'] + self.emotional_keywords['positive']:
            if keyword in text_lower:
                positive_count += 1
        
        for keyword in self.emotional_keywords['high_negative'] + self.emotional_keywords['negative']:
            if keyword in text_lower:
                negative_count += 1
        
        # Если есть и позитивные, и негативные маркеры - снижаем уверенность
        if positive_count > 0 and negative_count > 0:
            return 0.4  # Смешанные эмоции
        
        # Если есть ясное направление - повышаем уверенность
        if positive_count > 0 or negative_count > 0:
            return 0.8
        
        return 0.6  # Нейтральный контекст
    
    def _calculate_linguistic_confidence(self, text: str) -> float:
        """Вычисляет лингвистическую уверенность."""
        confidence = 0.5  # Базовое значение
        
        # Наличие вопросительных предложений снижает уверенность
        if '?' in text:
            confidence -= 0.1
        
        # Утвердительные конструкции повышают уверенность
        affirmative_patterns = ['я чувствую', 'я ощущаю', 'мне', 'я думаю', 'я уверен']
        for pattern in affirmative_patterns:
            if pattern in text.lower():
                confidence += 0.15
                break
        
        # Неопределенность снижает уверенность
        uncertainty_patterns = ['возможно', 'может быть', 'наверное', 'кажется', 'не знаю']
        for pattern in uncertainty_patterns:
            if pattern in text.lower():
                confidence -= 0.2
                break
        
        return max(0.1, min(confidence, 1.0))
    
    def calculate_overall_confidence(
        self, 
        text: str, 
        emotional_features: Optional[Dict] = None,
        previous_confidence: Optional[float] = None
    ) -> float:
        """Вычисляет общую уверенность предсказания."""
        
        # Анализ текстовых характеристик
        calibration = self.analyze_text_confidence(text)
        
        # Веса для разных компонентов
        weights = {
            'text_quality': 0.2,
            'emotional_clarity': 0.3,
            'context_consistency': 0.2,
            'linguistic_confidence': 0.2,
            'temporal_stability': 0.1
        }
        
        # Взвешенная сумма
        confidence = (
            calibration.text_quality_score * weights['text_quality'] +
            calibration.emotional_clarity * weights['emotional_clarity'] +
            calibration.context_consistency * weights['context_consistency'] +
            calibration.linguistic_confidence * weights['linguistic_confidence'] +
            calibration.temporal_stability * weights['temporal_stability']
        )
        
        # Корректировка на основе эмоциональных признаков
        if emotional_features:
            feature_confidence = self._assess_feature_confidence(emotional_features)
            confidence = (confidence + feature_confidence) / 2
        
        # Сглаживание с предыдущим значением
        if previous_confidence is not None:
            confidence = (confidence * 0.7) + (previous_confidence * 0.3)
        
        # Ensure minimum confidence for basic functionality
        confidence = max(0.3, min(confidence, 0.95))
        
        return confidence
    
    def _assess_feature_confidence(self, features: Dict) -> float:
        """Оценивает уверенность на основе эмоциональных признаков."""
        confidence = 0.5
        
        # Проверка валентности
        valence = features.get('valence', 0.5)
        if abs(valence - 0.5) > 0.2:  # Отклонение от нейтрального
            confidence += 0.2
        
        # Проверка возбуждения  
        arousal = features.get('arousal', 0.5)
        if abs(arousal - 0.5) > 0.2:
            confidence += 0.2
        
        # Проверка доминирования
        dominance = features.get('dominance', 0.5)
        if abs(dominance - 0.5) > 0.1:
            confidence += 0.1
        
        return min(confidence, 1.0)
    
    def get_confidence_explanation(self, calibration: ConfidenceCalibration) -> List[str]:
        """Возвращает объяснение уровня уверенности."""
        explanations = []
        
        if calibration.text_quality_score < 0.5:
            explanations.append("Низкое качество текста для анализа")
        elif calibration.text_quality_score > 0.8:
            explanations.append("Высокое качество текста")
        
        if calibration.emotional_clarity < 0.5:
            explanations.append("Нечеткие эмоциональные маркеры")
        elif calibration.emotional_clarity > 0.8:
            explanations.append("Ясные эмоциональные сигналы")
        
        if calibration.context_consistency < 0.5:
            explanations.append("Противоречивый или смешанный контекст")
        elif calibration.context_consistency > 0.8:
            explanations.append("Последовательный эмоциональный контекст")
        
        if calibration.linguistic_confidence < 0.5:
            explanations.append("Лингвистическая неопределенность")
        elif calibration.linguistic_confidence > 0.8:
            explanations.append("Четкие языковые паттерны")
        
        if not explanations:
            explanations.append("Стандартный уровень уверенности анализа")
        
        return explanations


# Глобальный экземпляр оптимизатора
confidence_optimizer = EmotimeConfidenceOptimizer()