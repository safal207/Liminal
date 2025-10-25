"""
ML Accuracy Optimization for Emotime
Оптимизация точности машинного обучения для эмоционального анализа.
"""

import numpy as np
from typing import Dict, List, Tuple, Optional, Any
from datetime import datetime
from dataclasses import dataclass
from enum import Enum

class EmotionalDimension(Enum):
    """Эмоциональные измерения по модели VAD + дополнительные."""
    VALENCE = "valence"        # Позитивность/негативность
    AROUSAL = "arousal"        # Возбуждение/спокойствие  
    DOMINANCE = "dominance"    # Контроль/подчинение
    TEMPO = "tempo"            # Темп/динамика
    INTENSITY = "intensity"    # Интенсивность

@dataclass
class MLPrediction:
    """Результат ML предсказания."""
    dimensions: Dict[str, float]
    confidence: float
    model_used: str
    features_extracted: Dict[str, Any]
    accuracy_score: float

class EnhancedEmotionalClassifier:
    """Улучшенный классификатор эмоций с повышенной точностью."""
    
    def __init__(self):
        self.emotional_patterns = self._build_enhanced_patterns()
        self.accuracy_feedback = []
        self.calibration_weights = {
            'lexical': 0.3,
            'syntactic': 0.2,
            'semantic': 0.25,
            'contextual': 0.15,
            'temporal': 0.1
        }
        
    def _build_enhanced_patterns(self) -> Dict[str, Dict]:
        """Создает расширенную базу эмоциональных паттернов."""
        return {
            'high_positive': {
                'keywords': [
                    'восторг', 'счастье', 'радость', 'экстаз', 'блаженство',
                    'ликование', 'эйфория', 'отлично', 'превосходно', 'великолепно',
                    'прекрасно', 'замечательно', 'потрясающе', 'фантастично', 'чудесно'
                ],
                'patterns': [
                    r'\b(очень|крайне|невероятно)\s+(счастлив|рад|доволен)',
                    r'\b(в восторге|на седьмом небе|переполнен радостью)',
                    r'[!]{2,}', # Множественные восклицательные знаки
                ],
                'valence': 0.9,
                'arousal': 0.8,
                'dominance': 0.7,
                'confidence_weight': 1.2
            },
            'moderate_positive': {
                'keywords': [
                    'хорошо', 'приятно', 'нравится', 'удовольствие', 'симпатично',
                    'позитивно', 'оптимистично', 'воодушевляет', 'радует', 'улыбаюсь'
                ],
                'valence': 0.7,
                'arousal': 0.6,
                'dominance': 0.6,
                'confidence_weight': 1.0
            },
            'high_negative': {
                'keywords': [
                    'ужас', 'кошмар', 'катастрофа', 'трагедия', 'беда',
                    'ненависть', 'отвращение', 'презрение', 'гнев', 'ярость',
                    'бешенство', 'злость', 'раздражение', 'досада'
                ],
                'patterns': [
                    r'\b(очень|крайне|невероятно)\s+(плохо|ужасно|отвратительно)',
                    r'\b(в ярости|вне себя|не выношу)',
                ],
                'valence': 0.1,
                'arousal': 0.8,
                'dominance': 0.3,
                'confidence_weight': 1.2
            },
            'moderate_negative': {
                'keywords': [
                    'плохо', 'грустно', 'печально', 'тоскливо', 'уныло',
                    'расстроен', 'огорчен', 'разочарован', 'недоволен'
                ],
                'valence': 0.3,
                'arousal': 0.4,
                'dominance': 0.4,
                'confidence_weight': 1.0
            },
            'stress_anxiety': {
                'keywords': [
                    'стресс', 'тревога', 'беспокойство', 'волнение', 'нервозность',
                    'паника', 'страх', 'ужас', 'боязнь', 'опасение',
                    'переживание', 'напряжение', 'суета'
                ],
                'valence': 0.2,
                'arousal': 0.9,
                'dominance': 0.2,
                'confidence_weight': 1.1
            },
            'calm_peaceful': {
                'keywords': [
                    'спокойствие', 'умиротворение', 'расслабление', 'тишина',
                    'гармония', 'баланс', 'равновесие', 'медитация',
                    'созерцание', 'безмятежность'
                ],
                'valence': 0.6,
                'arousal': 0.2,
                'dominance': 0.7,
                'confidence_weight': 1.0
            },
            'energy_excitement': {
                'keywords': [
                    'энергия', 'возбуждение', 'азарт', 'воодушевление',
                    'динамика', 'активность', 'живость', 'бодрость',
                    'энтузиазм', 'вдохновение'
                ],
                'valence': 0.7,
                'arousal': 0.9,
                'dominance': 0.8,
                'confidence_weight': 1.1
            },
            'focus_concentration': {
                'keywords': [
                    'концентрация', 'сосредоточенность', 'внимание', 'фокус',
                    'погружение', 'медитативность', 'созерцательность'
                ],
                'valence': 0.6,
                'arousal': 0.5,
                'dominance': 0.8,
                'confidence_weight': 1.0
            }
        }
    
    def extract_enhanced_features(self, text: str) -> Dict[str, Any]:
        """Извлекает расширенные признаки из текста."""
        text_lower = text.lower()
        features = {
            'lexical': self._extract_lexical_features(text, text_lower),
            'syntactic': self._extract_syntactic_features(text),
            'semantic': self._extract_semantic_features(text_lower),
            'contextual': self._extract_contextual_features(text),
            'temporal': self._extract_temporal_features(text)
        }
        return features
    
    def _extract_lexical_features(self, text: str, text_lower: str) -> Dict[str, Any]:
        """Извлекает лексические признаки."""
        return {
            'length': len(text),
            'word_count': len(text.split()),
            'punctuation_intensity': text.count('!') + text.count('?') * 0.5,
            'capitalization_ratio': sum(1 for c in text if c.isupper()) / len(text) if text else 0,
            'exclamation_ratio': text.count('!') / len(text.split()) if text.split() else 0,
            'emotional_intensifiers': sum(1 for word in ['очень', 'крайне', 'невероятно', 'чрезвычайно'] if word in text_lower)
        }
    
    def _extract_syntactic_features(self, text: str) -> Dict[str, Any]:
        """Извлекает синтаксические признаки."""
        sentences = text.split('.')
        return {
            'sentence_count': len([s for s in sentences if s.strip()]),
            'avg_sentence_length': np.mean([len(s.split()) for s in sentences if s.strip()]) if sentences else 0,
            'question_ratio': text.count('?') / len(sentences) if sentences else 0,
            'complex_sentence_indicators': sum(1 for word in ['но', 'однако', 'хотя', 'несмотря'] if word in text.lower())
        }
    
    def _extract_semantic_features(self, text_lower: str) -> Dict[str, Any]:
        """Извлекает семантические признаки."""
        pattern_matches = {}
        total_confidence = 0
        max_confidence = 0
        
        for pattern_name, pattern_data in self.emotional_patterns.items():
            matches = 0
            for keyword in pattern_data['keywords']:
                if keyword in text_lower:
                    matches += 1
            
            pattern_matches[pattern_name] = matches
            confidence = matches * pattern_data.get('confidence_weight', 1.0)
            total_confidence += confidence
            max_confidence = max(max_confidence, confidence)
        
        return {
            'pattern_matches': pattern_matches,
            'total_emotional_intensity': total_confidence,
            'max_pattern_confidence': max_confidence,
            'emotional_diversity': len([k for k, v in pattern_matches.items() if v > 0])
        }
    
    def _extract_contextual_features(self, text: str) -> Dict[str, Any]:
        """Извлекает контекстуальные признаки."""
        text_lower = text.lower()
        return {
            'first_person_indicators': sum(1 for word in ['я', 'мне', 'мой', 'моя', 'мое'] if word in text_lower.split()),
            'certainty_indicators': sum(1 for phrase in ['я уверен', 'точно знаю', 'определенно'] if phrase in text_lower),
            'uncertainty_indicators': sum(1 for phrase in ['возможно', 'может быть', 'не знаю', 'кажется'] if phrase in text_lower),
            'temporal_references': sum(1 for word in ['сейчас', 'сегодня', 'вчера', 'завтра'] if word in text_lower)
        }
    
    def _extract_temporal_features(self, text: str) -> Dict[str, Any]:
        """Извлекает временные признаки."""
        return {
            'current_time': datetime.now().hour,  # Время дня может влиять на эмоции
            'text_creation_speed_estimate': min(len(text) / 10, 1.0)  # Примерная оценка скорости печати
        }
    
    def predict_emotional_dimensions(self, text: str) -> MLPrediction:
        """Предсказывает эмоциональные измерения с повышенной точностью."""
        if not text or len(text.strip()) == 0:
            return MLPrediction(
                dimensions={d.value: 0.5 for d in EmotionalDimension},
                confidence=0.1,
                model_used="fallback",
                features_extracted={},
                accuracy_score=0.2
            )
        
        # Извлекаем признаки
        features = self.extract_enhanced_features(text)
        
        # Применяем многоуровневую классификацию
        dimensions = self._multi_level_classification(text, features)
        
        # Вычисляем confidence на основе качества признаков
        confidence = self._calculate_prediction_confidence(features)
        
        # Оцениваем точность предсказания
        accuracy = self._estimate_prediction_accuracy(features, dimensions)
        
        return MLPrediction(
            dimensions=dimensions,
            confidence=confidence,
            model_used="enhanced_multilevel",
            features_extracted=features,
            accuracy_score=accuracy
        )
    
    def _multi_level_classification(self, text: str, features: Dict[str, Any]) -> Dict[str, float]:
        """Многоуровневая классификация эмоций."""
        text_lower = text.lower()
        
        # Уровень 1: Лексический анализ
        lexical_scores = self._lexical_classification(text_lower, features['lexical'])
        
        # Уровень 2: Семантический анализ
        semantic_scores = self._semantic_classification(features['semantic'])
        
        # Уровень 3: Контекстуальный анализ
        contextual_scores = self._contextual_classification(features['contextual'])
        
        # Уровень 4: Синтаксический анализ
        syntactic_scores = self._syntactic_classification(features['syntactic'])
        
        # Взвешенное объединение результатов
        final_dimensions = {}
        for dim in EmotionalDimension:
            final_dimensions[dim.value] = (
                lexical_scores.get(dim.value, 0.5) * self.calibration_weights['lexical'] +
                semantic_scores.get(dim.value, 0.5) * self.calibration_weights['semantic'] +
                contextual_scores.get(dim.value, 0.5) * self.calibration_weights['contextual'] +
                syntactic_scores.get(dim.value, 0.5) * self.calibration_weights['syntactic']
            )
            
            # Нормализация в диапазон [0, 1]
            final_dimensions[dim.value] = np.clip(final_dimensions[dim.value], 0.0, 1.0)
        
        return final_dimensions
    
    def _lexical_classification(self, text_lower: str, lexical_features: Dict) -> Dict[str, float]:
        """Лексическая классификация."""
        dimensions = {d.value: 0.5 for d in EmotionalDimension}
        
        # Анализируем эмоциональные паттерны
        for pattern_name, pattern_data in self.emotional_patterns.items():
            matches = sum(1 for keyword in pattern_data['keywords'] if keyword in text_lower)
            
            if matches > 0:
                weight = min(matches / len(pattern_data['keywords']), 1.0)
                dimensions['valence'] = (dimensions['valence'] + pattern_data['valence'] * weight) / 2
                dimensions['arousal'] = (dimensions['arousal'] + pattern_data['arousal'] * weight) / 2
                dimensions['dominance'] = (dimensions['dominance'] + pattern_data['dominance'] * weight) / 2
        
        # Учитываем интенсификаторы
        if lexical_features['emotional_intensifiers'] > 0:
            for dim in ['valence', 'arousal']:
                if dimensions[dim] > 0.5:
                    dimensions[dim] = min(dimensions[dim] * 1.2, 1.0)
                else:
                    dimensions[dim] = max(dimensions[dim] * 0.8, 0.0)
        
        return dimensions
    
    def _semantic_classification(self, semantic_features: Dict) -> Dict[str, float]:
        """Семантическая классификация."""
        dimensions = {d.value: 0.5 for d in EmotionalDimension}
        
        pattern_matches = semantic_features['pattern_matches']
        total_intensity = semantic_features['total_emotional_intensity']
        
        if total_intensity > 0:
            # Вычисляем взвешенные значения на основе совпадений паттернов
            weighted_valence = 0
            weighted_arousal = 0
            weighted_dominance = 0
            total_weight = 0
            
            for pattern_name, matches in pattern_matches.items():
                if matches > 0:
                    pattern_data = self.emotional_patterns[pattern_name]
                    weight = matches * pattern_data.get('confidence_weight', 1.0)
                    
                    weighted_valence += pattern_data['valence'] * weight
                    weighted_arousal += pattern_data['arousal'] * weight
                    weighted_dominance += pattern_data['dominance'] * weight
                    total_weight += weight
            
            if total_weight > 0:
                dimensions['valence'] = weighted_valence / total_weight
                dimensions['arousal'] = weighted_arousal / total_weight
                dimensions['dominance'] = weighted_dominance / total_weight
        
        return dimensions
    
    def _contextual_classification(self, contextual_features: Dict) -> Dict[str, float]:
        """Контекстуальная классификация."""
        dimensions = {d.value: 0.5 for d in EmotionalDimension}
        
        # Первое лицо указывает на личный опыт (выше dominance)
        if contextual_features['first_person_indicators'] > 0:
            dimensions['dominance'] = min(dimensions['dominance'] + 0.1, 1.0)
        
        # Уверенность повышает dominance
        if contextual_features['certainty_indicators'] > 0:
            dimensions['dominance'] = min(dimensions['dominance'] + 0.15, 1.0)
        
        # Неуверенность снижает dominance
        if contextual_features['uncertainty_indicators'] > 0:
            dimensions['dominance'] = max(dimensions['dominance'] - 0.15, 0.0)
        
        return dimensions
    
    def _syntactic_classification(self, syntactic_features: Dict) -> Dict[str, float]:
        """Синтаксическая классификация."""
        dimensions = {d.value: 0.5 for d in EmotionalDimension}
        
        # Вопросы могут указывать на неуверенность
        if syntactic_features['question_ratio'] > 0.5:
            dimensions['dominance'] = max(dimensions['dominance'] - 0.1, 0.0)
        
        # Сложные предложения могут указывать на размышления
        if syntactic_features['complex_sentence_indicators'] > 0:
            dimensions['arousal'] = max(dimensions['arousal'] - 0.1, 0.0)
            dimensions['dominance'] = min(dimensions['dominance'] + 0.05, 1.0)
        
        return dimensions
    
    def _calculate_prediction_confidence(self, features: Dict[str, Any]) -> float:
        """Вычисляет уверенность предсказания."""
        confidence = 0.5  # Базовое значение
        
        # Лексические признаки
        if features['lexical']['word_count'] > 3:
            confidence += 0.1
        if features['lexical']['emotional_intensifiers'] > 0:
            confidence += 0.15
        
        # Семантические признаки
        if features['semantic']['total_emotional_intensity'] > 0:
            confidence += min(features['semantic']['total_emotional_intensity'] / 10.0, 0.2)
        
        # Контекстуальные признаки
        if features['contextual']['first_person_indicators'] > 0:
            confidence += 0.1
        if features['contextual']['certainty_indicators'] > 0:
            confidence += 0.1
        elif features['contextual']['uncertainty_indicators'] > 0:
            confidence -= 0.1
        
        return np.clip(confidence, 0.1, 0.95)
    
    def _estimate_prediction_accuracy(self, features: Dict[str, Any], dimensions: Dict[str, float]) -> float:
        """Оценивает точность предсказания."""
        accuracy = 0.6  # Базовая точность
        
        # Качество входных данных
        text_quality = min(features['lexical']['word_count'] / 10.0, 1.0)
        accuracy += text_quality * 0.2
        
        # Качество эмоциональных признаков
        emotional_quality = min(features['semantic']['total_emotional_intensity'] / 5.0, 1.0)
        accuracy += emotional_quality * 0.2
        
        # Консистентность размерностей
        dimension_consistency = self._calculate_dimension_consistency(dimensions)
        accuracy += dimension_consistency * 0.1
        
        return np.clip(accuracy, 0.3, 0.95)
    
    def _calculate_dimension_consistency(self, dimensions: Dict[str, float]) -> float:
        """Вычисляет консистентность эмоциональных размерностей."""
        # Проверяем логическую согласованность размерностей
        valence = dimensions['valence']
        arousal = dimensions['arousal']
        dominance = dimensions['dominance']
        
        # Высокое возбуждение + низкая валентность часто = стресс (логично)
        # Низкое возбуждение + высокая валентность часто = спокойствие (логично)
        
        consistency = 1.0
        
        # Проверяем экстремальные несоответствия
        if valence > 0.8 and arousal > 0.8 and dominance < 0.3:
            consistency -= 0.2  # Высокая радость но низкий контроль - подозрительно
        
        if valence < 0.2 and arousal < 0.2:
            consistency -= 0.1  # Очень низкие все параметры - подозрительно
        
        return max(consistency, 0.0)


# Глобальный экземпляр классификатора
enhanced_classifier = EnhancedEmotionalClassifier()