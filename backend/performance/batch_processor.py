"""
Batch Processing Optimizations
Пакетная обработка для повышения пропускной способности.
"""

import asyncio
from typing import List, Dict, Any, Optional
from datetime import datetime
from dataclasses import dataclass

@dataclass
class BatchRequest:
    """Запрос в батче."""
    id: str
    data: Dict[str, Any]
    timestamp: datetime
    future: asyncio.Future


class BatchProcessor:
    """Процессор пакетной обработки."""
    
    def __init__(self, batch_size: int = 10, batch_timeout: float = 1.0):
        self.batch_size = batch_size
        self.batch_timeout = batch_timeout
        self.pending_requests: List[BatchRequest] = []
        self._processing_lock = asyncio.Lock()
        self._batch_task: Optional[asyncio.Task] = None
    
    async def add_request(self, request_id: str, data: Dict[str, Any]) -> Any:
        """Добавляет запрос в батч и ожидает результат."""
        future = asyncio.Future()
        
        request = BatchRequest(
            id=request_id,
            data=data,
            timestamp=datetime.now(),
            future=future
        )
        
        async with self._processing_lock:
            self.pending_requests.append(request)
            
            # Запускаем обработку батча если достигли лимита
            if len(self.pending_requests) >= self.batch_size:
                await self._process_batch()
            elif self._batch_task is None:
                # Запускаем таймер для обработки по timeout
                self._batch_task = asyncio.create_task(self._batch_timeout_handler())
        
        # Ожидаем результат
        return await future
    
    async def _batch_timeout_handler(self):
        """Обработчик timeout для батча."""
        await asyncio.sleep(self.batch_timeout)
        
        async with self._processing_lock:
            if self.pending_requests:
                await self._process_batch()
    
    async def _process_batch(self):
        """Обрабатывает текущий батч запросов."""
        if not self.pending_requests:
            return
        
        batch_to_process = self.pending_requests.copy()
        self.pending_requests.clear()
        
        if self._batch_task:
            self._batch_task.cancel()
            self._batch_task = None
        
        try:
            # Обработка всего батча одновременно
            results = await self._process_batch_data(batch_to_process)
            
            # Возвращаем результаты каждому запросу
            for request, result in zip(batch_to_process, results):
                if not request.future.done():
                    request.future.set_result(result)
                    
        except Exception as e:
            # В случае ошибки возвращаем ошибку всем запросам
            for request in batch_to_process:
                if not request.future.done():
                    request.future.set_exception(e)
    
    async def _process_batch_data(self, batch: List[BatchRequest]) -> List[Any]:
        """Обрабатывает данные батча. Переопределяется в наследниках."""
        # Заглушка - в реальности здесь будет обработка Emotime
        results = []
        for request in batch:
            # Симуляция обработки
            await asyncio.sleep(0.1)
            results.append({
                "id": request.id,
                "processed_at": datetime.now().isoformat(),
                "data": request.data
            })
        return results


class EmotimeBatchProcessor(BatchProcessor):
    """Специализированный батч процессор для Emotime."""
    
    def __init__(self, emotime_engine=None, batch_size: int = 5, batch_timeout: float = 0.5):
        super().__init__(batch_size, batch_timeout)
        self.emotime_engine = emotime_engine
    
    async def _process_batch_data(self, batch: List[BatchRequest]) -> List[Any]:
        """Обрабатывает батч эмоциональных данных."""
        if not self.emotime_engine:
            # Fallback без батчевой обработки
            return await super()._process_batch_data(batch)
        
        results = []
        
        # Группируем по пользователям для более эффективной обработки
        user_groups = {}
        for request in batch:
            user_id = request.data.get("user_id", "default")
            if user_id not in user_groups:
                user_groups[user_id] = []
            user_groups[user_id].append(request)
        
        # Обрабатываем каждую группу
        for user_id, user_requests in user_groups.items():
            for request in user_requests:
                try:
                    # Здесь будет реальная обработка через Emotime
                    result = await self._process_single_emotime_request(request)
                    results.append(result)
                except Exception as e:
                    results.append({"error": str(e), "id": request.id})
        
        return results
    
    async def _process_single_emotime_request(self, request: BatchRequest) -> Dict[str, Any]:
        """Обрабатывает одиночный Emotime запрос с оптимизированной уверенностью."""
        try:
            # Импортируем внутри функции для избежания циклических импортов
            from ..emotime.sensors import TextSensor
            from ..emotime.core import EmotimeEngine
            from ..emotime.confidence_optimizer import confidence_optimizer
            from ..emotime.ml_accuracy_optimizer import enhanced_classifier
            
            text = request.data.get("text", "")
            user_id = request.data.get("user_id", "default")
            
            if not text:
                return {
                    "id": request.id,
                    "error": "Empty text input",
                    "confidence": 0.0,
                    "processed_at": datetime.now().isoformat()
                }
            
            # Создаем временный Emotime engine для обработки
            temp_engine = EmotimeEngine(user_id=user_id, enable_neo4j=False)
            
            # Создаем текстовый сенсор и обрабатываем
            text_sensor = TextSensor()
            metadata = {}
            if request.data.get("typing_speed"):
                metadata["typing_speed"] = request.data["typing_speed"]
                
            sensor_data = await text_sensor.process(text, metadata)
            await temp_engine.process_sensor_data(sensor_data)
            
            # Получаем текущее состояние
            current_state = await temp_engine.get_current_state()
            
            # ML Accuracy Enhancement: использовать улучшенный классификатор
            ml_prediction = enhanced_classifier.predict_emotional_dimensions(text)
            
            if current_state:
                # Объединяем результаты классического Emotime и улучшенного ML
                emotime_features = {
                    "valence": current_state.features.valence,
                    "arousal": current_state.features.arousal,
                    "dominance": current_state.features.dominance,
                    "tempo": current_state.features.tempo,
                    "intensity": current_state.features.intensity
                }
                
                # Смешиваем предсказания для повышения точности
                enhanced_features = {}
                blend_weight = 0.6  # 60% новый ML, 40% классический
                
                for dim in ['valence', 'arousal', 'dominance']:
                    ml_value = ml_prediction.dimensions.get(dim, 0.5)
                    emotime_value = emotime_features.get(dim, 0.5)
                    enhanced_features[dim] = (ml_value * blend_weight + emotime_value * (1 - blend_weight))
                
                # Сохраняем темп и интенсивность из Emotime
                enhanced_features['tempo'] = emotime_features['tempo']
                enhanced_features['intensity'] = emotime_features['intensity']
                
                # Используем оптимизированный confidence calculator
                optimized_confidence = confidence_optimizer.calculate_overall_confidence(
                    text=text,
                    emotional_features=enhanced_features,
                    previous_confidence=current_state.confidence
                )
                
                # Учитываем ML accuracy в итоговом confidence
                final_confidence = (optimized_confidence + ml_prediction.confidence) / 2
                
                # Получаем объяснение уверенности
                calibration = confidence_optimizer.analyze_text_confidence(text)
                confidence_explanation = confidence_optimizer.get_confidence_explanation(calibration)
                
                return {
                    "id": request.id,
                    "emotional_features": enhanced_features,  # Используем улучшенные признаки
                    "confidence": final_confidence,  # Используем итоговую уверенность
                    "confidence_breakdown": {
                        "text_quality": calibration.text_quality_score,
                        "emotional_clarity": calibration.emotional_clarity,
                        "context_consistency": calibration.context_consistency,
                        "linguistic_confidence": calibration.linguistic_confidence,
                        "explanations": confidence_explanation
                    },
                    "ml_enhancement": {
                        "ml_accuracy_score": ml_prediction.accuracy_score,
                        "ml_confidence": ml_prediction.confidence,
                        "ml_model": ml_prediction.model_used,
                        "feature_blend_weight": blend_weight,
                        "original_emotime": emotime_features,
                        "ml_prediction": ml_prediction.dimensions
                    },
                    "mode": {
                        "name": current_state.mode.name,
                        "type": current_state.mode.type.value
                    },
                    "processed_at": datetime.now().isoformat(),
                    "optimization": "ml_accuracy_enhanced"
                }
            else:
                # Fallback с базовой оптимизацией confidence
                base_confidence = confidence_optimizer.calculate_overall_confidence(text=text)
                calibration = confidence_optimizer.analyze_text_confidence(text)
                
                return {
                    "id": request.id,
                    "emotional_features": {
                        "valence": 0.5,
                        "arousal": 0.5,
                        "dominance": 0.5,
                        "tempo": 0.5,
                        "intensity": 0.5
                    },
                    "confidence": base_confidence,
                    "confidence_breakdown": {
                        "text_quality": calibration.text_quality_score,
                        "emotional_clarity": calibration.emotional_clarity,
                        "context_consistency": calibration.context_consistency,
                        "linguistic_confidence": calibration.linguistic_confidence,
                        "explanations": confidence_optimizer.get_confidence_explanation(calibration)
                    },
                    "processed_at": datetime.now().isoformat(),
                    "note": "fallback_with_confidence_optimization"
                }
                
        except Exception as e:
            # В случае ошибки все равно пытаемся оценить confidence по тексту
            try:
                from ..emotime.confidence_optimizer import confidence_optimizer
                fallback_confidence = confidence_optimizer.calculate_overall_confidence(
                    text=request.data.get("text", "")
                )
            except:
                fallback_confidence = 0.2
                
            return {
                "id": request.id,
                "emotional_features": {
                    "valence": 0.5,
                    "arousal": 0.5,
                    "dominance": 0.5,
                    "tempo": 0.5,
                    "intensity": 0.5
                },
                "confidence": fallback_confidence,
                "error": str(e),
                "processed_at": datetime.now().isoformat(),
                "note": "error_with_fallback_confidence"
            }


# Глобальный батч процессор
emotime_batch_processor = EmotimeBatchProcessor()