"""Модуль обнаружения аномалий на основе машинного обучения."""
import numpy as np
import pandas as pd
from sklearn.ensemble import IsolationForest
from sklearn.preprocessing import StandardScaler
import joblib
from typing import Dict, List, Optional, Tuple
from datetime import datetime, timedelta
import logging
from dataclasses import dataclass
import json

from backend.security_logging import security_logger

@dataclass
class SecurityEvent:
    """Класс для представления события безопасности."""
    timestamp: str
    source_ip: str
    event_type: str
    payload: Dict
    severity: int = 1

class AnomalyDetector:
    """Детектор аномалий на основе Isolation Forest."""
    
    def __init__(self, model_path: Optional[str] = None):
        """Инициализация детектора аномалий."""
        self.logger = logging.getLogger("anomaly_detector")
        self.scaler = StandardScaler()
        if model_path:
            self.model = joblib.load(model_path)
        else:
            self.model = IsolationForest(
                contamination=0.1,
                random_state=42
            )
        self.features = [
            'requests_per_minute',
            'unique_paths',
            'error_rate',
            'avg_payload_size',
            'unique_user_agents'
        ]
        
    def extract_features(self, events: List[SecurityEvent]) -> pd.DataFrame:
        """Извлечение признаков из событий безопасности."""
        features = []
        
        # Группируем события по минутам
        events_by_minute = {}
        for event in events:
            timestamp = datetime.fromisoformat(event.timestamp)
            minute_key = timestamp.replace(second=0, microsecond=0)
            if minute_key not in events_by_minute:
                events_by_minute[minute_key] = []
            events_by_minute[minute_key].append(event)
        
        # Извлекаем признаки для каждой минуты
        for minute, minute_events in events_by_minute.items():
            # Считаем количество запросов
            requests_per_minute = len(minute_events)
            
            # Уникальные пути
            paths = set()
            user_agents = set()
            total_payload_size = 0
            error_count = 0
            
            for event in minute_events:
                if 'path' in event.payload:
                    paths.add(event.payload['path'])
                if 'user_agent' in event.payload:
                    user_agents.add(event.payload['user_agent'])
                if 'payload_size' in event.payload:
                    total_payload_size += event.payload['payload_size']
                if 'status_code' in event.payload and event.payload['status_code'] >= 400:
                    error_count += 1
            
            avg_payload_size = total_payload_size / len(minute_events) if minute_events else 0
            error_rate = error_count / len(minute_events) if minute_events else 0
            
            features.append({
                'timestamp': minute,
                'requests_per_minute': requests_per_minute,
                'unique_paths': len(paths),
                'error_rate': error_rate,
                'avg_payload_size': avg_payload_size,
                'unique_user_agents': len(user_agents)
            })
        
        return pd.DataFrame(features)
    
    def train(self, events: List[SecurityEvent]):
        """Обучение модели на исторических данных."""
        df = self.extract_features(events)
        X = df[self.features]
        X_scaled = self.scaler.fit_transform(X)
        self.model.fit(X_scaled)
        
    def predict(self, events: List[SecurityEvent]) -> List[bool]:
        """Определение аномалий в новых событиях."""
        df = self.extract_features(events)
        X = df[self.features]
        X_scaled = self.scaler.transform(X)
        predictions = self.model.predict(X_scaled)
        return predictions == -1  # True для аномалий
    
    def save_model(self, path: str):
        """Сохранение модели."""
        joblib.dump(self.model, path)
        joblib.dump(self.scaler, path + '.scaler')
    
    def load_model(self, path: str):
        """Загрузка модели."""
        self.model = joblib.load(path)
        self.scaler = joblib.load(path + '.scaler')

class SecurityMonitor:
    """Монитор безопасности с обнаружением аномалий."""
    
    def __init__(self, model_path: Optional[str] = None):
        """Инициализация монитора безопасности."""
        self.detector = AnomalyDetector(model_path)
        self.events_buffer: List[SecurityEvent] = []
        self.window_size = timedelta(minutes=5)
        self.logger = logging.getLogger("security_monitor")
    
    def add_event(self, event: SecurityEvent):
        """Добавление нового события безопасности."""
        self.events_buffer.append(event)
        self._cleanup_old_events()
        
        # Проверяем наличие аномалий каждые 60 секунд
        if len(self.events_buffer) >= 60:
            self._check_anomalies()
    
    def _cleanup_old_events(self):
        """Очистка устаревших событий."""
        current_time = datetime.now()
        self.events_buffer = [
            event for event in self.events_buffer
            if current_time - datetime.fromisoformat(event.timestamp) <= self.window_size
        ]
    
    def _check_anomalies(self):
        """Проверка наличия аномалий."""
        anomalies = self.detector.predict(self.events_buffer)
        if any(anomalies):
            # Находим временные метки с аномалиями
            df = self.detector.extract_features(self.events_buffer)
            anomaly_times = df[anomalies]['timestamp'].tolist()
            
            for timestamp in anomaly_times:
                # Находим все события в этот момент времени
                related_events = [
                    event for event in self.events_buffer
                    if datetime.fromisoformat(event.timestamp).replace(second=0, microsecond=0) == timestamp
                ]
                
                # Создаем подробный отчет
                report = {
                    'timestamp': timestamp.isoformat(),
                    'num_events': len(related_events),
                    'unique_ips': len(set(event.source_ip for event in related_events)),
                    'event_types': list(set(event.event_type for event in related_events)),
                    'events': [
                        {
                            'timestamp': event.timestamp,
                            'source_ip': event.source_ip,
                            'event_type': event.event_type,
                            'severity': event.severity
                        }
                        for event in related_events
                    ]
                }
                
                # Логируем аномалию
                security_logger.log_suspicious_activity(
                    message=f"Detected anomaly at {timestamp}",
                    ip_address="multiple",
                    service="anomaly_detection",
                    details=json.dumps(report)
                )
                
                self.logger.warning(f"Anomaly detected! Report: {json.dumps(report, indent=2)}")

def main():
    """Основная функция для тестирования."""
    # Настройка логирования
    logging.basicConfig(level=logging.INFO)
    
    # Создание монитора безопасности
    monitor = SecurityMonitor()
    
    # Создание тестовых событий
    test_events = [
        SecurityEvent(
            timestamp=datetime.now().isoformat(),
            source_ip="192.168.1.1",
            event_type="http_request",
            payload={
                'path': '/api/test',
                'method': 'GET',
                'user_agent': 'Mozilla/5.0',
                'payload_size': 1024,
                'status_code': 200
            }
        )
        for _ in range(100)
    ]
    
    # Обучение на тестовых данных
    monitor.detector.train(test_events)
    
    # Тестирование обнаружения аномалий
    for event in test_events:
        monitor.add_event(event)

if __name__ == "__main__":
    main()
