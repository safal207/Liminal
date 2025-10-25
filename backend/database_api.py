#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
API эндпоинты для работы с DatabaseAdapter.

Предоставляет REST API для сохранения и запроса данных
через универсальный адаптер Datomic + Neo4j.
"""

from typing import Any, Dict, List, Optional
from fastapi import APIRouter, Depends, HTTPException, Request
from pydantic import BaseModel, Field
from database_adapter import DatabaseAdapter, DataType
import logging

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/database", tags=["database"])


# Pydantic модели для API
class DataStoreRequest(BaseModel):
    """Запрос на сохранение данных."""
    data: Dict[str, Any] = Field(..., description="Данные для сохранения")
    data_type: str = Field(..., description="Тип данных (emotion_history, philosophy, etc.)")
    user_id: Optional[str] = Field(None, description="ID пользователя")
    session_id: Optional[str] = Field(None, description="ID сессии")


class DataQueryRequest(BaseModel):
    """Запрос на получение данных."""
    data_type: str = Field(..., description="Тип данных для запроса")
    filters: Optional[Dict[str, Any]] = Field(None, description="Фильтры для запроса")
    limit: int = Field(100, description="Максимальное количество записей")


class DataStoreResponse(BaseModel):
    """Ответ на сохранение данных."""
    success: bool
    record_id: Optional[str] = None
    message: str
    stored_in: Optional[str] = None


class DataQueryResponse(BaseModel):
    """Ответ на запрос данных."""
    success: bool
    data: List[Dict[str, Any]]
    count: int
    message: str
    queried_from: Optional[str] = None


class HealthResponse(BaseModel):
    """Ответ на проверку здоровья БД."""
    status: str
    databases: Dict[str, Any]
    stats: Dict[str, int]
    fallback_enabled: bool


def get_database_adapter(request: Request) -> DatabaseAdapter:
    """Получение DatabaseAdapter из app.state."""
    return request.app.state.database_adapter


@router.post("/store", response_model=DataStoreResponse)
async def store_data(
    request: DataStoreRequest,
    adapter: DatabaseAdapter = Depends(get_database_adapter)
):
    """
    Сохранение данных в подходящую БД.
    
    Автоматически выбирает Datomic для временных данных,
    Neo4j для структурных связей.
    """
    try:
        # Преобразуем строку в enum
        try:
            data_type = DataType(request.data_type)
        except ValueError:
            raise HTTPException(
                status_code=400,
                detail=f"Неподдерживаемый тип данных: {request.data_type}. "
                       f"Доступные типы: {[dt.value for dt in DataType]}"
            )
        
        # Сохраняем данные
        record_id = await adapter.store_data(
            data=request.data,
            data_type=data_type,
            user_id=request.user_id,
            session_id=request.session_id
        )
        
        # Определяем, в какую БД сохранили
        preferred_db = adapter._choose_database(data_type)
        stored_in = adapter._get_available_database(preferred_db)
        
        return DataStoreResponse(
            success=True,
            record_id=record_id,
            message=f"Данные успешно сохранены (тип: {data_type.value})",
            stored_in=stored_in
        )
        
    except Exception as e:
        logger.error(f"Ошибка сохранения данных: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Ошибка сохранения данных: {str(e)}"
        )


@router.post("/query", response_model=DataQueryResponse)
async def query_data(
    request: DataQueryRequest,
    adapter: DatabaseAdapter = Depends(get_database_adapter)
):
    """
    Запрос данных из подходящей БД.
    
    Автоматически выбирает БД на основе типа данных.
    """
    try:
        # Преобразуем строку в enum
        try:
            data_type = DataType(request.data_type)
        except ValueError:
            raise HTTPException(
                status_code=400,
                detail=f"Неподдерживаемый тип данных: {request.data_type}. "
                       f"Доступные типы: {[dt.value for dt in DataType]}"
            )
        
        # Запрашиваем данные
        data = await adapter.query_data(
            data_type=data_type,
            filters=request.filters,
            limit=request.limit
        )
        
        # Определяем, из какой БД запрашивали
        preferred_db = adapter._choose_database(data_type)
        queried_from = adapter._get_available_database(preferred_db)
        
        return DataQueryResponse(
            success=True,
            data=data,
            count=len(data),
            message=f"Найдено записей: {len(data)} (тип: {data_type.value})",
            queried_from=queried_from
        )
        
    except Exception as e:
        logger.error(f"Ошибка запроса данных: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Ошибка запроса данных: {str(e)}"
        )


@router.get("/health", response_model=HealthResponse)
async def get_database_health(
    adapter: DatabaseAdapter = Depends(get_database_adapter)
):
    """
    Проверка состояния баз данных.
    
    Возвращает статус подключений к Datomic и Neo4j,
    статистику использования и настройки fallback.
    """
    try:
        health_status = adapter.get_health_status()
        
        return HealthResponse(
            status=health_status["status"],
            databases=health_status["databases"],
            stats=health_status["stats"],
            fallback_enabled=health_status["fallback_enabled"]
        )
        
    except Exception as e:
        logger.error(f"Ошибка получения статуса БД: {e}")
        raise HTTPException(
            status_code=500,
            detail=f"Ошибка получения статуса БД: {str(e)}"
        )


@router.get("/data-types")
async def get_supported_data_types():
    """
    Получение списка поддерживаемых типов данных.
    
    Показывает, какие типы данных поддерживаются
    и в какую БД они направляются.
    """
    data_types = {}
    
    for data_type in DataType:
        # Создаем временный адаптер для определения БД
        temp_adapter = DatabaseAdapter(auto_connect=False)
        preferred_db = temp_adapter._choose_database(data_type)
        
        data_types[data_type.value] = {
            "name": data_type.value,
            "preferred_database": preferred_db,
            "description": _get_data_type_description(data_type)
        }
    
    return {
        "supported_types": data_types,
        "routing_rules": {
            "datomic": "Временные данные, история, аудит, события",
            "neo4j": "Структурные связи, граф отношений, философские состояния"
        }
    }


def _get_data_type_description(data_type: DataType) -> str:
    """Получение описания типа данных."""
    descriptions = {
        DataType.TEMPORAL: "История изменений во времени",
        DataType.EVENT: "События системы",
        DataType.AUDIT: "Аудит действий пользователей",
        DataType.EMOTION_HISTORY: "История эмоциональных состояний",
        DataType.SESSION_DATA: "Данные пользовательских сессий",
        DataType.RELATIONSHIP: "Связи между сущностями",
        DataType.GRAPH: "Графовые структуры",
        DataType.PHILOSOPHY: "Философские состояния сознания",
        DataType.CONCEPT_MAP: "Карты концептов и идей",
        DataType.USER_NETWORK: "Сети и связи пользователей"
    }
    
    return descriptions.get(data_type, "Описание недоступно")


# Примеры использования API для документации
@router.get("/examples")
async def get_api_examples():
    """
    Примеры использования Database API.
    
    Показывает типичные запросы для разных типов данных.
    """
    return {
        "store_emotion": {
            "method": "POST",
            "url": "/api/database/store",
            "body": {
                "data": {
                    "emotion": "радость",
                    "intensity": 0.8,
                    "context": "успешное завершение задачи"
                },
                "data_type": "emotion_history",
                "user_id": "user-123",
                "session_id": "session-456"
            }
        },
        "store_philosophy": {
            "method": "POST",
            "url": "/api/database/store",
            "body": {
                "data": {
                    "state": "resonance",
                    "depth": 0.9,
                    "clarity": 0.7,
                    "transition_from": "questioning"
                },
                "data_type": "philosophy",
                "user_id": "user-123"
            }
        },
        "query_emotions": {
            "method": "POST",
            "url": "/api/database/query",
            "body": {
                "data_type": "emotion_history",
                "filters": {"user_id": "user-123"},
                "limit": 10
            }
        },
        "health_check": {
            "method": "GET",
            "url": "/api/database/health"
        }
    }
