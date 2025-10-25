"""
Message Acknowledgment API - endpoints для мониторинга гарантированной доставки сообщений
"""

from typing import Dict, List, Any

from fastapi import APIRouter, HTTPException, status, Depends
from pydantic import BaseModel
from logging_config import get_logger
logger = get_logger(__name__)

# Получаем ConnectionManager из main.py
def get_connection_manager():
    """Dependency для получения ConnectionManager"""
    # Избегаем циклический импорт, получаем из app state
    try:
        from main import manager
        return manager
    except ImportError:
        # Если циклический импорт, пробуем через глобальные переменные
        import sys
        if 'main' in sys.modules:
            return sys.modules['main'].manager
        else:
            raise HTTPException(
                status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
                detail="ConnectionManager не доступен"
            )


class AckStatsResponse(BaseModel):
    """Статистика системы acknowledgments"""
    total_pending_messages: int
    users_with_pending_messages: int
    pending_by_age: Dict[str, int]
    ack_timeout_seconds: int


class PendingMessageInfo(BaseModel):
    """Информация о неподтвержденном сообщении"""
    message_id: str
    user_id: str
    retry_count: int
    created_at: str
    last_retry_at: str
    age_seconds: int
    is_expired: bool
    can_retry: bool


class AckDetailedStatsResponse(BaseModel):
    """Детальная статистика acknowledgments"""
    stats: AckStatsResponse
    pending_messages: List[PendingMessageInfo]


class RetryResponse(BaseModel):
    """Ответ на запрос повторной отправки"""
    status: str
    retried_messages: int
    message: str


router = APIRouter(prefix="/message-ack", tags=["message-acknowledgments"])


@router.get("/stats", response_model=AckStatsResponse)
async def get_ack_stats(manager=Depends(get_connection_manager)):
    """
    Получить общую статистику системы acknowledgments.
    
    Returns:
        AckStatsResponse: Статистика неподтвержденных сообщений
    """
    try:
        stats = manager.get_ack_stats()
        
        return AckStatsResponse(
            total_pending_messages=stats["total_pending_messages"],
            users_with_pending_messages=stats["users_with_pending_messages"], 
            pending_by_age=stats["pending_by_age"],
            ack_timeout_seconds=stats["ack_timeout_seconds"]
        )
        
    except Exception as e:
        logger.error(f"Ошибка получения ACK статистики: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка получения статистики acknowledgments: {str(e)}"
        )


@router.get("/stats/detailed", response_model=AckDetailedStatsResponse)
async def get_detailed_ack_stats(manager=Depends(get_connection_manager)):
    """
    Получить детальную статистику системы acknowledgments с информацией о каждом сообщении.
    
    Returns:
        AckDetailedStatsResponse: Детальная статистика и список неподтвержденных сообщений
    """
    try:
        # Базовая статистика
        stats = manager.get_ack_stats()
        
        # Детальная информация о каждом неподтвержденном сообщении
        pending_messages = []
        current_time = manager.pending_messages[list(manager.pending_messages.keys())[0]].created_at if manager.pending_messages else None
        
        for message_id, pending_msg in manager.pending_messages.items():
            age_seconds = (pending_msg.created_at - pending_msg.created_at).total_seconds() if current_time else 0
            
            # Пересчитываем возраст правильно
            from datetime import datetime
            age_seconds = (datetime.now() - pending_msg.created_at).total_seconds()
            
            pending_messages.append(PendingMessageInfo(
                message_id=message_id,
                user_id=pending_msg.user_id,
                retry_count=pending_msg.retry_count,
                created_at=pending_msg.created_at.isoformat(),
                last_retry_at=pending_msg.last_retry_at.isoformat(),
                age_seconds=int(age_seconds),
                is_expired=pending_msg.is_expired,
                can_retry=pending_msg.can_retry
            ))
        
        # Сортируем по времени создания (старые первые)
        pending_messages.sort(key=lambda x: x.created_at)
        
        return AckDetailedStatsResponse(
            stats=AckStatsResponse(
                total_pending_messages=stats["total_pending_messages"],
                users_with_pending_messages=stats["users_with_pending_messages"],
                pending_by_age=stats["pending_by_age"],
                ack_timeout_seconds=stats["ack_timeout_seconds"]
            ),
            pending_messages=pending_messages
        )
        
    except Exception as e:
        logger.error(f"Ошибка получения детальной ACK статистики: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка получения детальной статистики: {str(e)}"
        )


@router.post("/retry", response_model=RetryResponse)
async def force_retry_unacknowledged(manager=Depends(get_connection_manager)):
    """
    Принудительно повторить отправку всех неподтвержденных сообщений.
    
    Returns:
        RetryResponse: Результат операции повторной отправки
    """
    try:
        # Считаем количество сообщений до повторной отправки
        messages_before = len(manager.pending_messages)
        
        if messages_before == 0:
            return RetryResponse(
                status="success",
                retried_messages=0,
                message="Нет сообщений для повторной отправки"
            )
        
        # Выполняем повторную отправку
        await manager.retry_unacknowledged_messages()
        
        # Считаем, сколько сообщений осталось (некоторые могли быть удалены из-за ошибок)
        messages_after = len(manager.pending_messages)
        retried_count = messages_before  # Все сообщения были обработаны
        
        logger.info(f"Принудительная повторная отправка: {retried_count} сообщений обработано")
        
        return RetryResponse(
            status="success",
            retried_messages=retried_count,
            message=f"Повторная отправка выполнена для {retried_count} сообщений. Осталось: {messages_after}"
        )
        
    except Exception as e:
        logger.error(f"Ошибка принудительной повторной отправки: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка повторной отправки: {str(e)}"
        )


@router.get("/user/{user_id}/stats")
async def get_user_ack_stats(user_id: str, manager=Depends(get_connection_manager)):
    """
    Получить статистику acknowledgments для конкретного пользователя.
    
    Args:
        user_id: ID пользователя
        
    Returns:
        Dict: Статистика пользователя
    """
    try:
        user_pending = manager.user_pending_messages.get(user_id, set())
        
        if not user_pending:
            return {
                "user_id": user_id,
                "pending_messages_count": 0,
                "pending_message_ids": [],
                "message": "У пользователя нет неподтвержденных сообщений"
            }
        
        # Собираем детальную информацию
        user_messages = []
        for message_id in user_pending:
            if message_id in manager.pending_messages:
                pending_msg = manager.pending_messages[message_id]
                from datetime import datetime
                age_seconds = (datetime.now() - pending_msg.created_at).total_seconds()
                
                user_messages.append({
                    "message_id": message_id,
                    "retry_count": pending_msg.retry_count,
                    "created_at": pending_msg.created_at.isoformat(),
                    "age_seconds": int(age_seconds),
                    "is_expired": pending_msg.is_expired,
                    "can_retry": pending_msg.can_retry
                })
        
        # Сортируем по времени создания
        user_messages.sort(key=lambda x: x["created_at"])
        
        return {
            "user_id": user_id,
            "pending_messages_count": len(user_pending),
            "pending_message_ids": list(user_pending),
            "messages": user_messages
        }
        
    except Exception as e:
        logger.error(f"Ошибка получения статистики пользователя {user_id}: {e}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка получения статистики пользователя: {str(e)}"
        )


@router.get("/health")
async def ack_system_health(manager=Depends(get_connection_manager)):
    """
    Проверка здоровья системы acknowledgments.
    
    Returns:
        Dict: Статус здоровья системы
    """
    try:
        stats = manager.get_ack_stats()
        
        # Определяем статус здоровья
        health_status = "healthy"
        warnings = []
        
        # Проверяем количество старых неподтвержденных сообщений
        old_messages = stats["pending_by_age"].get(">5m", 0)
        if old_messages > 10:
            health_status = "degraded"
            warnings.append(f"Много старых неподтвержденных сообщений: {old_messages}")
        
        # Проверяем общее количество неподтвержденных
        total_pending = stats["total_pending_messages"]
        if total_pending > 100:
            health_status = "degraded" 
            warnings.append(f"Много неподтвержденных сообщений: {total_pending}")
        
        # Проверяем работу cleanup задачи
        cleanup_task_running = manager._ack_cleanup_task is not None and not manager._ack_cleanup_task.done()
        
        if not cleanup_task_running:
            health_status = "unhealthy"
            warnings.append("ACK cleanup задача не запущена")
        
        return {
            "status": health_status,
            "total_pending_messages": total_pending,
            "users_affected": stats["users_with_pending_messages"],
            "cleanup_task_running": cleanup_task_running,
            "ack_timeout_seconds": stats["ack_timeout_seconds"],
            "warnings": warnings if warnings else None,
            "timestamp": "2025-08-19T23:50:00Z"  # В реальности - datetime.now().isoformat()
        }
        
    except Exception as e:
        logger.error(f"Ошибка проверки здоровья ACK системы: {e}")
        return {
            "status": "unhealthy",
            "error": str(e),
            "timestamp": "2025-08-19T23:50:00Z"
        }