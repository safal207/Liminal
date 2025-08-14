"""
Обработчики WebSocket сообщений.
"""

import json
import uuid
from datetime import datetime
from typing import Any, Awaitable, Callable, Dict

from fastapi import WebSocket
from loguru import logger

# Тип для обработчиков сообщений
MessageHandler = Callable[
    [Dict[str, Any], WebSocket, "ConnectionManager"], Awaitable[None]
]

# Реестр обработчиков
HANDLERS: Dict[str, MessageHandler] = {}


def register_handler(message_type: str):
    """Декоратор для регистрации обработчиков сообщений."""

    def decorator(handler: MessageHandler):
        HANDLERS[message_type] = handler
        return handler

    return decorator


async def handle_message(
    message: str, websocket: WebSocket, manager: "ConnectionManager"
):
    """
    Обрабатывает входящее сообщение и вызывает соответствующий обработчик.

    Аргументы:
        message (str): JSON-строка с данными сообщения
        websocket (WebSocket): Объект WebSocket соединения
        manager (ConnectionManager): Менеджер соединений

    Возвращает:
        dict: Ответное сообщение или None
    """
    try:
        # Отмечаем любую активность от клиента
        try:
            manager.mark_activity(websocket)
        except Exception:
            # Не должен падать из-за учета активности
            pass
        # Парсим JSON сообщение
        data = json.loads(message)
        message_type = data.get("type")

        if not message_type:
            logger.warning("Получено сообщение без типа")
            return {"error": "Message type is required"}

        handler = HANDLERS.get(message_type)
        if handler:
            # Вызываем обработчик и возвращаем результат
            result = await handler(data, websocket, manager)
            return result or {"status": "success", "type": message_type}
        else:
            logger.warning(f"Неизвестный тип сообщения: {message_type}")
            return {"error": f"Unknown message type: {message_type}"}

    except json.JSONDecodeError as e:
        error_msg = f"Ошибка декодирования JSON: {str(e)}"
        logger.error(error_msg)
        return {"error": "Invalid JSON format", "details": str(e)}

    except Exception as e:
        error_msg = f"Ошибка обработки сообщения: {str(e)}"
        logger.error(error_msg)
        return {"error": "Internal server error", "details": str(e)}


# Регистрируем обработчики
@register_handler("subscribe")
async def handle_subscribe(
    data: Dict[str, Any], websocket: WebSocket, manager: "ConnectionManager"
):
    """
    Обработчик подписки на канал.

    Ожидаемые данные:
    {
        "type": "subscribe",
        "user_id": "user123",
        "channel": "channel_name"
    }
    """
    user_id = data.get("user_id")
    channel = data.get("channel")

    if not user_id or not channel:
        error_msg = (
            f"Недостаточно данных для подписки: user_id={user_id}, channel={channel}"
        )
        logger.warning(error_msg)
        return {"status": "error", "error": error_msg}

    try:
        await manager.subscribe(user_id, channel, websocket)
        logger.info(f"Пользователь {user_id} подписался на канал {channel}")
        return {
            "status": "success",
            "type": "subscribe",
            "channel": channel,
            "user_id": user_id,
        }
    except Exception as e:
        error_msg = (
            f"Ошибка при подписке пользователя {user_id} на канал {channel}: {e}"
        )
        logger.error(error_msg)
        return {"status": "error", "error": error_msg}


@register_handler("unsubscribe")
async def handle_unsubscribe(
    data: Dict[str, Any], websocket: WebSocket, manager: "ConnectionManager"
):
    """
    Обработчик отписки от канала.

    Ожидаемые данные:
    {
        "type": "unsubscribe",
        "user_id": "user123",
        "channel": "channel_name"
    }
    """
    user_id = data.get("user_id")
    channel = data.get("channel")

    if not user_id or not channel:
        error_msg = (
            f"Недостаточно данных для отписки: user_id={user_id}, channel={channel}"
        )
        logger.warning(error_msg)
        return {"status": "error", "error": error_msg}

    try:
        await manager.unsubscribe(user_id, channel)
        logger.info(f"Пользователь {user_id} отписался от канала {channel}")
        return {
            "status": "success",
            "type": "unsubscribe",
            "channel": channel,
            "user_id": user_id,
        }
    except Exception as e:
        error_msg = (
            f"Ошибка при отписке пользователя {user_id} от канала {channel}: {e}"
        )
        logger.error(error_msg)
        return {"status": "error", "error": error_msg}


@register_handler("message")
async def handle_message_event(
    data: Dict[str, Any], websocket: WebSocket, manager: "ConnectionManager"
):
    """
    Обработчик отправки сообщения в канал.

    Ожидаемые данные:
    {
        "type": "message",
        "user_id": "user123",
        "channel": "channel_name",
        "message": {
            "text": "Hello, world!",
            "timestamp": "2023-01-01T12:00:00Z"
        }
    }
    """
    user_id = data.get("user_id")
    channel = data.get("channel")
    message = data.get("message")

    if not user_id or not channel or not message:
        error_msg = f"Недостаточно данных для отправки сообщения: user_id={user_id}, channel={channel}"
        logger.warning(error_msg)
        return {"status": "error", "error": error_msg}

    try:
        # Проверяем, подписан ли пользователь на канал
        if not manager.is_user_subscribed(user_id, channel):
            error_msg = f"Пользователь {user_id} не подписан на канал {channel}"
            logger.warning(error_msg)
            return {"status": "error", "error": error_msg}

        # Формируем сообщение для рассылки
        message_data = {
            "type": "message",
            "channel": channel,
            "sender": user_id,
            "message": message,
            "timestamp": datetime.utcnow().isoformat(),
        }

        # Отправляем сообщение всем подписчикам канала
        sent_count = await manager.broadcast(channel, message_data, sender_id=user_id)
        logger.info(
            f"Пользователь {user_id} отправил сообщение в канал {channel}, доставлено {sent_count} подписчикам"
        )

        return {
            "status": "success",
            "type": "message_sent",
            "channel": channel,
            "message_id": str(uuid.uuid4()),
            "delivered_to": sent_count,
        }

    except Exception as e:
        error_msg = f"Ошибка при отправке сообщения в канал {channel}: {str(e)}"
        logger.error(error_msg)
        return {"status": "error", "error": error_msg}


def register_handlers():
    """Импортирует все обработчики, чтобы они зарегистрировались."""
    # Просто импортируем этот модуль, чтобы декораторы сработали
    pass


# Heartbeat: обработчик ответа pong от клиента
@register_handler("pong")
async def handle_pong(
    data: Dict[str, Any], websocket: WebSocket, manager: "ConnectionManager"
):
    """
    Клиент отвечает на ping сообщением {"type": "pong"}.
    Мы отмечаем получение pong для обновления heartbeat.
    """
    try:
        manager.mark_pong(websocket)
    except Exception as e:
        logger.warning(f"Не удалось отметить pong: {e}")
    return {"status": "success", "type": "pong"}
