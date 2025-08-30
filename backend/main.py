"""
Главный модуль LIMINAL WebSocket Gateway.
Обеспечивает real-time коммуникацию между компонентами системы.
"""

import asyncio
import contextlib
import json
import logging
import sys

import uvicorn

# JWT Authentication imports
from auth.jwt_utils import jwt_manager
from fastapi import FastAPI, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse

# Импорт модуля health check
from health import router as health_router
from loguru import logger

# Импорт нашего модуля метрик
from metrics import (
    setup_metrics,
    websocket_auth_total,
    websocket_connections,
)
from ml.anomaly_detector import anomaly_detector

# Импорт ML модуля
from ml.api import router as ml_router
from ml.feature_extractor import feature_extractor
from ml.metrics_exporter import ml_metrics_collector
from ml.metrics_exporter import router as ml_metrics_router
from redis_client import RedisClient
from websocket.connection_manager import ConnectionManager
from websocket.handlers import handle_message, register_handlers

# Создание Redis клиента
redis_client = RedisClient()

# Создание ConnectionManager
# Параметры rate limiting можно вынести в конфиг, но пока оставим значения по умолчанию
manager = ConnectionManager(
    redis_client=redis_client,
    rate_limit_messages_per_second=10,  # 10 сообщений в секунду
    rate_limit_burst=20,  # с возможностью всплеска до 20 сообщений
)

app = FastAPI(
    title="LIMINAL WebSocket Gateway",
    description="WebSocket gateway for LIMINAL backend",
    version="1.0.0",
)

# Настраиваем метрики Prometheus
setup_metrics(app)

# Делаем redis_client доступным для других модулей через app.state
app.state.redis_client = redis_client

# Подключаем модуль health check
app.include_router(health_router)

# Подключаем ML API
app.include_router(ml_router)

# Подключаем ML метрики
app.include_router(ml_metrics_router)

# Настройка CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # В продакшене заменить на конкретные домены
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Регистрируем обработчики WebSocket
register_handlers()


# WebSocket endpoint
@app.websocket("/ws/{client_id}")
async def websocket_endpoint(websocket: WebSocket, client_id: str):
    metrics = websocket.app.state.metrics

    # Принимаем соединение для начальной коммуникации
    await websocket.accept()
    websocket_connections.labels(channel="all", authenticated="false").inc()

    # Запрашиваем аутентификацию
    await websocket.send_json(
        {
            "type": "auth_required",
            "message": "Необходима аутентификация. Отправьте JWT токен.",
        }
    )

    try:
        # Получаем данные аутентификации
        auth_timeout = 30  # 30 секунд на аутентификацию
        authenticated = False
        user_id = None

        # Ожидаем аутентификацию
        try:
            auth_data = await asyncio.wait_for(websocket.receive_text(), timeout=auth_timeout)
            try:
                auth_message = json.loads(auth_data)
                if "token" in auth_message:
                    token = auth_message["token"]
                    # Проверяем JWT токен
                    user_id = jwt_manager.extract_user_id_from_token(token)
                    if user_id:
                        # Инкрементируем успешные аутентификации
                        websocket_auth_total.labels(status="success").inc()
                        authenticated = True
                        logger.info(
                            f"Пользователь {user_id} успешно аутентифицирован на /ws/{client_id}"
                        )
                        await websocket.send_json(
                            {
                                "type": "auth_success",
                                "message": f"Пользователь {user_id} успешно аутентифицирован",
                            }
                        )

                        # Отслеживаем событие аутентификации в ML
                        ml_metrics_collector.track_connection_event(user_id, "auth_success")
                        ml_metrics_collector.track_jwt_event(
                            user_id,
                            {
                                "event_type": "auth_success",
                                "source_ip": websocket.client.host,
                                "token_age": 0,  # Можно вычислить из JWT
                            },
                        )
                    else:
                        logger.warning(f"Неверный токен для WebSocket соединения: {client_id}")
                        # Отслеживаем неудачную аутентификацию
                        ml_metrics_collector.track_connection_event("unknown", "auth_failure")
                        await websocket.send_json(
                            {"type": "auth_error", "message": "Неверный токен"}
                        )
                        await websocket.close(code=1008, reason="Неверный токен")
                        return
                else:
                    logger.warning(f"В сообщении аутентификации отсутствует токен: {client_id}")
                    await websocket.send_json(
                        {
                            "type": "auth_error",
                            "message": "В сообщении отсутствует токен",
                        }
                    )
                    await websocket.close(code=1008, reason="Токен не предоставлен")
                    return
            except json.JSONDecodeError:
                logger.warning(f"Ошибка декодирования JSON при аутентификации: {client_id}")
                await websocket.send_json(
                    {"type": "auth_error", "message": "Ошибка декодирования JSON"}
                )
                await websocket.close(code=1008, reason="Некорректный формат JSON")
                return
        except TimeoutError:
            logger.warning(f"Таймаут аутентификации для WebSocket соединения: {client_id}")
            await websocket.send_json(
                {"type": "auth_error", "message": "Время аутентификации истекло"}
            )
            await websocket.close(code=1008, reason="Время аутентификации истекло")
            return

        # Только если аутентифицирован - подключаем к manager
        if authenticated:
            # Регистрируем подключение
            await manager.connect(websocket, user_id)

            # Отслеживаем подключение в ML метриках
            ml_metrics_collector.track_connection_event(user_id, "connect")

            # Основной цикл обработки сообщений
            while True:
                try:
                    # Ожидаем сообщение от клиента
                    data = await websocket.receive_text()

                    # Разбираем сообщение для определения типа (и для heartbeat bypass)
                    message_obj = json.loads(data)
                    msg_type = message_obj.get("type")

                    # Отмечаем активность на любое сообщение
                    with contextlib.suppress(Exception):
                        manager.mark_activity(websocket)

                    # Пропускаем rate limiting для heartbeat ответов
                    if msg_type == "pong":
                        with contextlib.suppress(Exception):
                            manager.mark_pong(websocket)
                        # Переходим к следующему циклу, обработка уже выполнена
                        continue

                    # 1. Проверка Rate Limit (c пер-IP и пер-соединение)
                    ip_address = websocket.client.host
                    if await manager.is_rate_limited(user_id, ip_address, websocket):
                        await websocket.send_json(
                            {
                                "type": "error",
                                "code": 429,
                                "message": "Too Many Requests. Please slow down.",
                            }
                        )
                        # Пропускаем дальнейшую обработку
                        continue

                    # 2. Обновление метрик и логирование
                    metrics["WS_MESSAGES"].labels(endpoint="/ws", type="received").inc()
                    logger.info(
                        f"Получено сообщение от {user_id}: {data[:256]}..."
                    )  # Логируем только часть сообщения

                    # 2.1. ML Feature Extraction и Anomaly Detection
                    try:
                        # Извлекаем фичи для ML-анализа
                        message_size = len(data.encode("utf-8"))
                        user_channels = list(manager.user_channels.get(user_id, set()))
                        ip_address = websocket.client.host

                        feature_extractor.track_user_activity(
                            user_id=user_id,
                            message_size=message_size,
                            channels=user_channels,
                            ip_address=ip_address,
                        )

                        # Отслеживаем в ML метриках коллекторе
                        ml_metrics_collector.track_request(
                            user_id=user_id,
                            ip_address=ip_address,
                            channel=user_channels[0] if user_channels else None,
                        )

                        # Периодически анализируем на аномалии (каждое 10-е сообщение)
                        if (
                            feature_extractor.user_sessions.get(user_id, {}).get("message_count", 0)
                            % 10
                            == 0
                        ):
                            anomalies = anomaly_detector.analyze_user_activity(user_id)
                            if anomalies:
                                critical_anomalies = [
                                    a for a in anomalies if a.severity in ["high", "critical"]
                                ]
                                if critical_anomalies:
                                    logger.warning(
                                        f"Обнаружены критические аномалии для {user_id}: {len(critical_anomalies)}"
                                    )
                    except Exception as e:
                        logger.error(f"Ошибка ML-анализа для {user_id}: {e}")

                    # 3. Обработка сообщения через централизованный handler
                    response = await handle_message(data, websocket, manager)

                    if response:
                        if isinstance(response, dict):
                            response = json.dumps(response)
                        await websocket.send_text(response)
                        metrics["WS_MESSAGES"].labels(endpoint="/ws", type="sent").inc()

                except json.JSONDecodeError:
                    logger.error(f"Ошибка декодирования JSON от {user_id}: {data}")
                    # Отслеживаем ошибку в ML-системе
                    feature_extractor.track_error(user_id)
                    await websocket.send_text(json.dumps({"error": "Invalid JSON format"}))
                except WebSocketDisconnect:
                    # Этот блок должен быть внутри цикла, чтобы корректно завершить его
                    logger.info(f"Пользователь {user_id} (клиент {client_id}) отключился.")
                    # Отслеживаем отключение
                    ml_metrics_collector.track_connection_event(user_id, "disconnect")
                    await manager.disconnect(websocket, user_id)
                    break  # Выход из цикла while True
                except Exception as e:
                    logger.error(f"WebSocket error: {str(e)}")
                    if authenticated:
                        await manager.disconnect(websocket, user_id)

    except Exception as e:
        logger.error(f"WebSocket error: {str(e)}")
        if authenticated:
            await manager.disconnect(websocket, user_id)


# --- Унификация логгеров ---
# Перехватываем все сообщения от loguru и направляем их в стандартный logging
# Это нужно, чтобы uvicorn --log-config корректно писал ВСЕ логи в файл
class InterceptHandler(logging.Handler):
    def emit(self, record):
        # Получаем соответствующий уровень loguru
        try:
            level = logger.level(record.levelname).name
        except ValueError:
            level = record.levelno

        # Находим фрейм, откуда был сделан вызов
        frame, depth = logging.currentframe(), 2
        while frame.f_code.co_filename == logging.__file__:
            frame = frame.f_back
            depth += 1

        logger.opt(depth=depth, exception=record.exc_info).log(level, record.getMessage())


# Убираем стандартные обработчики loguru и ставим наш перехватчик
logger.remove()
logger.add(sys.stderr, level="INFO")  # Оставляем вывод в консоль для loguru

# Настраиваем стандартный logging для работы с перехватчиком
logging.basicConfig(handlers=[InterceptHandler()], level=0)
# --- Конец унификации ---


# Health check
@app.get("/health")
async def health_check():
    return {"status": "ok"}


# Метрики
# Endpoint для метрик добавляется автоматически через setup_metrics


@app.on_event("startup")
async def startup_event():
    """Запускает все необходимые сервисы при старте приложения."""
    # 1. Проверка доступности Redis (синхронный ping)
    try:
        client = getattr(redis_client, "client", None)
        if client is None:
            raise RuntimeError("Redis client attribute is None")
        client.ping()
        logger.info("Успешное подключение к Redis.")
    except Exception as e:
        # Переходим в деградированный режим (DummyRedis уже настроен внутри RedisClient)
        logger.warning(f"Redis недоступен на старте: {e}. Продолжаем в degraded режиме.")

    # 2. Явное ожидание загрузки Lua-скрипта
    # Это гарантирует, что Rate Limiting будет работать с самого начала.
    # Метод _load_rate_limit_script запускается в __init__, мы даем ему время завершиться.
    for _ in range(10):  # Пробуем до 5 секунд
        if manager.rate_limit_script_sha:
            logger.info("Lua-скрипт для Rate Limiting успешно загружен и готов к работе.")
            break
        await asyncio.sleep(0.5)
    else:
        logger.error(
            "Не удалось подтвердить загрузку Lua-скрипта. Rate Limiting может быть неактивен."
        )

    logger.info("Приложение успешно стартовало. Метрики Prometheus доступны на /metrics")

    # 3. Флаг завершения старта для /health/startup
    app.state.startup_complete = True


# HTML страница для тестирования
@app.get("/", response_class=HTMLResponse)
async def get_root():
    return """
    <!DOCTYPE html>
    <html>
    <head>
        <title>LIMINAL WebSocket Test</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; max-width: 800px; }
            #messages {
                height: 300px;
                overflow-y: auto;
                border: 1px solid #ccc;
                padding: 10px;
                margin: 10px 0;
                background: #f9f9f9;
            }
            .message { margin: 5px 0; padding: 5px; border-bottom: 1px solid #eee; }
            .system { color: #666; }
            .sent { color: #2c7be5; }
            .received { color: #00a854; }
            .error { color: #ff4d4f; }
            button { padding: 8px 16px; margin: 5px; }
            input { padding: 8px; margin: 5px; width: 200px; }
        </style>
    </head>
    <body>
        <h1>WebSocket Test Client</h1>
        <div>
            <input type="text" id="userId" value="test_user_1" placeholder="User ID">
            <button id="connect">Connect</button>
            <button id="disconnect" disabled>Disconnect</button>
        </div>
        <div>
            <input type="text" id="channel" value="test_channel" placeholder="Channel">
            <button id="subscribe" disabled>Subscribe</button>
            <button id="unsubscribe" disabled>Unsubscribe</button>
        </div>
        <div>
            <input type="text" id="message" placeholder="Message">
            <button id="send" disabled>Send</button>
        </div>
        <div id="messages"></div>
        <script>
            const socket = new WebSocket(`ws://${window.location.host}/ws/test_user_1`);
            const messages = document.getElementById('messages');

            function addMessage(text, type = 'system') {
                const div = document.createElement('div');
                div.className = `message ${type}`;
                div.textContent = `[${new Date().toLocaleTimeString()}] ${text}`;
                messages.appendChild(div);
                messages.scrollTop = messages.scrollHeight;
            }

            // WebSocket event handlers
            socket.onopen = () => {
                addMessage('Connected to WebSocket server');
                document.getElementById('connect').disabled = true;
                document.getElementById('disconnect').disabled = false;
                document.getElementById('subscribe').disabled = false;
                document.getElementById('unsubscribe').disabled = false;
                document.getElementById('send').disabled = false;

                // Auto-subscribe to the test channel
                socket.send(JSON.stringify({
                    type: 'subscribe',
                    user_id: 'test_user_1',
                    channel: 'test_channel'
                }));
            };

            socket.onmessage = (event) => {
                try {
                    const data = JSON.parse(event.data);
                    addMessage(`Received: ${JSON.stringify(data)}`, 'received');
                } catch (e) {
                    addMessage(`Error: ${event.data}`, 'error');
                }
            };

            socket.onclose = () => {
                addMessage('Disconnected from WebSocket server');
                document.getElementById('connect').disabled = false;
                document.getElementById('disconnect').disabled = true;
                document.getElementById('subscribe').disabled = true;
                document.getElementById('unsubscribe').disabled = true;
                document.getElementById('send').disabled = true;
            };

            // Button event listeners
            document.getElementById('connect').addEventListener('click', () => {
                window.location.reload();
            });

            document.getElementById('disconnect').addEventListener('click', () => {
                socket.close();
            });

            document.getElementById('subscribe').addEventListener('click', () => {
                const channel = document.getElementById('channel').value || 'test_channel';
                const userId = document.getElementById('userId').value || 'test_user_1';
                socket.send(JSON.stringify({
                    type: 'subscribe',
                    user_id: userId,
                    channel: channel
                }));
            });

            document.getElementById('unsubscribe').addEventListener('click', () => {
                const channel = document.getElementById('channel').value || 'test_channel';
                const userId = document.getElementById('userId').value || 'test_user_1';
                socket.send(JSON.stringify({
                    type: 'unsubscribe',
                    user_id: userId,
                    channel: channel
                }));
            });

            document.getElementById('send').addEventListener('click', () => {
                const message = document.getElementById('message').value;
                const channel = document.getElementById('channel').value || 'test_channel';
                const userId = document.getElementById('userId').value || 'test_user_1';

                if (message) {
                    const msg = {
                        type: 'message',
                        user_id: userId,
                        channel: channel,
                        message: {
                            text: message,
                            timestamp: new Date().toISOString()
                        }
                    };
                    socket.send(JSON.stringify(msg));
                    addMessage(`Sent: ${message}`, 'sent');
                    document.getElementById('message').value = '';
                }
            });

            // Send message on Enter key
            document.getElementById('message').addEventListener('keypress', (e) => {
                if (e.key === 'Enter') {
                    document.getElementById('send').click();
                }
            });
        </script>
    </body>
    </html>
    """


if __name__ == "__main__":
    # Запуск сервера
    uvicorn.run("main:app", host="0.0.0.0", port=8000, reload=True, log_level="info")
