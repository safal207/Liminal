print("DEBUG: Starting api.py imports")
import asyncio
import json
import logging
import os
import sys
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

print("DEBUG: Importing FastAPI and dependencies")
from fastapi import (
    Depends,
    FastAPI,
    HTTPException,
    WebSocket,
    WebSocketDisconnect,
    status,
)
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field

# Добавляем корневую директорию в PYTHONPATH для корректного импорта
print(f"DEBUG: Current sys.path: {sys.path}")
sys.path.append(str(Path(__file__).parent))
print(f"DEBUG: Updated sys.path: {sys.path}")

# Условный импорт Neo4jWriter и Neo4jDateTime
print("DEBUG: Before Neo4j imports")
print(f"DEBUG: TESTING={os.environ.get('TESTING')}")

if os.environ.get("TESTING"):
    print("API: Running in TESTING mode. Using mock Neo4jWriter.")

    # Mock Neo4j classes for testing
    class Neo4jDateTime:
        def isoformat(self):
            return datetime.now().isoformat() + "Z"

    class Neo4jWriter:
        def __init__(self, *args, **kwargs):
            print("API: Mock Neo4jWriter initialized.")
            self.driver = self

        def session(self):
            return self

        def __enter__(self):
            return self

        def __exit__(self, exc_type, exc_val, exc_tb):
            pass

        def run(self, *args, **kwargs):
            return MockResult()

        def create_indexes(self):
            pass

        def close(self):
            pass

        def create_dunewave_node(self, data):
            return {"id": "mock_wave_id", "type": "DuneWave", **data}

        def create_memory_fragment_node(self, data):
            return {"id": "mock_mem_id", "type": "MemoryFragment", **data}

        def link_dunewave_to_memory(self, wave_id, memory_id):
            return True

        def create_mentorship(self, younger_id, mentor_id):
            return True

        def find_wisdom_fragments(self, emotion=None, limit=10):
            return []

    class MockResult:
        def single(self):
            return None

        def __iter__(self):
            return iter([])

else:
    print("API: Running in PRODUCTION mode. Using real Neo4jWriter.")
    # Добавляем родительскую директорию в путь для импорта neo4j_writer
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    from neo4j_writer import Neo4jDateTime, Neo4jWriter


# Функция для сериализации объектов Neo4j
def neo4j_serializer(obj):
    if isinstance(obj, Neo4jDateTime):
        return obj.isoformat()
    raise TypeError(f"Type {type(obj)} not serializable")


# Функция для преобразования записи Neo4j в словарь
def record_to_dict(record):
    result = {}
    if hasattr(record, "items"):
        for key, value in record.items():
            if isinstance(value, (list, dict)):
                result[key] = json.loads(json.dumps(value, default=neo4j_serializer))
            else:
                result[key] = value
    elif hasattr(record, "isoformat"):
        return record.isoformat()
    else:
        return record
    return result


# Импорт метрик Prometheus
print("DEBUG: Before metrics import")
try:
    from metrics import (
        setup_metrics,
        websocket_auth_total,
        websocket_connections,
        websocket_messages_total,
    )

    print("DEBUG: Successfully imported metrics")
except ImportError as e:
    print(f"ERROR: Failed to import metrics: {e}")
    raise

print("DEBUG: Before memory_timeline import")
try:
    from memory_timeline import MemoryTimeline
    from memory_timeline import timeline as memory_timeline

    print("DEBUG: Successfully imported memory_timeline")
except ImportError as e:
    print(f"ERROR: Failed to import memory_timeline: {e}")
    raise

print("DEBUG: Before ml_features import")
try:
    from ml_features import (
        extract_traffic_features,
        extract_user_patterns,
        get_current_anomaly_scores,
        get_prediction_features,
        register_auth_event,
        register_channel_activity,
        register_ip_address,
        register_user_request,
    )

    # Проверка, включена ли ML-функциональность
    ML_ENABLED = os.environ.get("ML_ENABLED", "false").lower() == "true"
    print(f"DEBUG: Successfully imported ml_features. ML_ENABLED={ML_ENABLED}")
except ImportError as e:
    print(f"ERROR: Failed to import ml_features: {e}")
    # Fallback функции для случая отсутствия ML-модуля
    ML_ENABLED = False

    def extract_user_patterns():
        return {"enabled": False, "error": "ML features not available"}

    def extract_traffic_features():
        return {"enabled": False, "error": "ML features not available"}

    def get_current_anomaly_scores():
        return {"enabled": False, "error": "ML features not available"}

    def get_prediction_features():
        return {"enabled": False, "error": "ML features not available"}

    def register_ip_address(ip):
        pass

    def register_user_request(user_id):
        pass

    def register_channel_activity(channel):
        pass

    def register_auth_event(user_id, success):
        pass


print("DEBUG: Before connection_manager import")
try:
    # Проверяем, нужно ли использовать Redis для масштабирования
    use_redis = os.environ.get("USE_REDIS", "false").lower() == "true"
    if use_redis:
        from backend.websocket.redis_connection_manager import RedisConnectionManager

        redis_url = os.environ.get("REDIS_URL", "redis://localhost:6379/0")
        connection_manager = RedisConnectionManager(
            redis_url=redis_url,
            max_connections=100,
            max_connections_per_ip=10,
            redis_prefix="liminal",
        )
        print(f"DEBUG: Using Redis ConnectionManager with URL: {redis_url}")
    else:
        from backend.websocket.connection_manager import ConnectionManager

        connection_manager = ConnectionManager(
            max_connections=100, max_connections_per_ip=10
        )
        print("DEBUG: Using standard ConnectionManager")
    print("DEBUG: Successfully imported and initialized connection_manager")
except ImportError as e:
    print(f"ERROR: Failed to import connection_manager: {e}")
    raise

# Импорты для аутентификации
print("DEBUG: Before auth imports")
try:
    from backend.auth.jwt_utils import (
        authenticate_user,
        create_access_token_for_user,
        jwt_manager,
        verify_websocket_token,
    )
    from backend.auth.models import Token, UserLogin, WebSocketAuthMessage

    print("DEBUG: Successfully imported auth modules")
except ImportError as e:
    print(f"ERROR: Failed to import auth modules: {e}")
    raise

print("DEBUG: Creating FastAPI app")
app = FastAPI(
    title="LIMINAL API", description="API для работы с графовой базой данных LIMINAL"
)

# Инициализация логгера
logger = logging.getLogger(__name__)


# События для инициализации и завершения работы приложения
@app.on_event("startup")
async def startup_event():
    """Инициализация сервисов при запуске приложения"""
    logger.info("Starting application")

    # Метрики уже инициализированы во время импортов,
    # поэтому здесь только логируем это
    logger.info("Prometheus metrics already initialized")

    # Инициализация менеджера соединений (Redis или стандартного)
    if hasattr(connection_manager, "initialize"):
        initialized = await connection_manager.initialize()
        # Логируем в зависимости от типа менеджера для ясности
        if "redis" in connection_manager.__class__.__name__.lower():
            logger.info(f"Redis connection manager initialized: {initialized}")
        else:
            logger.info(f"Standard connection manager initialized: {initialized}")


@app.on_event("shutdown")
async def shutdown_event():
    """Корректное завершение работы при остановке приложения"""
    logger.info("Shutting down application")

    # Закрытие Redis соединения, если оно было открыто
    if os.environ.get("USE_REDIS", "false").lower() == "true":
        if hasattr(connection_manager, "shutdown"):
            await connection_manager.shutdown()
            logger.info("Redis connection closed")


# Инициализация временной шкалы памяти
print("DEBUG: Initializing MemoryTimeline")
timeline = MemoryTimeline()
print("DEBUG: MemoryTimeline initialized")

# Обслуживание статических файлов
import os

static_dir = os.path.join(os.path.dirname(__file__), "static")
app.mount("/static", StaticFiles(directory=static_dir), name="static")

# Настройка CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Настройка метрик Prometheus
print("DEBUG: Setting up Prometheus metrics")
try:
    setup_metrics(app)
    print("DEBUG: Prometheus metrics initialized at /metrics endpoint")
except Exception as e:
    print(f"ERROR: Failed to setup Prometheus metrics: {e}")
    logger.error(f"Failed to setup Prometheus metrics: {e}")
    # Продолжаем работу даже при ошибке настройки метрик


# Модели Pydantic
class DuneWaveCreate(BaseModel):
    phase: str
    emotion: str
    intensity: float
    context: str
    source: str

    class Config:
        json_encoders = {Neo4jDateTime: lambda v: v.isoformat() if v else None}


class MemoryFragmentCreate(BaseModel):
    content: str
    type: str
    growth_stage: str

    class Config:
        json_encoders = {Neo4jDateTime: lambda v: v.isoformat() if v else None}


class MentorshipCreate(BaseModel):
    younger_id: str
    mentor_id: str


class MemoryCreate(BaseModel):
    """Модель для создания нового воспоминания."""

    content: str = Field(..., description="Содержимое воспоминания")
    memory_type: str = Field(
        ..., description="Тип воспоминания (например, 'personal', 'work')"
    )
    metadata: Optional[Dict[str, Any]] = Field(
        default_factory=dict, description="Дополнительные метаданные"
    )


# Ленивая инициализация подключения к Neo4j
_neo4j_writer = None


def get_neo4j_writer():
    """Возвращает экземпляр Neo4jWriter с ленивой инициализацией."""
    global _neo4j_writer

    if _neo4j_writer is None:
        try:
            # В тестовом режиме используем мок
            if os.environ.get("TESTING"):
                _neo4j_writer = Neo4jWriter()
            else:
                # В продакшене используем реальное подключение
                _neo4j_writer = Neo4jWriter(
                    uri=os.getenv("NEO4J_URI", "bolt://localhost:7687"),
                    user=os.getenv("NEO4J_USER", "neo4j"),
                    password=os.getenv("NEO4J_PASSWORD", "password"),
                )
        except Exception as e:
            print(f"Ошибка при подключении к Neo4j: {e}")
            # Возвращаем мок в случае ошибки
            _neo4j_writer = Neo4jWriter()

    return _neo4j_writer


# Эндпоинты API
@app.get("/", response_model=dict)
async def root() -> dict:
    """Корневой эндпоинт для проверки работоспособности API."""
    return {"message": "Welcome to LIMINAL API"}


# Эндпоинты аутентификации
@app.post("/auth/login", response_model=Token)
async def login(user_data: UserLogin):
    """Аутентификация пользователя и получение JWT токена."""
    user = authenticate_user(user_data.username, user_data.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Неверные учетные данные",
            headers={"WWW-Authenticate": "Bearer"},
        )

    access_token = create_access_token_for_user(user)
    return {"access_token": access_token, "token_type": "bearer"}


@app.get("/auth/me")
async def get_current_user(token: str):
    """Получение информации о текущем пользователе по токену."""
    payload = jwt_manager.verify_token(token)
    if not payload:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Недействительный токен",
            headers={"WWW-Authenticate": "Bearer"},
        )

    return {
        "user_id": payload.get("sub"),
        "username": payload.get("username"),
        "message": "Токен действителен",
    }


# Endpoint для аутентификации и получения JWT токена
@app.post("/token", response_model=Token)
async def login_for_access_token(form_data: UserLogin):
    """
    Аутентифицирует пользователя и возвращает JWT токен.
    """
    user = authenticate_user(form_data.username, form_data.password)
    if not user:
        logger.warning(f"Failed login attempt for user: {form_data.username}")
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password",
            headers={"WWW-Authenticate": "Bearer"},
        )

    access_token = create_access_token_for_user(user)
    logger.info(f"Token issued for user: {form_data.username}")
    return {"access_token": access_token, "token_type": "bearer"}


# WebSocket endpoint с JWT аутентификацией
@app.websocket("/ws/timeline")
async def websocket_timeline(websocket: WebSocket, token: str = None):
    # Принимаем соединение и помещаем в ожидание аутентификации
    connection_accepted = await connection_manager.accept_pending_connection(websocket)
    if not connection_accepted:
        return  # Подключение отклонено из-за лимитов

    authenticated = False
    user_id = None

    try:
        # Проверяем, передан ли токен в URL
        if token:
            user_id = verify_websocket_token(token)
            if user_id:
                authenticated = await connection_manager.authenticate_connection(
                    websocket, user_id
                )
                if authenticated:
                    await websocket.send_json(
                        {
                            "type": "auth_success",
                            "message": f"Пользователь {user_id} успешно аутентифицирован через URL токен",
                        }
                    )

        if not authenticated:
            # Если аутентификация через URL не удалась, запрашиваем токен через сообщение
            await websocket.send_json(
                {
                    "type": "auth_required",
                    "message": "Необходима аутентификация. Отправьте JWT токен.",
                }
            )

            # Ожидаем сообщение с токеном
            auth_data = await websocket.receive_text()
        else:
            # Если уже аутентифицированы через URL, переходим к приёму сообщений
            auth_data = None

        try:
            auth_message = json.loads(auth_data)
            if auth_message.get("type") == "auth" and "token" in auth_message:
                token = auth_message["token"]
                user_id = verify_websocket_token(token)

                if user_id:
                    # Аутентификация успешна
                    authenticated = await connection_manager.authenticate_connection(
                        websocket, user_id
                    )
                    if authenticated:
                        await websocket.send_json(
                            {
                                "type": "auth_success",
                                "message": f"Пользователь {user_id} успешно аутентифицирован",
                            }
                        )

                        # Основной цикл обработки сообщений
                        while True:
                            data = await websocket.receive_text()
                            logger.debug(f"WebSocket received message: {data}")
                            try:
                                message = json.loads(data)
                                message_type = message.get("type")
                                logger.debug(f"Parsed message type: {message_type}")

                                if message_type == "subscribe":
                                    channel = message.get("channel")
                                    logger.debug(
                                        f"Subscribe request for channel: {channel}"
                                    )
                                    if channel:
                                        await connection_manager.subscribe(
                                            user_id, channel, websocket
                                        )

                                        # Специальная обработка для канала timeline
                                        if channel == "timeline":
                                            await memory_timeline.subscribe(websocket)

                                        response = {
                                            "type": "subscribed",
                                            "channel": channel,
                                        }
                                        logger.debug(
                                            f"Sending subscribe response: {response}"
                                        )
                                        await websocket.send_json(response)
                                elif message_type == "unsubscribe":
                                    channel = message.get("channel")
                                    if channel:
                                        await connection_manager.unsubscribe(
                                            user_id, channel
                                        )

                                        # Специальная обработка для канала timeline
                                        if channel == "timeline":
                                            logger.debug(
                                                f"Unsubscribing from memory_timeline for user {user_id}"
                                            )
                                            await memory_timeline.unsubscribe(websocket)

                                        response = {
                                            "type": "unsubscribed",
                                            "channel": channel,
                                        }
                                        logger.debug(
                                            f"Sending unsubscribe response: {response}"
                                        )
                                        await websocket.send_json(response)
                                elif message_type == "broadcast":
                                    channel = message.get("channel")
                                    content = message.get("content")
                                    if channel and content:
                                        await connection_manager.broadcast(
                                            channel,
                                            {
                                                "type": "message",
                                                "content": content,
                                                "sender": user_id,
                                                "timestamp": datetime.utcnow().isoformat(),
                                            },
                                            sender_id=user_id,
                                        )

                            except json.JSONDecodeError:
                                logger.error("JSON decode error in WebSocket message")
                                await websocket.send_json(
                                    {"type": "error", "message": "Неверный формат JSON"}
                                )
                            except Exception as e:
                                logger.error(f"Error processing WebSocket message: {e}")
                                await websocket.send_json(
                                    {
                                        "type": "error",
                                        "message": f"Ошибка обработки сообщения: {str(e)}",
                                    }
                                )
                    else:
                        await connection_manager.reject_connection(
                            websocket, "Authentication failed"
                        )
                        return
                else:
                    await connection_manager.reject_connection(
                        websocket, "Invalid token"
                    )
                    return
            else:
                await connection_manager.reject_connection(
                    websocket, "Invalid auth message format"
                )
                return

        except json.JSONDecodeError:
            await connection_manager.reject_connection(websocket, "Invalid JSON format")
            return

    except WebSocketDisconnect:
        if authenticated and user_id:
            await memory_timeline.unsubscribe(websocket)
            await connection_manager.disconnect(websocket, user_id)
        else:
            await connection_manager.reject_connection(
                websocket, "Disconnected during auth"
            )
    except Exception as e:
        print(f"WebSocket error: {e}")
        if authenticated and user_id:
            await memory_timeline.unsubscribe(websocket)
            await connection_manager.disconnect(websocket, user_id)
        else:
            await connection_manager.reject_connection(websocket, f"Error: {str(e)}")


# Debug endpoints
@app.get("/debug/subscribers/count")
async def get_subscribers_count():
    """Получить количество подписчиков."""
    return {"count": memory_timeline.get_subscriber_count()}


@app.get("/debug/connections/stats")
async def get_connection_stats():
    """Получить статистику WebSocket подключений."""
    return connection_manager.get_connection_stats()


# Memory Timeline endpoints
@app.post(
    "/timeline/memories/",
    response_model=Dict,
    status_code=status.HTTP_201_CREATED,
    responses={
        201: {"description": "Воспоминание успешно добавлено"},
        422: {"description": "Ошибка валидации входных данных"},
    },
)
async def add_memory(memory: MemoryCreate):
    """
    Добавляет новое воспоминание в таймлайн.

    - **content**: Текст воспоминания
    - **memory_type**: Тип воспоминания (например, 'personal', 'work')
    - **metadata**: Дополнительные метаданные в формате JSON
    """
    try:
        result = await memory_timeline.add_memory(
            content=memory.content,
            memory_type=memory.memory_type,
            metadata=memory.metadata or {},
        )
        return result
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Ошибка при добавлении воспоминания: {str(e)}",
        )


@app.get("/timeline/memories/", response_model=List[Dict])
async def get_memories(
    start_time: Optional[datetime] = None,
    end_time: Optional[datetime] = None,
    memory_type: Optional[str] = None,
    limit: int = 100,
):
    """Возвращает отфильтрованный список воспоминаний."""
    return memory_timeline.get_timeline(start_time, end_time, memory_type, limit)


# DuneWave endpoints
@app.post("/waves/", response_model=dict)
async def create_wave(
    wave: DuneWaveCreate, writer: Neo4jWriter = Depends(get_neo4j_writer)
):
    wave_data = wave.dict()
    wave_data["id"] = f"wave_{int(datetime.utcnow().timestamp())}"
    wave_data["timestamp"] = datetime.utcnow().isoformat()

    node = writer.create_dunewave_node(wave_data)
    if not node:
        raise HTTPException(status_code=500, detail="Не удалось создать DuneWave")
    return {"status": "success", "id": node["id"]}


@app.get("/waves/", response_model=List[dict])
async def get_waves(limit: int = 10, writer: Neo4jWriter = Depends(get_neo4j_writer)):
    try:
        with writer.driver.session() as session:
            result = session.run(
                "MATCH (d:DuneWave) RETURN d ORDER BY d.timestamp DESC LIMIT $limit",
                limit=limit,
            )
            waves = []
            for record in result:
                wave = dict(record["d"])
                # Преобразуем объекты Neo4j DateTime в строки
                for key, value in wave.items():
                    if hasattr(value, "isoformat"):
                        wave[key] = value.isoformat()
                waves.append(wave)
            return waves
    except Exception as e:
        print(f"Error in get_waves: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# MemoryFragment endpoints
@app.post("/fragments/", response_model=dict)
async def create_fragment(
    fragment: MemoryFragmentCreate, writer: Neo4jWriter = Depends(get_neo4j_writer)
):
    fragment_data = fragment.dict()
    fragment_data["id"] = f"mem_{int(datetime.utcnow().timestamp())}"
    fragment_data["timestamp"] = datetime.utcnow().isoformat()

    node = writer.create_memory_fragment_node(fragment_data)
    if not node:
        raise HTTPException(status_code=500, detail="Не удалось создать MemoryFragment")
    return {"status": "success", "id": node["id"]}


@app.get("/fragments/", response_model=List[dict])
async def get_fragments(
    limit: int = 10, writer: Neo4jWriter = Depends(get_neo4j_writer)
):
    try:
        with writer.driver.session() as session:
            result = session.run(
                "MATCH (m:MemoryFragment) RETURN m ORDER BY m.timestamp DESC LIMIT $limit",
                limit=limit,
            )
            fragments = []
            for record in result:
                fragment = dict(record["m"])
                # Преобразуем объекты Neo4j DateTime в строки
                for key, value in fragment.items():
                    if hasattr(value, "isoformat"):
                        fragment[key] = value.isoformat()
                fragments.append(fragment)
            return fragments
    except Exception as e:
        print(f"Error in get_fragments: {e}")
        raise HTTPException(status_code=500, detail=str(e))


# Relationships
@app.post("/relationships/link-wave-to-memory/", response_model=dict)
async def link_wave_to_memory(
    wave_id: str, memory_id: str, writer: Neo4jWriter = Depends(get_neo4j_writer)
):
    result = writer.link_dunewave_to_memory(wave_id, memory_id)
    if not result:
        raise HTTPException(status_code=400, detail="Не удалось создать связь")
    return {"status": "success"}


@app.post("/relationships/mentorship/", response_model=dict)
async def create_mentorship(
    mentorship: MentorshipCreate, writer: Neo4jWriter = Depends(get_neo4j_writer)
):
    result = writer.create_mentorship(mentorship.younger_id, mentorship.mentor_id)
    if not result:
        raise HTTPException(
            status_code=400, detail="Не удалось создать связь наставничества"
        )
    return {"status": "success"}


# ML-специфичные эндпоинты
@app.get("/ml_metrics")
async def get_ml_metrics():
    """Специальный endpoint для ML-фичей"""
    return {
        "user_patterns": extract_user_patterns(),
        "traffic_features": extract_traffic_features(),
        "anomaly_scores": get_current_anomaly_scores(),
        "prediction_features": get_prediction_features(),
    }


@app.get("/health")
async def health_check():
    """Health check endpoint для мониторинга состояния сервиса"""
    return {
        "status": "ok",
        "timestamp": datetime.utcnow().isoformat(),
        "ml_enabled": ML_ENABLED,
        "redis_connected": hasattr(connection_manager, "redis")
        and connection_manager.redis is not None,
    }


@app.get("/ready")
async def readiness_check():
    """Readiness probe: проверка готовности приложения обрабатывать запросы.

    Проверяет базовую работоспособность и статус критических зависимостей (например, Redis).
    """
    try:
        # Внутри обработчика запроса цикл событий должен быть доступен
        asyncio.get_running_loop()
        loop_ok = True
    except RuntimeError:
        loop_ok = False

    # Проверяем, используется ли Redis и каков статус подключения
    redis_cfg = hasattr(connection_manager, "_is_connected")
    redis_ok = connection_manager._is_connected if redis_cfg else True

    checks = {
        "app_loaded": True,
        "event_loop": loop_ok,
        "redis_configured": redis_cfg,
        "redis_connected": redis_ok,
        "ml_enabled": ML_ENABLED,
    }

    # Готовность определяется базовыми условиями
    ready = checks["app_loaded"] and checks["event_loop"]

    # Если Redis сконфигурирован, он ДОЛЖЕН быть подключен для полной готовности
    if redis_cfg:
        ready = ready and checks["redis_connected"]

    return {
        "ready": ready,
        "checks": checks,
        "timestamp": datetime.utcnow().isoformat(),
    }


# Middleware для сбора ML-данных
@app.middleware("http")
async def ml_data_collector(request, call_next):
    """Middleware для сбора данных для ML-анализа"""
    # Регистрируем IP адрес
    client_host = request.client.host if request.client else "unknown"
    register_ip_address(client_host)

    # Далее стандартная обработка запроса
    response = await call_next(request)
    return response


# Запуск сервера
if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000)
