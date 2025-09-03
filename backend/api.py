import asyncio
import json
import logging
import os
import sys
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

from fastapi import (Depends, FastAPI, HTTPException, WebSocket,
                     WebSocketDisconnect, status)
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field

sys.path.append(str(Path(__file__).parent.parent))

from auth.jwt_utils import (authenticate_user, create_access_token_for_user,
                            jwt_manager, verify_websocket_token)
from auth.models import Token, UserLogin
from memory_timeline import MemoryTimeline
from memory_timeline import timeline as memory_timeline
from metrics import setup_metrics

if os.environ.get("TESTING"):
    class Neo4jDateTime:
        def isoformat(self):
            return datetime.now().isoformat() + "Z"

    class Neo4jWriter:
        def __init__(self, *args, **kwargs):
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
    from neo4j_writer import Neo4jDateTime, Neo4jWriter


ML_ENABLED = os.environ.get("ML_ENABLED", "false").lower() == "true"
if not ML_ENABLED:
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
else:
    from ml_features import (extract_traffic_features, extract_user_patterns,
                             get_current_anomaly_scores,
                             get_prediction_features, register_ip_address)

use_redis = os.environ.get("USE_REDIS", "false").lower() == "true"
if use_redis:
    from backend.websocket.redis_connection_manager import \
        RedisConnectionManager
    redis_url = os.environ.get("REDIS_URL", "redis://localhost:6379/0")
    connection_manager = RedisConnectionManager(
        redis_url=redis_url,
        max_connections=100,
        max_connections_per_ip=10,
        redis_prefix="liminal",
    )
else:
    from backend.websocket.connection_manager import ConnectionManager
    connection_manager = ConnectionManager(
        max_connections=100, max_connections_per_ip=10
    )

app = FastAPI(
    title="LIMINAL API",
    description="API для работы с графовой базой данных LIMINAL"
)
logger = logging.getLogger(__name__)


@app.on_event("startup")
async def startup_event():
    logger.info("Starting application")
    logger.info("Prometheus metrics already initialized")
    if use_redis and hasattr(connection_manager, "initialize"):
        redis_initialized = await connection_manager.initialize()
        logger.info(f"Redis connection initialized: {redis_initialized}")


@app.on_event("shutdown")
async def shutdown_event():
    logger.info("Shutting down application")
    if use_redis and hasattr(connection_manager, "shutdown"):
        await connection_manager.shutdown()
        logger.info("Redis connection closed")


timeline = MemoryTimeline()
static_dir = os.path.join(os.path.dirname(__file__), "static")
app.mount("/static", StaticFiles(directory=static_dir), name="static")
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

try:
    setup_metrics(app)
    logger.info("Prometheus metrics initialized at /metrics endpoint")
except Exception as e:
    logger.error(f"Failed to setup Prometheus metrics: {e}")


class DuneWaveCreate(BaseModel):
    phase: str
    emotion: str
    intensity: float
    context: str
    source: str
    class Config:
        json_encoders = {
            Neo4jDateTime: lambda v: v.isoformat() if v else None
        }


class MemoryFragmentCreate(BaseModel):
    content: str
    type: str
    growth_stage: str
    class Config:
        json_encoders = {
            Neo4jDateTime: lambda v: v.isoformat() if v else None
        }


class MentorshipCreate(BaseModel):
    younger_id: str
    mentor_id: str


class MemoryCreate(BaseModel):
    content: str = Field(..., description="Содержимое воспоминания")
    memory_type: str = Field(
        ..., description="Тип воспоминания (например, 'personal', 'work')"
    )
    metadata: Optional[Dict[str, Any]] = Field(
        default_factory=dict, description="Дополнительные метаданные"
    )


_neo4j_writer = None


def get_neo4j_writer():
    global _neo4j_writer
    if _neo4j_writer is None:
        try:
            if os.environ.get("TESTING"):
                _neo4j_writer = Neo4jWriter()
            else:
                _neo4j_writer = Neo4jWriter(
                    uri=os.getenv("NEO4J_URI", "bolt://localhost:7687"),
                    user=os.getenv("NEO4J_USER", "neo4j"),
                    password=os.getenv("NEO4J_PASSWORD", "password"),
                )
        except Exception as e:
            logger.error(f"Ошибка при подключении к Neo4j: {e}")
            _neo4j_writer = Neo4jWriter()
    return _neo4j_writer


@app.get("/", response_model=dict)
async def root() -> dict:
    return {"message": "Welcome to LIMINAL API"}


@app.post("/auth/login", response_model=Token)
async def login(user_data: UserLogin):
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


@app.post("/token", response_model=Token)
async def login_for_access_token(form_data: UserLogin):
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


@app.websocket("/ws/timeline")
async def websocket_timeline(websocket: WebSocket, token: str = None):
    connection_accepted = await connection_manager.accept_pending_connection(
        websocket
    )
    if not connection_accepted:
        return
    authenticated = False
    user_id = None
    try:
        if token:
            user_id = verify_websocket_token(token)
            if user_id:
                authenticated = await connection_manager.authenticate_connection(
                    websocket, user_id
                )
                if authenticated:
                    msg = (f"Пользователь {user_id} успешно "
                           f"аутентифицирован через URL токен")
                    await websocket.send_json(
                        {"type": "auth_success", "message": msg}
                    )
        if not authenticated:
            msg = "Необходима аутентификация. Отправьте JWT токен."
            await websocket.send_json(
                {"type": "auth_required", "message": msg}
            )
            auth_data = await websocket.receive_text()
        else:
            auth_data = None
        try:
            auth_message = json.loads(auth_data)
            if auth_message.get("type") == "auth" and "token" in auth_message:
                token = auth_message["token"]
                user_id = verify_websocket_token(token)
                if user_id:
                    authenticated = await connection_manager.authenticate_connection(
                        websocket, user_id
                    )
                    if authenticated:
                        msg = (f"Пользователь {user_id} "
                               f"успешно аутентифицирован")
                        await websocket.send_json(
                            {"type": "auth_success", "message": msg}
                        )
                        await handle_websocket_messages(websocket, user_id)
                    else:
                        await connection_manager.reject_connection(
                            websocket, "Authentication failed"
                        )
                else:
                    await connection_manager.reject_connection(
                        websocket, "Invalid token"
                    )
            else:
                await connection_manager.reject_connection(
                    websocket, "Invalid auth message format"
                )
        except (json.JSONDecodeError, AttributeError):
            await connection_manager.reject_connection(
                websocket, "Invalid JSON format"
            )
    except WebSocketDisconnect:
        if authenticated and user_id:
            await memory_timeline.unsubscribe(websocket)
            await connection_manager.disconnect(websocket, user_id)
        else:
            await connection_manager.reject_connection(
                websocket, "Disconnected during auth"
            )
    except Exception as e:
        logger.error(f"WebSocket error: {e}")
        if authenticated and user_id:
            await memory_timeline.unsubscribe(websocket)
            await connection_manager.disconnect(websocket, user_id)
        else:
            await connection_manager.reject_connection(
                websocket, f"Error: {str(e)}"
            )


async def handle_websocket_messages(websocket: WebSocket, user_id: str):
    while True:
        data = await websocket.receive_text()
        logger.debug(f"WebSocket received message: {data}")
        try:
            message = json.loads(data)
            message_type = message.get("type")
            logger.debug(f"Parsed message type: {message_type}")
            if message_type == "subscribe":
                await handle_subscribe(message, user_id, websocket)
            elif message_type == "unsubscribe":
                await handle_unsubscribe(message, user_id, websocket)
            elif message_type == "broadcast":
                await handle_broadcast(message, user_id)
        except json.JSONDecodeError:
            logger.error("JSON decode error in WebSocket message")
            await websocket.send_json(
                {"type": "error", "message": "Неверный формат JSON"}
            )
        except Exception as e:
            logger.error(f"Error processing WebSocket message: {e}")
            await websocket.send_json(
                {"type": "error",
                 "message": f"Ошибка обработки сообщения: {str(e)}"}
            )


async def handle_subscribe(message: dict, user_id: str, websocket: WebSocket):
    channel = message.get("channel")
    if not channel:
        return
    logger.debug(f"Subscribe request for channel: {channel}")
    await connection_manager.subscribe(user_id, channel, websocket)
    if channel == "timeline":
        await memory_timeline.subscribe(websocket)
    response = {"type": "subscribed", "channel": channel}
    logger.debug(f"Sending subscribe response: {response}")
    await websocket.send_json(response)


async def handle_unsubscribe(message: dict, user_id: str,
                           websocket: WebSocket):
    channel = message.get("channel")
    if not channel:
        return
    await connection_manager.unsubscribe(user_id, channel)
    if channel == "timeline":
        logger.debug(f"Unsubscribing from memory_timeline for user {user_id}")
        await memory_timeline.unsubscribe(websocket)
    response = {"type": "unsubscribed", "channel": channel}
    logger.debug(f"Sending unsubscribe response: {response}")
    await websocket.send_json(response)


async def handle_broadcast(message: dict, user_id: str):
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


@app.get("/debug/subscribers/count")
async def get_subscribers_count():
    return {"count": memory_timeline.get_subscriber_count()}


@app.get("/debug/connections/stats")
async def get_connection_stats():
    return connection_manager.get_connection_stats()


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
    return memory_timeline.get_timeline(
        start_time, end_time, memory_type, limit
    )


@app.post("/waves/", response_model=dict)
async def create_wave(
    wave: DuneWaveCreate, writer: Neo4jWriter = Depends(get_neo4j_writer)
):
    wave_data = wave.dict()
    wave_data["id"] = f"wave_{int(datetime.utcnow().timestamp())}"
    wave_data["timestamp"] = datetime.utcnow().isoformat()
    node = writer.create_dunewave_node(wave_data)
    if not node:
        raise HTTPException(
            status_code=500, detail="Не удалось создать DuneWave"
        )
    return {"status": "success", "id": node["id"]}


@app.get("/waves/", response_model=List[dict])
async def get_waves(
    limit: int = 10, writer: Neo4jWriter = Depends(get_neo4j_writer)
):
    try:
        with writer.driver.session() as session:
            result = session.run(
                "MATCH (d:DuneWave) "
                "RETURN d ORDER BY d.timestamp DESC LIMIT $limit",
                limit=limit,
            )
            waves = []
            for record in result:
                wave = dict(record["d"])
                for key, value in wave.items():
                    if hasattr(value, "isoformat"):
                        wave[key] = value.isoformat()
                waves.append(wave)
            return waves
    except Exception as e:
        logger.error(f"Error in get_waves: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/fragments/", response_model=dict)
async def create_fragment(
    fragment: MemoryFragmentCreate,
    writer: Neo4jWriter = Depends(get_neo4j_writer),
):
    fragment_data = fragment.dict()
    fragment_data["id"] = f"mem_{int(datetime.utcnow().timestamp())}"
    fragment_data["timestamp"] = datetime.utcnow().isoformat()
    node = writer.create_memory_fragment_node(fragment_data)
    if not node:
        raise HTTPException(
            status_code=500, detail="Не удалось создать MemoryFragment"
        )
    return {"status": "success", "id": node["id"]}


@app.get("/fragments/", response_model=List[dict])
async def get_fragments(
    limit: int = 10, writer: Neo4jWriter = Depends(get_neo4j_writer)
):
    try:
        with writer.driver.session() as session:
            result = session.run(
                "MATCH (m:MemoryFragment) "
                "RETURN m ORDER BY m.timestamp DESC LIMIT $limit",
                limit=limit,
            )
            fragments = []
            for record in result:
                fragment = dict(record["m"])
                for key, value in fragment.items():
                    if hasattr(value, "isoformat"):
                        fragment[key] = value.isoformat()
                fragments.append(fragment)
            return fragments
    except Exception as e:
        logger.error(f"Error in get_fragments: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/relationships/link-wave-to-memory/", response_model=dict)
async def link_wave_to_memory(
    wave_id: str,
    memory_id: str,
    writer: Neo4jWriter = Depends(get_neo4j_writer),
):
    result = writer.link_dunewave_to_memory(wave_id, memory_id)
    if not result:
        raise HTTPException(
            status_code=400, detail="Не удалось создать связь"
        )
    return {"status": "success"}


@app.post("/relationships/mentorship/", response_model=dict)
async def create_mentorship(
    mentorship: MentorshipCreate,
    writer: Neo4jWriter = Depends(get_neo4j_writer),
):
    result = writer.create_mentorship(
        mentorship.younger_id, mentorship.mentor_id
    )
    if not result:
        raise HTTPException(
            status_code=400, detail="Не удалось создать связь наставничества"
        )
    return {"status": "success"}


@app.get("/ml_metrics")
async def get_ml_metrics():
    return {
        "user_patterns": extract_user_patterns(),
        "traffic_features": extract_traffic_features(),
        "anomaly_scores": get_current_anomaly_scores(),
        "prediction_features": get_prediction_features(),
    }


@app.get("/health")
async def health_check():
    return {
        "status": "ok",
        "timestamp": datetime.utcnow().isoformat(),
        "ml_enabled": ML_ENABLED,
        "redis_connected": (
            hasattr(connection_manager, "redis")
            and connection_manager.redis is not None
        ),
    }


@app.get("/ready")
async def readiness_check():
    try:
        asyncio.get_running_loop()
        loop_ok = True
    except RuntimeError:
        loop_ok = False
    redis_cfg = hasattr(connection_manager, "redis")
    redis_ok = connection_manager.redis is not None if redis_cfg else True
    checks = {
        "app_loaded": True,
        "event_loop": loop_ok,
        "redis_configured": redis_cfg,
        "redis_connected": redis_ok,
        "ml_enabled": ML_ENABLED,
    }
    ready = checks["app_loaded"] and checks["event_loop"]
    return {
        "ready": ready,
        "checks": checks,
        "timestamp": datetime.utcnow().isoformat(),
    }


@app.middleware("http")
async def ml_data_collector(request, call_next):
    client_host = request.client.host if request.client else "unknown"
    register_ip_address(client_host)
    response = await call_next(request)
    return response


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
