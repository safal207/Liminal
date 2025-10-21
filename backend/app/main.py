"""FastAPI application entrypoint for the modularised backend."""
from __future__ import annotations

import asyncio
from contextlib import asynccontextmanager
from datetime import datetime
from pathlib import Path
from typing import Awaitable, Callable, Dict, AsyncIterator

from fastapi import Depends, FastAPI, Request, Response
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles

from backend.health import router as health_router
from backend.redis_client import RedisClient
from backend.metrics import setup_metrics

from .dependencies import (
    get_connection_manager,
    get_memory_service,
    get_ml_service,
    get_websocket_service,
    init_services,
    shutdown_services,
)
from .routes import auth, debug, fragments, waves, ws


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncIterator[None]:
    """Initialise and tear down shared application services."""

    await init_services()
    app.state.startup_complete = True
    try:
        yield
    finally:
        app.state.startup_complete = False
        await shutdown_services()


app = FastAPI(
    title="LIMINAL API",
    description="API для работы с графовой базой данных LIMINAL",
    lifespan=lifespan,
)

# Static files ---------------------------------------------------------
static_dir = Path(__file__).resolve().parent.parent / "static"
if static_dir.exists():
    app.mount("/static", StaticFiles(directory=static_dir), name="static")


# Middleware -----------------------------------------------------------
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

setup_metrics(app)


@app.middleware("http")
async def ml_data_collector(
    request: Request,
    call_next: Callable[[Request], Awaitable[Response]],
) -> Response:
    client_host = request.client.host if request.client else "unknown"
    ml_service = getattr(request.app.state, "ml_service", get_ml_service())
    ml_service.register_ip_address(client_host)
    return await call_next(request)


# Routers --------------------------------------------------------------
app.include_router(health_router)
app.include_router(auth.router)
app.include_router(waves.router)
app.include_router(fragments.router)
app.include_router(debug.router)
app.include_router(ws.router)


# Application state ----------------------------------------------------
app.state.redis_client = RedisClient()
app.state.connection_manager = get_connection_manager()
app.state.ml_service = get_ml_service()
app.state.websocket_service = get_websocket_service()
app.state.memory_service = get_memory_service()
app.state.startup_complete = False


# Routes ---------------------------------------------------------------
@app.get("/")
async def root() -> Dict[str, str]:
    return {"message": "Welcome to LIMINAL API"}


@app.get("/health")
async def health_check():
    return {
        "status": "ok",
        "ml_enabled": get_ml_service().enabled,
        "redis_connected": hasattr(app.state.connection_manager, "redis")
        and getattr(app.state.connection_manager, "redis", None) is not None,
    }


@app.get("/ready")
async def readiness_check(manager=Depends(get_connection_manager)):
    try:
        asyncio.get_running_loop()
        loop_ok = True
    except RuntimeError:
        loop_ok = False

    redis_cfg = hasattr(manager, "_is_connected")
    redis_ok = getattr(manager, "_is_connected", True) if redis_cfg else True

    checks = {
        "app_loaded": True,
        "event_loop": loop_ok,
        "redis_configured": redis_cfg,
        "redis_connected": redis_ok,
        "ml_enabled": get_ml_service().enabled,
    }

    ready = checks["app_loaded"] and checks["event_loop"]
    if redis_cfg:
        ready = ready and redis_ok

    return {
        "ready": ready,
        "checks": checks,
        "timestamp": datetime.utcnow().isoformat(),
    }
