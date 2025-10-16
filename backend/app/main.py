"""FastAPI application entrypoint for the modularised backend."""
from __future__ import annotations

import asyncio
from datetime import datetime
from pathlib import Path
from typing import Dict

from fastapi import Depends, FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles

from backend.health import router as health_router
from backend.redis_client import RedisClient
from backend.metrics import setup_metrics

from .dependencies import (
    get_connection_manager,
    get_ml_service,
    init_services,
    shutdown_services,
)
from .routes import auth, debug, fragments, waves, ws

app = FastAPI(
    title="LIMINAL API",
    description="API для работы с графовой базой данных LIMINAL",
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
async def ml_data_collector(request: Request, call_next):
    client_host = request.client.host if request.client else "unknown"
    get_ml_service().register_ip_address(client_host)
    response = await call_next(request)
    return response


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
app.state.startup_complete = False


# Lifespan events ------------------------------------------------------
@app.on_event("startup")
async def on_startup():
    await init_services()
    app.state.startup_complete = True


@app.on_event("shutdown")
async def on_shutdown():
    await shutdown_services()


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
