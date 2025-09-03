"""
Health check endpoints для мониторинга состояния системы.
Реализованы liveness, readiness и startup пробы.
"""

from typing import Any, Dict

from fastapi import APIRouter, HTTPException, Request, status
from loguru import logger
# Импортируем наш кастомный RedisClient и DummyRedis для определения деградации
from redis_client import DummyRedis, RedisClient

router = APIRouter(prefix="/health", tags=["health"])


@router.get("/live", status_code=status.HTTP_200_OK)
async def liveness_check() -> Dict[str, str]:
    """
    Liveness probe.
    Проверяет, что приложение запущено и отвечает на запросы.
    Не проверяет зависимости (БД, Redis и т.д.).
    """
    return {"status": "alive"}


@router.get("/ready", status_code=status.HTTP_200_OK)
async def readiness_check(request: Request) -> Dict[str, Any]:
    """
    Readiness probe.
    Проверяет, готово ли приложение принимать трафик, включая проверку
    критически важных зависимостей (например, Redis).
    """
    component_statuses: Dict[str, Dict[str, Any]] = {}

    # 1) Redis
    try:
        redis_client: RedisClient = request.app.state.redis_client  # type: ignore[attr-defined]
        if not redis_client:
            raise ConnectionError("Redis client missing in app.state")

        # В текущей реализаци redis_client.client может быть DummyRedis
        client = getattr(redis_client, "client", None)
        if client is None:
            raise ConnectionError("Redis client attribute is None")

        # Если работаем в деградации (DummyRedis), считаем компонент неготовым
        if isinstance(client, DummyRedis):
            raise ConnectionError("Redis is in Dummy mode (not connected)")

        # Проверяем ping к реальному Redis
        client.ping()
        component_statuses["redis"] = {"status": "ok"}
        logger.debug("Readiness check: Redis connection is OK.")
    except Exception as e:
        logger.error(f"Readiness check failed for Redis: {e}")
        component_statuses["redis"] = {"status": "error", "details": str(e)}

    # 2) Neo4j (зарезервировано на будущее)
    # try:
    #     ...
    #     component_statuses["neo4j"] = {"status": "ok"}
    # except Exception as e:
    #     component_statuses["neo4j"] = {"status": "error", "details": str(e)}

    # Итоговый статус
    all_ok = all(comp.get("status") == "ok" for comp in component_statuses.values())
    if all_ok:
        return {"status": "ready", "components": component_statuses}
    raise HTTPException(
        status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
        detail={"status": "not_ready", "components": component_statuses},
    )


@router.get("/startup", status_code=status.HTTP_200_OK)
async def startup_check(request: Request) -> Dict[str, str]:
    """
    Startup probe.
    Возвращает 200 только после завершения старта приложения
    (устанавливается в main.py в app.state.startup_complete = True).
    """
    started = bool(getattr(request.app.state, "startup_complete", False))  # type: ignore[attr-defined]
    if started:
        return {"status": "started"}
    raise HTTPException(
        status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
        detail={"status": "starting"},
    )
