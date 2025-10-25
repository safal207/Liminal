import structlog
logger = structlog.getLogger("test")
logger.info("Тест структурного логирования", source="WSL")
