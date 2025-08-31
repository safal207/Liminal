"""Honeypot система для обнаружения атак."""
import asyncio
import json
import logging
import socket
import ssl
from datetime import datetime
from typing import Dict, List, Optional, Tuple

from aiohttp import web
from pydantic import BaseModel

from backend.security_logging import security_logger

class HoneypotEvent(BaseModel):
    """Модель события honeypot."""
    timestamp: str
    source_ip: str
    destination_port: int
    protocol: str
    payload: Optional[str]
    headers: Optional[Dict[str, str]]
    method: Optional[str]
    path: Optional[str]
    attack_type: Optional[str]


class AttackPattern(BaseModel):
    """Модель паттерна атаки."""
    pattern: str
    type: str
    severity: str
    description: str


class HoneypotManager:
    """Менеджер honeypot сервисов."""

    def __init__(self):
        """Инициализация менеджера."""
        self.logger = logging.getLogger("honeypot")
        self.events: List[HoneypotEvent] = []
        self.attack_patterns = self._load_attack_patterns()
        
    def _load_attack_patterns(self) -> List[AttackPattern]:
        """Загрузка паттернов атак."""
        return [
            AttackPattern(
                pattern=r"(?i)(union\s+select|select\s+.*\s+from|insert\s+into|update\s+.*\s+set|delete\s+from)",
                type="SQL Injection",
                severity="HIGH",
                description="SQL injection attempt detected"
            ),
            AttackPattern(
                pattern=r"(?i)(<script>|javascript:|onload=|onerror=)",
                type="XSS",
                severity="HIGH",
                description="Cross-site scripting attempt detected"
            ),
            AttackPattern(
                pattern=r"(?i)(../|\.\.\\|/etc/passwd|/etc/shadow)",
                type="Path Traversal",
                severity="HIGH",
                description="Directory traversal attempt detected"
            ),
            AttackPattern(
                pattern=r"(?i)(\${.*}|\#{.*}|%{.*})",
                type="Template Injection",
                severity="HIGH",
                description="Template injection attempt detected"
            ),
        ]

    async def start_http_honeypot(self, host: str = "0.0.0.0", port: int = 8080):
        """Запуск HTTP honeypot."""
        app = web.Application()
        app.router.add_route("*", "/{tail:.*}", self.handle_http_request)
        
        runner = web.AppRunner(app)
        await runner.setup()
        site = web.TCPSite(runner, host, port)
        await site.start()
        
        self.logger.info(f"HTTP Honeypot started on {host}:{port}")

    async def start_ssh_honeypot(self, host: str = "0.0.0.0", port: int = 2222):
        """Запуск SSH honeypot."""
        server = await asyncio.start_server(
            self.handle_ssh_connection, host, port
        )
        
        self.logger.info(f"SSH Honeypot started on {host}:{port}")
        
        async with server:
            await server.serve_forever()

    async def handle_http_request(self, request: web.Request) -> web.Response:
        """Обработка HTTP запроса."""
        event = HoneypotEvent(
            timestamp=datetime.now().isoformat(),
            source_ip=request.remote,
            destination_port=request.transport.get_extra_info('socket').getsockname()[1],
            protocol="HTTP",
            method=request.method,
            path=request.path,
            headers=dict(request.headers),
            payload=await request.text()
        )
        
        # Анализ на наличие атак
        for pattern in self.attack_patterns:
            if pattern.pattern.search(event.payload or ""):
                event.attack_type = pattern.type
                self._handle_attack(event, pattern)
                break
        
        self.events.append(event)
        
        # Возвращаем правдоподобный ответ
        return web.Response(
            text="Service Temporarily Unavailable",
            status=503
        )

    async def handle_ssh_connection(
        self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter
    ):
        """Обработка SSH соединения."""
        addr = writer.get_extra_info('peername')
        event = HoneypotEvent(
            timestamp=datetime.now().isoformat(),
            source_ip=addr[0],
            destination_port=addr[1],
            protocol="SSH",
            payload=""
        )
        
        try:
            # Симулируем SSH баннер
            writer.write(b"SSH-2.0-OpenSSH_8.2p1 Ubuntu-4ubuntu0.2\r\n")
            await writer.drain()
            
            # Читаем данные
            data = await reader.read(1024)
            event.payload = data.decode()
            
            self.events.append(event)
            
            # Анализируем на предмет атак
            for pattern in self.attack_patterns:
                if pattern.pattern.search(event.payload):
                    event.attack_type = pattern.type
                    self._handle_attack(event, pattern)
                    break
                    
        except Exception as e:
            self.logger.error(f"Error in SSH honeypot: {e}")
        finally:
            writer.close()
            await writer.wait_closed()

    def _handle_attack(self, event: HoneypotEvent, pattern: AttackPattern):
        """Обработка обнаруженной атаки."""
        # Логируем атаку
        security_logger.log_suspicious_activity(
            message=f"Honeypot detected {pattern.type} attack",
            ip_address=event.source_ip,
            service="honeypot"
        )
        
        # Отправляем уведомление в SIEM
        self._notify_siem(event, pattern)

    def _notify_siem(self, event: HoneypotEvent, pattern: AttackPattern):
        """Отправка уведомления в SIEM."""
        try:
            wazuh_event = {
                "timestamp": event.timestamp,
                "rule": {
                    "level": 10,
                    "description": pattern.description,
                    "id": f"100{hash(pattern.type) % 1000}",
                    "firedtimes": 1
                },
                "agent": {
                    "name": "honeypot",
                    "id": "000"
                },
                "manager": {
                    "name": "wazuh-manager"
                },
                "data": {
                    "srcip": event.source_ip,
                    "dstport": event.destination_port,
                    "protocol": event.protocol,
                    "attack_type": pattern.type,
                    "severity": pattern.severity
                }
            }
            
            # Отправляем событие в Wazuh
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            sock.sendto(
                json.dumps(wazuh_event).encode(),
                ("wazuh-manager", 514)
            )
            
        except Exception as e:
            self.logger.error(f"Error sending event to SIEM: {e}")

async def main():
    """Основная функция запуска."""
    honeypot = HoneypotManager()
    
    # Запускаем различные типы honeypot
    await asyncio.gather(
        honeypot.start_http_honeypot(),
        honeypot.start_ssh_honeypot()
    )

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    asyncio.run(main())
