"""Расширенная система honeypot с поддержкой различных протоколов."""
import asyncio
import logging
import ssl
from abc import ABC, abstractmethod
from datetime import datetime
from email.parser import Parser
from typing import Dict, List, Optional, Tuple

import aiosmtpd.controller
from aiosmtpd.smtp import SMTP
from pyftpdlib.authorizers import DummyAuthorizer
from pyftpdlib.handlers import FTPHandler
from pyftpdlib.servers import FTPServer
from pymongo import MongoClient

from backend.security_logging import security_logger
from .honeypot import HoneypotEvent, AttackPattern

class BaseHoneypot(ABC):
    """Базовый класс для всех типов honeypot."""
    
    def __init__(self, host: str = "0.0.0.0", port: int = 0):
        """Инициализация honeypot."""
        self.host = host
        self.port = port
        self.logger = logging.getLogger(f"honeypot.{self.__class__.__name__}")
        
    @abstractmethod
    async def start(self):
        """Запуск honeypot."""
        pass
        
    @abstractmethod
    async def stop(self):
        """Остановка honeypot."""
        pass
        
    def create_event(self, **kwargs) -> HoneypotEvent:
        """Создание события honeypot."""
        return HoneypotEvent(
            timestamp=datetime.now().isoformat(),
            source_ip=kwargs.get("source_ip", "unknown"),
            destination_port=self.port,
            protocol=self.__class__.__name__.replace("Honeypot", "").upper(),
            **kwargs
        )

class FTPHoneypot(BaseHoneypot):
    """FTP honeypot для обнаружения атак на FTP."""
    
    def __init__(self, host: str = "0.0.0.0", port: int = 2121):
        """Инициализация FTP honeypot."""
        super().__init__(host, port)
        self.authorizer = DummyAuthorizer()
        self.server = None
        
    async def start(self):
        """Запуск FTP honeypot."""
        class CustomHandler(FTPHandler):
            def on_connect(self):
                event = self.create_event(
                    source_ip=self.remote_ip,
                    payload=f"Connection from {self.remote_ip}"
                )
                security_logger.log_suspicious_activity(
                    message=f"FTP connection attempt from {self.remote_ip}",
                    ip_address=self.remote_ip,
                    service="honeypot.ftp"
                )
            
            def on_login(self, username, password):
                event = self.create_event(
                    source_ip=self.remote_ip,
                    payload=f"Login attempt: {username}:{password}"
                )
                security_logger.log_suspicious_activity(
                    message=f"FTP login attempt: {username}",
                    ip_address=self.remote_ip,
                    service="honeypot.ftp"
                )
        
        handler = CustomHandler
        handler.authorizer = self.authorizer
        handler.banner = "FTP Server Ready"
        
        self.server = FTPServer((self.host, self.port), handler)
        await asyncio.to_thread(self.server.serve_forever)
        
    async def stop(self):
        """Остановка FTP honeypot."""
        if self.server:
            self.server.close_all()

class SMTPHoneypot(BaseHoneypot):
    """SMTP honeypot для обнаружения спама и атак на почту."""
    
    def __init__(self, host: str = "0.0.0.0", port: int = 2525):
        """Инициализация SMTP honeypot."""
        super().__init__(host, port)
        self.controller = None
        
    class CustomSMTPHandler:
        async def handle_RCPT(self, server, session, envelope, address, rcpt_options):
            event = HoneypotEvent(
                timestamp=datetime.now().isoformat(),
                source_ip=session.peer[0],
                destination_port=server.transport.get_extra_info('socket').getsockname()[1],
                protocol="SMTP",
                payload=f"RCPT TO: {address}"
            )
            security_logger.log_suspicious_activity(
                message=f"SMTP recipient attempt: {address}",
                ip_address=session.peer[0],
                service="honeypot.smtp"
            )
            return "250 OK"
            
        async def handle_DATA(self, server, session, envelope):
            parser = Parser()
            message = parser.parsestr(envelope.content.decode())
            
            event = HoneypotEvent(
                timestamp=datetime.now().isoformat(),
                source_ip=session.peer[0],
                destination_port=server.transport.get_extra_info('socket').getsockname()[1],
                protocol="SMTP",
                payload=envelope.content.decode()
            )
            security_logger.log_suspicious_activity(
                message="SMTP message received",
                ip_address=session.peer[0],
                service="honeypot.smtp"
            )
            return "250 Message accepted for delivery"
    
    async def start(self):
        """Запуск SMTP honeypot."""
        self.controller = aiosmtpd.controller.Controller(
            self.CustomSMTPHandler(),
            hostname=self.host,
            port=self.port
        )
        await self.controller.start()
        
    async def stop(self):
        """Остановка SMTP honeypot."""
        if self.controller:
            self.controller.stop()

class DatabaseHoneypot(BaseHoneypot):
    """Database honeypot для обнаружения атак на базы данных."""
    
    def __init__(self, host: str = "0.0.0.0", port: int = 27017):
        """Инициализация Database honeypot."""
        super().__init__(host, port)
        self.server = None
        
    async def start(self):
        """Запуск Database honeypot."""
        class FakeMongoDB(asyncio.Protocol):
            def connection_made(self, transport):
                self.transport = transport
                peername = transport.get_extra_info('peername')
                event = HoneypotEvent(
                    timestamp=datetime.now().isoformat(),
                    source_ip=peername[0],
                    destination_port=peername[1],
                    protocol="MONGODB",
                    payload="Connection attempt"
                )
                security_logger.log_suspicious_activity(
                    message=f"MongoDB connection attempt from {peername[0]}",
                    ip_address=peername[0],
                    service="honeypot.mongodb"
                )
                
            def data_received(self, data):
                peername = self.transport.get_extra_info('peername')
                event = HoneypotEvent(
                    timestamp=datetime.now().isoformat(),
                    source_ip=peername[0],
                    destination_port=peername[1],
                    protocol="MONGODB",
                    payload=data.hex()
                )
                security_logger.log_suspicious_activity(
                    message=f"MongoDB data received from {peername[0]}",
                    ip_address=peername[0],
                    service="honeypot.mongodb"
                )
                # Отправляем фейковый ответ MongoDB
                response = b"\x3a\x00\x00\x00" # Минимальный валидный ответ MongoDB
                self.transport.write(response)
        
        server = await asyncio.get_event_loop().create_server(
            FakeMongoDB,
            self.host,
            self.port
        )
        self.server = server
        await server.serve_forever()
        
    async def stop(self):
        """Остановка Database honeypot."""
        if self.server:
            self.server.close()
            await self.server.wait_closed()

class HoneypotCluster:
    """Кластер различных типов honeypot."""
    
    def __init__(self):
        """Инициализация кластера."""
        self.honeypots: List[BaseHoneypot] = [
            FTPHoneypot(),
            SMTPHoneypot(),
            DatabaseHoneypot()
        ]
        
    async def start_all(self):
        """Запуск всех honeypot."""
        tasks = [pot.start() for pot in self.honeypots]
        await asyncio.gather(*tasks)
        
    async def stop_all(self):
        """Остановка всех honeypot."""
        tasks = [pot.stop() for pot in self.honeypots]
        await asyncio.gather(*tasks)

async def main():
    """Основная функция запуска."""
    cluster = HoneypotCluster()
    
    try:
        await cluster.start_all()
        
        # Держим сервер работающим
        while True:
            await asyncio.sleep(3600)
            
    except KeyboardInterrupt:
        await cluster.stop_all()

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    asyncio.run(main())
