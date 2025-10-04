"""Модуль для интеграции с ELK Stack."""
import json
import logging
import socket
from datetime import datetime
from typing import Any, Dict, Optional

from elasticsearch import Elasticsearch
from pydantic import BaseModel


class SecurityEvent(BaseModel):
    """Модель события безопасности."""
    timestamp: str = datetime.now().isoformat()
    event_type: str
    service: str
    severity: str
    message: str
    user: Optional[str] = None
    ip_address: Optional[str] = None
    additional_data: Optional[Dict[str, Any]] = None


class ELKLogger:
    """Логгер для отправки событий в ELK Stack."""

    def __init__(
        self,
        logstash_host: str = "logstash",
        logstash_port: int = 5000,
        elasticsearch_host: str = "elasticsearch",
        elasticsearch_port: int = 9200,
        elasticsearch_user: str = "elastic",
        elasticsearch_password: str = "secure_password",
    ):
        """Инициализация логгера."""
        self.logstash_host = logstash_host
        self.logstash_port = logstash_port

        # Настройка стандартного логгера
        self.logger = logging.getLogger("security")
        self.logger.setLevel(logging.INFO)

        # Настройка Elasticsearch
        self.es = Elasticsearch(
            [f"http://{elasticsearch_host}:{elasticsearch_port}"],
            basic_auth=(elasticsearch_user, elasticsearch_password)
        )

        # Настройка TCP соединения для Logstash выполняется лениво
        self._socket_factory = lambda: socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock: Optional[socket.socket] = None

    def _ensure_socket(self) -> None:
        """Подключение к Logstash при необходимости."""
        if self.sock is not None:
            return

        try:
            sock = self._socket_factory()
            sock.settimeout(5)
            sock.connect((self.logstash_host, self.logstash_port))
        except OSError as exc:
            self.logger.warning(
                "Unable to connect to Logstash at %s:%s: %s",
                self.logstash_host,
                self.logstash_port,
                exc,
            )
        else:
            sock.settimeout(None)
            self.sock = sock

    def log_security_event(self, event: SecurityEvent) -> None:
        """Логирование события безопасности."""
        try:
            # Отправка в Logstash
            event_dict = event.dict()
            event_dict["type"] = "security"

            self._ensure_socket()
            if self.sock is not None:
                try:
                    self.sock.send(json.dumps(event_dict).encode() + b"\n")
                except OSError as exc:
                    self.logger.warning(
                        "Lost connection to Logstash at %s:%s: %s",
                        self.logstash_host,
                        self.logstash_port,
                        exc,
                    )
                    self.sock.close()
                    self.sock = None
            
            # Сохранение в Elasticsearch
            if event.severity in ["ERROR", "CRITICAL"]:
                self.es.index(
                    index=f"security-alerts-{datetime.now():%Y.%m.%d}",
                    document=event_dict
                )
            
            # Стандартное логирование
            log_message = (
                f"[{event.service}] {event.severity}: {event.message}"
                f"{f' (User: {event.user})' if event.user else ''}"
                f"{f' (IP: {event.ip_address})' if event.ip_address else ''}"
            )
            self.logger.log(
                logging.ERROR if event.severity in ["ERROR", "CRITICAL"] else logging.INFO,
                log_message
            )
            
        except Exception as e:
            self.logger.error(f"Failed to log security event: {e}")

    def log_auth_attempt(
        self,
        success: bool,
        username: str,
        ip_address: str,
        service: str = "auth"
    ) -> None:
        """Логирование попытки аутентификации."""
        event = SecurityEvent(
            event_type="auth",
            service=service,
            severity="INFO" if success else "WARNING",
            message=f"{'Successful' if success else 'Failed'} authentication attempt",
            user=username,
            ip_address=ip_address,
            additional_data={
                "auth_result": "SUCCESS" if success else "FAILED",
                "timestamp": datetime.now().isoformat()
            }
        )
        self.log_security_event(event)

    def log_rbac_violation(
        self,
        user: str,
        resource: str,
        action: str,
        role: str,
        service: str = "rbac"
    ) -> None:
        """Логирование нарушения RBAC."""
        event = SecurityEvent(
            event_type="rbac",
            service=service,
            severity="ERROR",
            message=f"RBAC violation: {action} {resource}",
            user=user,
            additional_data={
                "resource": resource,
                "action": action,
                "role": role,
                "timestamp": datetime.now().isoformat()
            }
        )
        self.log_security_event(event)

    def log_suspicious_activity(
        self,
        message: str,
        ip_address: Optional[str] = None,
        user: Optional[str] = None,
        service: str = "security"
    ) -> None:
        """Логирование подозрительной активности."""
        event = SecurityEvent(
            event_type="suspicious",
            service=service,
            severity="WARNING",
            message=message,
            user=user,
            ip_address=ip_address,
            additional_data={
                "timestamp": datetime.now().isoformat()
            }
        )
        self.log_security_event(event)

    def close(self) -> None:
        """Закрытие соединений."""
        if self.sock is not None:
            self.sock.close()
            self.sock = None


# Глобальный экземпляр логгера
security_logger = ELKLogger()
