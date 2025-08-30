#!/usr/bin/env python
"""
Интеллектуальная система логирования для OpenAI адаптера

Система логирования для анализа работы OpenAI адаптера с четырьмя категориями:
1. Experience (опыт) - запись всех возникающих ошибок
2. Insights (инсайты) - запись решений проблем
3. Karma (карма) - анализ повторяющихся ошибок
4. Hypotheses (гипотезы) - запись идей при тупиковых ситуациях

Этот подход позволяет систематически анализировать проблемы,
находить решения и избегать повторяющихся ошибок.
"""

import hashlib
import json
import os
import threading
from datetime import datetime
from pathlib import Path
from typing import Any


class AdapterLogger:
    """
    Интеллектуальный логгер для анализа работы OpenAI адаптера
    """

    def __init__(self, log_dir: str | None = None):
        """
        Инициализация логгера с указанием директории для логов

        Args:
            log_dir: Директория для хранения логов
        """
        self.log_dir = log_dir or str(Path(__file__).parent.parent / "logs" / "adapter")
        self.error_counters = {}  # Счетчики повторений ошибок
        self.insights_registry = {}  # Реестр инсайтов
        self._lock = threading.Lock()  # Для потокобезопасности

        # Создаем директорию, если она не существует
        os.makedirs(self.log_dir, exist_ok=True)

        # Пути к файлам логов
        self.experience_path = os.path.join(self.log_dir, "experience.log")
        self.insights_path = os.path.join(self.log_dir, "insights.log")
        self.karma_path = os.path.join(self.log_dir, "karma.log")
        self.hypotheses_path = os.path.join(self.log_dir, "hypotheses.log")

        # Инициализируем файлы, если они не существуют
        for path in [
            self.experience_path,
            self.insights_path,
            self.karma_path,
            self.hypotheses_path,
        ]:
            if not os.path.exists(path):
                with open(path, "w", encoding="utf-8") as f:
                    f.write(f"# {os.path.basename(path)} - создан {datetime.now().isoformat()}\n\n")

    def log_error(self, error_message: str, context: dict[str, Any] = None) -> str:
        """
        Записывает ошибку в файл опыта (experience.log)

        Args:
            error_message: Текст ошибки
            context: Контекст ошибки (словарь с дополнительной информацией)

        Returns:
            Хеш ошибки для последующего отслеживания
        """
        # Генерируем уникальный хеш для ошибки
        error_hash = self._generate_error_hash(error_message, context)

        # Инкрементируем счетчик для этой ошибки
        with self._lock:
            self.error_counters[error_hash] = self.error_counters.get(error_hash, 0) + 1
            error_count = self.error_counters[error_hash]

        # Формируем запись для лога
        timestamp = datetime.now().isoformat()
        log_entry = {
            "timestamp": timestamp,
            "error_hash": error_hash,
            "error_message": error_message,
            "context": context or {},
            "occurrence": error_count,
        }

        # Записываем в файл опыта
        with open(self.experience_path, "a", encoding="utf-8") as f:
            f.write(f"\n## Ошибка {error_hash[:8]} ({timestamp})\n")
            f.write(f"```\n{json.dumps(log_entry, ensure_ascii=False, indent=2)}\n```\n")

        # Если ошибка повторилась, обновляем файл кармы
        if error_count > 1:
            self._update_karma(error_hash, error_message, context, error_count)

        return error_hash

    def log_insight(self, error_hash: str, solution: str, details: dict[str, Any] = None) -> None:
        """
        Записывает инсайт (решение проблемы) в файл инсайтов (insights.log)

        Args:
            error_hash: Хеш ошибки, для которой найдено решение
            solution: Текст решения
            details: Детали решения
        """
        timestamp = datetime.now().isoformat()

        # Формируем запись для лога
        log_entry = {
            "timestamp": timestamp,
            "error_hash": error_hash,
            "solution": solution,
            "details": details or {},
        }

        # Записываем в файл инсайтов
        with open(self.insights_path, "a", encoding="utf-8") as f:
            f.write(f"\n## Инсайт для {error_hash[:8]} ({timestamp})\n")
            f.write(f"```\n{json.dumps(log_entry, ensure_ascii=False, indent=2)}\n```\n")

        # Обновляем реестр инсайтов
        with self._lock:
            self.insights_registry[error_hash] = log_entry

    def log_hypothesis(
        self, issue_description: str, hypothesis: str, experiment: str = None
    ) -> None:
        """
        Записывает гипотезу для сложной проблемы в файл гипотез (hypotheses.log)

        Args:
            issue_description: Описание проблемы
            hypothesis: Текст гипотезы
            experiment: Предлагаемый эксперимент для проверки гипотезы
        """
        timestamp = datetime.now().isoformat()

        # Формируем запись для лога
        log_entry = {
            "timestamp": timestamp,
            "issue": issue_description,
            "hypothesis": hypothesis,
            "experiment": experiment,
            "status": "open",
        }

        # Записываем в файл гипотез
        with open(self.hypotheses_path, "a", encoding="utf-8") as f:
            f.write(f"\n## Гипотеза от {timestamp}\n")
            f.write(f"```\n{json.dumps(log_entry, ensure_ascii=False, indent=2)}\n```\n")

    def resolve_hypothesis(self, issue_description: str, result: str, is_confirmed: bool) -> None:
        """
        Отмечает гипотезу как подтвержденную или опровергнутую

        Args:
            issue_description: Описание проблемы (для поиска)
            result: Результат проверки гипотезы
            is_confirmed: True если гипотеза подтвердилась, иначе False
        """
        timestamp = datetime.now().isoformat()

        # Формируем запись для лога
        log_entry = {
            "timestamp": timestamp,
            "issue": issue_description,
            "result": result,
            "status": "confirmed" if is_confirmed else "rejected",
        }

        # Записываем в файл гипотез
        with open(self.hypotheses_path, "a", encoding="utf-8") as f:
            f.write(f"\n## Результат гипотезы от {timestamp}\n")
            f.write(f"```\n{json.dumps(log_entry, ensure_ascii=False, indent=2)}\n```\n")

    def _update_karma(
        self, error_hash: str, error_message: str, context: dict[str, Any], count: int
    ) -> None:
        """
        Обновляет файл кармы при повторной ошибке

        Args:
            error_hash: Хеш ошибки
            error_message: Сообщение об ошибке
            context: Контекст ошибки
            count: Количество повторений ошибки
        """
        timestamp = datetime.now().isoformat()

        # Проверяем, есть ли инсайт для этой ошибки
        insight = self.insights_registry.get(error_hash)

        # Формируем запись для лога кармы
        log_entry = {
            "timestamp": timestamp,
            "error_hash": error_hash,
            "error_message": error_message,
            "context": context,
            "count": count,
            "has_insight": insight is not None,
            "insight": insight if insight else None,
        }

        # Записываем в файл кармы
        with open(self.karma_path, "a", encoding="utf-8") as f:
            f.write(f"\n## Карма ошибки {error_hash[:8]} ({timestamp})\n")
            f.write(f"```\n{json.dumps(log_entry, ensure_ascii=False, indent=2)}\n```\n")

    def _generate_error_hash(self, error_message: str, context: dict[str, Any] = None) -> str:
        """
        Генерирует уникальный хеш для ошибки на основе сообщения и контекста

        Args:
            error_message: Сообщение об ошибке
            context: Контекст ошибки

        Returns:
            Хеш ошибки (строка)
        """
        # Создаем строку для хеширования
        context_str = json.dumps(context or {}, sort_keys=True)
        hash_input = f"{error_message}{context_str}"

        # Генерируем MD5 хеш
        return hashlib.md5(hash_input.encode("utf-8")).hexdigest()

    def get_error_stats(self) -> dict[str, Any]:
        """
        Возвращает статистику по ошибкам

        Returns:
            Словарь со статистикой
        """
        return {
            "total_errors": sum(self.error_counters.values()),
            "unique_errors": len(self.error_counters),
            "repeating_errors": sum(1 for count in self.error_counters.values() if count > 1),
            "solved_errors": len(self.insights_registry),
            "top_errors": sorted(
                [(hash, count) for hash, count in self.error_counters.items()],
                key=lambda x: x[1],
                reverse=True,
            )[:5],
        }


# Глобальный экземпляр логгера
adapter_logger = AdapterLogger()


# Функции для упрощенного доступа к логгеру
def log_error(error_message: str, context: dict[str, Any] = None) -> str:
    return adapter_logger.log_error(error_message, context)


def log_insight(error_hash: str, solution: str, details: dict[str, Any] = None) -> None:
    adapter_logger.log_insight(error_hash, solution, details)


def log_hypothesis(issue_description: str, hypothesis: str, experiment: str = None) -> None:
    adapter_logger.log_hypothesis(issue_description, hypothesis, experiment)


def resolve_hypothesis(issue_description: str, result: str, is_confirmed: bool) -> None:
    adapter_logger.resolve_hypothesis(issue_description, result, is_confirmed)
