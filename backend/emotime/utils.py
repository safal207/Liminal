"""
🌿✨ Emotime Utils — утилиты для правильной работы в разных окружениях

Решает проблемы с кодировкой, логированием и совместимостью
"""

import locale
import os
import sys
from typing import Any, Dict


def setup_encoding():
    """Настраивает корректную кодировку для Windows и других систем."""
    try:
        # Попытка настроить UTF-8
        if sys.platform.startswith("win"):
            # Windows: попробуем изменить кодовую страницу консоли
            try:
                import subprocess

                subprocess.run(["chcp", "65001"], shell=True, capture_output=True)
            except:
                pass

        # Установка переменных окружения для UTF-8
        os.environ["PYTHONIOENCODING"] = "utf-8"

        # Попробуем установить системную локаль на UTF-8
        try:
            locale.setlocale(locale.LC_ALL, "")
        except:
            pass

        return True
    except Exception as e:
        print(f"WARNING: Could not setup UTF-8 encoding: {e}")
        return False


def safe_print(message: str, fallback_encoding: str = "ascii"):
    """Безопасный вывод с fallback на ASCII."""
    try:
        print(message)
    except UnicodeEncodeError:
        # Fallback: заменяем проблемные символы
        try:
            safe_message = message.encode(fallback_encoding, errors="replace").decode(
                fallback_encoding
            )
            print(safe_message)
        except:
            # Последний fallback: только ASCII символы
            ascii_message = "".join(c if ord(c) < 128 else "?" for c in message)
            print(ascii_message)


def sanitize_for_console(text: str) -> str:
    """Очищает текст от проблемных символов для консоли."""
    # Заменяем эмодзи на ASCII эквиваленты
    emoji_map = {
        "🌿": "[leaf]",
        "✨": "[sparkle]",
        "💓": "[heart]",
        "❤️‍🔥": "[fire-heart]",
        "💔": "[broken-heart]",
        "🎯": "[target]",
        "⚡": "[lightning]",
        "💭": "[thought]",
        "😐": "[neutral]",
        "🧘‍♀️": "[meditation]",
        "💡": "[bulb]",
        "🔥": "[fire]",
        "✅": "[OK]",
        "❌": "[ERROR]",
        "⚠️": "[WARNING]",
        "📊": "[chart]",
        "🚀": "[rocket]",
    }

    result = text
    for emoji, replacement in emoji_map.items():
        result = result.replace(emoji, replacement)

    # Заменяем другие проблемные Unicode символы
    result = result.replace("█", "=").replace("░", "-")

    return result


def get_console_safe_symbols() -> Dict[str, str]:
    """Возвращает ASCII-безопасные символы для разных целей."""
    return {
        "heart_calm": "*",
        "heart_focus": "**",
        "heart_stress": "!",
        "heart_joy": "+",
        "progress_fill": "=",
        "progress_empty": "-",
        "bullet": "•",
        "arrow": "->",
        "check": "[OK]",
        "error": "[ERROR]",
        "warning": "[WARN]",
        "info": "[INFO]",
    }


class SafeLogger:
    """Безопасный логгер для разных кодировок."""

    def __init__(self, prefix: str = "Emotime"):
        self.prefix = prefix
        self.symbols = get_console_safe_symbols()

    def info(self, message: str):
        safe_message = (
            f"{self.symbols['info']} {self.prefix}: {sanitize_for_console(message)}"
        )
        safe_print(safe_message)

    def warning(self, message: str):
        safe_message = (
            f"{self.symbols['warning']} {self.prefix}: {sanitize_for_console(message)}"
        )
        safe_print(safe_message)

    def error(self, message: str):
        safe_message = (
            f"{self.symbols['error']} {self.prefix}: {sanitize_for_console(message)}"
        )
        safe_print(safe_message)

    def heartbeat(self, mode_name: str, confidence: float):
        """Выводит сердцебиение в ASCII-совместимом формате."""
        if mode_name == "calm":
            symbol = self.symbols["heart_calm"]
        elif mode_name == "focus":
            symbol = self.symbols["heart_focus"]
        elif mode_name == "stress":
            symbol = self.symbols["heart_stress"]
        elif mode_name == "joy":
            symbol = self.symbols["heart_joy"]
        else:
            symbol = "-"

        # Прогресс-бар из ASCII символов
        progress_length = 10
        filled = int(confidence * progress_length)
        progress_bar = self.symbols["progress_fill"] * filled + self.symbols[
            "progress_empty"
        ] * (progress_length - filled)

        message = f"{symbol} Emotime: {mode_name} | confidence: {progress_bar} {confidence:.1%}"
        safe_print(message)


# Глобальный безопасный логгер
safe_logger = SafeLogger()


# Автоматическая настройка кодировки при импорте
setup_encoding()
