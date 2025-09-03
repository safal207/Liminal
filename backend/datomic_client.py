"""
Модуль для работы с Datomic базой данных.
Обеспечивает подключение и базовые операции с данными.
"""

import os
from datetime import datetime
from typing import Any, Dict, List, Optional
from backend.storage.datomic_adapter import connect, _NoDatomic


class DatomicClient:
    """Клиент для работы с Datomic базой данных."""

    def __init__(self):
        """Инициализация клиента Datomic."""
        self.conn = None
        self.enabled = False
        try:
            self.conn = connect()
            self.enabled = True
            print("✅ Datomic client initialized successfully.")
        except _NoDatomic as e:
            print(f"⚠️ Datomic client is not available: {e}")

    def connect(self) -> bool:
        """Подключение к базе данных."""
        return self.enabled

    def create_database(self) -> bool:
        """Создание новой базы данных."""
        if not self.enabled:
            return False
        print("⚠️ create_database is not implemented in the adapter.")
        return False

    def transact(self, data: List[Dict]) -> Dict:
        """Выполнение транзакции."""
        if not self.enabled or not self.conn:
            raise ConnectionError("Datomic client is not connected.")
        try:
            result = self.conn.transact(data)
            print("✅ Транзакция успешно выполнена")
            return result
        except Exception as e:
            print(f"❌ Ошибка при выполнении транзакции: {str(e)}")
            raise

    def query(self, query: str, params: Optional[Dict] = None) -> List[Dict]:
        """Выполнение запроса к базе данных."""
        if not self.enabled or not self.conn:
            raise ConnectionError("Datomic client is not connected.")
        try:
            db = self.conn.db()
            if params:
                result = db.query(query, params)
            else:
                result = db.query(query)
            return result
        except Exception as e:
            print(f"❌ Ошибка при выполнении запроса: {str(e)}")
            raise

    def add_emotion_entry(
        self,
        user_id: str,
        emotion: str,
        intensity: float,
        timestamp: Optional[datetime] = None,
    ) -> Dict:
        """Добавление записи об эмоции."""
        if not self.enabled:
            return {}
        if not timestamp:
            timestamp = datetime.utcnow()
        data = [
            {
                "db/id": "emotion-temp",
                "entry/user": user_id,
                "entry/emotion": emotion,
                "entry/intensity": float(intensity),
                "entry/timestamp": timestamp,
            }
        ]
        return self.transact(data)

    def get_emotion_history(self, user_id: str, limit: int = 100) -> List[Dict]:
        """Получение истории эмоций пользователя."""
        if not self.enabled:
            return []
        query = """
        [:find ?e ?emotion ?intensity ?timestamp
         :in $ ?user ?limit
         :where
         [?e :entry/user ?user]
         [?e :entry/emotion ?emotion]
         [?e :entry/intensity ?intensity]
         [?e :entry/timestamp ?timestamp]]
        """
        results = self.query(query, {"?user": user_id, "?limit": limit})
        return [
            {
                "id": str(r[0]),
                "emotion": r[1],
                "intensity": float(r[2]),
                "timestamp": r[3],
            }
            for r in results
        ]

    def close(self):
        """Закрытие соединения с базой данных."""
        if self.conn:
            self.conn.release()
            self.conn = None
            print("🔌 Соединение с базой данных закрыто")


if __name__ == "__main__":
    client = DatomicClient()
    if client.connect():
        try:
            result = client.add_emotion_entry(
                user_id="user-123", emotion="радость", intensity=0.8
            )
            print(f"Добавлена запись: {result}")
            history = client.get_emotion_history("user-123")
            print(f"История эмоций: {history}")
        except Exception as e:
            print(f"Произошла ошибка: {str(e)}")
        finally:
            client.close()
