"""
Модуль для работы с Datomic базой данных.
Обеспечивает подключение и базовые операции с данными.
"""

from datetime import datetime
from typing import Dict, List, Optional

from datomic import Client, Connection


class DatomicClient:
    """Клиент для работы с Datomic базой данных."""

    def __init__(
        self,
        uri: str = "http://localhost:8080",
        db_name: str = "liminal",
        storage_type: str = "dev",
    ):
        """
        Инициализация клиента Datomic.

        Args:
            uri: Адрес сервера Datomic (по умолчанию: http://localhost:8080)
            db_name: Имя базы данных (по умолчанию: liminal)
            storage_type: Тип хранилища (по умолчанию: dev)
        """
        self.uri = uri
        self.db_name = db_name
        self.storage_type = storage_type
        self.conn = None

    def connect(self) -> bool:
        """Подключение к базе данных."""
        try:
            client = Client(self.uri, self.db_name, self.storage_type)
            self.conn = Connection(client)
            print(f"✅ Успешно подключено к базе данных {self.db_name}")
            return True
        except Exception as e:
            print(f"❌ Ошибка подключения к Datomic: {str(e)}")
            return False

    def create_database(self) -> bool:
        """Создание новой базы данных."""
        try:
            client = Client(self.uri, self.db_name, self.storage_type)
            client.create_database()
            self.conn = Connection(client)
            print(f"✅ База данных {self.db_name} успешно создана")
            return True
        except Exception as e:
            print(f"❌ Ошибка при создании базы данных: {str(e)}")
            return False

    def transact(self, data: List[Dict]) -> Dict:
        """
        Выполнение транзакции.

        Args:
            data: Список словарей с данными для транзакции

        Returns:
            Результат выполнения транзакции
        """
        if not self.conn:
            raise ConnectionError("Сначала подключитесь к базе данных")

        try:
            result = self.conn.transact(data)
            print("✅ Транзакция успешно выполнена")
            return result
        except Exception as e:
            print(f"❌ Ошибка при выполнении транзакции: {str(e)}")
            raise

    def query(self, query: str, params: Optional[Dict] = None) -> List[Dict]:
        """
        Выполнение запроса к базе данных.

        Args:
            query: Datalog запрос в виде строки
            params: Параметры запроса

        Returns:
            Список результатов запроса
        """
        if not self.conn:
            raise ConnectionError("Сначала подключитесь к базе данных")

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
        """
        Добавление записи об эмоции.

        Args:
            user_id: ID пользователя
            emotion: Название эмоции
            intensity: Интенсивность эмоции (от 0.0 до 1.0)
            timestamp: Временная метка (по умолчанию текущее время)

        Returns:
            Результат транзакции
        """
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
        """
        Получение истории эмоций пользователя.

        Args:
            user_id: ID пользователя
            limit: Максимальное количество записей

        Returns:
            Список записей об эмоциях
        """
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


# Пример использования
if __name__ == "__main__":
    # Инициализация клиента
    client = DatomicClient()

    try:
        # Подключение к существующей базе данных или создание новой
        if not client.connect():
            print("Попытка создать новую базу данных...")
            client.create_database()

        # Пример добавления записи
        result = client.add_emotion_entry(
            user_id="user-123", emotion="радость", intensity=0.8
        )
        print(f"Добавлена запись: {result}")

        # Пример запроса истории
        history = client.get_emotion_history("user-123")
        print(f"История эмоций: {history}")

    except Exception as e:
        print(f"Произошла ошибка: {str(e)}")
    finally:
        # Всегда закрываем соединение
        client.close()
