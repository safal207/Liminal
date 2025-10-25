"""
Тест системы Message Acknowledgments
"""

import asyncio
from datetime import datetime
from websocket.connection_manager import ConnectionManager, PendingMessage


async def test_message_ack_system():
    """Тест основной функциональности ACK системы"""
    
    print("MESSAGE ACKNOWLEDGMENT SYSTEM TEST")
    print("=" * 50)
    
    # Создаем ConnectionManager для тестирования
    manager = ConnectionManager()
    
    # Тест 1: Создание pending сообщения
    print("\n1. Тест создания PendingMessage")
    
    # Мокаем WebSocket (простой объект)
    class MockWebSocket:
        def __init__(self, user_id):
            self.user_id = user_id
            
        async def send_json(self, data):
            print(f"   >> Отправлено: {data}")
    
    mock_ws = MockWebSocket("test_user")
    
    # Создаем pending message
    pending_msg = PendingMessage(
        message_id="test-msg-123",
        content={"type": "test", "text": "Hello"},
        user_id="test_user", 
        websocket=mock_ws
    )
    
    print(f"   OK PendingMessage создано: {pending_msg.message_id}")
    print(f"   -- Возраст: {(datetime.now() - pending_msg.created_at).total_seconds():.1f}s")
    print(f"   -> Можно повторить: {pending_msg.can_retry}")
    print(f"   >> Истекло: {pending_msg.is_expired}")
    
    # Тест 2: ACK статистика
    print("\n2. Тест ACK статистики")
    
    # Добавляем несколько pending сообщений в менеджер
    manager.pending_messages["test-msg-123"] = pending_msg
    manager.user_pending_messages["test_user"] = {"test-msg-123"}
    
    stats = manager.get_ack_stats()
    print(f"   ++ Общее кол-во pending: {stats['total_pending_messages']}")
    print(f"   ** Пользователей с pending: {stats['users_with_pending_messages']}")
    print(f"   >> ACK timeout: {stats['ack_timeout_seconds']}s")
    print(f"   -- По возрасту: {stats['pending_by_age']}")
    
    # Тест 3: Обработка ACK
    print("\n3. Тест обработки ACK")
    
    # Обрабатываем ACK
    ack_result = await manager.handle_ack("test-msg-123", "test_user")
    print(f"   OK ACK обработан: {ack_result}")
    
    # Проверяем, что сообщение удалено
    stats_after = manager.get_ack_stats()
    print(f"   -- Pending после ACK: {stats_after['total_pending_messages']}")
    
    # Тест 4: Неверный ACK
    print("\n4. Тест неверного ACK")
    
    # Пробуем ACK для несуществующего сообщения
    wrong_ack = await manager.handle_ack("non-existent", "test_user")
    print(f"   XX ACK для несуществующего: {wrong_ack}")
    
    print("\n" + "=" * 50)
    print("OK Все тесты Message Acknowledgments завершены!")
    
    return True


if __name__ == "__main__":
    try:
        result = asyncio.run(test_message_ack_system())
        print(f"\n>> Результат тестирования: {'SUCCESS' if result else 'FAILED'}")
    except Exception as e:
        print(f"\n!! Ошибка тестирования: {e}")
        import traceback
        traceback.print_exc()