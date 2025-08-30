from datetime import datetime, timedelta

from datomic_client import DatomicClient


def test_datomic_operations():
    """Тестирование основных операций с Datomic."""
    print("🚀 Запуск теста работы с Datomic...")

    # Инициализация клиента
    client = DatomicClient()

    try:
        # Попытка подключиться к существующей базе данных
        if not client.connect():
            print("ℹ️ База данных не найдена. Создаем новую...")
            if not client.create_database():
                print("❌ Не удалось создать базу данных")
                return

        # Добавление тестовых данных
        emotions = ["радость", "грусть", "гнев", "страх", "удивление", "доверие"]
        user_id = "test-user-1"

        print("\n➕ Добавляем тестовые данные...")
        for i, emotion in enumerate(emotions):
            # Создаем временные метки с разницей в днях
            timestamp = datetime.utcnow() - timedelta(days=len(emotions) - i)
            intensity = 0.1 + (i * 0.15)  # Разная интенсивность
            if intensity > 1.0:
                intensity = 1.0

            client.add_emotion_entry(
                user_id=user_id,
                emotion=emotion,
                intensity=intensity,
                timestamp=timestamp,
            )
            print(f"  - Добавлена эмоция '{emotion}' с интенсивностью {intensity:.2f}")

        # Получение истории
        print("\n📜 Получаем историю эмоций...")
        history = client.get_emotion_history(user_id)

        if not history:
            print("❌ История пуста")
            return

        print("\n📊 История эмоций:")
        for entry in history:
            print(f"  - {entry['emotion']}: {entry['intensity']:.2f} ({entry['timestamp']})")

        # Пример запроса с использованием Datalog
        print("\n🔍 Выполняем сложный запрос...")
        query = """
        [:find ?emotion (avg ?intensity) (count ?e)
         :in $ ?user
         :where
         [?e :entry/user ?user]
         [?e :entry/emotion ?emotion]
         [?e :entry/intensity ?intensity]]
        """

        results = client.query(query, {"?user": user_id})

        print("\n📊 Статистика по эмоциям:")
        for emotion, avg_intensity, count in results:
            print(
                f"  - {emotion}: средняя интенсивность {float(avg_intensity):.2f} (всего записей: {count})"
            )

        print("\n✅ Все тесты успешно пройдены!")

    except Exception as e:
        print(f"\n❌ Произошла ошибка: {str(e)}")
        import traceback

        traceback.print_exc()
    finally:
        client.close()


if __name__ == "__main__":
    test_datomic_operations()
