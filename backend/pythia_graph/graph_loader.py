import os

from neo4j import GraphDatabase

# Настройки подключения
driver = GraphDatabase.driver("bolt://localhost:7687", auth=("neo4j", "test1234"))


# Функция загрузки .cypher построчно
def run_cypher_file(file_path):
    with open(file_path, encoding="utf-8") as file:
        content = file.read()

    # Разделяем команды по `;`
    statements = [stmt.strip() for stmt in content.split(";") if stmt.strip()]

    with driver.session() as session:
        for stmt in statements:
            try:
                session.run(stmt)
                print(f"✅ Выполнено: {stmt[:60]}...")
            except Exception as e:
                print(f"❌ Ошибка в команде: {stmt[:60]}...\n{e}")


# Путь до .cypher рядом с этим скриптом
cypher_path = os.path.join(os.path.dirname(__file__), "interaction_model.cypher")

# Запуск
if __name__ == "__main__":
    run_cypher_file(cypher_path)
    driver.close()
