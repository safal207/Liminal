from datetime import datetime

from neo4j import GraphDatabase


class FlowCoordinator:
    def __init__(self, uri="bolt://localhost:7687", user="neo4j", password="test1234"):
        self.driver = GraphDatabase.driver(uri, auth=(user, password))

    def close(self):
        self.driver.close()

    def create_concept(self, concept_name):
        with self.driver.session() as session:
            session.run("CREATE (c:Concept {name: $name})", name=concept_name)
            print(f"🌟 Пифия Графа: создан узел смысла '{concept_name}'")

    def create_relationship(self, concept1, concept2, rel_type):
        with self.driver.session() as session:
            session.run(
                """
                MATCH (a:Concept {name: $concept1}), (b:Concept {name: $concept2})
                CREATE (a)-[:RELATES_TO {type: $rel_type}]->(b)
            """,
                concept1=concept1,
                concept2=concept2,
                rel_type=rel_type,
            )
            print(f"🔗 Связь '{rel_type}' между '{concept1}' → '{concept2}'")

    def find_concept(self, concept_name):
        with self.driver.session() as session:
            result = session.run(
                "MATCH (c:Concept {name: $name}) RETURN c", name=concept_name
            )
            record = result.single()
            if record:
                print(f"🔍 Найден узел '{concept_name}'")
                return record["c"]
            else:
                print(f"❔ Узел '{concept_name}' не найден")
                return None

    def update_concept(self, concept_name, properties):
        with self.driver.session() as session:
            session.run(
                "MATCH (c:Concept {name: $name}) SET c += $props",
                name=concept_name,
                props=properties,
            )
            print(f"🔄 Обновлён узел '{concept_name}' с {properties}")

    def find_path(self, concept1, concept2, max_depth=5):
        with self.driver.session() as session:
            result = session.run(
                f"""
                MATCH path = shortestPath((a:Concept {{name: $concept1}})-[*..{max_depth}]-(b:Concept {{name: $concept2}}))
                RETURN path
            """,
                concept1=concept1,
                concept2=concept2,
            )
            record = result.single()
            if record:
                print(f"🧭 Путь от '{concept1}' к '{concept2}' найден")
                return record["path"]
            else:
                print(f"❔ Путь от '{concept1}' к '{concept2}' не найден")
                return None

    def translate_concept(self, concept_name, from_lang="ru", to_lang="en"):
        with self.driver.session() as session:
            result = session.run(
                """
                MATCH (c:Concept {name: $concept_name, lang: $from_lang})-[:TRANSLATES_TO]->(t:Concept {lang: $to_lang})
                RETURN t.name AS translated_name
            """,
                concept_name=concept_name,
                from_lang=from_lang,
                to_lang=to_lang,
            )
            record = result.single()
            return record["translated_name"] if record else None

    def pre_emotion_prompt(self, user_input):
        print("🛑 Шаг назад: Остановись. Побудь в тишине.")
        print("🌌 Почувствуй: не нужно искать ответ — просто задай себе вопрос:")
        print("💭 «Что я чувствую прямо сейчас?»")
        return input("👤 Вы (после паузы): ")

    def generate_response(self, user_input, concept="страх", target="надежда"):
        print("🔁 Перед анализом, система предлагает шаг назад.")
        user_feeling = self.pre_emotion_prompt(user_input)
        print(f"📥 Зафиксировано ощущение: '{user_feeling}'")
        path = self.find_path(concept, target)
        if not path:
            return f"Я чувствую твоё '{concept}', но путь к '{target}' пока сокрыт. Мы найдём его вместе."
        return (
            f"🌌 Я слышу в тебе '{concept}'.\n"
            f"🔗 Вот путь, который я вижу: {concept} → {' → '.join([n['name'] for n in path.nodes[1:-1]])} → {target}.\n"
            f"✨ Каждый шаг — это внутренняя трансформация. И ты движешься к '{target}'."
        )
