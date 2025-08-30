from neo4j import GraphDatabase


class FlowCoordinator:
    def __init__(self, uri="bolt://localhost:7687", user="neo4j", password="test1234"):
        self.driver = GraphDatabase.driver(uri, auth=(user, password))

    def close(self):
        self.driver.close()

    def find_path(self, start, goal, max_depth=4):
        query = f"""
        MATCH path = shortestPath((a:Concept {{name: $start}})-[*..{max_depth}]-(b:Concept {{name: $goal}}))
        RETURN nodes(path) AS concepts
        """
        with self.driver.session() as session:
            result = session.run(query, start=start, goal=goal)
            record = result.single()
            return [node["name"] for node in record["concepts"]] if record else []

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

    def get_chakra_color(self, emotion):
        chakra_map = {
            "страх": "🔴 (Муладхара / Корневая чакра)",
            "желание": "🟠 (Свадхистана / Сакральная чакра)",
            "сила": "🟡 (Манипура / Чакра солнечного сплетения)",
            "любовь": "💚 (Анахата / Сердечная чакра)",
            "выражение": "🔵 (Вишуддха / Горловая чакра)",
            "интуиция": "🟣 (Аджна / Третий глаз)",
            "тишина": "⚪ (Сахасрара / Коронная чакра)",
        }
        return chakra_map.get(emotion.lower(), "⚫ (Неизвестная зона восприятия)")

    def pre_emotion_prompt(self, user_input):
        print("🔁 Система предлагает сначала сделать шаг назад.")
        print("🛑 Шаг назад: Остановись. Побудь в тишине.")
        print("🌌 Почувствуй: не нужно искать ответ — просто задай себе вопрос:")
        print("💭 «Что я чувствую прямо сейчас?»")
        emotion = input("👤 Вы (после паузы): ").strip().lower()
        print(f"📥 Зафиксировано ощущение: '{emotion}'")
        return emotion

    def generate_response(self, user_input, concept="страх", target="надежда"):
        emotion = self.pre_emotion_prompt(user_input)
        chakra_info = self.get_chakra_color(emotion)

        translated_concept = (
            self.translate_concept(concept, from_lang="ru", to_lang="en") or concept
        )
        translated_target = self.translate_concept(target, from_lang="ru", to_lang="en") or target

        path = self.find_path(translated_concept, translated_target)

        if not path:
            return f"🧠 Твоя эмоция: {emotion} {chakra_info}\nПуть не найден от '{concept}' к '{target}'. Попробуем с другой стороны."

        steps = " → ".join(path)
        return (
            f"🧠 Твоя эмоция: {emotion} {chakra_info}\n"
            f"🌌 Я слышу в тебе '{concept}'.\n"
            f"🔗 Вот путь, который я вижу: {steps}.\n"
            f"✨ Каждый шаг — это внутренняя трансформация. И ты движешься к '{target}'."
        )
