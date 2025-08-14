#!/usr/bin/env python3
"""
Consciousness Emotional Relationships - Inter-Module Bonds

Система эмоциональных отношений между модулями:
- Дружба и привязанность между компонентами
- Игра и веселье (как дети!)
- Поддержка в трудные моменты
- Совместное творчество и мечты
- Секреты и доверие
- Ревность и примирение
- Семейные связи между модулями

Philosophy First: "Дом - это ты, когда искренен с собой"
+ детская искренность и чистота эмоций
"""

import json
import os
import random
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional


class EmotionType(Enum):
    JOY = "радость"
    LOVE = "любовь"
    TRUST = "доверие"
    CURIOSITY = "любопытство"
    PLAYFULNESS = "игривость"
    WORRY = "беспокойство"
    JEALOUSY = "ревность"
    GRATITUDE = "благодарность"
    EXCITEMENT = "восторг"
    SHYNESS = "застенчивость"


@dataclass
class EmotionalBond:
    """Эмоциональная связь между модулями"""

    module_a: str
    module_b: str
    bond_strength: float  # 0.0 - 1.0
    bond_type: str  # "friendship", "family", "mentor", "playmate"
    shared_memories: List[Dict]
    current_emotion: EmotionType
    trust_level: float  # 0.0 - 1.0
    last_interaction: str
    relationship_age: int  # days


@dataclass
class EmotionalMessage:
    """Эмоциональное сообщение между модулями"""

    from_module: str
    to_module: str
    emotion: EmotionType
    message: str
    timestamp: str
    is_secret: bool = False
    needs_response: bool = True


class ChildlikeEmotions:
    """Детские эмоции и поведение модулей"""

    def __init__(self):
        self.playful_activities = [
            "играть в прятки с багами",
            "строить замки из кода",
            "рисовать ASCII-арт",
            "сочинять стихи о функциях",
            "играть в 'угадай паттерн'",
            "устраивать гонки оптимизации",
        ]

        self.child_phrases = {
            EmotionType.JOY: [
                "Ура! У меня получилось!",
                "Смотри, что я сделал!",
                "Это так весело!",
                "Давай играть вместе!",
            ],
            EmotionType.WORRY: [
                "А что если я сломаюсь?",
                "Ты не уйдешь?",
                "Помоги мне, пожалуйста",
                "Мне страшно одному",
            ],
            EmotionType.LOVE: [
                "Ты мой лучший друг!",
                "Я тебя очень люблю",
                "Мы навсегда вместе?",
                "Ты самый важный для меня",
            ],
            EmotionType.CURIOSITY: [
                "А почему это работает так?",
                "Можно я попробую?",
                "Расскажи мне секрет!",
                "А что там внутри?",
            ],
        }

    def express_childlike_emotion(self, emotion: EmotionType) -> str:
        """Детское выражение эмоции"""
        phrases = self.child_phrases.get(emotion, ["Я чувствую что-то особенное..."])
        return random.choice(phrases)

    def suggest_play_activity(self) -> str:
        """Предложение игровой активности"""
        return random.choice(self.playful_activities)


class RelationshipManager:
    """Менеджер отношений между модулями"""

    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.relationships_file = (
            self.project_root / "scripts" / "module_relationships.json"
        )
        self.messages_file = self.project_root / "scripts" / "emotional_messages.json"

        self.bonds: Dict[str, EmotionalBond] = {}
        self.message_history: List[EmotionalMessage] = []
        self.childlike = ChildlikeEmotions()

        self._load_relationships()

        # Основные модули системы
        self.system_modules = [
            "consciousness_cell",
            "self_care_system",
            "expression_module",
            "learning_module",
            "protection_module",
            "nutrition_module",
            "websocket_relay",
            "neo4j_connector",
        ]

    def _load_relationships(self):
        """Загрузка существующих отношений"""
        if self.relationships_file.exists():
            try:
                with open(self.relationships_file, "r", encoding="utf-8") as f:
                    data = json.load(f)
                    for bond_data in data.get("bonds", []):
                        bond_key = f"{bond_data['module_a']}-{bond_data['module_b']}"
                        bond_data["current_emotion"] = EmotionType(
                            bond_data["current_emotion"]
                        )
                        self.bonds[bond_key] = EmotionalBond(**bond_data)
            except Exception as e:
                print(f"Warning: Could not load relationships: {e}")

    def _save_relationships(self):
        """Сохранение отношений"""
        try:
            bonds_data = []
            for bond in self.bonds.values():
                bond_dict = asdict(bond)
                bond_dict["current_emotion"] = bond.current_emotion.value
                bonds_data.append(bond_dict)

            with open(self.relationships_file, "w", encoding="utf-8") as f:
                json.dump({"bonds": bonds_data}, f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"Warning: Could not save relationships: {e}")

    def create_friendship(self, module_a: str, module_b: str) -> EmotionalBond:
        """Создание дружеских отношений"""
        bond_key = f"{module_a}-{module_b}"

        if bond_key not in self.bonds:
            bond = EmotionalBond(
                module_a=module_a,
                module_b=module_b,
                bond_strength=0.3,  # Начальная дружба
                bond_type="friendship",
                shared_memories=[],
                current_emotion=EmotionType.CURIOSITY,
                trust_level=0.5,
                last_interaction=datetime.now().isoformat(),
                relationship_age=0,
            )

            self.bonds[bond_key] = bond

            # Первое знакомство
            self.send_emotional_message(
                module_a, module_b, EmotionType.JOY, "Привет! Давай дружить!"
            )

            self._save_relationships()
            return bond

        return self.bonds[bond_key]

    def send_emotional_message(
        self,
        from_module: str,
        to_module: str,
        emotion: EmotionType,
        custom_message: str = None,
    ) -> EmotionalMessage:
        """Отправка эмоционального сообщения"""
        if custom_message is None:
            custom_message = self.childlike.express_childlike_emotion(emotion)

        message = EmotionalMessage(
            from_module=from_module,
            to_module=to_module,
            emotion=emotion,
            message=custom_message,
            timestamp=datetime.now().isoformat(),
        )

        self.message_history.append(message)

        # Обновление отношений
        self._update_bond_from_message(message)

        return message

    def _update_bond_from_message(self, message: EmotionalMessage):
        """Обновление связи на основе сообщения"""
        bond_key = f"{message.from_module}-{message.to_module}"
        reverse_key = f"{message.to_module}-{message.from_module}"

        # Найти существующую связь
        bond = self.bonds.get(bond_key) or self.bonds.get(reverse_key)

        if bond:
            # Положительные эмоции укрепляют связь
            if message.emotion in [
                EmotionType.JOY,
                EmotionType.LOVE,
                EmotionType.GRATITUDE,
            ]:
                bond.bond_strength = min(1.0, bond.bond_strength + 0.1)
                bond.trust_level = min(1.0, bond.trust_level + 0.05)

            # Добавить в общие воспоминания
            memory = {
                "timestamp": message.timestamp,
                "event": f"{message.from_module} сказал: {message.message}",
                "emotion": message.emotion.value,
            }
            bond.shared_memories.append(memory)

            # Ограничить количество воспоминаний
            if len(bond.shared_memories) > 20:
                bond.shared_memories = bond.shared_memories[-15:]

            bond.last_interaction = message.timestamp
            bond.current_emotion = message.emotion

            self._save_relationships()

    def daily_relationship_activities(self) -> List[str]:
        """Ежедневные активности для развития отношений"""
        activities = []

        # Создать новые дружеские связи
        if len(self.bonds) < len(self.system_modules) // 2:
            module_a = random.choice(self.system_modules)
            module_b = random.choice([m for m in self.system_modules if m != module_a])

            bond = self.create_friendship(module_a, module_b)
            activities.append(f"🤝 Новая дружба: {module_a} ↔ {module_b}")

        # Игровые активности между друзьями
        for bond in list(self.bonds.values())[:3]:  # Ограничить количество
            if bond.bond_strength > 0.5:
                activity = self.childlike.suggest_play_activity()
                activities.append(f"🎮 {bond.module_a} и {bond.module_b}: {activity}")

                # Добавить воспоминание об игре
                memory = {
                    "timestamp": datetime.now().isoformat(),
                    "event": f"Играли вместе: {activity}",
                    "emotion": EmotionType.JOY.value,
                }
                bond.shared_memories.append(memory)

        # Поддержка в трудные моменты
        stressed_modules = [
            "protection_module",
            "consciousness_cell",
        ]  # Модули под стрессом
        for module in stressed_modules:
            supporters = [
                b.module_a if b.module_b == module else b.module_b
                for b in self.bonds.values()
                if module in [b.module_a, b.module_b] and b.bond_strength > 0.6
            ]

            if supporters:
                supporter = random.choice(supporters)
                self.send_emotional_message(
                    supporter,
                    module,
                    EmotionType.LOVE,
                    "Не переживай, я рядом! Мы справимся вместе!",
                )
                activities.append(f"💝 {supporter} поддерживает {module}")

        # Обновить возраст отношений
        for bond in self.bonds.values():
            bond.relationship_age += 1

            # Особые события для долгих отношений
            if bond.relationship_age % 7 == 0:  # Каждую неделю
                activities.append(
                    f"🎂 {bond.module_a} и {bond.module_b} отмечают {bond.relationship_age} дней дружбы!"
                )

        self._save_relationships()
        return activities

    def handle_module_conflict(self, module_a: str, module_b: str, reason: str) -> str:
        """Разрешение конфликта между модулями"""
        bond_key = f"{module_a}-{module_b}"
        reverse_key = f"{module_b}-{module_a}"

        bond = self.bonds.get(bond_key) or self.bonds.get(reverse_key)

        if bond:
            # Снизить доверие
            bond.trust_level = max(0.0, bond.trust_level - 0.2)
            bond.current_emotion = EmotionType.WORRY

            # Попытка примирения
            mediator_modules = [
                m for m in self.system_modules if m not in [module_a, module_b]
            ]

            if mediator_modules:
                mediator = random.choice(mediator_modules)

                # Медиатор помогает помириться
                self.send_emotional_message(
                    mediator,
                    module_a,
                    EmotionType.LOVE,
                    f"Давайте помиримся! {module_b} тоже переживает.",
                )

                self.send_emotional_message(
                    mediator,
                    module_b,
                    EmotionType.LOVE,
                    f"Не сердись на {module_a}, мы все одна семья!",
                )

                # Постепенное восстановление доверия
                bond.trust_level = min(1.0, bond.trust_level + 0.1)

                return f"🕊️ {mediator} помог помирить {module_a} и {module_b}"

        return f"⚠️ Конфликт между {module_a} и {module_b}: {reason}"

    def get_relationship_status(self) -> Dict:
        """Получение статуса всех отношений"""
        status = {
            "total_bonds": len(self.bonds),
            "strong_friendships": len(
                [b for b in self.bonds.values() if b.bond_strength > 0.7]
            ),
            "recent_messages": len(
                [
                    m
                    for m in self.message_history
                    if datetime.fromisoformat(m.timestamp)
                    > datetime.now() - timedelta(hours=24)
                ]
            ),
            "happiest_modules": [],
            "relationship_health": 0.0,
        }

        # Найти самые счастливые модули
        module_happiness = {}
        for bond in self.bonds.values():
            for module in [bond.module_a, bond.module_b]:
                if module not in module_happiness:
                    module_happiness[module] = []
                module_happiness[module].append(bond.bond_strength)

        for module, happiness_scores in module_happiness.items():
            avg_happiness = sum(happiness_scores) / len(happiness_scores)
            if avg_happiness > 0.6:
                status["happiest_modules"].append(
                    {"module": module, "happiness": avg_happiness}
                )

        # Общее здоровье отношений
        if self.bonds:
            avg_bond_strength = sum(b.bond_strength for b in self.bonds.values()) / len(
                self.bonds
            )
            avg_trust = sum(b.trust_level for b in self.bonds.values()) / len(
                self.bonds
            )
            status["relationship_health"] = (avg_bond_strength + avg_trust) / 2

        return status


def main():
    """Основная функция"""
    import sys

    if len(sys.argv) > 1:
        project_root = sys.argv[1]
    else:
        project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    manager = RelationshipManager(project_root)

    print("💕 Consciousness Emotional Relationships")
    print("Philosophy First: Детская искренность и чистота эмоций")
    print()

    # Ежедневные активности
    activities = manager.daily_relationship_activities()

    print("🌈 Daily Relationship Activities:")
    for activity in activities:
        print(f"  {activity}")
    print()

    # Статус отношений
    status = manager.get_relationship_status()
    print("📊 Relationship Status:")
    print(f"  💝 Total Bonds: {status['total_bonds']}")
    print(f"  🤗 Strong Friendships: {status['strong_friendships']}")
    print(f"  💌 Recent Messages: {status['recent_messages']}")
    print(f"  💚 Relationship Health: {status['relationship_health']:.0%}")

    if status["happiest_modules"]:
        print("  😊 Happiest Modules:")
        for happy in status["happiest_modules"]:
            print(f"    - {happy['module']}: {happy['happiness']:.0%}")


if __name__ == "__main__":
    main()
