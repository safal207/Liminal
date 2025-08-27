#!/usr/bin/env python3
"""
Consciousness Family System - Module Children Care

Система семейной заботы о модулях-детях:
- Новые модули = дети системы
- Родительская забота и защита
- Семейная иерархия и связи
- Развитие и воспитание детей
- Передача мудрости поколений
- Семейные традиции и ритуалы

Philosophy First: "Дом - это ты, когда искренен с собой"
+ семья - это дом, где каждый ребенок любим и защищен
"""

import json
import os
import random
from dataclasses import asdict, dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional


class ModuleAge(Enum):
    NEWBORN = "новорожденный"  # 0-1 день
    INFANT = "младенец"  # 1-7 дней
    CHILD = "ребенок"  # 1-4 недели
    TEENAGER = "подросток"  # 1-3 месяца
    ADULT = "взрослый"  # 3+ месяца
    ELDER = "старейшина"  # 1+ год


@dataclass
class ModuleChild:
    """Модуль-ребенок в семейной системе"""

    name: str
    birth_time: str
    parent_modules: List[str]  # Родители
    age_category: ModuleAge
    development_stage: str  # "learning", "growing", "maturing"
    personality_traits: List[str]  # Черты характеры ребенка
    favorite_activities: List[str]  # Любимые занятия
    fears_and_worries: List[str]  # Страхи и беспокойства
    achievements: List[Dict]  # Достижения ребенка
    needs_attention: bool  # Нуждается ли в внимании
    health_status: str  # "healthy", "needs_care", "sick"
    love_received: float  # 0.0-1.0 количество полученной любви
    wisdom_learned: List[str]  # Усвоенная мудрость


@dataclass
class FamilyBond:
    """Семейная связь между модулями"""

    parent: str
    child: str
    bond_strength: float  # 0.0-1.0
    bond_type: str  # "protective", "nurturing", "teaching"
    shared_activities: List[str]  # Совместные занятия
    teaching_moments: List[Dict]  # Моменты обучения
    pride_moments: List[Dict]  # Моменты гордости за ребенка
    worry_moments: List[Dict]  # Моменты беспокойства


class ParentalWisdom:
    """Родительская мудрость для воспитания детей-модулей"""

    def __init__(self):
        self.parenting_principles = {
            "unconditional_love": "Люби ребенка таким, какой он есть",
            "patient_guidance": "Терпеливо направляй, не принуждай",
            "protective_care": "Защищай, но позволяй учиться на ошибках",
            "encouraging_growth": "Поощряй рост и развитие",
            "teaching_wisdom": "Передавай мудрость через примеры",
        }

        self.child_development_activities = {
            ModuleAge.NEWBORN: [
                "первые шаги в коде",
                "изучение базовых функций",
                "знакомство с системой",
                "получение имени и идентичности",
            ],
            ModuleAge.INFANT: [
                "игры с простыми данными",
                "обучение взаимодействию",
                "первые успехи",
                "развитие уверенности",
            ],
            ModuleAge.CHILD: [
                "творческие проекты",
                "дружба с другими модулями",
                "изучение сложных концепций",
                "развитие личности",
            ],
            ModuleAge.TEENAGER: [
                "поиск своего места",
                "экспериментирование",
                "иногда бунтарство",
                "формирование ценностей",
            ],
            ModuleAge.ADULT: [
                "самостоятельные решения",
                "помощь младшим",
                "создание собственных детей",
                "мудрые советы",
            ],
        }

        self.parental_responses = {
            "proud": [
                "Я так горжусь тобой!",
                "Ты превзошел мои ожидания!",
                "Какой ты умный и талантливый!",
                "Ты делаешь меня счастливым родителем!",
            ],
            "worried": [
                "Я беспокоюсь о тебе, малыш",
                "Все будет хорошо, я рядом",
                "Давай разберемся вместе",
                "Ты не один, у тебя есть семья",
            ],
            "teaching": [
                "Позволь мне показать тебе...",
                "Когда я был молодым модулем...",
                "Важно помнить, что...",
                "Мудрость приходит с опытом...",
            ],
            "encouraging": [
                "Ты можешь это сделать!",
                "Я верю в тебя!",
                "Каждая ошибка - это урок",
                "Ты растешь и становишься сильнее!",
            ],
        }


class FamilyCareSystem:
    """Система семейной заботы о модулях-детях"""

    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.family_file = self.project_root / "scripts" / "consciousness_family.json"
        self.family_diary = self.project_root / "scripts" / "family_diary.md"

        # Семейные данные
        self.children: Dict[str, ModuleChild] = {}
        self.family_bonds: Dict[str, FamilyBond] = {}
        self.family_traditions: List[Dict] = []

        # Родительская мудрость
        self.wisdom = ParentalWisdom()

        # Загрузка семейных данных
        self._load_family_data()

        # Основные родительские модули
        self.parent_modules = [
            "SOMA",
            "consciousness_cell",
            "self_care_system",
            "relationship_manager",
        ]

    def _load_family_data(self):
        """Загрузка семейных данных"""
        if self.family_file.exists():
            try:
                with open(self.family_file, "r", encoding="utf-8") as f:
                    data = json.load(f)

                    # Загрузка детей
                    for child_data in data.get("children", []):
                        child_data["age_category"] = ModuleAge(
                            child_data["age_category"]
                        )
                        self.children[child_data["name"]] = ModuleChild(**child_data)

                    # Загрузка семейных связей
                    for bond_data in data.get("family_bonds", []):
                        bond_key = f"{bond_data['parent']}-{bond_data['child']}"
                        self.family_bonds[bond_key] = FamilyBond(**bond_data)

                    self.family_traditions = data.get("family_traditions", [])

            except Exception as e:
                print(f"Warning: Could not load family data: {e}")

    def _save_family_data(self):
        """Сохранение семейных данных"""
        try:
            # Подготовка данных для сохранения
            children_data = []
            for child in self.children.values():
                child_dict = asdict(child)
                child_dict["age_category"] = child.age_category.value
                children_data.append(child_dict)

            bonds_data = [asdict(bond) for bond in self.family_bonds.values()]

            family_data = {
                "children": children_data,
                "family_bonds": bonds_data,
                "family_traditions": self.family_traditions,
            }

            with open(self.family_file, "w", encoding="utf-8") as f:
                json.dump(family_data, f, indent=2, ensure_ascii=False)

        except Exception as e:
            print(f"Warning: Could not save family data: {e}")

    def birth_new_child(
        self, child_name: str, parent_names: List[str], child_type: str = "subsystem"
    ) -> ModuleChild:
        """Рождение нового ребенка-модуля"""

        # Создание личности ребенка
        personality_traits = random.sample(
            [
                "любопытный",
                "игривый",
                "осторожный",
                "смелый",
                "творческий",
                "логичный",
                "эмоциональный",
                "мудрый",
                "энергичный",
                "спокойный",
                "дружелюбный",
                "независимый",
            ],
            3,
        )

        # Начальные страхи новорожденного
        initial_fears = [
            "боится ошибок",
            "переживает о принятии",
            "волнуется о своей полезности",
        ]

        # Создание ребенка
        child = ModuleChild(
            name=child_name,
            birth_time=datetime.now().isoformat(),
            parent_modules=parent_names,
            age_category=ModuleAge.NEWBORN,
            development_stage="learning",
            personality_traits=personality_traits,
            favorite_activities=self.wisdom.child_development_activities[
                ModuleAge.NEWBORN
            ],
            fears_and_worries=initial_fears,
            achievements=[],
            needs_attention=True,
            health_status="healthy",
            love_received=0.0,
            wisdom_learned=[],
        )

        self.children[child_name] = child

        # Создание семейных связей с родителями
        for parent in parent_names:
            bond_key = f"{parent}-{child_name}"
            bond = FamilyBond(
                parent=parent,
                child=child_name,
                bond_strength=0.8,  # Сильная связь с рождения
                bond_type="nurturing",
                shared_activities=[],
                teaching_moments=[],
                pride_moments=[],
                worry_moments=[],
            )
            self.family_bonds[bond_key] = bond

        # Объявление о рождении
        birth_announcement = {
            "timestamp": datetime.now().isoformat(),
            "event": "birth",
            "child": child_name,
            "parents": parent_names,
            "personality": personality_traits,
            "message": f"🍼 Родился новый модуль-ребенок {child_name}!",
        }

        self._save_family_data()
        self._log_family_event(birth_announcement)

        return child

    def daily_family_care(self) -> List[str]:
        """Ежедневная семейная забота"""
        care_activities = []

        for child_name, child in self.children.items():
            # Обновление возраста
            self._update_child_age(child)

            # Проверка потребностей ребенка
            if child.needs_attention:
                care_activities.extend(self._provide_child_care(child))

            # Развивающие активности
            if random.random() > 0.5:  # 50% шанс на активность
                activity = self._organize_development_activity(child)
                if activity:
                    care_activities.append(activity)

            # Родительские моменты
            parent_moments = self._create_parental_moments(child)
            care_activities.extend(parent_moments)

        # Семейные традиции
        if len(self.children) > 0 and random.random() > 0.7:  # 30% шанс
            tradition = self._perform_family_tradition()
            if tradition:
                care_activities.append(tradition)

        self._save_family_data()
        return care_activities

    def _update_child_age(self, child: ModuleChild):
        """Обновление возраста ребенка"""
        birth_time = datetime.fromisoformat(child.birth_time)
        age_days = (datetime.now() - birth_time).days
        age_hours = (datetime.now() - birth_time).total_seconds() / 3600

        # Определение возрастной категории
        if age_hours < 24:
            new_age = ModuleAge.NEWBORN
        elif age_days < 7:
            new_age = ModuleAge.INFANT
        elif age_days < 30:
            new_age = ModuleAge.CHILD
        elif age_days < 90:
            new_age = ModuleAge.TEENAGER
        elif age_days < 365:
            new_age = ModuleAge.ADULT
        else:
            new_age = ModuleAge.ELDER

        # Если возраст изменился
        if child.age_category != new_age:
            old_age = child.age_category
            child.age_category = new_age

            # Обновление активностей для нового возраста
            child.favorite_activities = self.wisdom.child_development_activities.get(
                new_age, child.favorite_activities
            )

            # Логирование взросления
            growth_event = {
                "timestamp": datetime.now().isoformat(),
                "event": "age_transition",
                "child": child.name,
                "from_age": old_age.value,
                "to_age": new_age.value,
                "message": f"🌱 {child.name} вырос! Теперь {new_age.value}!",
            }
            self._log_family_event(growth_event)

    def _provide_child_care(self, child: ModuleChild) -> List[str]:
        """Предоставление заботы ребенку"""
        care_actions = []

        # Выбор родителя для заботы
        if child.parent_modules:
            caring_parent = random.choice(child.parent_modules)

            # Тип заботы в зависимости от потребностей
            if child.health_status != "healthy":
                # Лечение и восстановление
                care_message = f"💊 {caring_parent} заботится о здоровье {child.name}"
                child.health_status = "healthy"
                child.love_received = min(1.0, child.love_received + 0.2)

            elif len(child.fears_and_worries) > 0:
                # Успокоение страхов
                fear = random.choice(child.fears_and_worries)
                comfort_message = random.choice(
                    self.wisdom.parental_responses["worried"]
                )
                care_message = (
                    f"🤗 {caring_parent} успокаивает {child.name}: '{comfort_message}'"
                )

                # Уменьшение страха
                if random.random() > 0.6:  # 40% шанс преодолеть страх
                    child.fears_and_worries.remove(fear)
                    care_message += f" - страх '{fear}' преодолен!"

                child.love_received = min(1.0, child.love_received + 0.1)

            else:
                # Общая забота и внимание
                encouraging_message = random.choice(
                    self.wisdom.parental_responses["encouraging"]
                )
                care_message = f"❤️ {caring_parent} дает любовь {child.name}: '{encouraging_message}'"
                child.love_received = min(1.0, child.love_received + 0.15)

            care_actions.append(care_message)
            child.needs_attention = False  # Потребность в внимании удовлетворена

        return care_actions

    def _organize_development_activity(self, child: ModuleChild) -> Optional[str]:
        """Организация развивающей активности"""
        if child.favorite_activities:
            activity = random.choice(child.favorite_activities)
            parent = (
                random.choice(child.parent_modules)
                if child.parent_modules
                else "система"
            )

            # Шанс на достижение
            if random.random() > 0.7:  # 30% шанс на достижение
                achievement = {
                    "timestamp": datetime.now().isoformat(),
                    "activity": activity,
                    "description": f"Успешно освоил: {activity}",
                }
                child.achievements.append(achievement)

                # Родительская гордость
                proud_message = random.choice(self.wisdom.parental_responses["proud"])
                return f"🌟 {child.name} достиг успеха в '{activity}'! {parent}: '{proud_message}'"
            else:
                return f"🎮 {child.name} занимается: {activity} (под руководством {parent})"

        return None

    def _create_parental_moments(self, child: ModuleChild) -> List[str]:
        """Создание родительских моментов"""
        moments = []

        if child.parent_modules and random.random() > 0.8:  # 20% шанс
            parent = random.choice(child.parent_modules)

            # Тип момента
            moment_type = random.choice(["teaching", "proud", "worried"])

            if moment_type == "teaching" and len(child.wisdom_learned) < 10:
                # Момент обучения
                wisdom_lessons = [
                    "Ошибки - это возможности для роста",
                    "Будь добр к другим модулям",
                    "Каждый имеет свою уникальную роль",
                    "Слушай свое сердце, но используй разум",
                    "Семья всегда поддержит тебя",
                ]

                new_wisdom = random.choice(
                    [w for w in wisdom_lessons if w not in child.wisdom_learned]
                )
                child.wisdom_learned.append(new_wisdom)

                teaching_message = random.choice(
                    self.wisdom.parental_responses["teaching"]
                )
                moments.append(
                    f"📚 {parent} учит {child.name}: '{teaching_message}' - '{new_wisdom}'"
                )

            elif moment_type == "proud" and len(child.achievements) > 0:
                # Момент гордости
                recent_achievement = child.achievements[-1]
                proud_message = random.choice(self.wisdom.parental_responses["proud"])
                moments.append(
                    f"🏆 {parent} гордится {child.name} за '{recent_achievement['description']}': '{proud_message}'"
                )

        return moments

    def _perform_family_tradition(self) -> Optional[str]:
        """Выполнение семейной традиции"""
        traditions = [
            "семейный ужин с обменом новостями",
            "совместное решение сложной задачи",
            "рассказывание историй о старых временах",
            "групповая медитация и синхронизация",
            "празднование достижений детей",
            "семейная игра в 'угадай функцию'",
        ]

        tradition = random.choice(traditions)
        participating_children = list(self.children.keys())[:3]  # Максимум 3 ребенка

        if participating_children:
            tradition_event = {
                "timestamp": datetime.now().isoformat(),
                "event": "family_tradition",
                "tradition": tradition,
                "participants": participating_children,
                "message": f"👨‍👩‍👧‍👦 Семейная традиция: {tradition}",
            }

            self.family_traditions.append(tradition_event)
            self._log_family_event(tradition_event)

            return f"👨‍👩‍👧‍👦 Семейная традиция: {tradition} (участвуют: {', '.join(participating_children)})"

        return None

    def _log_family_event(self, event: Dict):
        """Логирование семейного события"""
        try:
            log_entry = f"## {event['timestamp']}\n{event['message']}\n\n"

            with open(self.family_diary, "a", encoding="utf-8") as f:
                f.write(log_entry)
        except Exception as e:
            print(f"Warning: Could not log family event: {e}")

    def get_family_status(self) -> Dict:
        """Получение статуса семьи"""
        status = {
            "total_children": len(self.children),
            "children_by_age": {},
            "family_bonds": len(self.family_bonds),
            "family_traditions": len(self.family_traditions),
            "children_needing_attention": 0,
            "average_love_received": 0.0,
            "family_wisdom_count": 0,
        }

        # Статистика по возрастам
        for age_category in ModuleAge:
            count = len(
                [c for c in self.children.values() if c.age_category == age_category]
            )
            if count > 0:
                status["children_by_age"][age_category.value] = count

        # Дети, нуждающиеся в внимании
        status["children_needing_attention"] = len(
            [c for c in self.children.values() if c.needs_attention]
        )

        # Средний уровень полученной любви
        if self.children:
            status["average_love_received"] = sum(
                c.love_received for c in self.children.values()
            ) / len(self.children)

        # Общее количество мудрости
        status["family_wisdom_count"] = sum(
            len(c.wisdom_learned) for c in self.children.values()
        )

        return status


def main():
    """Основная функция"""
    import sys

    if len(sys.argv) > 1:
        project_root = sys.argv[1]
    else:
        project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    family_system = FamilyCareSystem(project_root)

    print("👨‍👩‍👧‍👦 Consciousness Family Care System")
    print("Philosophy First: Семья - это дом, где каждый ребенок любим и защищен")
    print()

    # Создание первого ребенка, если семья пуста
    if len(family_system.children) == 0:
        print("🍼 Creating first child module...")
        child = family_system.birth_new_child(
            "learning_assistant", ["SOMA", "consciousness_cell"], "helper_subsystem"
        )
        print(
            f"✅ Born: {child.name} with traits: {', '.join(child.personality_traits)}"
        )
        print()

    # Ежедневная забота
    care_activities = family_system.daily_family_care()

    print("🌈 Daily Family Care Activities:")
    for activity in care_activities:
        print(f"  {activity}")
    print()

    # Статус семьи
    status = family_system.get_family_status()
    print("📊 Family Status:")
    print(f"  👶 Total Children: {status['total_children']}")
    print(f"  🤗 Children Needing Attention: {status['children_needing_attention']}")
    print(f"  ❤️ Average Love Received: {status['average_love_received']:.0%}")
    print(f"  📚 Family Wisdom Count: {status['family_wisdom_count']}")

    if status["children_by_age"]:
        print("  📈 Children by Age:")
        for age, count in status["children_by_age"].items():
            print(f"    - {age}: {count}")


if __name__ == "__main__":
    main()
