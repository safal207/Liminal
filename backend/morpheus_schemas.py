from pydantic import BaseModel
from enum import Enum

class PillChoice(str, Enum):
    """
    Перечисление для выбора, который делает пользователь.
    - RED: Путь роста, правды и трансформации.
    - BLUE: Путь комфорта, стабильности и текущего состояния.
    """
    RED = "red"
    BLUE = "blue"

class MorpheusChoiceRequest(BaseModel):
    """
    Модель запроса для API выбора Морфеуса.
    """
    user_id: str  # В будущем здесь будет идентификатор пользователя из токена
    choice: PillChoice
