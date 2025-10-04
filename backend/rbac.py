"""RBAC система для Liminal."""
from enum import Enum
from typing import Dict, List, Optional, Set

from fastapi import Depends, HTTPException, Security, status
from fastapi.security import OAuth2PasswordBearer, SecurityScopes
from jose import JWTError, jwt
from passlib.context import CryptContext
from pydantic import BaseModel, ValidationError

from .vault_client import get_vault_client

# Константы
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30

# Контекст для хеширования паролей
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

# OAuth2 схема с поддержкой scopes
oauth2_scheme = OAuth2PasswordBearer(
    tokenUrl="token",
    scopes={
        "admin": "Полный доступ к системе",
        "user": "Базовый доступ к API",
        "ml": "Доступ к ML функциям",
        "analytics": "Доступ к аналитике",
    },
)


class Role(str, Enum):
    """Роли пользователей."""
    ADMIN = "admin"
    USER = "user"
    ML_ENGINEER = "ml_engineer"
    ANALYST = "analyst"


class Permission(str, Enum):
    """Разрешения для действий."""
    READ = "read"
    WRITE = "write"
    DELETE = "delete"
    EXECUTE = "execute"


class Resource(str, Enum):
    """Ресурсы системы."""
    USERS = "users"
    ML_MODELS = "ml_models"
    ANALYTICS = "analytics"
    CONSCIOUSNESS = "consciousness"
    SYSTEM = "system"


class RBACMatrix:
    """Матрица прав доступа."""
    
    def __init__(self):
        """Инициализация матрицы прав."""
        self._matrix: Dict[Role, Dict[Resource, Set[Permission]]] = {
            Role.ADMIN: {
                resource: set(Permission) for resource in Resource
            },
            Role.USER: {
                Resource.ML_MODELS: {Permission.READ},
                Resource.ANALYTICS: {Permission.READ},
                Resource.CONSCIOUSNESS: {Permission.READ},
            },
            Role.ML_ENGINEER: {
                Resource.ML_MODELS: {Permission.READ, Permission.WRITE, Permission.EXECUTE},
                Resource.ANALYTICS: {Permission.READ},
                Resource.CONSCIOUSNESS: {Permission.READ, Permission.WRITE},
            },
            Role.ANALYST: {
                Resource.ANALYTICS: {Permission.READ, Permission.WRITE},
                Resource.ML_MODELS: {Permission.READ},
                Resource.CONSCIOUSNESS: {Permission.READ},
            },
        }
    
    def has_permission(
        self, role: Role, resource: Resource, permission: Permission
    ) -> bool:
        """Проверка наличия разрешения."""
        return permission in self._matrix.get(role, {}).get(resource, set())


# Глобальный экземпляр матрицы прав
rbac_matrix = RBACMatrix()


class Token(BaseModel):
    """Модель токена доступа."""
    access_token: str
    token_type: str


class TokenData(BaseModel):
    """Данные токена."""
    username: str
    scopes: List[str] = []
    role: Role


class User(BaseModel):
    """Модель пользователя."""
    username: str
    email: Optional[str] = None
    full_name: Optional[str] = None
    disabled: Optional[bool] = None
    role: Role = Role.USER


class UserInDB(User):
    """Модель пользователя в БД."""
    hashed_password: str


def verify_password(plain_password: str, hashed_password: str) -> bool:
    """Проверка пароля."""
    return pwd_context.verify(plain_password, hashed_password)


def get_password_hash(password: str) -> str:
    """Хеширование пароля."""
    return pwd_context.hash(password)


async def get_current_user(
    security_scopes: SecurityScopes,
    token: str = Depends(oauth2_scheme),
) -> User:
    """Получение текущего пользователя."""
    if security_scopes.scopes:
        authenticate_value = f'Bearer scope="{security_scopes.scope_str}"'
    else:
        authenticate_value = "Bearer"
        
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": authenticate_value},
    )
    
    try:
        # Получаем секрет для JWT из Vault
        vault_client = get_vault_client()
        secret = vault_client.read_secret("shared/jwt")
        secret_key = secret["secret_key"]
        
        payload = jwt.decode(token, secret_key, algorithms=[ALGORITHM])
        username: str = payload.get("sub")
        if username is None:
            raise credentials_exception
        token_scopes = payload.get("scopes", [])
        role = Role(payload.get("role", Role.USER))
        token_data = TokenData(username=username, scopes=token_scopes, role=role)
    except (JWTError, ValidationError, ValueError):
        raise credentials_exception
        
    # Здесь должна быть логика получения пользователя из БД
    # Для примера возвращаем тестового пользователя
    user = User(
        username=token_data.username,
        role=token_data.role,
        disabled=False
    )
    
    if user.disabled:
        raise HTTPException(status_code=400, detail="Inactive user")
        
    for scope in security_scopes.scopes:
        if scope not in token_data.scopes:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Not enough permissions",
                headers={"WWW-Authenticate": authenticate_value},
            )
            
    return user


def check_permission(
    resource: Resource,
    permission: Permission,
    user: User = Security(get_current_user, scopes=["user"]),
) -> bool:
    """Проверка разрешения для пользователя."""
    return rbac_matrix.has_permission(user.role, resource, permission)


# Декоратор для проверки прав доступа
def require_permission(resource: Resource, permission: Permission):
    """Декоратор для проверки прав доступа."""
    def decorator(func):
        async def wrapper(*args, user: User = Security(get_current_user, scopes=["user"]), **kwargs):
            if not check_permission(resource, permission, user):
                raise HTTPException(
                    status_code=status.HTTP_403_FORBIDDEN,
                    detail="Not enough permissions",
                )
            return await func(*args, user=user, **kwargs)
        return wrapper
    return decorator
