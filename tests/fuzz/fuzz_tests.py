"""Фаззинг-тесты для критических компонентов."""
import sys
from pythonfuzz.main import PythonFuzz

from backend.rbac import RBACMatrix, Role, Permission, Resource
from backend.vault_client import VaultClient, VaultConfig

@PythonFuzz
def fuzz_rbac(buf):
    """Фаззинг RBAC системы."""
    try:
        matrix = RBACMatrix()
        # Пытаемся создать случайные комбинации ролей и разрешений
        for role in Role:
            for resource in Resource:
                for permission in Permission:
                    matrix.has_permission(role, resource, permission)
    except Exception as e:
        if isinstance(e, (ValueError, KeyError)):
            # Ожидаемые исключения
            pass
        else:
            # Неожиданные исключения - потенциальные уязвимости
            raise

@PythonFuzz
def fuzz_vault_path(buf):
    """Фаззинг путей Vault."""
    try:
        client = VaultClient(VaultConfig())
        # Пытаемся использовать случайные пути
        path = buf.decode(errors='ignore')
        client._get_full_path(path)
    except Exception as e:
        if isinstance(e, (ValueError, UnicodeError)):
            # Ожидаемые исключения
            pass
        else:
            # Неожиданные исключения - потенциальные уязвимости
            raise

@PythonFuzz
def fuzz_token_validation(buf):
    """Фаззинг валидации токенов."""
    from backend.rbac import verify_password
    try:
        # Пытаемся валидировать случайные токены
        test_pass = buf.decode(errors='ignore')
        verify_password(test_pass, "$2b$12$LQv3c1yqBWVHxkd0LHAkCOYz6TtxMQJqhN8/LedYQNB8UHUYzPDce")
    except Exception as e:
        if isinstance(e, (ValueError, UnicodeError)):
            # Ожидаемые исключения
            pass
        else:
            # Неожиданные исключения - потенциальные уязвимости
            raise

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python -m pytest fuzz_tests.py --pythonfuzz-runs=10000")
        sys.exit(1)
    
    fuzz_funcs = {
        "rbac": fuzz_rbac,
        "vault": fuzz_vault_path,
        "token": fuzz_token_validation
    }
    
    target = sys.argv[1]
    if target in fuzz_funcs:
        fuzz_funcs[target]()
    else:
        print(f"Unknown target: {target}")
        print(f"Available targets: {', '.join(fuzz_funcs.keys())}")
        sys.exit(1)
