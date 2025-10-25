#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Скрипт для проверки синтаксической корректности файлов Python.
"""

import sys
import os
import py_compile

def check_syntax(file_path):
    """
    Проверяет синтаксическую корректность файла Python.
    
    Args:
        file_path: Путь к файлу для проверки
        
    Returns:
        bool: True, если синтаксис корректен, иначе False
    """
    try:
        py_compile.compile(file_path, doraise=True)
        print(f"✅ Синтаксис файла {file_path} корректен")
        return True
    except py_compile.PyCompileError as e:
        print(f"❌ Ошибка синтаксиса в файле {file_path}:")
        print(f"  {e}")
        return False
    except Exception as e:
        print(f"❌ Ошибка при проверке файла {file_path}:")
        print(f"  {e}")
        return False

if __name__ == "__main__":
    # Проверяем файлы
    files_to_check = [
        "backend/personality/ml_adapter.py",
        "backend/personality/multilingual_support.py"
    ]
    
    success = True
    for file_path in files_to_check:
        full_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), file_path)
        if not check_syntax(full_path):
            success = False
    
    # Выводим общий результат
    if success:
        print("\n✅ Все файлы прошли проверку синтаксиса успешно!")
        sys.exit(0)
    else:
        print("\n❌ Некоторые файлы содержат синтаксические ошибки!")
        sys.exit(1)
