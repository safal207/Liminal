"""Compatibility shim exposing backend.logging_config as top-level module.

Many modules import ``logging_config`` from the project root, while the actual
implementation lives in ``backend.logging_config``.  This lightweight shim makes
``import logging_config`` succeed without duplicating code.
"""
from backend.logging_config import *  # noqa: F401,F403 - re-export everything
