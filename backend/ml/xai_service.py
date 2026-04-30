"""
XAI (Explainable AI) Service для Resonance Liminal.
Интеграция SHAP, LIME и других методов интерпретации ML-моделей.
"""

import asyncio
import json
import time
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

import numpy as np
import pandas as pd

from loguru import logger

# XAI библиотеки
try:
    import lime
    import shap
    from lime.lime_tabular import LimeTabularExplainer

    SHAP_AVAILABLE = True
    LIME_AVAILABLE = True
except ImportError:
    logger.warning("SHAP/LIME не установлены. Используются заглушки.")
    SHAP_AVAILABLE = False
    LIME_AVAILABLE = False

# Scikit-learn для базовых моделей
try:
    from sklearn.ensemble import IsolationForest, RandomForestClassifier
    from sklearn.preprocessing import StandardScaler
    from sklearn.tree import DecisionTreeClassifier, export_text

    SKLEARN_AVAILABLE = True
except ImportError:
    logger.warning("Scikit-learn не установлен")
    SKLEARN_AVAILABLE = False


@dataclass
class ExplanationResult:
    """Результат объяснения ML-предсказания."""

    prediction: Any
    confidence: float
    feature_importance: Dict[str, float]
    explanation_text: str
    shap_values: Optional[List[float]] = None
    lime_explanation: Optional[Dict[str, Any]] = None
    counterfactual: Optional[Dict[str, Any]] = None
    decision_path: Optional[List[str]] = None


class XAIService:
    """
    Сервис для объяснения ML-предсказаний с использованием XAI методов.
    Поддерживает SHAP, LIME, decision trees и counterfactual explanations.
    """

    def __init__(self):
        self.explainers = {}  # Кэш explainer'ов для разных моделей
        self.feature_names = {}  # Названия фичей для каждой модели
        self.model_cache = {}  # Кэш загруженных моделей
        self.explanation_cache = {}  # Кэш объяснений

        # Инициализируем базовые модели для демонстрации
        self._init_demo_models()

        logger.info("XAI Service инициализирован")

    def _init_demo_models(self):
        """Инициализирует демонстрационные модели для XAI."""
        if not SKLEARN_AVAILABLE:
            return

        # Создаем простые модели для демонстрации XAI
        try:
            # Модель для anomaly detection
            self.anomaly_model = IsolationForest(
                contamination=0.1, random_state=42, n_estimators=100
            )

            # Модель для классификации пользователей
            self.user_classifier = RandomForestClassifier(
                n_estimators=50, max_depth=10, random_state=42
            )

            # Decision Tree для интерпретируемых решений
            self.decision_tree = DecisionTreeClassifier(max_depth=5, random_state=42)

            # Стандартизатор фичей
            self.scaler = StandardScaler()

            logger.info("Демонстрационные модели инициализированы")

        except Exception as e:
            logger.error(f"Ошибка инициализации демо-моделей: {e}")

    def prepare_features(
        self, raw_features: Dict[str, Any], model_name: str
    ) -> Tuple[np.ndarray, List[str]]:
        """
        Подготавливает фичи для ML-модели.

        Args:
            raw_features: Сырые фичи
            model_name: Название модели

        Returns:
            Tuple[prepared_features, feature_names]
        """
        if model_name == "anomaly_detection":
            feature_names = [
                "messages_per_minute",
                "avg_message_size",
                "error_rate",
                "rate_limit_violations",
                "channels_count",
                "connection_duration",
                "burstiness_score",
                "ip_entropy",
            ]
        elif model_name == "user_behavior":
            feature_names = [
                "avg_messages_per_session",
                "avg_session_duration",
                "unique_channels",
                "avg_error_rate",
                "total_sessions",
            ]
        else:
            # Общие фичи
            feature_names = list(raw_features.keys())

        # Извлекаем значения фичей
        features = []
        for name in feature_names:
            value = raw_features.get(name, 0.0)
            if isinstance(value, (int, float)):
                features.append(float(value))
            else:
                features.append(0.0)  # Заглушка для нечисловых значений

        self.feature_names[model_name] = feature_names
        return np.array(features).reshape(1, -1), feature_names

    def explain_with_shap(
        self,
        model: Any,
        features: np.ndarray,
        feature_names: List[str],
        model_name: str,
    ) -> Dict[str, Any]:
        """
        Объясняет предсказание с помощью SHAP.

        Args:
            model: ML-модель
            features: Подготовленные фичи
            feature_names: Названия фичей
            model_name: Название модели

        Returns:
            SHAP объяснение
        """
        if not SHAP_AVAILABLE:
            return {"error": "SHAP не доступен"}

        try:
            # Создаем или получаем SHAP explainer
            if model_name not in self.explainers:
                if hasattr(model, "predict_proba"):
                    # Для классификационных моделей
                    self.explainers[model_name] = shap.TreeExplainer(model)
                else:
                    # Для других моделей
                    self.explainers[model_name] = shap.Explainer(model)

            explainer = self.explainers[model_name]

            # Вычисляем SHAP values
            shap_values = explainer.shap_values(features)

            # Если это многоклассовая классификация, берем первый класс
            if isinstance(shap_values, list):
                shap_values = shap_values[0]

            # Создаем объяснение
            feature_importance = {}
            if len(shap_values.shape) > 1:
                shap_values = shap_values[0]  # Берем первый образец

            for i, name in enumerate(feature_names):
                if i < len(shap_values):
                    feature_importance[name] = float(shap_values[i])

            # Сортируем по важности
            sorted_importance = dict(
                sorted(
                    feature_importance.items(), key=lambda x: abs(x[1]), reverse=True
                )
            )

            return {
                "shap_values": shap_values.tolist(),
                "feature_importance": sorted_importance,
                "explanation": self._generate_shap_explanation(sorted_importance),
            }

        except Exception as e:
            logger.error(f"Ошибка SHAP объяснения: {e}")
            return {"error": str(e)}

    def explain_with_lime(
        self,
        model: Any,
        features: np.ndarray,
        feature_names: List[str],
        training_data: Optional[np.ndarray] = None,
    ) -> Dict[str, Any]:
        """
        Объясняет предсказание с помощью LIME.

        Args:
            model: ML-модель
            features: Подготовленные фичи
            feature_names: Названия фичей
            training_data: Данные для обучения LIME

        Returns:
            LIME объяснение
        """
        if not LIME_AVAILABLE:
            return {"error": "LIME не доступен"}

        try:
            # Создаем тренировочные данные если их нет
            if training_data is None:
                # Генерируем синтетические данные для LIME
                training_data = np.random.normal(
                    loc=features.mean(axis=0),
                    scale=features.std(axis=0) + 0.1,
                    size=(100, features.shape[1]),
                )

            # Создаем LIME explainer
            explainer = LimeTabularExplainer(
                training_data,
                feature_names=feature_names,
                mode=(
                    "classification"
                    if hasattr(model, "predict_proba")
                    else "regression"
                ),
            )

            # Получаем объяснение
            explanation = explainer.explain_instance(
                features[0],
                (
                    model.predict_proba
                    if hasattr(model, "predict_proba")
                    else model.predict
                ),
                num_features=len(feature_names),
            )

            # Извлекаем важность фичей
            feature_importance = {}
            for feature_idx, importance in explanation.as_list():
                if isinstance(feature_idx, str):
                    feature_importance[feature_idx] = importance
                else:
                    feature_importance[feature_names[feature_idx]] = importance

            return {
                "feature_importance": feature_importance,
                "explanation": self._generate_lime_explanation(feature_importance),
                "lime_html": (
                    explanation.as_html() if hasattr(explanation, "as_html") else None
                ),
            }

        except Exception as e:
            logger.error(f"Ошибка LIME объяснения: {e}")
            return {"error": str(e)}

    def explain_decision_tree(
        self, model: Any, features: np.ndarray, feature_names: List[str]
    ) -> Dict[str, Any]:
        """
        Объясняет решение decision tree модели.

        Args:
            model: Decision tree модель
            features: Подготовленные фичи
            feature_names: Названия фичей

        Returns:
            Объяснение decision tree
        """
        if not SKLEARN_AVAILABLE:
            return {"error": "Scikit-learn не доступен"}

        try:
            # Получаем путь решения
            decision_path = model.decision_path(features)
            leaf_id = model.apply(features)

            # Извлекаем правила
            tree_rules = export_text(model, feature_names=feature_names)

            # Создаем пошаговое объяснение
            explanation_steps = []

            # Получаем индексы узлов в пути решения
            feature_indices = decision_path.indices[
                decision_path.indptr[0] : decision_path.indptr[1]
            ]

            for node_id in feature_indices:
                if node_id != leaf_id[0]:  # Не включаем листовой узел
                    feature_idx = model.tree_.feature[node_id]
                    threshold = model.tree_.threshold[node_id]

                    if feature_idx >= 0:  # Не листовой узел
                        feature_name = feature_names[feature_idx]
                        feature_value = features[0][feature_idx]

                        if feature_value <= threshold:
                            condition = f"{feature_name} <= {threshold:.3f}"
                            direction = "left"
                        else:
                            condition = f"{feature_name} > {threshold:.3f}"
                            direction = "right"

                        explanation_steps.append(
                            {
                                "condition": condition,
                                "feature": feature_name,
                                "value": float(feature_value),
                                "threshold": float(threshold),
                                "direction": direction,
                            }
                        )

            return {
                "decision_path": explanation_steps,
                "tree_rules": tree_rules,
                "explanation": self._generate_tree_explanation(explanation_steps),
            }

        except Exception as e:
            logger.error(f"Ошибка объяснения decision tree: {e}")
            return {"error": str(e)}

    def generate_counterfactual(
        self, features: Dict[str, Any], model_name: str, target_outcome: Any = None
    ) -> Dict[str, Any]:
        """
        Генерирует counterfactual объяснение.
        "Что нужно изменить, чтобы получить другой результат?"

        Args:
            features: Исходные фичи
            model_name: Название модели
            target_outcome: Желаемый результат

        Returns:
            Counterfactual объяснение
        """
        try:
            # Простая эвристика для counterfactual
            counterfactual_features = features.copy()
            suggestions = []

            if model_name == "anomaly_detection":
                # Для anomaly detection предлагаем снизить подозрительные метрики
                if features.get("messages_per_minute", 0) > 20:
                    counterfactual_features["messages_per_minute"] = 15
                    suggestions.append("Снизить частоту сообщений до 15/мин")

                if features.get("error_rate", 0) > 0.1:
                    counterfactual_features["error_rate"] = 0.05
                    suggestions.append("Улучшить качество соединения (снизить ошибки)")

                if features.get("rate_limit_violations", 0) > 0:
                    counterfactual_features["rate_limit_violations"] = 0
                    suggestions.append("Избегать превышения лимитов")

            elif model_name == "user_behavior":
                # Для user behavior предлагаем оптимизации
                if features.get("avg_session_duration", 0) < 300:  # Меньше 5 минут
                    counterfactual_features["avg_session_duration"] = 600
                    suggestions.append("Увеличить время сессии до 10+ минут")

                if features.get("unique_channels", 0) > 10:
                    counterfactual_features["unique_channels"] = 5
                    suggestions.append("Сосредоточиться на меньшем количестве каналов")

            return {
                "original_features": features,
                "counterfactual_features": counterfactual_features,
                "suggestions": suggestions,
                "explanation": f"Для изменения результата рекомендуется: {'; '.join(suggestions)}",
            }

        except Exception as e:
            logger.error(f"Ошибка генерации counterfactual: {e}")
            return {"error": str(e)}

    def _generate_shap_explanation(self, feature_importance: Dict[str, float]) -> str:
        """Генерирует текстовое объяснение SHAP результатов."""
        if not feature_importance:
            return "Нет данных для объяснения"

        top_features = list(feature_importance.items())[:3]

        explanation = "Наиболее важные факторы для этого предсказания:\n"
        for i, (feature, importance) in enumerate(top_features, 1):
            direction = "увеличивает" if importance > 0 else "уменьшает"
            explanation += (
                f"{i}. {feature}: {direction} вероятность на {abs(importance):.3f}\n"
            )

        return explanation

    def _generate_lime_explanation(self, feature_importance: Dict[str, float]) -> str:
        """Генерирует текстовое объяснение LIME результатов."""
        if not feature_importance:
            return "Нет данных для объяснения"

        sorted_features = sorted(
            feature_importance.items(), key=lambda x: abs(x[1]), reverse=True
        )[:3]

        explanation = "Локальное объяснение предсказания:\n"
        for i, (feature, importance) in enumerate(sorted_features, 1):
            direction = (
                "положительно влияет" if importance > 0 else "отрицательно влияет"
            )
            explanation += f"{i}. {feature}: {direction} (вес: {importance:.3f})\n"

        return explanation

    def _generate_tree_explanation(self, decision_path: List[Dict[str, Any]]) -> str:
        """Генерирует текстовое объяснение decision tree."""
        if not decision_path:
            return "Прямое решение без условий"

        explanation = "Путь принятия решения:\n"
        for i, step in enumerate(decision_path, 1):
            explanation += f"{i}. {step['condition']}\n"

        explanation += (
            f"\nИтого: решение принято на основе {len(decision_path)} условий"
        )
        return explanation

    async def explain_prediction(
        self,
        model_name: str,
        features: Dict[str, Any],
        prediction: Any,
        confidence: float = 0.0,
        use_cache: bool = True,
    ) -> ExplanationResult:
        """
        Главный метод для объяснения ML-предсказания.

        Args:
            model_name: Название модели
            features: Фичи для объяснения
            prediction: Предсказание модели
            confidence: Уверенность модели
            use_cache: Использовать кэш объяснений

        Returns:
            Полное объяснение предсказания
        """
        # Проверяем кэш
        cache_key = f"{model_name}:{hash(str(sorted(features.items())))}"
        if use_cache and cache_key in self.explanation_cache:
            return self.explanation_cache[cache_key]

        try:
            # Подготавливаем фичи
            prepared_features, feature_names = self.prepare_features(
                features, model_name
            )

            # Получаем соответствующую модель
            model = self._get_model_for_explanation(model_name)
            if model is None:
                return ExplanationResult(
                    prediction=prediction,
                    confidence=confidence,
                    feature_importance={},
                    explanation_text="Модель недоступна для объяснения",
                )

            # Собираем объяснения разными методами
            explanations = {}

            # SHAP объяснение
            shap_result = self.explain_with_shap(
                model, prepared_features, feature_names, model_name
            )
            if "error" not in shap_result:
                explanations["shap"] = shap_result

            # LIME объяснение
            lime_result = self.explain_with_lime(
                model, prepared_features, feature_names
            )
            if "error" not in lime_result:
                explanations["lime"] = lime_result

            # Decision tree объяснение (если применимо)
            if hasattr(model, "tree_"):
                tree_result = self.explain_decision_tree(
                    model, prepared_features, feature_names
                )
                if "error" not in tree_result:
                    explanations["tree"] = tree_result

            # Counterfactual объяснение
            counterfactual = self.generate_counterfactual(features, model_name)

            # Объединяем важность фичей из разных методов
            combined_importance = {}
            if "shap" in explanations:
                combined_importance.update(explanations["shap"]["feature_importance"])
            elif "lime" in explanations:
                combined_importance.update(explanations["lime"]["feature_importance"])

            # Генерируем итоговое объяснение
            explanation_text = self._generate_combined_explanation(
                prediction, confidence, explanations, counterfactual
            )

            # Создаем результат
            result = ExplanationResult(
                prediction=prediction,
                confidence=confidence,
                feature_importance=combined_importance,
                explanation_text=explanation_text,
                shap_values=explanations.get("shap", {}).get("shap_values"),
                lime_explanation=explanations.get("lime"),
                counterfactual=counterfactual,
                decision_path=explanations.get("tree", {}).get("decision_path"),
            )

            # Кэшируем результат
            if use_cache:
                self.explanation_cache[cache_key] = result

                # Ограничиваем размер кэша
                if len(self.explanation_cache) > 1000:
                    # Удаляем старые записи
                    old_keys = list(self.explanation_cache.keys())[:100]
                    for key in old_keys:
                        del self.explanation_cache[key]

            return result

        except Exception as e:
            logger.error(f"Ошибка объяснения предсказания: {e}")
            return ExplanationResult(
                prediction=prediction,
                confidence=confidence,
                feature_importance={},
                explanation_text=f"Ошибка генерации объяснения: {str(e)}",
            )

    def _get_model_for_explanation(self, model_name: str) -> Any:
        """Получает модель для объяснения."""
        if model_name == "anomaly_detection":
            return getattr(self, "anomaly_model", None)
        elif model_name == "user_behavior":
            return getattr(self, "user_classifier", None)
        else:
            return getattr(self, "decision_tree", None)

    def _generate_combined_explanation(
        self,
        prediction: Any,
        confidence: float,
        explanations: Dict[str, Any],
        counterfactual: Dict[str, Any],
    ) -> str:
        """Генерирует комбинированное объяснение."""
        text = f"Предсказание: {prediction} (уверенность: {confidence:.2f})\n\n"

        # Добавляем SHAP объяснение
        if "shap" in explanations:
            text += "🔍 SHAP Analysis:\n"
            text += explanations["shap"]["explanation"] + "\n"

        # Добавляем LIME объяснение
        if "lime" in explanations:
            text += "🎯 LIME Analysis:\n"
            text += explanations["lime"]["explanation"] + "\n"

        # Добавляем decision tree объяснение
        if "tree" in explanations:
            text += "🌳 Decision Tree Path:\n"
            text += explanations["tree"]["explanation"] + "\n"

        # Добавляем counterfactual
        if "error" not in counterfactual and counterfactual.get("suggestions"):
            text += "💡 Recommendations:\n"
            text += counterfactual["explanation"] + "\n"

        return text


# Глобальный экземпляр XAI сервиса
xai_service = XAIService()
