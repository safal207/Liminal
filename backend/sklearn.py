"""
Simple fallback implementation of sklearn for testing.
This provides mock ML functionality when sklearn is not available.
"""

from typing import Any, List, Optional
import random

class MockModel:
    """Mock ML model that provides basic functionality."""
    
    def __init__(self):
        self.is_fitted = False
    
    def fit(self, X, y=None):
        """Mock fit method."""
        self.is_fitted = True
        return self
    
    def predict(self, X):
        """Mock predict method."""
        if not self.is_fitted:
            raise ValueError("Model not fitted")
        # Return random predictions
        return [random.choice([0, 1]) for _ in range(len(X))]
    
    def predict_proba(self, X):
        """Mock predict_proba method."""
        if not self.is_fitted:
            raise ValueError("Model not fitted")
        # Return random probabilities
        return [[random.random(), random.random()] for _ in range(len(X))]

# Ensemble module
class ensemble:
    class IsolationForest(MockModel):
        def __init__(self, contamination=0.1, random_state=None):
            super().__init__()
            self.contamination = contamination
    
    class RandomForestClassifier(MockModel):
        def __init__(self, n_estimators=100, random_state=None):
            super().__init__()
            self.n_estimators = n_estimators

# Preprocessing module  
class preprocessing:
    class StandardScaler:
        def __init__(self):
            self.fitted = False
        
        def fit(self, X):
            self.fitted = True
            return self
        
        def transform(self, X):
            if not self.fitted:
                raise ValueError("Scaler not fitted")
            return X  # Return unchanged for mock
        
        def fit_transform(self, X):
            return self.fit(X).transform(X)

# Metrics module
class metrics:
    @staticmethod
    def accuracy_score(y_true, y_pred):
        """Mock accuracy score."""
        return random.uniform(0.7, 0.95)
    
    @staticmethod
    def classification_report(y_true, y_pred):
        """Mock classification report."""
        return "Mock classification report"

# Model selection module
class model_selection:
    @staticmethod
    def train_test_split(X, y=None, test_size=0.2, random_state=None):
        """Mock train test split."""
        split_idx = int(len(X) * (1 - test_size))
        if y is not None:
            return X[:split_idx], X[split_idx:], y[:split_idx], y[split_idx:]
        else:
            return X[:split_idx], X[split_idx:]
    
    class GridSearchCV:
        def __init__(self, estimator, param_grid, cv=5):
            self.estimator = estimator
            self.param_grid = param_grid
            self.cv = cv
            self.best_estimator_ = estimator
        
        def fit(self, X, y):
            return self

# Base module
class base:
    class BaseEstimator:
        """Mock base estimator."""
        pass
    
    class TransformerMixin:
        """Mock transformer mixin."""
        pass