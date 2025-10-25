#!/usr/bin/env python3
"""
🧠🌊 LIMINAL-RGL Integration Module

Integration of Retrosplenial Gateway Layer into LIMINAL architecture
for brain-accurate navigation through liminal states.
"""

import asyncio
import time
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass

from retrosplenial_gateway import (
    RetrosplenialGateway, NavigationEvent, NavigationContext, 
    SemanticDirection
)


@dataclass
class LiminalState:
    """Liminal state with neural navigation."""
    state_id: str
    description: str
    emotional_profile: Dict[str, float]
    difficulty_level: float  # 0-1
    transition_type: str     # "threshold", "passage", "transformation"
    dominant_direction: Optional[SemanticDirection] = None


class LiminalNavigationSystem:
    """
    Navigation system through liminal states using RGL.
    
    Integrates brain compass into LIMINAL architecture for precise 
    navigation through "complex transitions".
    """
    
    def __init__(self):
        # Инициализация мозгового компаса
        self.brain_compass = RetrosplenialGateway()
        
        # Лиминальные состояния
        self.liminal_states = self._initialize_liminal_states()
        self.current_state: Optional[LiminalState] = None
        self.transition_history: List[Dict] = []
        
        # Контекст для LIMINAL процессинга
        self._setup_liminal_context()
        
        print("LIMINAL Navigation System initialized with brain compass!")
    
    def _initialize_liminal_states(self) -> Dict[str, LiminalState]:
        """Initialize liminal states."""
        return {
            "uncertainty_threshold": LiminalState(
                state_id="uncertainty_threshold",
                description="Uncertainty threshold - moment between knowing and not knowing",
                emotional_profile={"uncertainty": 0.8, "curiosity": 0.6, "anxiety": 0.4},
                difficulty_level=0.6,
                transition_type="threshold",
                dominant_direction=SemanticDirection.WEST  # Reflection
            ),
            
            "creative_liminal": LiminalState(
                state_id="creative_liminal",
                description="Creative liminal space - between idea and manifestation",
                emotional_profile={"inspiration": 0.9, "flow": 0.8, "excitement": 0.7},
                difficulty_level=0.4,
                transition_type="transformation", 
                dominant_direction=SemanticDirection.EAST  # Creativity
            ),
            
            "learning_passage": LiminalState(
                state_id="learning_passage",
                description="Learning passage - between not knowing and understanding",
                emotional_profile={"curiosity": 0.8, "focus": 0.7, "determination": 0.6},
                difficulty_level=0.5,
                transition_type="passage",
                dominant_direction=SemanticDirection.NORTH  # Growth
            ),
            
            "emotional_crisis": LiminalState(
                state_id="emotional_crisis", 
                description="Emotional crisis - between old and new way of being",
                emotional_profile={"distress": 0.9, "confusion": 0.8, "hope": 0.3},
                difficulty_level=0.9,
                transition_type="transformation",
                dominant_direction=SemanticDirection.SOUTH  # Survival/grounding
            ),
            
            "breakthrough_moment": LiminalState(
                state_id="breakthrough_moment",
                description="Breakthrough moment - between obstacle and solution",
                emotional_profile={"excitement": 0.9, "clarity": 0.8, "empowerment": 0.7},
                difficulty_level=0.3,
                transition_type="threshold",
                dominant_direction=SemanticDirection.NORTH  # Evolution
            )
        }
    
    def _setup_liminal_context(self):
        """Настройка контекста для лиминального процессинга."""
        context = NavigationContext(
            current_state="liminal_processing_system",
            target_state="successful_transition",
            emotional_context={
                "openness_to_change": 0.8,
                "tolerance_for_uncertainty": 0.6,
                "growth_orientation": 0.7
            },
            temporal_context={
                "processing_mode": "liminal",
                "transition_awareness": True
            }
        )
        self.brain_compass.set_navigation_context(context)
    
    async def enter_liminal_state(self, state_id: str, context: Dict[str, Any]) -> Dict[str, Any]:
        """
        Вход в лиминальное состояние с нейронной навигацией.
        """
        if state_id not in self.liminal_states:
            raise ValueError(f"Unknown liminal state: {state_id}")
        
        liminal_state = self.liminal_states[state_id]
        self.current_state = liminal_state
        
        print(f"\n=== ENTERING LIMINAL STATE: {liminal_state.description} ===")
        
        # Создание навигационного события для входа
        entry_event = NavigationEvent(
            event_id=f"liminal_entry_{state_id}_{int(time.time())}",
            event_type="liminal_state_entry",
            content=f"Entering {liminal_state.description}. Context: {context}",
            timestamp=datetime.now(),
            source_layer="liminal_system",
            emotional_valence=self._calculate_emotional_valence(liminal_state),
            urgency_level=liminal_state.difficulty_level
        )
        
        # Обновление контекста для этого лиминального состояния
        updated_context = NavigationContext(
            current_state=state_id,
            target_state="successful_transition",
            emotional_context=liminal_state.emotional_profile,
            temporal_context=context
        )
        self.brain_compass.set_navigation_context(updated_context)
        
        # Получение нейронной навигации
        direction = await self.brain_compass.process_navigation_event(entry_event)
        analytics = self.brain_compass.get_navigation_analytics()
        
        # Результат входа
        entry_result = {
            "liminal_state": liminal_state.state_id,
            "neural_direction": {
                "primary": direction.primary_direction.value,
                "strength": direction.strength,
                "confidence": direction.confidence,
                "guidance": direction.primary_direction.description
            },
            "brain_state": {
                "theta_frequency": analytics['theta_oscillations']['current_frequency'],
                "theta_type": analytics['theta_oscillations']['theta_type'],
                "gamma_synchrony": analytics['gamma_synchrony']['synchrony_strength'],
                "theta_gamma_coupling": analytics['gamma_synchrony']['gamma_theta_coupling']
            },
            "navigation_guidance": self._generate_navigation_guidance(direction, liminal_state),
            "entry_timestamp": datetime.now().isoformat()
        }
        
        # Логирование перехода
        self.transition_history.append({
            "action": "enter_liminal_state",
            "state": state_id,
            "direction": direction.primary_direction.value,
            "strength": direction.strength,
            "timestamp": datetime.now().isoformat()
        })
        
        return entry_result
    
    async def navigate_liminal_challenge(self, challenge_description: str, 
                                       emotional_state: Dict[str, float]) -> Dict[str, Any]:
        """
        Навигация через вызов внутри лиминального состояния.
        """
        if not self.current_state:
            raise ValueError("No active liminal state. Enter a liminal state first.")
        
        print(f"\n--- NAVIGATING LIMINAL CHALLENGE ---")
        print(f"Challenge: {challenge_description}")
        
        # Создание события для навигации через вызов
        challenge_event = NavigationEvent(
            event_id=f"liminal_challenge_{int(time.time())}",
            event_type="liminal_challenge",
            content=challenge_description,
            timestamp=datetime.now(),
            source_layer="liminal_challenge",
            emotional_valence=self._calculate_challenge_valence(emotional_state),
            urgency_level=self.current_state.difficulty_level,
            context_metadata={
                "liminal_state": self.current_state.state_id,
                "emotional_state": emotional_state
            }
        )
        
        # Получение нейронного руководства
        direction = await self.brain_compass.process_navigation_event(challenge_event)
        analytics = self.brain_compass.get_navigation_analytics()
        
        # Специфичные советы для лиминального состояния
        liminal_advice = self._generate_liminal_advice(direction, challenge_description)
        
        navigation_result = {
            "challenge": challenge_description,
            "neural_guidance": {
                "direction": direction.primary_direction.value,
                "strength": direction.strength,
                "confidence": direction.confidence,
                "advice": liminal_advice
            },
            "oscillation_state": {
                "theta": f"{analytics['theta_oscillations']['theta_type']} @ {analytics['theta_oscillations']['current_frequency']:.1f}Hz",
                "gamma": f"{analytics['gamma_synchrony']['gamma_band']} @ {analytics['gamma_synchrony']['current_frequency']:.1f}Hz",
                "coupling": analytics['gamma_synchrony']['gamma_theta_coupling'],
                "memory_anchors": analytics['memory_anchors']
            },
            "emotional_navigation": self._analyze_emotional_navigation(emotional_state, direction),
            "timestamp": datetime.now().isoformat()
        }
        
        return navigation_result
    
    async def complete_liminal_transition(self, outcome_description: str) -> Dict[str, Any]:
        """
        Завершение лиминального перехода.
        """
        if not self.current_state:
            raise ValueError("No active liminal state to complete.")
        
        print(f"\n=== COMPLETING LIMINAL TRANSITION ===")
        print(f"Outcome: {outcome_description}")
        
        # Событие завершения
        completion_event = NavigationEvent(
            event_id=f"liminal_completion_{self.current_state.state_id}_{int(time.time())}",
            event_type="liminal_transition_completion",
            content=f"Completing transition from {self.current_state.description}. Outcome: {outcome_description}",
            timestamp=datetime.now(),
            source_layer="liminal_system",
            emotional_valence=0.7,  # Положительная валентность для завершения
            urgency_level=0.3       # Низкая срочность при завершении
        )
        
        # Финальная нейронная обработка
        final_direction = await self.brain_compass.process_navigation_event(completion_event)
        final_analytics = self.brain_compass.get_navigation_analytics()
        
        # Результат завершения
        completion_result = {
            "completed_state": self.current_state.state_id,
            "outcome": outcome_description,
            "final_direction": {
                "direction": final_direction.primary_direction.value,
                "strength": final_direction.strength,
                "integration_level": final_direction.confidence
            },
            "neural_integration": {
                "memory_anchors_created": final_analytics['memory_anchors'],
                "theta_gamma_coupling": final_analytics['gamma_synchrony']['gamma_theta_coupling'],
                "processing_events": final_analytics['events_processed']
            },
            "transition_summary": self._generate_transition_summary(),
            "completion_timestamp": datetime.now().isoformat()
        }
        
        # Очистка текущего состояния
        completed_state = self.current_state
        self.current_state = None
        
        # Логирование завершения
        self.transition_history.append({
            "action": "complete_liminal_transition",
            "state": completed_state.state_id,
            "outcome": outcome_description,
            "direction": final_direction.primary_direction.value,
            "timestamp": datetime.now().isoformat()
        })
        
        return completion_result
    
    def _calculate_emotional_valence(self, liminal_state: LiminalState) -> float:
        """Расчет эмоциональной валентности состояния."""
        positive_emotions = ["joy", "excitement", "hope", "inspiration", "flow", "clarity", "empowerment"]
        negative_emotions = ["anxiety", "distress", "confusion", "uncertainty", "fear"]
        
        positive_score = sum(value for emotion, value in liminal_state.emotional_profile.items() 
                           if emotion in positive_emotions)
        negative_score = sum(value for emotion, value in liminal_state.emotional_profile.items()
                           if emotion in negative_emotions)
        
        total = positive_score + negative_score
        if total == 0:
            return 0.0
        
        return (positive_score - negative_score) / total
    
    def _calculate_challenge_valence(self, emotional_state: Dict[str, float]) -> float:
        """Расчет валентности для вызова."""
        return sum(emotional_state.values()) / len(emotional_state) if emotional_state else 0.0
    
    def _generate_navigation_guidance(self, direction, liminal_state) -> str:
        """Генерация руководства по навигации."""
        direction_guidance = {
            SemanticDirection.NORTH: f"Focus on growth and learning within {liminal_state.description}. This is an opportunity for transcendence.",
            SemanticDirection.SOUTH: f"Ground yourself and ensure safety while in {liminal_state.description}. Return to your core foundations.",
            SemanticDirection.EAST: f"Channel your creative energy in {liminal_state.description}. This is time for manifestation and action.",
            SemanticDirection.WEST: f"Reflect deeply on the meaning of {liminal_state.description}. Understanding comes through introspection."
        }
        
        return direction_guidance.get(direction.primary_direction, "Navigate with awareness and intention.")
    
    def _generate_liminal_advice(self, direction, challenge: str) -> str:
        """Генерация специфических советов для лиминального вызова."""
        advice_templates = {
            SemanticDirection.NORTH: f"Transform this challenge '{challenge}' into a growth opportunity. What can you learn here?",
            SemanticDirection.SOUTH: f"For challenge '{challenge}', focus on what you need for emotional and physical safety right now.",
            SemanticDirection.EAST: f"Turn challenge '{challenge}' into creative action. What can you create or build from this experience?",
            SemanticDirection.WEST: f"Understand challenge '{challenge}' more deeply. What is it teaching you about yourself?"
        }
        
        return advice_templates.get(direction.primary_direction, "Navigate this challenge with mindful awareness.")
    
    def _analyze_emotional_navigation(self, emotional_state: Dict[str, float], direction) -> Dict[str, Any]:
        """Анализ эмоциональной навигации."""
        return {
            "emotional_state": emotional_state,
            "direction_alignment": f"{direction.primary_direction.value} provides {direction.primary_direction.description.lower()}",
            "strength_interpretation": "Strong" if direction.strength > 1.5 else "Moderate" if direction.strength > 1.0 else "Gentle",
            "confidence_level": "High" if direction.confidence > 0.7 else "Medium" if direction.confidence > 0.4 else "Developing"
        }
    
    def _generate_transition_summary(self) -> Dict[str, Any]:
        """Генерация сводки перехода."""
        if not self.transition_history:
            return {"message": "No transition data available"}
        
        # Анализ истории переходов
        directions_used = [event.get("direction") for event in self.transition_history if event.get("direction")]
        avg_strength = sum(event.get("strength", 0) for event in self.transition_history) / len(self.transition_history)
        
        return {
            "total_navigation_events": len(self.transition_history),
            "directions_explored": list(set(directions_used)),
            "average_direction_strength": avg_strength,
            "transition_pattern": "adaptive" if len(set(directions_used)) > 2 else "focused",
            "journey_description": f"Navigated through {self.current_state.description if self.current_state else 'liminal space'} using neural compass guidance"
        }
    
    def get_liminal_analytics(self) -> Dict[str, Any]:
        """Получение аналитики лиминальной системы."""
        brain_analytics = self.brain_compass.get_navigation_analytics()
        
        return {
            "system_status": "operational",
            "current_liminal_state": self.current_state.state_id if self.current_state else None,
            "available_states": list(self.liminal_states.keys()),
            "transition_history": self.transition_history[-5:],  # Last 5 transitions
            "brain_compass": brain_analytics,
            "integration_quality": {
                "neural_accuracy": "high",
                "liminal_mapping": "complete", 
                "real_time_processing": "enabled"
            }
        }