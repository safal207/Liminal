package graphql

import (
	"fmt"
	// "log" // убран неиспользуемый импорт

	"github.com/graphql-go/graphql"
)

// MutationResponse представляет ответ на мутацию
type MutationResponse struct {
	Success bool   `json:"success"`
	Message string `json:"message"`
	ID      string `json:"id,omitempty"`
}

// ConsciousnessStateInput представляет входные данные для состояния сознания
type ConsciousnessStateInput struct {
	State              string                  `json:"state"`
	Label              string                  `json:"label"`
	Description        string                  `json:"description,omitempty"`
	ColorClass         string                  `json:"colorClass,omitempty"`
	Metrics            *MetricsInput           `json:"metrics,omitempty"`
	PhilosophicalAspects *PhilosophicalAspectsInput `json:"philosophicalAspects,omitempty"`
}

// MetricsInput представляет входные данные для метрик
type MetricsInput struct {
	Clarity   float64 `json:"clarity,omitempty"`
	Depth     float64 `json:"depth,omitempty"`
	Resonance float64 `json:"resonance,omitempty"`
	Purity    float64 `json:"purity,omitempty"`
}

// PhilosophicalAspectsInput представляет входные данные для философских аспектов
type PhilosophicalAspectsInput struct {
	PathOfPurity       bool    `json:"pathOfPurity,omitempty"`
	HomeStateDetection float64 `json:"homeStateDetection,omitempty"`
	ResonanceFrequency float64 `json:"resonanceFrequency,omitempty"`
	InnerTruthfulness  float64 `json:"innerTruthfulness,omitempty"`
}

// TransitionInput представляет входные данные для перехода
type TransitionInput struct {
	SourceID                string `json:"sourceId"`
	TargetID                string `json:"targetId"`
	Trigger                 string `json:"trigger"`
	TriggerLabel            string `json:"triggerLabel,omitempty"`
	PhilosophicalSignificance string `json:"philosophicalSignificance,omitempty"`
}

// Определение типов для входных данных
var metricsInputType = graphql.NewInputObject(graphql.InputObjectConfig{
	Name: "MetricsInput",
	Fields: graphql.InputObjectConfigFieldMap{
		"clarity":   &graphql.InputObjectFieldConfig{Type: graphql.Float},
		"depth":     &graphql.InputObjectFieldConfig{Type: graphql.Float},
		"resonance": &graphql.InputObjectFieldConfig{Type: graphql.Float},
		"purity":    &graphql.InputObjectFieldConfig{Type: graphql.Float},
	},
})

var philosophicalAspectsInputType = graphql.NewInputObject(graphql.InputObjectConfig{
	Name: "PhilosophicalAspectsInput",
	Fields: graphql.InputObjectConfigFieldMap{
		"pathOfPurity":       &graphql.InputObjectFieldConfig{Type: graphql.Boolean},
		"homeStateDetection": &graphql.InputObjectFieldConfig{Type: graphql.Float},
		"resonanceFrequency": &graphql.InputObjectFieldConfig{Type: graphql.Float},
		"innerTruthfulness":  &graphql.InputObjectFieldConfig{Type: graphql.Float},
	},
})

var consciousnessStateInputType = graphql.NewInputObject(graphql.InputObjectConfig{
	Name: "ConsciousnessStateInput",
	Fields: graphql.InputObjectConfigFieldMap{
		"state":              &graphql.InputObjectFieldConfig{Type: graphql.NewNonNull(graphql.String)},
		"label":              &graphql.InputObjectFieldConfig{Type: graphql.NewNonNull(graphql.String)},
		"description":        &graphql.InputObjectFieldConfig{Type: graphql.String},
		"colorClass":         &graphql.InputObjectFieldConfig{Type: graphql.String},
		"metrics":            &graphql.InputObjectFieldConfig{Type: metricsInputType},
		"philosophicalAspects": &graphql.InputObjectFieldConfig{Type: philosophicalAspectsInputType},
	},
})

var transitionInputType = graphql.NewInputObject(graphql.InputObjectConfig{
	Name: "TransitionInput",
	Fields: graphql.InputObjectConfigFieldMap{
		"sourceId":               &graphql.InputObjectFieldConfig{Type: graphql.NewNonNull(graphql.ID)},
		"targetId":               &graphql.InputObjectFieldConfig{Type: graphql.NewNonNull(graphql.ID)},
		"trigger":                &graphql.InputObjectFieldConfig{Type: graphql.NewNonNull(graphql.String)},
		"triggerLabel":           &graphql.InputObjectFieldConfig{Type: graphql.String},
		"philosophicalSignificance": &graphql.InputObjectFieldConfig{Type: graphql.String},
	},
})

var mutationResponseType = graphql.NewObject(graphql.ObjectConfig{
	Name: "MutationResponse",
	Fields: graphql.Fields{
		"success": &graphql.Field{Type: graphql.NewNonNull(graphql.Boolean)},
		"message": &graphql.Field{Type: graphql.String},
		"id":      &graphql.Field{Type: graphql.ID},
	},
})

// CreateMutationType создает тип мутации для GraphQL схемы
func CreateMutationType(subscriptionManager *SubscriptionManager) *graphql.Object {
	return graphql.NewObject(graphql.ObjectConfig{
		Name: "Mutation",
		Fields: graphql.Fields{
			// Создание нового состояния сознания
			"createConsciousnessState": &graphql.Field{
				Type: mutationResponseType,
				Args: graphql.FieldConfigArgument{
					"input": &graphql.ArgumentConfig{
						Type: graphql.NewNonNull(consciousnessStateInputType),
					},
				},
				Resolve: func(p graphql.ResolveParams) (interface{}, error) {
					input, _ := p.Args["input"].(map[string]interface{})
					
					// Преобразуем входные данные в структуру
					stateInput := &ConsciousnessStateInput{}
					if state, ok := input["state"].(string); ok {
						stateInput.State = state
					}
					if label, ok := input["label"].(string); ok {
						stateInput.Label = label
					}
					if description, ok := input["description"].(string); ok {
						stateInput.Description = description
					}
					if colorClass, ok := input["colorClass"].(string); ok {
						stateInput.ColorClass = colorClass
					}
					
					// Создаем новое состояние сознания
					newState := &ConsciousnessState{
						ID:          generateID(),
						State:       stateInput.State,
						Label:       stateInput.Label,
						Description: stateInput.Description,
						ColorClass:  stateInput.ColorClass,
						Metrics: &Metrics{
							Clarity:   0.5,
							Depth:     0.5,
							Resonance: 0.5,
							Purity:    0.5,
						},
						PhilosophicalAspects: &PhilosophicalAspects{
							PathOfPurity:       false,
							HomeStateDetection: 0.0,
							ResonanceFrequency: 0.5,
							InnerTruthfulness:  0.5,
						},
					}
					
					// Обрабатываем метрики, если они предоставлены
					if metricsInput, ok := input["metrics"].(map[string]interface{}); ok {
						if clarity, ok := metricsInput["clarity"].(float64); ok {
							newState.Metrics.Clarity = clarity
						}
						if depth, ok := metricsInput["depth"].(float64); ok {
							newState.Metrics.Depth = depth
						}
						if resonance, ok := metricsInput["resonance"].(float64); ok {
							newState.Metrics.Resonance = resonance
						}
						if purity, ok := metricsInput["purity"].(float64); ok {
							newState.Metrics.Purity = purity
						}
					}
					
					// Обрабатываем философские аспекты, если они предоставлены
					if aspectsInput, ok := input["philosophicalAspects"].(map[string]interface{}); ok {
						if pathOfPurity, ok := aspectsInput["pathOfPurity"].(bool); ok {
							newState.PhilosophicalAspects.PathOfPurity = pathOfPurity
						}
						if homeStateDetection, ok := aspectsInput["homeStateDetection"].(float64); ok {
							newState.PhilosophicalAspects.HomeStateDetection = homeStateDetection
						}
						if resonanceFrequency, ok := aspectsInput["resonanceFrequency"].(float64); ok {
							newState.PhilosophicalAspects.ResonanceFrequency = resonanceFrequency
						}
						if innerTruthfulness, ok := aspectsInput["innerTruthfulness"].(float64); ok {
							newState.PhilosophicalAspects.InnerTruthfulness = innerTruthfulness
						}
					}
					
					// Добавляем состояние в хранилище данных (в реальном приложении здесь будет сохранение в Neo4j)
					// Для демонстрации просто добавляем в глобальный массив
					demoConsciousnessStates = append(demoConsciousnessStates, newState)
					
					// Уведомляем подписчиков о новом состоянии
					if subscriptionManager != nil {
						subscriptionManager.Publish("consciousnessStateUpdated", newState, nil)
						
						// Обновляем граф сознания
						graph := &ConsciousnessGraph{
							Nodes:           demoConsciousnessStates,
							Links:           demoTransitions,
							ActiveState:     "home_authentic",
							PathOfPurityScore: 0.75,
						}
						subscriptionManager.Publish("consciousnessGraphUpdated", graph, nil)
					}
					
					return &MutationResponse{
						Success: true,
						Message: fmt.Sprintf("Состояние сознания '%s' успешно создано", stateInput.Label),
						ID:      newState.ID,
					}, nil
				},
			},
			
			// Активация состояния сознания (переход в новое состояние)
			"activateState": &graphql.Field{
				Type: mutationResponseType,
				Args: graphql.FieldConfigArgument{
					"id": &graphql.ArgumentConfig{
						Type: graphql.NewNonNull(graphql.ID),
					},
				},
				Resolve: func(p graphql.ResolveParams) (interface{}, error) {
					id, _ := p.Args["id"].(string)
					
					// Находим состояние по ID
					var targetState *ConsciousnessState
					for _, state := range demoConsciousnessStates {
						if state.ID == id {
							targetState = state
							break
						}
					}
					
					if targetState == nil {
						return &MutationResponse{
							Success: false,
							Message: fmt.Sprintf("Состояние с ID '%s' не найдено", id),
						}, nil
					}
					
					// Активируем состояние (в реальном приложении здесь будет обновление в Neo4j)
					activeStateID := targetState.ID
					
					// Уведомляем подписчиков об изменении активного состояния
					if subscriptionManager != nil {
						subscriptionManager.Publish("activeStateChanged", targetState, nil)
					}
					
					return &MutationResponse{
						Success: true,
						Message: fmt.Sprintf("Состояние '%s' активировано", targetState.Label),
						ID:      activeStateID,
					}, nil
				},
			},
			
			// Создание нового перехода между состояниями
			"createTransition": &graphql.Field{
				Type: mutationResponseType,
				Args: graphql.FieldConfigArgument{
					"input": &graphql.ArgumentConfig{
						Type: graphql.NewNonNull(transitionInputType),
					},
				},
				Resolve: func(p graphql.ResolveParams) (interface{}, error) {
					input, _ := p.Args["input"].(map[string]interface{})
					
					// Преобразуем входные данные в структуру
					transInput := &TransitionInput{}
					if sourceID, ok := input["sourceId"].(string); ok {
						transInput.SourceID = sourceID
					}
					if targetID, ok := input["targetId"].(string); ok {
						transInput.TargetID = targetID
					}
					if trigger, ok := input["trigger"].(string); ok {
						transInput.Trigger = trigger
					}
					if triggerLabel, ok := input["triggerLabel"].(string); ok {
						transInput.TriggerLabel = triggerLabel
					}
					if significance, ok := input["philosophicalSignificance"].(string); ok {
						transInput.PhilosophicalSignificance = significance
					}
					
					// Находим исходное и целевое состояния
					var sourceState, targetState *ConsciousnessState
					for _, state := range demoConsciousnessStates {
						if state.ID == transInput.SourceID {
							sourceState = state
						}
						if state.ID == transInput.TargetID {
							targetState = state
						}
					}
					
					if sourceState == nil || targetState == nil {
						return &MutationResponse{
							Success: false,
							Message: "Исходное или целевое состояние не найдено",
						}, nil
					}
					
					// Создаем новый переход
					newTransition := &Transition{
						ID:                     fmt.Sprintf("%s-%s", transInput.SourceID, transInput.TargetID),
						Source:                 sourceState,
						Target:                 targetState,
						Trigger:                transInput.Trigger,
						TriggerLabel:           transInput.TriggerLabel,
						Count:                  1,
						PhilosophicalSignificance: transInput.PhilosophicalSignificance,
						TemporalPatterns: &TemporalPatterns{
							Frequency:       0.1,
							AverageDuration: 60,
							CycleDetection:  false,
						},
					}
					
					// Добавляем переход в хранилище данных (в реальном приложении здесь будет сохранение в Neo4j)
					demoTransitions = append(demoTransitions, newTransition)
					
					// Уведомляем подписчиков о новом переходе
					if subscriptionManager != nil {
						subscriptionManager.Publish("transitionExecuted", newTransition, nil)
						
						// Обновляем граф сознания
						graph := &ConsciousnessGraph{
							Nodes:           demoConsciousnessStates,
							Links:           demoTransitions,
							ActiveState:     "home_authentic",
							PathOfPurityScore: 0.75,
						}
						subscriptionManager.Publish("consciousnessGraphUpdated", graph, nil)
					}
					
					return &MutationResponse{
						Success: true,
						Message: fmt.Sprintf("Переход от '%s' к '%s' успешно создан", sourceState.Label, targetState.Label),
						ID:      newTransition.ID,
					}, nil
				},
			},
		},
	})
}
