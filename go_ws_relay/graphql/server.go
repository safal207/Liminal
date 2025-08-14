package graphql

import (
	"encoding/json"
	// "log" // убран неиспользуемый импорт
	"net/http"
	"sync"

	"github.com/graphql-go/graphql"
)

// GraphQLServer представляет собой сервер GraphQL для графа сознания
type GraphQLServer struct {
	schema             graphql.Schema
	subscriptionManager *SubscriptionManager
	mutex              sync.RWMutex
}

// ConsciousnessState представляет состояние сознания
type ConsciousnessState struct {
	ID                string              `json:"id"`
	State             string              `json:"state"`
	Label             string              `json:"label"`
	Description       string              `json:"description"`
	ColorClass        string              `json:"colorClass"`
	Metrics           *Metrics            `json:"metrics"`
	PhilosophicalAspects *PhilosophicalAspects `json:"philosophicalAspects"`
}

// Metrics представляет метрики состояния сознания
type Metrics struct {
	Clarity   float64 `json:"clarity"`
	Depth     float64 `json:"depth"`
	Resonance float64 `json:"resonance"`
	Purity    float64 `json:"purity"`
}

// PhilosophicalAspects представляет философские аспекты состояния
type PhilosophicalAspects struct {
	PathOfPurity       bool    `json:"pathOfPurity"`
	HomeStateDetection float64 `json:"homeStateDetection"`
	ResonanceFrequency float64 `json:"resonanceFrequency"`
	InnerTruthfulness  float64 `json:"innerTruthfulness"`
}

// Transition представляет переход между состояниями сознания
type Transition struct {
	ID                     string           `json:"id"`
	Source                 *ConsciousnessState `json:"source"`
	Target                 *ConsciousnessState `json:"target"`
	Trigger                string           `json:"trigger"`
	TriggerLabel           string           `json:"triggerLabel"`
	Count                  int              `json:"count"`
	TemporalPatterns       *TemporalPatterns `json:"temporalPatterns"`
	PhilosophicalSignificance string         `json:"philosophicalSignificance"`
}

// TemporalPatterns представляет временные паттерны перехода
type TemporalPatterns struct {
	Frequency       float64 `json:"frequency"`
	AverageDuration int     `json:"averageDuration"`
	CycleDetection  bool    `json:"cycleDetection"`
}

// ConsciousnessGraph представляет граф сознания
type ConsciousnessGraph struct {
	Nodes           []*ConsciousnessState `json:"nodes"`
	Links           []*Transition        `json:"links"`
	ActiveState     string               `json:"activeState"`
	PathOfPurityScore float64            `json:"pathOfPurityScore"`
}

// Глобальные демо-данные для GraphQL сервера
var (
	demoConsciousnessStates []*ConsciousnessState
	demoTransitions        []*Transition
)

// NewGraphQLServer создает новый сервер GraphQL
func NewGraphQLServer() (*GraphQLServer, error) {
	// Инициализируем менеджер подписок
	subscriptionManager := NewSubscriptionManager()

	// Инициализируем демо-данные
	demoConsciousnessStates = []*ConsciousnessState{
		{
			ID:    "home_authentic",
			State: "home_authentic",
			Label: "Дом (Аутентичность)",
			Description: "Состояние искренности с собой",
			ColorClass: "node-home_authentic",
			Metrics: &Metrics{
				Clarity:   0.9,
				Depth:     0.7,
				Resonance: 0.8,
				Purity:    0.95,
			},
			PhilosophicalAspects: &PhilosophicalAspects{
				PathOfPurity:       true,
				HomeStateDetection: 1.0,
				ResonanceFrequency: 0.85,
				InnerTruthfulness:  0.9,
			},
		},
		{
			ID:    "flow",
			State: "flow",
			Label: "Поток",
			Description: "Состояние полного погружения в деятельность",
			ColorClass: "node-flow",
			Metrics: &Metrics{
				Clarity:   0.8,
				Depth:     0.9,
				Resonance: 0.7,
				Purity:    0.85,
			},
			PhilosophicalAspects: &PhilosophicalAspects{
				PathOfPurity:       true,
				HomeStateDetection: 0.3,
				ResonanceFrequency: 0.9,
				InnerTruthfulness:  0.8,
			},
		},
		{
			ID:    "contemplation",
			State: "contemplation",
			Label: "Созерцание",
			Description: "Состояние глубокого наблюдения",
			ColorClass: "node-contemplation",
			Metrics: &Metrics{
				Clarity:   0.95,
				Depth:     0.8,
				Resonance: 0.6,
				Purity:    0.9,
			},
			PhilosophicalAspects: &PhilosophicalAspects{
				PathOfPurity:       true,
				HomeStateDetection: 0.5,
				ResonanceFrequency: 0.7,
				InnerTruthfulness:  0.95,
			},
		},
	}
	
	demoTransitions = []*Transition{
		{
			ID:     "home_authentic-flow",
			Source: demoConsciousnessStates[0], // home_authentic
			Target: demoConsciousnessStates[1], // flow
			Trigger: "deep_focus",
			TriggerLabel: "Глубокий фокус",
			Count:  3,
			PhilosophicalSignificance: "Переход от аутентичности к потоку через глубокий фокус",
			TemporalPatterns: &TemporalPatterns{
				Frequency:       0.3,
				AverageDuration: 120,
				CycleDetection:  false,
			},
		},
		{
			ID:     "flow-contemplation",
			Source: demoConsciousnessStates[1], // flow
			Target: demoConsciousnessStates[2], // contemplation
			Trigger: "natural_pause",
			TriggerLabel: "Естественная пауза",
			Count:  2,
			PhilosophicalSignificance: "Переход от потока к созерцанию через естественную паузу",
			TemporalPatterns: &TemporalPatterns{
				Frequency:       0.2,
				AverageDuration: 60,
				CycleDetection:  false,
			},
		},
		{
			ID:     "contemplation-home_authentic",
			Source: demoConsciousnessStates[2], // contemplation
			Target: demoConsciousnessStates[0], // home_authentic
			Trigger: "insight_integration",
			TriggerLabel: "Интеграция инсайта",
			Count:  1,
			PhilosophicalSignificance: "Возвращение к аутентичности через интеграцию инсайта",
			TemporalPatterns: &TemporalPatterns{
				Frequency:       0.1,
				AverageDuration: 90,
				CycleDetection:  true,
			},
		},
	}
	// Определение типа ConsciousnessState
	consciousnessStateType := graphql.NewObject(graphql.ObjectConfig{
		Name: "ConsciousnessState",
		Fields: graphql.Fields{
			"id":        &graphql.Field{Type: graphql.ID},
			"state":     &graphql.Field{Type: graphql.String},
			"label":     &graphql.Field{Type: graphql.String},
			"description": &graphql.Field{Type: graphql.String},
			"colorClass": &graphql.Field{Type: graphql.String},
			"metrics": &graphql.Field{
				Type: graphql.NewObject(graphql.ObjectConfig{
					Name: "Metrics",
					Fields: graphql.Fields{
						"clarity":   &graphql.Field{Type: graphql.Float},
						"depth":     &graphql.Field{Type: graphql.Float},
						"resonance": &graphql.Field{Type: graphql.Float},
						"purity":    &graphql.Field{Type: graphql.Float},
					},
				}),
			},
			"philosophicalAspects": &graphql.Field{
				Type: graphql.NewObject(graphql.ObjectConfig{
					Name: "PhilosophicalAspects",
					Fields: graphql.Fields{
						"pathOfPurity":       &graphql.Field{Type: graphql.Boolean},
						"homeStateDetection": &graphql.Field{Type: graphql.Float},
						"resonanceFrequency": &graphql.Field{Type: graphql.Float},
						"innerTruthfulness":  &graphql.Field{Type: graphql.Float},
					},
				}),
			},
		},
	})

	// Определение типа Transition и других типов...
	// (Упрощено для примера)

	// Определение корневого Query типа
	queryType := graphql.NewObject(graphql.ObjectConfig{
		Name: "Query",
		Fields: graphql.Fields{
			"consciousnessGraph": &graphql.Field{
				Type: graphql.NewObject(graphql.ObjectConfig{
					Name: "ConsciousnessGraph",
					Fields: graphql.Fields{
						"nodes": &graphql.Field{
							Type: graphql.NewList(consciousnessStateType),
						},
						"links": &graphql.Field{
							// Упрощено для примера
							Type: graphql.NewList(graphql.String),
						},
						"activeState": &graphql.Field{
							Type: graphql.String,
						},
						"pathOfPurityScore": &graphql.Field{
							Type: graphql.Float,
						},
					},
				}),
				Resolve: func(p graphql.ResolveParams) (interface{}, error) {
					// Возвращаем демо-данные для графа сознания
					return &ConsciousnessGraph{
						Nodes:           demoConsciousnessStates,
						Links:           demoTransitions,
						ActiveState:     "home_authentic",
						PathOfPurityScore: 0.75,
					}, nil
				},
			},
			// Другие запросы из схемы...
		},
	})

	// Добавляем тип мутации
	mutationType := CreateMutationType(subscriptionManager)

	// Создаем схему GraphQL с поддержкой мутаций
	schema, err := graphql.NewSchema(graphql.SchemaConfig{
		Query: queryType,
		Mutation: mutationType,
	})
	if err != nil {
		return nil, err
	}

	return &GraphQLServer{
		schema: schema,
		subscriptionManager: subscriptionManager,
	}, nil
}

// HandleGraphQL обрабатывает GraphQL запросы
func (s *GraphQLServer) HandleGraphQL(w http.ResponseWriter, r *http.Request) {
	// Разрешаем CORS
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

	// Обработка preflight запросов
	if r.Method == http.MethodOptions {
		w.WriteHeader(http.StatusOK)
		return
	}

	// Проверяем метод запроса
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Декодируем запрос
	var params struct {
		Query         string                 `json:"query"`
		OperationName string                 `json:"operationName"`
		Variables     map[string]interface{} `json:"variables"`
	}

	if err := json.NewDecoder(r.Body).Decode(&params); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Выполняем запрос
	result := graphql.Do(graphql.Params{
		Schema:         s.schema,
		RequestString:  params.Query,
		OperationName:  params.OperationName,
		VariableValues: params.Variables,
		Context:        r.Context(),
	})

	// Возвращаем результат
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(result)
}

// HandleWebSocket обрабатывает WebSocket соединения для GraphQL подписок
func (s *GraphQLServer) HandleWebSocket(w http.ResponseWriter, r *http.Request) {
	// Разрешаем CORS
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

	// Обработка preflight запросов
	if r.Method == http.MethodOptions {
		w.WriteHeader(http.StatusOK)
		return
	}

	// Проверяем метод запроса
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Инициализируем WebSocket соединение для подписок
	s.subscriptionManager.HandleSubscription(w, r)
}

// getConsciousnessGraphData возвращает демо-данные графа сознания
func getConsciousnessGraphData() (*ConsciousnessGraph, error) {
	// Здесь будем получать данные из Neo4j
	// Возвращаем демо-данные для примера
	
	meditativeState := &ConsciousnessState{
		ID:          "meditative",
		State:       "meditative",
		Label:       "Медитативное",
		Description: "Состояние глубокой медитации и внутреннего покоя",
		ColorClass:  "node-meditative",
		Metrics: &Metrics{
			Clarity:   0.9,
			Depth:     0.85,
			Resonance: 0.7,
			Purity:    0.95,
		},
		PhilosophicalAspects: &PhilosophicalAspects{
			PathOfPurity:       true,
			HomeStateDetection: 0.9,
			ResonanceFrequency: 0.85,
			InnerTruthfulness:  0.95,
		},
	}
	
	flowState := &ConsciousnessState{
		ID:          "flow",
		State:       "flow",
		Label:       "Поток",
		Description: "Состояние полного погружения и гармонии с деятельностью",
		ColorClass:  "node-flow",
		Metrics: &Metrics{
			Clarity:   0.8,
			Depth:     0.7,
			Resonance: 0.9,
			Purity:    0.85,
		},
		PhilosophicalAspects: &PhilosophicalAspects{
			PathOfPurity:       true,
			HomeStateDetection: 0.85,
			ResonanceFrequency: 0.9,
			InnerTruthfulness:  0.8,
		},
	}
	
	// Создание графа
	graph := &ConsciousnessGraph{
		Nodes:           []*ConsciousnessState{meditativeState, flowState},
		Links:           []*Transition{},
		ActiveState:     "meditative",
		PathOfPurityScore: 0.9,
	}
	
	return graph, nil
}
