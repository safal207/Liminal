package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"

	"github.com/neo4j/neo4j-go-driver/v4/neo4j"
)

// Neo4j credentials - эти же используются в Python backend
const (
	neo4jURI      = "neo4j://localhost:7687"
	neo4jUsername = "neo4j"
	neo4jPassword = "NewStrongPass123!"
)

// GraphData структура для представления графа состояний сознания
type GraphData struct {
	Nodes []Node `json:"nodes"`
	Links []Link `json:"links"`
}

// Node представляет узел состояния сознания с философскими метриками
type Node struct {
	ID                string  `json:"id"`
	State             string  `json:"state"`
	Label             string  `json:"label"`
	Description       string  `json:"description"`
	PresenceLevel     float64 `json:"presenceLevel"`
	HarmonyIndex      float64 `json:"harmonyIndex"`
	AuthenticityScore float64 `json:"authenticityScore"`
	EmotionalCharge   float64 `json:"emotionalCharge"`
	ColorClass        string  `json:"colorClass"`
}

// Link представляет связь между состояниями сознания
type Link struct {
	ID      string `json:"id"`
	Source  string `json:"source"`
	Target  string `json:"target"`
	Trigger string `json:"trigger"`
	Count   int    `json:"count"`
}

// EventMessage представляет сообщение о переходе между состояниями сознания
type EventMessage struct {
	Source      string `json:"source"`
	Target      string `json:"target"`
	Type        string `json:"type"`
	Description string `json:"description"`
	Insight     string `json:"insight,omitempty"`
}

// handleGetConsciousnessGraph обрабатывает запрос на получение данных графа сознания из Neo4j
func handleGetConsciousnessGraph(w http.ResponseWriter, r *http.Request) {
	log.Println("Получен запрос на данные графа сознания")
	
	// Настройка CORS заголовков
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	
	// Обработка OPTIONS запроса для CORS preflight
	if r.Method == http.MethodOptions {
		w.WriteHeader(http.StatusOK)
		return
	}
	
	// Получаем данные из Neo4j или используем демо-данные
	graphData, err := getConsciousnessGraphFromNeo4j()
	if err != nil {
		log.Printf("Ошибка при получении данных из Neo4j: %v. Используем демо-данные.", err)
		// Если не удалось получить данные из Neo4j, используем демо-данные
		graphData = getConsciousnessDemoData()
	}
	
	// Отправляем данные как JSON
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(graphData)
}

// getConsciousnessGraphFromNeo4j получает данные графа сознания из Neo4j
func getConsciousnessGraphFromNeo4j() (*GraphData, error) {
	log.Println("Попытка подключения к Neo4j:", neo4jURI)
	
	// Создание драйвера Neo4j
	driver, err := neo4j.NewDriver(neo4jURI, neo4j.BasicAuth(neo4jUsername, neo4jPassword, ""))
	if err != nil {
		return nil, fmt.Errorf("ошибка при создании драйвера Neo4j: %w", err)
	}
	defer driver.Close()
	
	// Проверяем соединение
	if err := driver.VerifyConnectivity(); err != nil {
		return nil, fmt.Errorf("ошибка при подключении к Neo4j: %w", err)
	}
	
	// Получаем сессию
	session := driver.NewSession(neo4j.SessionConfig{AccessMode: neo4j.AccessModeRead})
	defer session.Close()
	
	// Граф для результата
	result := &GraphData{
		Nodes: []Node{},
		Links: []Link{},
	}
	
	// Получение узлов (состояний)
	nodeQuery := `
		MATCH (n:ConsciousnessState)
		RETURN n.id as id, n.state as state, n.label as label, n.description as description, 
		       n.presenceLevel as presenceLevel, n.harmonyIndex as harmonyIndex, 
		       n.authenticityScore as authenticityScore, n.emotionalCharge as emotionalCharge
	`
	
	// Запуск запроса для получения узлов
	nodeRecords, err := session.Run(nodeQuery, map[string]interface{}{})
	if err != nil {
		return nil, fmt.Errorf("ошибка при выполнении запроса узлов: %w", err)
	}
	
	// Обработка результатов запроса узлов
	nodeIDs := make(map[string]bool)
	for nodeRecords.Next() {
		record := nodeRecords.Record()
		
		id, _ := record.Get("id")
		state, _ := record.Get("state")
		label, _ := record.Get("label")
		description, _ := record.Get("description")
		presenceLevel, _ := record.Get("presenceLevel")
		harmonyIndex, _ := record.Get("harmonyIndex")
		authenticityScore, _ := record.Get("authenticityScore")
		emotionalCharge, _ := record.Get("emotionalCharge")
		
		// Создаем узел
		node := Node{
			ID:                id.(string),
			State:             state.(string),
			Label:             label.(string),
			Description:       description.(string),
			PresenceLevel:     presenceLevel.(float64),
			HarmonyIndex:      harmonyIndex.(float64),
			AuthenticityScore: authenticityScore.(float64),
			EmotionalCharge:   emotionalCharge.(float64),
			ColorClass:        "node-" + state.(string),
		}
		
		// Добавляем узел в результат
		result.Nodes = append(result.Nodes, node)
		nodeIDs[id.(string)] = true
	}
	
	// Получение связей (переходов)
	linkQuery := `
		MATCH (source:ConsciousnessState)-[r:TRANSITIONS_TO]->(target:ConsciousnessState)
		RETURN source.id as sourceId, target.id as targetId, 
		       r.trigger as trigger, r.count as count
	`
	
	// Запуск запроса для получения связей
	linkRecords, err := session.Run(linkQuery, map[string]interface{}{})
	if err != nil {
		return nil, fmt.Errorf("ошибка при выполнении запроса связей: %w", err)
	}
	
	// Обработка результатов запроса связей
	for linkRecords.Next() {
		record := linkRecords.Record()
		
		sourceId, _ := record.Get("sourceId")
		targetId, _ := record.Get("targetId")
		trigger, _ := record.Get("trigger")
		count, _ := record.Get("count")
		
		// Проверяем, что узлы существуют
		if !nodeIDs[sourceId.(string)] || !nodeIDs[targetId.(string)] {
			continue
		}
		
		// Создаем связь
		link := Link{
			ID:       sourceId.(string) + "-" + targetId.(string),
			Source:   sourceId.(string),
			Target:   targetId.(string),
			Trigger:  trigger.(string),
			Count:    int(count.(int64)),
		}
		
		// Добавляем связь в результат
		result.Links = append(result.Links, link)
	}
	
	log.Printf("Успешно получены данные из Neo4j: %d узлов, %d связей", 
		len(result.Nodes), len(result.Links))
	
	return result, nil
}

// getConsciousnessDemoData возвращает демо-данные графа сознания
func getConsciousnessDemoData() *GraphData {
	log.Println("Генерация демо-данных графа сознания на основе философских концепций")
	
	// Состояния сознания на основе философских концепций из документации
	nodes := []Node{
		{
			ID:                "TRANSITION_LIMINAL",
			State:             "TRANSITION_LIMINAL",
			Label:             "Лиминальное переходное",
			Description:       "Между мирами, в процессе становления",
			PresenceLevel:     0.3,
			HarmonyIndex:      0.2,
			AuthenticityScore: 0.4,
			EmotionalCharge:   65,
			ColorClass:        "node-liminal",
		},
		{
			ID:                "PRESENCE_NOW",
			State:             "PRESENCE_NOW",
			Label:             "Осознанное присутствие",
			Description:       "Полное присутствие в настоящем моменте",
			PresenceLevel:     0.9,
			HarmonyIndex:      0.6,
			AuthenticityScore: 0.7,
			EmotionalCharge:   50,
			ColorClass:        "node-presence",
		},
		{
			ID:                "HARMONY_BALANCE",
			State:             "HARMONY_BALANCE",
			Label:             "Гармония и баланс",
			Description:       "Состояние внутреннего равновесия и целостности",
			PresenceLevel:     0.8,
			HarmonyIndex:      0.9,
			AuthenticityScore: 0.8,
			EmotionalCharge:   30,
			ColorClass:        "node-harmony",
		},
		{
			ID:                "HOME_AUTHENTIC",
			State:             "HOME_AUTHENTIC",
			Label:             "Аутентичный дом",
			Description:       "Дом - это ты, когда искренен с собой",
			PresenceLevel:     0.7,
			HarmonyIndex:      0.8,
			AuthenticityScore: 0.95,
			EmotionalCharge:   40,
			ColorClass:        "node-home",
		},
		{
			ID:                "QUESTION_SPACE",
			State:             "QUESTION_SPACE",
			Label:             "Пространство вопросов",
			Description:       "Мы научились задавать правильные вопросы",
			PresenceLevel:     0.6,
			HarmonyIndex:      0.7,
			AuthenticityScore: 0.85,
			EmotionalCharge:   55,
			ColorClass:        "node-question",
		},
	}
	
	// Переходы между состояниями на основе философских концепций
	links := []Link{
		{
			ID:      "TRANSITION_LIMINAL-PRESENCE_NOW",
			Source:  "TRANSITION_LIMINAL",
			Target:  "PRESENCE_NOW",
			Trigger: "DEEP_BREATH",
			Count:   3,
		},
		{
			ID:      "PRESENCE_NOW-HARMONY_BALANCE",
			Source:  "PRESENCE_NOW",
			Target:  "HARMONY_BALANCE",
			Trigger: "MEDITATION",
			Count:   5,
		},
		{
			ID:      "HARMONY_BALANCE-HOME_AUTHENTIC",
			Source:  "HARMONY_BALANCE",
			Target:  "HOME_AUTHENTIC",
			Trigger: "AUTHENTICITY_INSIGHT",
			Count:   2,
		},
		{
			ID:      "HOME_AUTHENTIC-QUESTION_SPACE",
			Source:  "HOME_AUTHENTIC",
			Target:  "QUESTION_SPACE",
			Trigger: "CURIOSITY",
			Count:   4,
		},
		{
			ID:      "QUESTION_SPACE-TRANSITION_LIMINAL",
			Source:  "QUESTION_SPACE",
			Target:  "TRANSITION_LIMINAL",
			Trigger: "UNCERTAINTY",
			Count:   1,
		},
	}
	
	return &GraphData{
		Nodes: nodes,
		Links: links,
	}
}
