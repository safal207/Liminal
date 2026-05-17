package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/neo4j/neo4j-go-driver/v4/neo4j"
)

var (
	neo4jURI      = getEnvOrDefault("NEO4J_URI", "neo4j://localhost:7687")
	neo4jUsername = getEnvOrDefault("NEO4J_USERNAME", "neo4j")
	neo4jPassword = mustGetEnv("NEO4J_PASSWORD")
)

func getEnvOrDefault(key, fallback string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return fallback
}

func mustGetEnv(key string) string {
	v := os.Getenv(key)
	if v == "" {
		log.Fatalf("required environment variable %s is not set", key)
	}
	return v
}

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

		idVal, _ := record.Get("id")
		stateVal, _ := record.Get("state")
		labelVal, _ := record.Get("label")
		descVal, _ := record.Get("description")
		presenceVal, _ := record.Get("presenceLevel")
		harmonyVal, _ := record.Get("harmonyIndex")
		authVal, _ := record.Get("authenticityScore")
		chargeVal, _ := record.Get("emotionalCharge")

		id, ok1 := idVal.(string)
		stateStr, ok2 := stateVal.(string)
		labelStr, ok3 := labelVal.(string)
		descStr, ok4 := descVal.(string)
		presenceLevel, ok5 := presenceVal.(float64)
		harmonyIndex, ok6 := harmonyVal.(float64)
		authenticityScore, ok7 := authVal.(float64)
		emotionalCharge, ok8 := chargeVal.(float64)

		if !ok1 || !ok2 || !ok3 || !ok4 || !ok5 || !ok6 || !ok7 || !ok8 {
			log.Printf("Skipping node record with unexpected field types: id=%v state=%v", idVal, stateVal)
			continue
		}

		node := Node{
			ID:                id,
			State:             stateStr,
			Label:             labelStr,
			Description:       descStr,
			PresenceLevel:     presenceLevel,
			HarmonyIndex:      harmonyIndex,
			AuthenticityScore: authenticityScore,
			EmotionalCharge:   emotionalCharge,
			ColorClass:        "node-" + stateStr,
		}

		result.Nodes = append(result.Nodes, node)
		nodeIDs[id] = true
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

		srcVal, _ := record.Get("sourceId")
		tgtVal, _ := record.Get("targetId")
		triggerVal, _ := record.Get("trigger")
		countVal, _ := record.Get("count")

		sourceID, ok1 := srcVal.(string)
		targetID, ok2 := tgtVal.(string)
		trigger, ok3 := triggerVal.(string)
		countInt, ok4 := countVal.(int64)

		if !ok1 || !ok2 || !ok3 || !ok4 {
			log.Printf("Skipping link record with unexpected field types: src=%v tgt=%v", srcVal, tgtVal)
			continue
		}

		if !nodeIDs[sourceID] || !nodeIDs[targetID] {
			continue
		}

		link := Link{
			ID:      sourceID + "-" + targetID,
			Source:  sourceID,
			Target:  targetID,
			Trigger: trigger,
			Count:   int(countInt),
		}

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
