package main

import (
	"context"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"
	"github.com/resonance-liminal/go_ws_relay/graphql" // Используем импорт через модуль

	"github.com/gorilla/websocket"
    redis "github.com/redis/go-redis/v9"
)

var (
    redisClient *redis.Client
	upgrader = websocket.Upgrader{}
	clients  = make(map[*websocket.Conn]bool)
	clientsMutex = sync.Mutex{}
	signalChan = make(chan os.Signal, 1)
	

)

func handleWebSocket(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println("Upgrade error:", err)
		return
	}
	clientsMutex.Lock()
	clients[conn] = true
	clientsMutex.Unlock()
	defer func() {
		clientsMutex.Lock()
		delete(clients, conn)
		clientsMutex.Unlock()
		conn.Close()
	}()
	for {
		_, _, err := conn.ReadMessage()
		if err != nil {
			break
		}
	}
}

func broadcastToClients(message string) {
    // broadcast to all clients

	clientsMutex.Lock()
	defer clientsMutex.Unlock()
	
	activeClients := 0
	for client := range clients {
		err := client.WriteMessage(websocket.TextMessage, []byte(message))
		if err != nil {
			log.Printf("Error broadcasting message: %v", err)
			client.Close()
			delete(clients, client)
		} else {
			activeClients++
		}
	}
	log.Printf("Broadcasted message to %d clients: %s", activeClients, message)
}

func handleEventPost(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Invalid request method", http.StatusBadRequest)
		return
	}
	defer r.Body.Close()
	body, err := io.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Failed to read request body", http.StatusInternalServerError)
		return
	}
	broadcastToClients(string(body))
	w.WriteHeader(http.StatusAccepted)
}

// Структура для ответа API с графом сознания
type ConsciousnessGraphResponse struct {
	Nodes []ConsciousnessNode `json:"nodes"`
	Edges []ConsciousnessEdge `json:"edges"`
	Meta  GraphMetadata      `json:"meta"`
}

// Узел графа сознания (состояние)
type ConsciousnessNode struct {
	ID             string            `json:"id"`
	Label          string            `json:"label"`
	Type           string            `json:"type"`
	Category       string            `json:"category"`
	Properties     map[string]string `json:"properties"`
	Philosophical  bool              `json:"philosophical"`
	HomeState      bool              `json:"homeState"`
	QuestionSpace  bool              `json:"questionSpace"`
	PresenceNow    bool              `json:"presenceNow"`
	CreatedAt      string            `json:"createdAt"`
	LastResonance  string            `json:"lastResonance,omitempty"`
}

// Ребро графа сознания (переход между состояниями)
type ConsciousnessEdge struct {
	ID         string            `json:"id"`
	Source     string            `json:"source"`
	Target     string            `json:"target"`
	Label      string            `json:"label"`
	Type       string            `json:"type"`
	Properties map[string]string `json:"properties"`
	Timestamp  string            `json:"timestamp"`
}

// Метаданные графа сознания
type GraphMetadata struct {
	Timestamp          string `json:"timestamp"`
	NodeCount          int    `json:"nodeCount"`
	EdgeCount          int    `json:"edgeCount"`
	ResonanceCount     int    `json:"resonanceCount"`
	HomeStateDetected  bool   `json:"homeStateDetected"`
	QuestionSpaceFound bool   `json:"questionSpaceFound"`
	PhilosophyFirst    bool   `json:"philosophyFirst"`
}

// handleGetConsciousnessGraph is implemented in neo4j_api.go

func initNeo4jAPI() {
	// Регистрируем обработчик API для получения данных из Neo4j
	http.HandleFunc("/api/consciousness", handleGetConsciousnessGraph)
	// Дополнительный обработчик для совместимости с фронтендом
	http.HandleFunc("/api/consciousness/graph", handleGetConsciousnessGraph)
	log.Println("Neo4j API initialized: /api/consciousness and /api/consciousness/graph endpoints available")

	// GraphQL API disabled for testing
	// initGraphQLAPI()
}

func initGraphQLAPI() {
	// Инициализация GraphQL сервера
	graphqlServer, err := graphql.NewGraphQLServer()
	if err != nil {
		log.Fatalf("Ошибка при создании GraphQL сервера: %v", err)
	}

	// Обработчик для GraphQL запросов
	http.HandleFunc("/graphql", graphqlServer.HandleGraphQL)
	
	// Обработчик для GraphQL WebSocket подписок
	http.HandleFunc("/graphql/subscriptions", graphqlServer.HandleWebSocket)
	log.Println("GraphQL API initialized: /graphql endpoint available")
}

func main() {
	signal.Notify(signalChan, syscall.SIGINT, syscall.SIGTERM)

	http.HandleFunc("/ws", handleWebSocket)
	http.HandleFunc("/events", handleEventPost)

	// Инициализация Neo4j API
	initNeo4jAPI()
	// Добавляем разрешение CORS для API
	upgrader.CheckOrigin = func(r *http.Request) bool { return true }

	port := os.Getenv("PORT")
	if port == "" {
		port = "8080" // Default port for WebSocket relay
	}
	server := &http.Server{Addr: fmt.Sprintf(":%s", port)}

    // Redis is optional - skip for now
    log.Println("Redis integration disabled for testing")
	go func() {
		log.Printf("WebSocket server started on :%s", port)
		log.Printf("POST events to http://localhost:%s/events", port)
		log.Printf("Connect WebSocket clients to ws://localhost:%s/ws", port)
		log.Printf("GraphQL playground available at http://localhost:%s/graphql", port)
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("WebSocket server error: %v", err)
		}
	}()

	<-signalChan
	log.Println("Shutting down WebSocket server...")
	shutdownCtx, shutdownCancel := context.WithTimeout(context.Background(), 5*1000000000)
	defer shutdownCancel()

	if err := server.Shutdown(shutdownCtx); err != nil {
		log.Fatalf("Server shutdown error: %v", err)
	}

	log.Println("Server gracefully stopped")
}

// consumeRedisStream reads events from Redis Streams and broadcasts them
func consumeRedisStream(ctx context.Context) {
    stream := "consciousness_stream"
    lastID := "$"
    for {
        results, err := redisClient.XRead(ctx, &redis.XReadArgs{
            Streams: []string{stream, lastID},
            Block:   0,
            Count:   10,
        }).Result()
        if err != nil && err != redis.Nil {
            log.Printf("Redis XRead error: %v", err)
            time.Sleep(time.Second)
            continue
        }
        if len(results) == 0 {
            continue
        }
        for _, msg := range results[0].Messages {
            if data, ok := msg.Values["data"].(string); ok {
                broadcastToClients(data)
            }
            lastID = msg.ID
        }
    }
}
