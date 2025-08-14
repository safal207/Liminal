package graphql

import (
	"encoding/json"
	"log"
	"net/http"
	"sync"
	"time"

	"github.com/gorilla/websocket"
)

// Константы для WebSocket подписок
const (
	// Время ожидания для записи сообщения клиенту
	writeWait = 10 * time.Second
	
	// Время между ping сообщениями
	pingPeriod = 30 * time.Second
	
	// Максимальный размер сообщения
	maxMessageSize = 1024 * 1024
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
	CheckOrigin: func(r *http.Request) bool {
		return true // Разрешаем все источники для демонстрации
	},
}

// Subscription представляет подписку на события GraphQL
type Subscription struct {
	ID         string
	Topic      string
	Connection *websocket.Conn
	Filter     map[string]interface{}
}

// SubscriptionManager управляет подписками GraphQL
type SubscriptionManager struct {
	subscriptions map[string][]*Subscription
	mutex         sync.RWMutex
}

// NewSubscriptionManager создает новый менеджер подписок
func NewSubscriptionManager() *SubscriptionManager {
	return &SubscriptionManager{
		subscriptions: make(map[string][]*Subscription),
	}
}

// Subscribe добавляет новую подписку
func (sm *SubscriptionManager) Subscribe(topic string, conn *websocket.Conn, filter map[string]interface{}) string {
	sm.mutex.Lock()
	defer sm.mutex.Unlock()
	
	id := generateID()
	subscription := &Subscription{
		ID:         id,
		Topic:      topic,
		Connection: conn,
		Filter:     filter,
	}
	
	if _, ok := sm.subscriptions[topic]; !ok {
		sm.subscriptions[topic] = make([]*Subscription, 0)
	}
	
	sm.subscriptions[topic] = append(sm.subscriptions[topic], subscription)
	return id
}

// Unsubscribe удаляет подписку
func (sm *SubscriptionManager) Unsubscribe(id string) {
	sm.mutex.Lock()
	defer sm.mutex.Unlock()
	
	for topic, subs := range sm.subscriptions {
		for i, sub := range subs {
			if sub.ID == id {
				// Удаляем подписку из списка
				sm.subscriptions[topic] = append(subs[:i], subs[i+1:]...)
				return
			}
		}
	}
}

// Publish отправляет данные всем подписчикам на тему
func (sm *SubscriptionManager) Publish(topic string, data interface{}, filterFunc func(filter map[string]interface{}, data interface{}) bool) {
	sm.mutex.RLock()
	defer sm.mutex.RUnlock()
	
	if subs, ok := sm.subscriptions[topic]; ok {
		for _, sub := range subs {
			// Если есть функция фильтрации и она возвращает false, пропускаем
			if filterFunc != nil && !filterFunc(sub.Filter, data) {
				continue
			}
			
			// Отправляем данные подписчику
			message := map[string]interface{}{
				"type":  "data",
				"topic": topic,
				"data":  data,
			}
			
			jsonMessage, err := json.Marshal(message)
			if err != nil {
				log.Printf("Ошибка маршалинга сообщения: %v", err)
				continue
			}
			
			sub.Connection.SetWriteDeadline(time.Now().Add(writeWait))
			if err := sub.Connection.WriteMessage(websocket.TextMessage, jsonMessage); err != nil {
				log.Printf("Ошибка отправки сообщения: %v", err)
				sm.Unsubscribe(sub.ID)
			}
		}
	}
}

// HandleSubscription обрабатывает WebSocket соединение для подписок GraphQL
func (sm *SubscriptionManager) HandleSubscription(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("Ошибка обновления до WebSocket: %v", err)
		return
	}
	
	conn.SetReadLimit(maxMessageSize)
	
	// Запускаем горутину для чтения сообщений
	go func() {
		defer func() {
			conn.Close()
		}()
		
		for {
			messageType, message, err := conn.ReadMessage()
			if err != nil {
				if websocket.IsUnexpectedCloseError(err, websocket.CloseGoingAway, websocket.CloseAbnormalClosure) {
					log.Printf("Ошибка чтения WebSocket: %v", err)
				}
				break
			}
			
			if messageType == websocket.TextMessage {
				var request map[string]interface{}
				if err := json.Unmarshal(message, &request); err != nil {
					log.Printf("Ошибка разбора JSON: %v", err)
					continue
				}
				
				// Обрабатываем запрос подписки
				if requestType, ok := request["type"].(string); ok {
					switch requestType {
					case "subscribe":
						topic, _ := request["topic"].(string)
						filter, _ := request["filter"].(map[string]interface{})
						id := sm.Subscribe(topic, conn, filter)
						
						// Отправляем подтверждение подписки
						response := map[string]interface{}{
							"type":        "subscribed",
							"id":          id,
							"topic":       topic,
						}
						
						jsonResponse, _ := json.Marshal(response)
						conn.WriteMessage(websocket.TextMessage, jsonResponse)
						
					case "unsubscribe":
						id, _ := request["id"].(string)
						sm.Unsubscribe(id)
						
						// Отправляем подтверждение отписки
						response := map[string]interface{}{
							"type": "unsubscribed",
							"id":   id,
						}
						
						jsonResponse, _ := json.Marshal(response)
						conn.WriteMessage(websocket.TextMessage, jsonResponse)
					}
				}
			}
		}
	}()
	
	// Запускаем горутину для отправки ping сообщений
	go func() {
		ticker := time.NewTicker(pingPeriod)
		defer func() {
			ticker.Stop()
			conn.Close()
		}()
		
		for {
			select {
			case <-ticker.C:
				conn.SetWriteDeadline(time.Now().Add(writeWait))
				if err := conn.WriteMessage(websocket.PingMessage, nil); err != nil {
					return
				}
			}
		}
	}()
}

// Вспомогательная функция для генерации ID
func generateID() string {
	return time.Now().Format("20060102150405.000000000")
}
