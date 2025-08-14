/**
 * GraphQL Subscription Client
 * Клиент для работы с GraphQL подписками через WebSocket
 */
class GraphQLSubscriptionClient {
    /**
     * Создает новый клиент GraphQL подписок
     * @param {string} url - URL для WebSocket подключения к GraphQL подпискам
     */
    constructor(url) {
        this.url = url;
        this.subscriptions = new Map();
        this.nextSubscriptionId = 1;
        this.socket = null;
        this.connected = false;
        this.connecting = false;
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 5;
        this.reconnectTimeout = 1000; // начальный таймаут 1 секунда
        this.pingInterval = null;
        this.pongTimeout = null;
        this.lastPongReceived = 0;
    }

    /**
     * Подключается к серверу GraphQL подписок
     * @returns {Promise} - Промис, который разрешается при успешном подключении
     */
    connect() {
        if (this.connected) {
            return Promise.resolve();
        }

        if (this.connecting) {
            return new Promise((resolve, reject) => {
                const checkConnected = setInterval(() => {
                    if (this.connected) {
                        clearInterval(checkConnected);
                        resolve();
                    }
                }, 100);
            });
        }

        this.connecting = true;
        return new Promise((resolve, reject) => {
            try {
                console.log(`[GraphQL Subscription] Connecting to ${this.url}`);
                this.socket = new WebSocket(this.url);

                this.socket.onopen = () => {
                    console.log('[GraphQL Subscription] Connected');
                    this.connected = true;
                    this.connecting = false;
                    this.reconnectAttempts = 0;
                    this.startPingPong();
                    
                    // Переподписываемся на все активные подписки
                    this.subscriptions.forEach((handlers, id) => {
                        this.sendSubscriptionStart(id, handlers.query, handlers.variables);
                    });
                    
                    resolve();
                };

                this.socket.onclose = (event) => {
                    console.log(`[GraphQL Subscription] Disconnected: ${event.code} ${event.reason}`);
                    this.connected = false;
                    this.connecting = false;
                    this.stopPingPong();
                    
                    if (this.reconnectAttempts < this.maxReconnectAttempts) {
                        const timeout = this.reconnectTimeout * Math.pow(2, this.reconnectAttempts);
                        console.log(`[GraphQL Subscription] Reconnecting in ${timeout}ms...`);
                        setTimeout(() => {
                            this.reconnectAttempts++;
                            this.connect();
                        }, timeout);
                    } else {
                        console.error('[GraphQL Subscription] Max reconnect attempts reached');
                    }
                };

                this.socket.onerror = (error) => {
                    console.error('[GraphQL Subscription] WebSocket error:', error);
                    if (!this.connected) {
                        this.connecting = false;
                        reject(error);
                    }
                };

                this.socket.onmessage = (event) => {
                    try {
                        const message = JSON.parse(event.data);
                        this.handleMessage(message);
                    } catch (error) {
                        console.error('[GraphQL Subscription] Error parsing message:', error, event.data);
                    }
                };
            } catch (error) {
                this.connecting = false;
                reject(error);
            }
        });
    }

    /**
     * Запускает механизм ping/pong для поддержания соединения
     */
    startPingPong() {
        this.stopPingPong();
        this.lastPongReceived = Date.now();
        
        // Отправляем ping каждые 30 секунд
        this.pingInterval = setInterval(() => {
            if (this.connected) {
                this.sendMessage({ type: 'ping' });
                
                // Если не получили pong в течение 10 секунд, закрываем соединение
                this.pongTimeout = setTimeout(() => {
                    console.warn('[GraphQL Subscription] No pong received, closing connection');
                    if (this.socket) {
                        this.socket.close(4000, 'Pong timeout');
                    }
                }, 10000);
            }
        }, 30000);
    }

    /**
     * Останавливает механизм ping/pong
     */
    stopPingPong() {
        if (this.pingInterval) {
            clearInterval(this.pingInterval);
            this.pingInterval = null;
        }
        if (this.pongTimeout) {
            clearTimeout(this.pongTimeout);
            this.pongTimeout = null;
        }
    }

    /**
     * Обрабатывает входящие сообщения от сервера
     * @param {Object} message - Сообщение от сервера
     */
    handleMessage(message) {
        switch (message.type) {
            case 'connection_ack':
                console.log('[GraphQL Subscription] Connection acknowledged');
                break;
                
            case 'pong':
                // Получили pong от сервера
                this.lastPongReceived = Date.now();
                if (this.pongTimeout) {
                    clearTimeout(this.pongTimeout);
                    this.pongTimeout = null;
                }
                break;
                
            case 'data':
                // Получили данные по подписке
                const subscription = this.subscriptions.get(message.id);
                if (subscription && subscription.onData) {
                    subscription.onData(message.payload.data);
                }
                break;
                
            case 'error':
                // Получили ошибку по подписке
                const errorSub = this.subscriptions.get(message.id);
                if (errorSub && errorSub.onError) {
                    errorSub.onError(message.payload);
                } else {
                    console.error('[GraphQL Subscription] Error:', message.payload);
                }
                break;
                
            case 'complete':
                // Подписка завершена
                const completeSub = this.subscriptions.get(message.id);
                if (completeSub && completeSub.onComplete) {
                    completeSub.onComplete();
                }
                this.subscriptions.delete(message.id);
                break;
                
            default:
                console.log('[GraphQL Subscription] Unknown message type:', message);
        }
    }

    /**
     * Отправляет сообщение на сервер
     * @param {Object} message - Сообщение для отправки
     */
    sendMessage(message) {
        if (!this.connected) {
            console.warn('[GraphQL Subscription] Cannot send message, not connected');
            return;
        }
        
        try {
            this.socket.send(JSON.stringify(message));
        } catch (error) {
            console.error('[GraphQL Subscription] Error sending message:', error);
        }
    }

    /**
     * Отправляет запрос на начало подписки
     * @param {string} id - ID подписки
     * @param {string} query - GraphQL запрос подписки
     * @param {Object} variables - Переменные для запроса
     */
    sendSubscriptionStart(id, query, variables) {
        this.sendMessage({
            type: 'start',
            id: id,
            payload: {
                query: query,
                variables: variables || {}
            }
        });
    }

    /**
     * Отправляет запрос на остановку подписки
     * @param {string} id - ID подписки
     */
    sendSubscriptionStop(id) {
        this.sendMessage({
            type: 'stop',
            id: id
        });
    }

    /**
     * Подписывается на GraphQL подписку
     * @param {string} query - GraphQL запрос подписки
     * @param {Object} variables - Переменные для запроса
     * @param {Function} onData - Обработчик полученных данных
     * @param {Function} onError - Обработчик ошибок
     * @param {Function} onComplete - Обработчик завершения подписки
     * @returns {string} - ID подписки для последующей отписки
     */
    subscribe(query, variables, onData, onError, onComplete) {
        const id = String(this.nextSubscriptionId++);
        
        this.subscriptions.set(id, {
            query,
            variables,
            onData,
            onError,
            onComplete
        });
        
        // Если уже подключены, отправляем запрос на подписку
        if (this.connected) {
            this.sendSubscriptionStart(id, query, variables);
        } else {
            // Иначе сначала подключаемся
            this.connect().then(() => {
                this.sendSubscriptionStart(id, query, variables);
            }).catch(error => {
                if (onError) {
                    onError(error);
                }
            });
        }
        
        return id;
    }

    /**
     * Отписывается от GraphQL подписки
     * @param {string} id - ID подписки
     */
    unsubscribe(id) {
        if (this.subscriptions.has(id)) {
            if (this.connected) {
                this.sendSubscriptionStop(id);
            }
            this.subscriptions.delete(id);
        }
    }

    /**
     * Отписывается от всех подписок и закрывает соединение
     */
    close() {
        // Отписываемся от всех подписок
        this.subscriptions.forEach((_, id) => {
            if (this.connected) {
                this.sendSubscriptionStop(id);
            }
        });
        
        this.subscriptions.clear();
        this.stopPingPong();
        
        // Закрываем соединение
        if (this.socket) {
            this.socket.close(1000, 'Client closed');
            this.socket = null;
        }
        
        this.connected = false;
        this.connecting = false;
    }
}

// Экспортируем класс
if (typeof module !== 'undefined') {
    module.exports = GraphQLSubscriptionClient;
}
