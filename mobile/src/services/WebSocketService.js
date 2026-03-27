/**
 * WebSocketService — реалтайм-соединение с BurnoutGuard backend
 */

const WS_URL = 'ws://localhost:8000/ws';

class WebSocketServiceClass {
  constructor() {
    this.socket = null;
    this.listeners = [];
    this.reconnectTimer = null;
  }

  async connect() {
    try {
      this.socket = new WebSocket(WS_URL);

      this.socket.onopen    = () => console.log('[WS] Connected');
      this.socket.onclose   = () => {
        console.log('[WS] Disconnected — will retry in 5s');
        this.reconnectTimer = setTimeout(() => this.connect(), 5000);
      };
      this.socket.onerror   = (e) => console.warn('[WS] Error:', e.message);
      this.socket.onmessage = (e) => {
        try {
          const data = JSON.parse(e.data);
          this.listeners.forEach(fn => fn(data));
        } catch {}
      };
    } catch (err) {
      console.warn('[WS] Could not connect:', err.message);
    }
  }

  reconnect() {
    if (this.socket) this.socket.close();
    clearTimeout(this.reconnectTimer);
    this.connect();
  }

  disconnect() {
    clearTimeout(this.reconnectTimer);
    if (this.socket) this.socket.close();
    this.socket = null;
  }

  addListener(fn) {
    this.listeners.push(fn);
    return () => { this.listeners = this.listeners.filter(l => l !== fn); };
  }
}

export const WebSocketService = new WebSocketServiceClass();
