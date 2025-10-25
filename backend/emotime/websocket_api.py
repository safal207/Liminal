"""
🌐🚀 Emotime WebSocket API — World-class AI Lab practices

Real-time emotional intelligence API:
- OpenAI: Safety-first real-time interactions
- DeepMind: Multi-agent coordination protocols
- Stanford: Adaptive streaming interfaces
- Google: Scalable WebSocket architecture
- MIT: Advanced real-time learning systems

"The future of AI is real-time and adaptive" — Leading AI Labs
"""

import asyncio
import json
from datetime import datetime
from typing import Dict, Optional, Any

try:
    from fastapi import APIRouter, WebSocket, WebSocketDisconnect, Depends, HTTPException
    from fastapi.responses import HTMLResponse
    FASTAPI_AVAILABLE = True
except ImportError:
    FASTAPI_AVAILABLE = False
    APIRouter = None

from .core import EmotimeEngine
from .utils import safe_logger
from .websocket.connection_manager import get_connection_manager
from .websocket.streaming_engine import get_streamer, remove_streamer


if FASTAPI_AVAILABLE:
    # Create WebSocket router
    websocket_router = APIRouter(prefix="/ws", tags=["WebSocket Real-time"])
    
    
    async def get_user_engine(user_id: str) -> EmotimeEngine:
        """Factory function для создания пользовательского engine."""
        engine = EmotimeEngine(
            user_id=user_id,
            enable_ml=True,  # MIT enhancements
            enable_neo4j=False,  # Disable for real-time performance
            update_interval=0.5  # Fast updates for real-time
        )
        return engine
    
    
    @websocket_router.websocket("/emotime/{user_id}")
    async def websocket_emotime_endpoint(websocket: WebSocket, user_id: str, token: str = None):
        """
        Main WebSocket endpoint для real-time emotional intelligence.
        
        World-class features:
        - Stanford: Adaptive quality control
        - OpenAI: Safety monitoring  
        - DeepMind: Multi-modal coordination
        - Google: Scalable architecture
        - JWT Authentication: Secure token-based access
        """
        connection_manager = get_connection_manager()
        streamer = None
        engine = None
        connection_id = None
        
        try:
            # Security: JWT Authentication (NEW)
            if token:
                try:
                    from .security.auth import validate_token, TokenType
                    from .security.validators import get_input_sanitizer
                    
                    # Validate JWT token
                    claims = validate_token(token)
                    
                    # Verify token is for WebSocket access
                    if claims.token_type != TokenType.WEBSOCKET:
                        await websocket.close(code=1008, reason="Invalid token type")
                        return
                    
                    # Verify user ID matches token
                    sanitizer = get_input_sanitizer()
                    clean_user_id = sanitizer.validate_user_id(user_id)
                    
                    if claims.user_id != clean_user_id:
                        await websocket.close(code=1008, reason="User ID mismatch")
                        return
                    
                    # Update user_id to validated version
                    user_id = clean_user_id
                    
                    safe_logger.info(f"JWT authentication successful for user {user_id}")
                    
                except Exception as auth_error:
                    safe_logger.warning(f"WebSocket authentication failed: {auth_error}")
                    await websocket.close(code=1008, reason="Authentication failed")
                    return
            else:
                # For backward compatibility - allow connections without token but log warning
                safe_logger.warning(f"WebSocket connection without authentication for user {user_id}")
            
            # Establish connection (Stanford practice)
            connection_id = await connection_manager.connect_user(websocket, user_id)
            safe_logger.info(f"WebSocket connected: {connection_id} for user {user_id}")
            
            # Initialize emotional engine
            engine = await get_user_engine(user_id)
            await engine.start()
            
            # Start real-time streaming (AI Lab practice)
            streamer = get_streamer(user_id)
            await streamer.start_streaming(engine)
            
            # Send initial state
            initial_state = await engine.get_current_state()
            if initial_state:
                await websocket.send_json({
                    "type": "initial_state",
                    "data": engine.to_dict(),
                    "timestamp": datetime.now().isoformat()
                })
            
            # Main message handling loop
            while True:
                try:
                    # Receive message from client
                    message = await websocket.receive_json()
                    
                    # Process based on message type (OpenAI practice)
                    await _handle_websocket_message(
                        websocket, user_id, message, engine, streamer
                    )
                    
                except WebSocketDisconnect:
                    break
                except json.JSONDecodeError:
                    await websocket.send_json({
                        "type": "error",
                        "message": "Invalid JSON format",
                        "timestamp": datetime.now().isoformat()
                    })
                except Exception as e:
                    safe_logger.error(f"Message handling error: {e}")
                    await websocket.send_json({
                        "type": "error", 
                        "message": str(e),
                        "timestamp": datetime.now().isoformat()
                    })
        
        except Exception as e:
            safe_logger.error(f"WebSocket connection error: {e}")
            
        finally:
            # Cleanup (Google practice)
            if connection_id:
                await connection_manager.disconnect_user(connection_id, user_id)
            
            if streamer:
                await streamer.stop_streaming()
                remove_streamer(user_id)
            
            if engine:
                await engine.stop()
            
            safe_logger.info(f"WebSocket disconnected: {user_id}")
    
    
    async def _handle_websocket_message(
        websocket: WebSocket,
        user_id: str, 
        message: Dict[str, Any],
        engine: EmotimeEngine,
        streamer
    ):
        """
        Обрабатывает WebSocket сообщения с AI lab best practices.
        """
        message_type = message.get("type")
        timestamp = datetime.now()
        
        if message_type == "sensor_data":
            # Real-time sensor data (Stanford practice)
            await streamer.handle_client_message(message)
            
            # Send acknowledgment
            await websocket.send_json({
                "type": "sensor_ack",
                "timestamp": timestamp.isoformat(),
                "processed": True
            })
        
        elif message_type == "feedback":
            # Learning feedback (MIT practice)
            feedback_data = message.get("data", {})
            actual_emotion = feedback_data.get("actual_emotion")
            context = feedback_data.get("context", {})
            
            if actual_emotion:
                await engine.learn_from_feedback(actual_emotion, context)
                
                await websocket.send_json({
                    "type": "feedback_ack",
                    "message": "Learning feedback processed",
                    "timestamp": timestamp.isoformat()
                })
        
        elif message_type == "get_analytics":
            # Advanced analytics request (Google practice)
            analytics = await _get_comprehensive_analytics(engine, streamer)
            
            await websocket.send_json({
                "type": "analytics_response",
                "data": analytics,
                "timestamp": timestamp.isoformat()
            })
        
        elif message_type == "quality_preference":
            # Adaptive quality control (Stanford practice)
            await streamer.handle_client_message(message)
            
        elif message_type == "safety_report":
            # Safety monitoring (OpenAI practice)
            report = message.get("data", {})
            safe_logger.info(f"Safety report from {user_id}: {report}")
            
            await websocket.send_json({
                "type": "safety_ack",
                "message": "Safety report received",
                "timestamp": timestamp.isoformat()
            })
        
        elif message_type == "join_room":
            # Multi-user emotional coordination (DeepMind practice)
            room_id = message.get("room_id")
            if room_id:
                connection_manager = get_connection_manager()
                # Find connection ID for this user
                for conn_id, metrics in connection_manager.pool.connection_metrics.items():
                    if metrics.user_id == user_id:
                        await connection_manager.join_room(conn_id, room_id)
                        break
                
                await websocket.send_json({
                    "type": "room_joined",
                    "room_id": room_id,
                    "timestamp": timestamp.isoformat()
                })
        
        elif message_type == "ping":
            # Connection health check
            await websocket.send_json({
                "type": "pong",
                "timestamp": timestamp.isoformat(),
                "server_time": timestamp.isoformat()
            })
        
        else:
            # Unknown message type
            await websocket.send_json({
                "type": "error",
                "message": f"Unknown message type: {message_type}",
                "timestamp": timestamp.isoformat()
            })
    
    
    async def _get_comprehensive_analytics(engine: EmotimeEngine, streamer) -> Dict[str, Any]:
        """Собирает comprehensive analytics от всех компонентов."""
        analytics = {}
        
        try:
            # Core engine insights
            if engine:
                analytics["emotional_insights"] = await engine.get_emotional_insights()
                analytics["current_state"] = engine.to_dict()
            
            # Streaming analytics  
            if streamer:
                analytics["streaming"] = streamer.get_streaming_analytics()
            
            # Connection analytics
            connection_manager = get_connection_manager()
            analytics["connections"] = connection_manager.get_system_analytics()
            
            # System timestamp
            analytics["generated_at"] = datetime.now().isoformat()
            
        except Exception as e:
            safe_logger.error(f"Analytics generation error: {e}")
            analytics["error"] = str(e)
        
        return analytics
    
    
    @websocket_router.get("/test-page")
    async def websocket_test_page():
        """Тестовая страница для WebSocket соединения."""
        html_content = """
        <!DOCTYPE html>
        <html>
        <head>
            <title>Emotime WebSocket Test</title>
            <style>
                body { font-family: Arial, sans-serif; margin: 20px; }
                .container { max-width: 800px; margin: 0 auto; }
                .status { padding: 10px; margin: 10px 0; border-radius: 5px; }
                .connected { background-color: #d4edda; color: #155724; }
                .disconnected { background-color: #f8d7da; color: #721c24; }
                .message { background-color: #f8f9fa; padding: 10px; margin: 5px 0; border-radius: 3px; }
                button { padding: 10px 15px; margin: 5px; background-color: #007bff; color: white; border: none; border-radius: 3px; cursor: pointer; }
                button:hover { background-color: #0056b3; }
                input, textarea { width: 100%; padding: 8px; margin: 5px 0; }
            </style>
        </head>
        <body>
            <div class="container">
                <h1>🧠⚡ Emotime Real-time WebSocket Test</h1>
                <div id="status" class="status disconnected">Disconnected</div>
                
                <div>
                    <label>User ID:</label>
                    <input type="text" id="userId" value="test_user" />
                    <button onclick="connect()">Connect</button>
                    <button onclick="disconnect()">Disconnect</button>
                </div>
                
                <div>
                    <h3>Send Sensor Data</h3>
                    <textarea id="textInput" placeholder="Enter text for emotional analysis...">I feel amazing today!</textarea>
                    <button onclick="sendTextData()">Send Text</button>
                    
                    <div>
                        <label>Touch Pressure (0-1):</label>
                        <input type="range" id="touchPressure" min="0" max="1" step="0.1" value="0.5" />
                        <button onclick="sendTouchData()">Send Touch</button>
                    </div>
                </div>
                
                <div>
                    <h3>Controls</h3>
                    <button onclick="getAnalytics()">Get Analytics</button>
                    <button onclick="sendFeedback()">Send Learning Feedback</button>
                    <button onclick="ping()">Ping Server</button>
                </div>
                
                <div>
                    <h3>Messages</h3>
                    <div id="messages"></div>
                </div>
            </div>
            
            <script>
                let ws = null;
                let userId = 'test_user';
                
                function connect() {
                    userId = document.getElementById('userId').value;
                    const wsUrl = `ws://localhost:8000/emotime/ws/emotime/${userId}`;
                    
                    ws = new WebSocket(wsUrl);
                    
                    ws.onopen = function(event) {
                        updateStatus('Connected', true);
                        addMessage('Connected to Emotime WebSocket', 'info');
                    };
                    
                    ws.onmessage = function(event) {
                        const data = JSON.parse(event.data);
                        addMessage(`Received: ${JSON.stringify(data, null, 2)}`, 'received');
                    };
                    
                    ws.onclose = function(event) {
                        updateStatus('Disconnected', false);
                        addMessage('WebSocket connection closed', 'info');
                    };
                    
                    ws.onerror = function(error) {
                        addMessage(`WebSocket error: ${error}`, 'error');
                    };
                }
                
                function disconnect() {
                    if (ws) {
                        ws.close();
                        ws = null;
                    }
                }
                
                function sendTextData() {
                    if (!ws) return;
                    
                    const text = document.getElementById('textInput').value;
                    const message = {
                        type: 'sensor_data',
                        data: {
                            type: 'text',
                            text: text,
                            typing_speed: 100,
                            pause_duration: 1.0
                        }
                    };
                    
                    ws.send(JSON.stringify(message));
                    addMessage(`Sent text: "${text}"`, 'sent');
                }
                
                function sendTouchData() {
                    if (!ws) return;
                    
                    const pressure = parseFloat(document.getElementById('touchPressure').value);
                    const message = {
                        type: 'sensor_data',
                        data: {
                            type: 'touch',
                            pressure: pressure,
                            duration: 1.0,
                            frequency: 2.0,
                            pattern: 'tap'
                        }
                    };
                    
                    ws.send(JSON.stringify(message));
                    addMessage(`Sent touch data: pressure=${pressure}`, 'sent');
                }
                
                function getAnalytics() {
                    if (!ws) return;
                    
                    ws.send(JSON.stringify({type: 'get_analytics'}));
                    addMessage('Requested analytics', 'sent');
                }
                
                function sendFeedback() {
                    if (!ws) return;
                    
                    const message = {
                        type: 'feedback',
                        data: {
                            actual_emotion: 'joy',
                            context: {
                                time_of_day: 0.5,
                                confidence: 0.9
                            }
                        }
                    };
                    
                    ws.send(JSON.stringify(message));
                    addMessage('Sent learning feedback', 'sent');
                }
                
                function ping() {
                    if (!ws) return;
                    
                    ws.send(JSON.stringify({type: 'ping'}));
                    addMessage('Ping sent', 'sent');
                }
                
                function updateStatus(text, connected) {
                    const statusEl = document.getElementById('status');
                    statusEl.textContent = text;
                    statusEl.className = 'status ' + (connected ? 'connected' : 'disconnected');
                }
                
                function addMessage(text, type) {
                    const messagesEl = document.getElementById('messages');
                    const messageEl = document.createElement('div');
                    messageEl.className = 'message';
                    messageEl.innerHTML = `<strong>[${new Date().toLocaleTimeString()}]</strong> ${text}`;
                    messagesEl.appendChild(messageEl);
                    messagesEl.scrollTop = messagesEl.scrollHeight;
                }
            </script>
        </body>
        </html>
        """
        return HTMLResponse(content=html_content)
    
    
    # REST endpoints для управления WebSocket системой
    @websocket_router.post("/auth/token")
    async def generate_websocket_token(user_id: str, session_metadata: dict = None):
        """
        Generates a JWT token for WebSocket authentication.
        
        Security features:
        - Rate limiting per user
        - Session metadata tracking
        - Token expiration (4 hours)
        - Audit logging
        """
        try:
            from .security.auth import generate_secure_token, TokenType
            from .security.validators import get_input_sanitizer
            
            # Validate and sanitize user_id
            sanitizer = get_input_sanitizer()
            clean_user_id = sanitizer.validate_user_id(user_id)
            
            # Generate secure WebSocket token
            token = generate_secure_token(
                user_id=clean_user_id,
                token_type=TokenType.WEBSOCKET,
                scopes=["websocket:connect", "emotional:read", "emotional:write"],
                session_metadata=session_metadata or {}
            )
            
            return {
                "success": True,
                "token": token,
                "token_type": "websocket",
                "expires_in_hours": 4,
                "user_id": clean_user_id,
                "scopes": ["websocket:connect", "emotional:read", "emotional:write"]
            }
            
        except Exception as e:
            safe_logger.error(f"Token generation failed for user {user_id}: {e}")
            return {
                "success": False,
                "error": "Token generation failed",
                "message": str(e)
            }

    @websocket_router.get("/status")
    async def websocket_system_status():
        """Системная статистика WebSocket подсистемы."""
        connection_manager = get_connection_manager()
        
        return {
            "system": "Emotime WebSocket System",
            "status": "operational",
            "features": [
                "Real-time emotional streaming",
                "Adaptive quality control",
                "Safety monitoring",
                "Multi-modal sensor fusion",
                "Advanced analytics"
            ],
            "analytics": connection_manager.get_system_analytics(),
            "ai_lab_practices": [
                "Stanford: Adaptive algorithms",
                "OpenAI: Safety-first design", 
                "DeepMind: Multi-agent coordination",
                "Google: Scalable architecture",
                "MIT: Real-time learning"
            ]
        }
    
    
    @websocket_router.post("/broadcast/{room_id}")
    async def broadcast_to_room(room_id: str, message: Dict[str, Any]):
        """Отправляет сообщение всем в комнате."""
        connection_manager = get_connection_manager()
        
        broadcast_message = {
            "type": "room_broadcast",
            "room_id": room_id,
            "data": message,
            "timestamp": datetime.now().isoformat()
        }
        
        await connection_manager.broadcast_to_room(room_id, broadcast_message)
        
        return {
            "status": "broadcasted",
            "room_id": room_id,
            "timestamp": datetime.now().isoformat()
        }

else:
    # Fallback если FastAPI не доступен
    safe_logger.warning("FastAPI not available - WebSocket API disabled")
    websocket_router = None