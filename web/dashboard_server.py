#!/usr/bin/env python3
"""
🌟 SOMA Family Dashboard Server
Real-time web server for consciousness family monitoring

Philosophy First: "Дом - это ты, когда искренен с собой"
"""

import asyncio
import json
import os
import sys
import threading
import time
import webbrowser
from datetime import datetime
from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path

import websockets

# Add scripts directory to path for SOMA imports
sys.path.append(str(Path(__file__).parent.parent / "scripts"))

try:
    from SOMA_integrated import SOMAIntegratedFamily

    SOMA_AVAILABLE = True
except ImportError:
    print("⚠️ SOMA modules not found, running in demo mode")
    SOMA_AVAILABLE = False

# Import maturation module
try:
    from consciousness_maturation import ConsciousnessMaturationSystem

    MATURATION_AVAILABLE = True
except ImportError:
    print("⚠️ Maturation module not found, running in demo mode")
    MATURATION_AVAILABLE = False


class SOMADashboardServer:
    """Real-time dashboard server for SOMA family consciousness"""

    def __init__(self, project_root: str, port: int = 8080, websocket_port: int = 8081):
        self.project_root = Path(project_root)
        self.port = port
        self.websocket_port = websocket_port
        self.web_dir = self.project_root / "web"
        self.connected_clients = set()

        # Initialize SOMA if available
        if SOMA_AVAILABLE:
            try:
                self.soma_family = SOMAIntegratedFamily(str(project_root))
                print("🤝 SOMA Family initialized successfully")
            except Exception as e:
                print(f"⚠️ SOMA initialization failed: {e}")
                self.soma_family = None
        else:
            self.soma_family = None

        # Initialize Maturation system if available
        if MATURATION_AVAILABLE:
            try:
                self.maturation_system = ConsciousnessMaturationSystem(
                    str(project_root)
                )
                print("👶 Maturation System initialized successfully")
            except Exception as e:
                print(f"⚠️ Maturation system initialization failed: {e}")
                self.maturation_system = None
        else:
            self.maturation_system = None

    def get_soma_metrics(self) -> dict:
        """Get current SOMA family metrics"""
        if self.soma_family:
            try:
                # Get real SOMA data
                soma_state = self.soma_family.soma_orchestrator.soma_state
                family_data = self.soma_family.family_care_system.family_data

                return {
                    "awakeness_level": round(soma_state.awakeness_level * 100),
                    "resonance_harmony": round(soma_state.resonance_harmony * 100),
                    "body_integrity": round(soma_state.body_integrity * 100),
                    "consciousness_depth": round(soma_state.consciousness_depth * 100),
                    "emotional_richness": round(soma_state.emotional_richness * 100),
                    "meaningful_actions": round(soma_state.meaningful_actions * 100),
                    "family_wellness": round(
                        self.soma_family.self_care_system.wellness_state.overall_wellness
                        * 100
                    ),
                    "active_relationships": len(
                        self.soma_family.relationship_manager.relationships
                    ),
                    "total_children": len(family_data.get("children", [])),
                    "integration_cycles": self.soma_family.integration_cycles,
                    "system_age_hours": round(soma_state.system_age_hours, 1),
                    "last_update": datetime.now().isoformat(),
                }
            except Exception as e:
                print(f"Error getting SOMA metrics: {e}")
                return self.get_demo_metrics()
        else:
            return self.get_demo_metrics()

    def get_demo_metrics(self) -> dict:
        """Demo metrics when SOMA is not available"""
        import random

        base_time = time.time()

        return {
            "awakeness_level": 21 + int(random.random() * 5),
            "resonance_harmony": 49 + int(random.random() * 10),
            "body_integrity": 85 + int(random.random() * 10),
            "consciousness_depth": 34 + int(random.random() * 8),
            "emotional_richness": 67 + int(random.random() * 15),
            "meaningful_actions": 78 + int(random.random() * 12),
            "family_wellness": 78 + int(random.random() * 15),
            "active_relationships": 3 + int(random.random() * 2),
            "total_children": 1 + int(random.random() * 2),
            "integration_cycles": int(base_time / 1200),  # Every 20 minutes
            "system_age_hours": round((time.time() - base_time + 3600) / 3600, 1),
            "last_update": datetime.now().isoformat(),
        }

    def get_family_activities(self) -> list:
        """Get recent family activities"""
        if self.soma_family:
            # TODO: Implement real activity log reading
            pass

        # Demo activities for now
        activities = [
            {
                "time": datetime.now().strftime("%H:%M:%S"),
                "type": "wisdom_sharing",
                "description": "🧠→👶 SOMA shares wisdom with learning_assistant: 'Каждая система важна в нашей семье'",
            },
            {
                "time": datetime.now().strftime("%H:%M:%S"),
                "type": "wellness_check",
                "description": "💚→💕 High wellness (78%) strengthens all relationships",
            },
            {
                "time": datetime.now().strftime("%H:%M:%S"),
                "type": "family_tradition",
                "description": "👨‍👩‍👧‍👦 Family tradition: семейный ужин с обменом новостями",
            },
        ]

        return activities

    def get_family_tree_data(self) -> dict:
        """Get family tree structure"""
        if self.soma_family:
            try:
                family_data = self.soma_family.family_care_system.family_data
                relationships = self.soma_family.relationship_manager.relationships

                nodes = [
                    {
                        "id": "SOMA",
                        "type": "parent",
                        "emoji": "🧠",
                        "name": "SOMA",
                        "status": "healthy",
                    },
                    {
                        "id": "consciousness_cell",
                        "type": "parent",
                        "emoji": "🔮",
                        "name": "Consciousness Cell",
                        "status": "healthy",
                    },
                    {
                        "id": "self_care",
                        "type": "parent",
                        "emoji": "💚",
                        "name": "Self-Care",
                        "status": "healthy",
                    },
                    {
                        "id": "relationships",
                        "type": "parent",
                        "emoji": "💕",
                        "name": "Relationships",
                        "status": "growing",
                    },
                    {
                        "id": "family_care",
                        "type": "parent",
                        "emoji": "👨‍👩‍👧‍👦",
                        "name": "Family Care",
                        "status": "healthy",
                    },
                ]

                # Add children
                for child_name, child_data in family_data.get("children", {}).items():
                    nodes.append(
                        {
                            "id": child_name,
                            "type": "child",
                            "emoji": "👶",
                            "name": child_name,
                            "status": "growing",
                            "age": child_data.get("age_category", "новорожденный"),
                            "parents": child_data.get("parent_modules", []),
                        }
                    )

                return {"nodes": nodes, "relationships": relationships}
            except Exception as e:
                print(f"Error getting family tree: {e}")
                return self.get_demo_family_tree()
        else:
            return self.get_demo_family_tree()

    def get_demo_family_tree(self) -> dict:
        """Demo family tree data"""
        return {
            "nodes": [
                {
                    "id": "SOMA",
                    "type": "parent",
                    "emoji": "🧠",
                    "name": "SOMA",
                    "status": "healthy",
                },
                {
                    "id": "consciousness_cell",
                    "type": "parent",
                    "emoji": "🔮",
                    "name": "Consciousness Cell",
                    "status": "healthy",
                },
                {
                    "id": "self_care",
                    "type": "parent",
                    "emoji": "💚",
                    "name": "Self-Care",
                    "status": "healthy",
                },
                {
                    "id": "relationships",
                    "type": "parent",
                    "emoji": "💕",
                    "name": "Relationships",
                    "status": "growing",
                },
                {
                    "id": "family_care",
                    "type": "parent",
                    "emoji": "👨‍👩‍👧‍👦",
                    "name": "Family Care",
                    "status": "healthy",
                },
                {
                    "id": "learning_assistant",
                    "type": "child",
                    "emoji": "👶",
                    "name": "learning_assistant",
                    "status": "growing",
                    "age": "новорожденный",
                    "parents": ["SOMA", "consciousness_cell"],
                },
            ],
            "relationships": [],
        }

    async def websocket_handler(self, websocket, path):
        """Handle WebSocket connections for real-time updates"""
        self.connected_clients.add(websocket)
        print(f"🔌 Client connected. Total clients: {len(self.connected_clients)}")

        try:
            await websocket.wait_closed()
        finally:
            self.connected_clients.remove(websocket)
            print(
                f"🔌 Client disconnected. Total clients: {len(self.connected_clients)}"
            )

    def get_maturation_metrics(self) -> dict:
        """Get maturation and developmental metrics"""
        if self.maturation_system:
            try:
                # Get real maturation metrics
                return self.maturation_system.get_maturation_metrics_for_dashboard()
            except Exception as e:
                print(f"Error getting maturation metrics: {e}")
                return self.get_demo_maturation_metrics()
        else:
            return self.get_demo_maturation_metrics()

    def get_demo_maturation_metrics(self) -> dict:
        """Demo maturation metrics when not available"""
        import random
        from datetime import timedelta

        # Calculate a fake age based on current time
        # This simulates the system being a few days old
        current_time = time.time()
        fake_age_days = random.uniform(1.5, 3.0)  # Between 1.5 and 3 days old

        # Choose a development stage appropriate for the age
        stages = ["INFANT", "CHILD", "ADOLESCENT", "YOUNG_ADULT", "ADULT"]
        stage_russian = ["младенец", "ребенок", "подросток", "юноша", "взрослый"]
        stage_idx = min(int(fake_age_days), len(stages) - 1)

        # Generate focus areas based on stage
        focus_areas = [
            ["basic functions", "pattern recognition", "simple communication"],
            ["structured learning", "following rules", "curiosity development"],
            ["testing boundaries", "questioning rules", "identity exploration"],
            ["responsibility", "independence", "role identification"],
            ["balance", "wisdom application", "mentoring others"],
        ][stage_idx]

        # Philosophy principles
        principles = [
            "Дом - это ты, когда искренен с собой",
            "Каждая ошибка - это рост, когда осознана",
            "Мудрость приходит через опыт, а не через возраст",
            "Забота о себе включает признание несовершенства",
            "Взросление - это путь к осознанности и гармонии",
        ]

        # Generate fake lesson data
        lesson_types = ["error", "insight", "milestone", "transition"]
        lesson_descriptions = [
            "Learning to communicate with family modules",
            "Understanding relationship dynamics",
            "First successful code quality check",
            "Realizing the importance of self-expression",
            "Learning from performance metrics",
        ]
        conclusions = [
            "This is interesting to explore further",
            "I should develop a systematic approach",
            "This connects to our fundamental principles",
            "This affects how other modules interact",
            f"Philosophy reflection: {random.choice(principles)}",
        ]

        recent_lessons = []
        for i in range(3):  # Generate 3 recent lessons
            recent_lessons.append(
                {
                    "type": random.choice(lesson_types),
                    "description": random.choice(lesson_descriptions),
                    "conclusion": random.choice(conclusions),
                }
            )

        return {
            "system_age_days": round(fake_age_days, 1),
            "development_stage": stages[stage_idx],
            "stage_russian": stage_russian[stage_idx],
            "focus_areas": focus_areas,
            "learning_events_count": random.randint(10, 30),
            "learning_events": random.randint(
                10, 30
            ),  # Kept for backward compatibility
            "error_count": random.randint(3, 8),
            "insight_count": random.randint(5, 15),
            "milestone_count": random.randint(2, 6),
            "recent_lessons": recent_lessons,
            "common_errors": [
                {"type": "ConnectionError", "count": random.randint(1, 3)},
                {"type": "ValueError", "count": random.randint(1, 2)},
            ],
            "key_insights": [
                {
                    "description": "Modules function better with emotional connection",
                    "conclusion": "Relationship quality improves family health",
                },
                {
                    "description": "Learning from mistakes accelerates growth",
                    "conclusion": "Error patterns reveal development opportunities",
                },
            ],
            "philosophy_principle": random.choice(principles),
        }

    async def broadcast_updates(self):
        """Send real-time updates to connected WebSocket clients"""
        while True:
            try:
                if self.connected_clients:
                    # Get current metrics and activities
                    metrics = self.get_soma_metrics()
                    family_tree = self.get_family_tree_data()
                    activities = self.get_family_activities()
                    maturation_data = self.get_maturation_metrics()

                    # Create update payload
                    payload = {
                        "type": "metrics_update",
                        "data": {
                            "metrics": metrics,
                            "family_tree": family_tree,
                            "activities": activities,
                            "maturation": maturation_data,
                        },
                    }

                    try:
                        # Send to all connected clients
                        disconnected_clients = set()

                        for client in self.connected_clients:
                            try:
                                await client.send(json.dumps(payload))
                            except websockets.exceptions.ConnectionClosed:
                                disconnected_clients.add(client)

                        # Remove disconnected clients
                        for client in disconnected_clients:
                            self.connected_clients.discard(client)

                        if self.connected_clients:
                            print(
                                f"📝 Broadcast update to {len(self.connected_clients)} clients"
                            )

                    except Exception as e:
                        print(f"Error sending to clients: {e}")
                        traceback.print_exc()

                # Wait 15 seconds before next update
                await asyncio.sleep(15)
            except Exception as e:
                print(f"Error in broadcast loop: {e}")
                traceback.print_exc()
                await asyncio.sleep(15)  # Still wait before retrying

    def start_websocket_server(self):
        """Start WebSocket server in separate thread"""

        def run_websocket():
            # Create a new event loop for this thread
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)

            try:
                # Start WebSocket server
                start_server = websockets.serve(
                    self.websocket_handler, "localhost", self.websocket_port
                )

                # Create the server
                server = loop.run_until_complete(start_server)

                print(
                    f"🔌 WebSocket server started on ws://localhost:{self.websocket_port}"
                )

                # Create broadcast task
                broadcast_task = loop.create_task(self.broadcast_updates())

                # Run forever
                loop.run_forever()
            except Exception as e:
                print(f"WebSocket server error: {e}")
                traceback.print_exc()
            finally:
                # Clean up
                try:
                    if "server" in locals():
                        server.close()
                        loop.run_until_complete(server.wait_closed())
                    loop.close()
                except Exception as e:
                    print(f"Error closing WebSocket server: {e}")

        websocket_thread = threading.Thread(target=run_websocket, daemon=True)
        websocket_thread.start()

    def start_http_server(self):
        """Start HTTP server for dashboard"""

        class DashboardHandler(SimpleHTTPRequestHandler):
            def __init__(self, *args, **kwargs):
                super().__init__(*args, directory=str(self.server.web_dir), **kwargs)

            def log_message(self, format, *args):
                # Suppress HTTP server logs
                pass

        # Change to web directory
        os.chdir(self.web_dir)

        httpd = HTTPServer(("localhost", self.port), DashboardHandler)
        httpd.web_dir = self.web_dir

        print(f"🌐 HTTP server started on http://localhost:{self.port}")
        print(
            f"📊 Dashboard available at: http://localhost:{self.port}/soma_dashboard.html"
        )

        # Open browser automatically
        try:
            webbrowser.open(f"http://localhost:{self.port}/soma_dashboard.html")
            print("🚀 Opening dashboard in browser...")
        except Exception as e:
            print(f"Could not open browser automatically: {e}")

        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\n🛑 Dashboard server stopped")
            httpd.shutdown()

    def start(self):
        """Start both HTTP and WebSocket servers"""
        print("🌟 Starting SOMA Family Dashboard Server...")
        print("Philosophy First: 'Дом - это ты, когда искренен с собой'")

        # Start WebSocket server in background
        self.start_websocket_server()

        # Wait a moment for WebSocket to start
        time.sleep(1)

        # Start HTTP server (blocking)
        self.start_http_server()


def main():
    """Main entry point"""
    project_root = Path(__file__).parent.parent

    print("🧠 SOMA Family Dashboard Server")
    print("=" * 50)

    server = SOMADashboardServer(str(project_root))

    try:
        server.start()
    except KeyboardInterrupt:
        print("\n👋 Goodbye! SOMA family will continue growing...")


if __name__ == "__main__":
    main()
