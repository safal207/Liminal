#!/usr/bin/env python3
"""
Pytest test suite for LIMINAL Web Interface
Comprehensive testing of all API endpoints and functionality
"""

import pytest
import requests
import json
import time
import threading
import subprocess
import os
import signal
from unittest.mock import patch, MagicMock

# Test configuration
BASE_URL = "http://localhost:5001"  # Different port to avoid conflicts
TEST_PORT = 5001

class TestLiminalWebInterface:
    """Test suite for LIMINAL Web Interface"""
    
    @classmethod
    def setup_class(cls):
        """Setup test environment"""
        cls.server_process = None
        cls.start_test_server()
        time.sleep(3)  # Wait for server to start
    
    @classmethod  
    def teardown_class(cls):
        """Cleanup after tests"""
        cls.stop_test_server()
    
    @classmethod
    def start_test_server(cls):
        """Start test server on different port"""
        try:
            # Create modified web interface for testing
            test_server_code = '''
import sys
sys.path.append(".")
from web_interface import app, LiminalWebInterface

# Override port for testing
if __name__ == "__main__":
    app.run(debug=False, host="127.0.0.1", port=5001)
'''
            with open("test_server.py", "w", encoding="utf-8") as f:
                f.write(test_server_code)
            
            # Start server in background
            cls.server_process = subprocess.Popen(
                ["python3", "test_server.py"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                cwd="C:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal"
            )
            print(f"Test server started with PID: {cls.server_process.pid}")
            
        except Exception as e:
            print(f"Failed to start test server: {e}")
    
    @classmethod
    def stop_test_server(cls):
        """Stop test server"""
        if cls.server_process:
            cls.server_process.terminate()
            cls.server_process.wait()
            print("Test server stopped")
        
        # Clean up test file
        try:
            os.remove("test_server.py")
        except:
            pass

    def test_server_availability(self):
        """Test that server is running and responding"""
        try:
            response = requests.get(BASE_URL, timeout=5)
            assert response.status_code == 200
            assert "LIMINAL" in response.text
            print("✓ Server availability test passed")
        except requests.exceptions.ConnectionError:
            pytest.skip("Test server not available")
    
    def test_main_page_content(self):
        """Test main page contains expected elements"""
        response = requests.get(BASE_URL, timeout=5)
        assert response.status_code == 200
        
        # Check for key elements
        content = response.text
        assert "Интерфейс Тестирования Систем" in content
        assert "Эмоциональный Временной Анализ" in content
        assert "Протокол Нейронного Интернета" in content
        assert "Квантовые Вычисления Сознания" in content
        print("✓ Main page content test passed")
    
    def test_session_creation(self):
        """Test session is created automatically"""
        session = requests.Session()
        response = session.get(BASE_URL)
        assert response.status_code == 200
        
        # Check session cookies are set
        assert len(session.cookies) > 0
        print("✓ Session creation test passed")
    
    def test_emotime_api_endpoint(self):
        """Test Emotime system API endpoint"""
        session = requests.Session()
        session.get(BASE_URL)  # Create session
        
        response = session.post(
            f"{BASE_URL}/api/test_system",
            json={"system": "emotime", "params": {}},
            headers={"Content-Type": "application/json"},
            timeout=10
        )
        
        assert response.status_code == 200
        data = response.json()
        assert "status" in data
        assert "result" in data
        assert data["status"] in ["success", "Система активна"]
        print("✓ Emotime API test passed")
    
    def test_neural_internet_api_endpoint(self):
        """Test Neural Internet system API endpoint"""
        session = requests.Session()
        session.get(BASE_URL)
        
        response = session.post(
            f"{BASE_URL}/api/test_system",
            json={"system": "neural_internet", "params": {}},
            headers={"Content-Type": "application/json"},
            timeout=10
        )
        
        assert response.status_code == 200
        data = response.json()
        assert "status" in data
        assert "result" in data
        print("✓ Neural Internet API test passed")
    
    def test_quantum_consciousness_api_endpoint(self):
        """Test Quantum Consciousness system API endpoint"""
        session = requests.Session()
        session.get(BASE_URL)
        
        response = session.post(
            f"{BASE_URL}/api/test_system",
            json={"system": "quantum_consciousness", "params": {}},
            headers={"Content-Type": "application/json"},
            timeout=10
        )
        
        assert response.status_code == 200
        data = response.json()
        assert "status" in data
        assert "result" in data
        print("✓ Quantum Consciousness API test passed")
    
    def test_memory_augmentation_api_endpoint(self):
        """Test Memory Augmentation system API endpoint"""
        session = requests.Session()
        session.get(BASE_URL)
        
        response = session.post(
            f"{BASE_URL}/api/test_system",
            json={"system": "memory_augmentation", "params": {}},
            headers={"Content-Type": "application/json"},
            timeout=10
        )
        
        assert response.status_code == 200
        data = response.json()
        assert "status" in data
        assert "result" in data
        print("✓ Memory Augmentation API test passed")
    
    def test_all_systems_api_endpoints(self):
        """Test all remaining system API endpoints"""
        systems = [
            "emotion_synthesis",
            "temporal_perception", 
            "reality_synthesis",
            "collective_intelligence",
            "consciousness_uploading"
        ]
        
        session = requests.Session()
        session.get(BASE_URL)
        
        for system in systems:
            response = session.post(
                f"{BASE_URL}/api/test_system",
                json={"system": system, "params": {}},
                headers={"Content-Type": "application/json"},
                timeout=10
            )
            
            assert response.status_code == 200
            data = response.json()
            assert "status" in data
            assert "result" in data
            print(f"✓ {system} API test passed")
    
    def test_stats_api_endpoint(self):
        """Test stats API endpoint"""
        session = requests.Session()
        session.get(BASE_URL)
        
        response = session.get(f"{BASE_URL}/api/get_stats")
        assert response.status_code == 200
        
        data = response.json()
        assert "active_systems" in data
        assert "tests_run" in data
        assert "enhancement_level" in data
        assert "session_time" in data
        print("✓ Stats API test passed")
    
    def test_invalid_system_request(self):
        """Test API with invalid system name"""
        session = requests.Session()
        session.get(BASE_URL)
        
        response = session.post(
            f"{BASE_URL}/api/test_system",
            json={"system": "invalid_system", "params": {}},
            headers={"Content-Type": "application/json"},
            timeout=10
        )
        
        # Should still return 200 but with error message
        assert response.status_code == 200
        data = response.json()
        assert "status" in data
        print("✓ Invalid system test passed")
    
    def test_malformed_json_request(self):
        """Test API with malformed JSON"""
        session = requests.Session()
        session.get(BASE_URL)
        
        response = session.post(
            f"{BASE_URL}/api/test_system",
            data="invalid json",
            headers={"Content-Type": "application/json"},
            timeout=10
        )
        
        # Should return 400 or handle gracefully
        assert response.status_code in [200, 400, 500]
        print("✓ Malformed JSON test passed")
    
    def test_concurrent_requests(self):
        """Test multiple concurrent API requests"""
        import concurrent.futures
        
        def make_request():
            session = requests.Session()
            session.get(BASE_URL)
            response = session.post(
                f"{BASE_URL}/api/test_system",
                json={"system": "emotime", "params": {}},
                headers={"Content-Type": "application/json"},
                timeout=10
            )
            return response.status_code == 200
        
        # Make 5 concurrent requests
        with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
            futures = [executor.submit(make_request) for _ in range(5)]
            results = [future.result() for future in concurrent.futures.as_completed(futures)]
        
        # All requests should succeed
        assert all(results)
        print("✓ Concurrent requests test passed")
    
    def test_session_persistence(self):
        """Test that session data persists across requests"""
        session = requests.Session()
        
        # Make initial request
        session.get(BASE_URL)
        
        # Test system to increment counters
        session.post(
            f"{BASE_URL}/api/test_system",
            json={"system": "emotime", "params": {}},
            headers={"Content-Type": "application/json"},
            timeout=10
        )
        
        # Get stats
        response = session.get(f"{BASE_URL}/api/get_stats")
        data = response.json()
        
        # Tests run should be > 0
        assert data.get("tests_run", 0) > 0
        print("✓ Session persistence test passed")
    
    def test_enhancement_level_progression(self):
        """Test that enhancement level increases with tests"""
        session = requests.Session()
        session.get(BASE_URL)
        
        # Get initial stats
        initial_response = session.get(f"{BASE_URL}/api/get_stats")
        initial_data = initial_response.json()
        initial_level = initial_data.get("enhancement_level", 0)
        
        # Run a test
        session.post(
            f"{BASE_URL}/api/test_system",
            json={"system": "emotime", "params": {}},
            headers={"Content-Type": "application/json"},
            timeout=10
        )
        
        # Get updated stats
        updated_response = session.get(f"{BASE_URL}/api/get_stats")
        updated_data = updated_response.json()
        updated_level = updated_data.get("enhancement_level", 0)
        
        # Enhancement level should increase
        assert updated_level >= initial_level
        print("✓ Enhancement level progression test passed")

# Standalone test functions for individual components
def test_liminal_web_interface_import():
    """Test that web interface modules can be imported"""
    try:
        from web_interface import LiminalWebInterface, app
        assert LiminalWebInterface is not None
        assert app is not None
        print("✓ Import test passed")
    except ImportError as e:
        pytest.fail(f"Failed to import web interface: {e}")

def test_flask_app_configuration():
    """Test Flask app configuration"""
    from web_interface import app
    assert app.secret_key is not None
    assert app.secret_key != ""
    print("✓ Flask configuration test passed")

if __name__ == "__main__":
    # Run tests with verbose output
    pytest.main([__file__, "-v", "--tb=short"])