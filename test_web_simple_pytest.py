#!/usr/bin/env python3
"""
Simple Pytest test suite for LIMINAL Web Interface
Tests basic functionality without complex server setup
"""

import pytest
import requests
import json
import time
from unittest.mock import patch, MagicMock

# Test configuration - using main server on 5000
BASE_URL = "http://localhost:5000"

class TestLiminalWebBasic:
    """Basic test suite for LIMINAL Web Interface"""
    
    def test_import_web_interface(self):
        """Test that web interface modules can be imported"""
        try:
            from web_interface import LiminalWebInterface, app
            assert LiminalWebInterface is not None
            assert app is not None
            print("✓ Web interface import test passed")
        except ImportError as e:
            pytest.fail(f"Failed to import web interface: {e}")
    
    def test_flask_app_config(self):
        """Test Flask app configuration"""
        from web_interface import app
        assert app.secret_key is not None
        assert app.secret_key != ""
        assert app.secret_key == 'liminal-neural-enhancement-2024'
        print("✓ Flask app configuration test passed")
    
    def test_liminal_interface_initialization(self):
        """Test LiminalWebInterface class initialization"""
        from web_interface import LiminalWebInterface
        
        interface = LiminalWebInterface()
        
        # Test active systems
        assert len(interface.active_systems) == 9
        assert interface.active_systems['emotime'] == True
        assert interface.active_systems['neural_internet'] == True
        assert interface.active_systems['quantum_consciousness'] == True
        
        # Test global stats
        assert interface.global_stats['total_users'] > 0
        assert interface.global_stats['neural_networks_active'] > 0
        
        print("✓ LiminalWebInterface initialization test passed")
    
    def test_session_creation_method(self):
        """Test user session creation method"""
        from web_interface import LiminalWebInterface
        
        interface = LiminalWebInterface()
        session_id = interface.create_user_session()
        
        assert session_id is not None
        assert len(session_id) > 0
        assert session_id.startswith('user_')
        
        print("✓ Session creation method test passed")
    
    def test_system_testing_methods(self):
        """Test individual system testing methods"""
        from web_interface import LiminalWebInterface
        import asyncio
        
        interface = LiminalWebInterface()
        session_id = interface.create_user_session()
        
        # Test emotime system
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            result = loop.run_until_complete(
                interface.test_emotime(session_id, {})
            )
            
            assert 'status' in result
            assert 'result' in result
            assert 'enhancement' in result
            
            print("✓ System testing methods test passed")
        finally:
            loop.close()
    
    def test_server_availability_quick(self):
        """Quick test if server is running"""
        try:
            response = requests.get(BASE_URL, timeout=2)
            if response.status_code == 200:
                print("✓ Server is running and accessible")
                assert "LIMINAL" in response.text or "html" in response.text.lower()
            else:
                print(f"⚠ Server returned status code: {response.status_code}")
        except requests.exceptions.RequestException as e:
            print(f"⚠ Server not accessible: {e}")
            pytest.skip("Server not running for live tests")
    
    def test_api_endpoint_quick(self):
        """Quick API endpoint test if server is running"""
        try:
            session = requests.Session()
            session.get(BASE_URL, timeout=2)
            
            response = session.post(
                f"{BASE_URL}/api/test_system",
                json={"system": "emotime", "params": {}},
                headers={"Content-Type": "application/json"},
                timeout=5
            )
            
            if response.status_code == 200:
                data = response.json()
                assert "status" in data
                assert "result" in data
                print("✓ API endpoint quick test passed")
            else:
                print(f"⚠ API returned status: {response.status_code}")
                
        except requests.exceptions.RequestException:
            print("⚠ API not accessible - server may not be running")
            pytest.skip("Server not running for API tests")
    
    def test_stats_endpoint_quick(self):
        """Quick stats endpoint test"""
        try:
            session = requests.Session() 
            session.get(BASE_URL, timeout=2)
            
            response = session.get(f"{BASE_URL}/api/get_stats", timeout=5)
            
            if response.status_code == 200:
                data = response.json()
                assert "active_systems" in data
                assert "tests_run" in data
                print("✓ Stats endpoint quick test passed")
            else:
                print(f"⚠ Stats API returned status: {response.status_code}")
                
        except requests.exceptions.RequestException:
            print("⚠ Stats API not accessible")
            pytest.skip("Server not running for stats test")
    
    def test_html_template_exists(self):
        """Test that HTML template exists"""
        import os
        template_path = "C:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/templates/index.html"
        
        assert os.path.exists(template_path), "HTML template not found"
        
        with open(template_path, 'r', encoding='utf-8') as f:
            content = f.read()
            
        assert "LIMINAL" in content
        assert "Эмоциональный Временной Анализ" in content
        assert "testSystem" in content  # JavaScript function
        
        print("✓ HTML template test passed")
    
    def test_web_interface_file_exists(self):
        """Test that web interface Python file exists and is valid"""
        import os
        web_interface_path = "C:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/web_interface.py"
        
        assert os.path.exists(web_interface_path), "Web interface file not found"
        
        with open(web_interface_path, 'r', encoding='utf-8') as f:
            content = f.read()
            
        # Check key components
        assert "from flask import Flask" in content
        assert "class LiminalWebInterface" in content
        assert "@app.route('/')" in content
        assert "@app.route('/api/test_system'" in content
        
        print("✓ Web interface file test passed")

def test_all_systems_defined():
    """Test that all systems are properly defined"""
    from web_interface import LiminalWebInterface
    
    interface = LiminalWebInterface()
    expected_systems = {
        'emotime',
        'neural_internet', 
        'quantum_consciousness',
        'memory_augmentation',
        'emotion_synthesis',
        'temporal_perception',
        'reality_synthesis',
        'collective_intelligence',
        'consciousness_uploading'
    }
    
    actual_systems = set(interface.active_systems.keys())
    assert actual_systems == expected_systems
    
    print("✓ All systems definition test passed")

def test_mock_api_response():
    """Test API response structure with mocked data"""
    from web_interface import LiminalWebInterface
    import asyncio
    
    interface = LiminalWebInterface()
    session_id = interface.create_user_session()
    
    # Mock test for consistent results
    with patch('asyncio.sleep'), patch('random.choice') as mock_choice:
        mock_choice.return_value = "Emotional state: Optimized"
        
        loop = asyncio.new_event_loop()
        try:
            result = loop.run_until_complete(
                interface.test_emotime(session_id, {})
            )
            
            assert result['status'] == 'Система активна'
            assert 'enhancement' in result
            assert isinstance(result['enhancement'], (int, float))
            
            print("✓ Mock API response test passed")
        finally:
            loop.close()

if __name__ == "__main__":
    # Run tests with verbose output
    pytest.main([__file__, "-v", "--tb=short", "-x"])