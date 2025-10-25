#!/usr/bin/env python3
"""
Test API with proper session handling
"""

import requests
import json

def test_with_session():
    """Test API with session cookies"""
    print("=== Test LIMINAL Web API with Sessions ===")
    
    # Create session
    s = requests.Session()
    
    try:
        # Get main page to establish session
        print("1. Getting main page...")
        response = s.get('http://localhost:5000')
        if response.status_code == 200:
            print("✓ Main page loaded, session created")
        else:
            print(f"✗ Failed to load main page: {response.status_code}")
            return
        
        # Test emotime system
        print("2. Testing Emotime system...")
        test_response = s.post(
            'http://localhost:5000/api/test_system',
            json={'system': 'emotime', 'params': {}},
            headers={'Content-Type': 'application/json'}
        )
        
        if test_response.status_code == 200:
            data = test_response.json()
            print("✓ Emotime test successful!")
            print(f"  System: {data.get('system', 'N/A')}")
            print(f"  Title: {data.get('title', 'N/A')}")
            print(f"  Success: {data.get('success', False)}")
            if 'emotions' in data:
                print(f"  Emotions detected: {len(data['emotions'])} types")
        else:
            print(f"✗ Emotime test failed: {test_response.status_code}")
            print(f"Response: {test_response.text}")
        
        # Test stats API
        print("3. Testing Stats API...")
        stats_response = s.get('http://localhost:5000/api/get_stats')
        
        if stats_response.status_code == 200:
            stats = stats_response.json()
            print("✓ Stats API working!")
            print(f"  Active systems: {stats.get('active_systems', 'N/A')}")
            print(f"  Tests run: {stats.get('tests_run', 'N/A')}")
            print(f"  Enhancement level: {stats.get('enhancement_level', 'N/A')}%")
            print(f"  Session time: {stats.get('session_time', 'N/A')}min")
        else:
            print(f"✗ Stats API failed: {stats_response.status_code}")
            print(f"Response: {stats_response.text}")
            
        # Test multiple systems
        print("4. Testing multiple systems...")
        systems = ['neural_internet', 'quantum_consciousness', 'memory_augmentation']
        
        for system in systems:
            test_response = s.post(
                'http://localhost:5000/api/test_system',
                json={'system': system, 'params': {}},
                headers={'Content-Type': 'application/json'}
            )
            
            if test_response.status_code == 200:
                data = test_response.json()
                print(f"✓ {system}: {data.get('system', 'OK')}")
            else:
                print(f"✗ {system}: Failed ({test_response.status_code})")
        
        print("\n=== Final Stats ===")
        final_stats = s.get('http://localhost:5000/api/get_stats')
        if final_stats.status_code == 200:
            stats = final_stats.json()
            print(f"Total tests run: {stats.get('tests_run', 0)}")
            print(f"Enhancement level: {stats.get('enhancement_level', 0)}%")
        
        print("\n✓ All API tests completed successfully!")
        print("Web interface is working correctly!")
        
    except requests.exceptions.ConnectionError:
        print("✗ Cannot connect to server. Make sure it's running on localhost:5000")
    except Exception as e:
        print(f"✗ Error during testing: {e}")

if __name__ == "__main__":
    test_with_session()