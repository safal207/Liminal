"""
Message Acknowledgments Integration Test
Tests the complete ACK system including API endpoints and metrics.
"""

import asyncio
import json
import requests
import time
from websocket.connection_manager import ConnectionManager, PendingMessage


def test_api_endpoint():
    """Test Message ACK API endpoint"""
    print("\nAPI ENDPOINT TEST")
    print("-" * 30)
    
    try:
        # Test stats endpoint
        response = requests.get("http://localhost:8000/message-ack/stats", timeout=5)
        if response.status_code == 200:
            stats = response.json()
            print(f"   API Status: OK")
            print(f"   Total pending: {stats['total_pending_messages']}")
            print(f"   Timeout: {stats['ack_timeout_seconds']}s")
            return True
        else:
            print(f"   API Status: ERROR {response.status_code}")
            return False
    except Exception as e:
        print(f"   API Status: ERROR {e}")
        return False


def test_prometheus_metrics():
    """Test Prometheus metrics endpoint"""
    print("\nPROMETHEUS METRICS TEST")
    print("-" * 30)
    
    try:
        # Test metrics endpoint
        response = requests.get("http://localhost:8000/metrics", timeout=5)
        if response.status_code == 200:
            metrics_text = response.text
            
            # Check for ACK metrics
            ack_metrics = [
                "websocket_message_acks_total",
                "websocket_pending_messages",
                "websocket_message_retries_total",
                "websocket_ack_response_time"
            ]
            
            found_metrics = []
            for metric in ack_metrics:
                if metric in metrics_text:
                    found_metrics.append(metric)
            
            print(f"   Metrics Status: OK")
            print(f"   Found ACK metrics: {len(found_metrics)}/{len(ack_metrics)}")
            for metric in found_metrics:
                print(f"     - {metric}")
            
            return len(found_metrics) >= 2  # At least 2 metrics should be present
        else:
            print(f"   Metrics Status: ERROR {response.status_code}")
            return False
    except Exception as e:
        print(f"   Metrics Status: ERROR {e}")
        return False


async def test_ack_core_logic():
    """Test ACK system core logic"""
    print("\nCORE LOGIC TEST")
    print("-" * 30)
    
    try:
        # Create manager
        manager = ConnectionManager()
        
        # Create mock websocket
        class MockWebSocket:
            def __init__(self, user_id):
                self.user_id = user_id
                
            async def send_json(self, data):
                pass
        
        mock_ws = MockWebSocket("test_user")
        
        # Create pending message
        pending_msg = PendingMessage(
            message_id="integration-test-123",
            content={"type": "test", "text": "Integration test message"},
            user_id="test_user",
            websocket=mock_ws
        )
        
        # Add to manager
        manager.pending_messages["integration-test-123"] = pending_msg
        manager.user_pending_messages["test_user"] = {"integration-test-123"}
        
        # Test stats before ACK
        stats_before = manager.get_ack_stats()
        print(f"   Before ACK - Pending: {stats_before['total_pending_messages']}")
        
        # Process ACK
        ack_result = await manager.handle_ack("integration-test-123", "test_user")
        print(f"   ACK processing: {'SUCCESS' if ack_result else 'FAILED'}")
        
        # Test stats after ACK
        stats_after = manager.get_ack_stats()
        print(f"   After ACK - Pending: {stats_after['total_pending_messages']}")
        
        # Test should show pending messages went from 1 to 0
        success = (stats_before['total_pending_messages'] == 1 and 
                  stats_after['total_pending_messages'] == 0 and 
                  ack_result)
        
        print(f"   Core Logic: {'PASSED' if success else 'FAILED'}")
        return success
        
    except Exception as e:
        print(f"   Core Logic: ERROR {e}")
        return False


async def main():
    """Run full integration test suite"""
    print("MESSAGE ACKNOWLEDGMENTS INTEGRATION TEST")
    print("=" * 50)
    
    # Test components
    tests = [
        ("API Endpoint", test_api_endpoint),
        ("Prometheus Metrics", test_prometheus_metrics),
        ("Core ACK Logic", test_ack_core_logic)
    ]
    
    results = []
    
    for test_name, test_func in tests:
        if asyncio.iscoroutinefunction(test_func):
            result = await test_func()
        else:
            result = test_func()
        results.append((test_name, result))
    
    # Summary
    print("\nINTEGRATION TEST SUMMARY")
    print("=" * 50)
    
    passed = 0
    total = len(results)
    
    for test_name, result in results:
        status = "PASSED" if result else "FAILED"
        print(f"   {test_name}: {status}")
        if result:
            passed += 1
    
    success_rate = (passed / total) * 100
    print(f"\nOverall: {passed}/{total} tests passed ({success_rate:.0f}%)")
    
    if success_rate >= 80:
        print("INTEGRATION TEST: SUCCESS")
        return True
    else:
        print("INTEGRATION TEST: FAILED")
        return False


if __name__ == "__main__":
    try:
        result = asyncio.run(main())
        exit(0 if result else 1)
    except Exception as e:
        print(f"Integration test error: {e}")
        exit(1)