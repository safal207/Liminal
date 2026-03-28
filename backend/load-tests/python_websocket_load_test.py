"""
Нагрузочный тест WebSocket соединений с Home State Detection
Без JWT аутентификации для упрощения
"""

import asyncio
import json
import time
import statistics
from datetime import datetime
from typing import List, Dict, Any

import websockets
import aiohttp
from concurrent.futures import ThreadPoolExecutor


class LoadTestResults:
    def __init__(self):
        self.connection_times: List[float] = []
        self.message_response_times: List[float] = []
        self.successful_connections = 0
        self.failed_connections = 0
        self.successful_messages = 0
        self.failed_messages = 0
        self.errors: List[str] = []


async def single_websocket_test(
    user_id: str, test_duration: int = 30, messages_per_second: int = 2
) -> Dict[str, Any]:
    """Тест одного WebSocket соединения"""

    results = {
        "user_id": user_id,
        "connected": False,
        "messages_sent": 0,
        "messages_received": 0,
        "errors": [],
        "response_times": [],
        "home_state_detections": 0,
    }

    uri = f"ws://localhost:8000/ws/{user_id}"

    try:
        # Пытаемся подключиться без JWT для упрощения
        async with websockets.connect(uri, timeout=10) as websocket:
            results["connected"] = True

            # Отправляем тестовые сообщения
            start_time = time.time()
            message_interval = 1.0 / messages_per_second

            messages = [
                "Привет! Тестируем систему",
                "Честно говоря, я волнуюсь о результатах тестирования",
                "Понимаю, что нахожусь в процессе нагрузочного тестирования",
                "Чувствую себя дома в этом тестовом окружении",
                "На самом деле, система работает стабильно",
                "Сейчас я присутствую в моменте тестирования",
            ]

            message_count = 0
            while time.time() - start_time < test_duration:
                try:
                    # Выбираем сообщение
                    text = messages[message_count % len(messages)]

                    # Создаем сообщение
                    message = {
                        "type": "message",
                        "user_id": user_id,
                        "channel": "load_test_channel",
                        "message": {
                            "text": text,
                            "timestamp": datetime.now().isoformat(),
                        },
                    }

                    # Отправляем и засекаем время
                    send_time = time.time()
                    await websocket.send(json.dumps(message))
                    results["messages_sent"] += 1

                    # Ждем ответ (если есть)
                    try:
                        response = await asyncio.wait_for(websocket.recv(), timeout=5.0)
                        receive_time = time.time()
                        response_time = receive_time - send_time
                        results["response_times"].append(response_time)
                        results["messages_received"] += 1

                        # Проверяем, есть ли Home State Detection в ответе
                        try:
                            response_data = json.loads(response)
                            if (
                                response_data.get("type") == "home_state_update"
                                or "home_state" in response
                            ):
                                results["home_state_detections"] += 1
                        except json.JSONDecodeError:
                            pass

                    except asyncio.TimeoutError:
                        # Нет ответа - это нормально для многих сообщений
                        pass

                    message_count += 1
                    await asyncio.sleep(message_interval)

                except Exception as e:
                    results["errors"].append(f"Message error: {str(e)}")
                    break

    except Exception as e:
        results["errors"].append(f"Connection error: {str(e)}")

    return results


async def http_api_load_test(concurrent_requests: int = 50) -> Dict[str, Any]:
    """Нагрузочный тест HTTP API"""

    results = {
        "total_requests": 0,
        "successful_requests": 0,
        "failed_requests": 0,
        "response_times": [],
        "errors": [],
    }

    async def single_request(session: aiohttp.ClientSession, user_id: str):
        try:
            start_time = time.time()

            # Тест Home State API
            async with session.post(
                f"http://localhost:8000/home-state/analyze/load_test_{user_id}",
                json={
                    "text": f"Тестирую аутентичность пользователя {user_id}. Честно говоря, волнуюсь о результатах.",
                    "timestamp": time.time(),
                    "user_id": f"load_test_{user_id}",
                    "response_time": 2.5,
                },
            ) as response:
                end_time = time.time()
                response_time = end_time - start_time

                if response.status == 200:
                    results["successful_requests"] += 1
                    results["response_times"].append(response_time)

                    # Проверяем структуру ответа
                    data = await response.json()
                    if "authenticity_level" not in data:
                        results["errors"].append(
                            f"Missing authenticity_level in response"
                        )
                else:
                    results["failed_requests"] += 1
                    results["errors"].append(f"HTTP {response.status}")

                results["total_requests"] += 1

        except Exception as e:
            results["failed_requests"] += 1
            results["errors"].append(f"Request error: {str(e)}")
            results["total_requests"] += 1

    # Запускаем конкурентные запросы
    async with aiohttp.ClientSession() as session:
        tasks = [single_request(session, i) for i in range(concurrent_requests)]
        await asyncio.gather(*tasks, return_exceptions=True)

    return results


async def comprehensive_load_test():
    """Комплексный нагрузочный тест"""

    print("🚀 Запуск комплексного нагрузочного тестирования LIMINAL")
    print("=" * 60)

    # 1. HTTP API нагрузочный тест
    print("\n📊 1. HTTP API Load Test (50 concurrent requests)")
    http_results = await http_api_load_test(50)

    print(f"   Всего запросов: {http_results['total_requests']}")
    print(f"   Успешных: {http_results['successful_requests']}")
    print(f"   Неудачных: {http_results['failed_requests']}")

    if http_results["response_times"]:
        avg_time = statistics.mean(http_results["response_times"])
        p95_time = statistics.quantiles(http_results["response_times"], n=20)[18]
        print(f"   Среднее время ответа: {avg_time:.3f}s")
        print(f"   95-й перцентиль: {p95_time:.3f}s")

    if http_results["errors"]:
        print(
            f"   ⚠️  Ошибки: {len(http_results['errors'])} (первые 3: {http_results['errors'][:3]})"
        )

    # 2. WebSocket нагрузочный тест (упрощенный без JWT)
    print(f"\n🔗 2. WebSocket Load Test (10 connections, 30s each)")
    print("   ПРИМЕЧАНИЕ: Тест без JWT аутентификации")

    websocket_tasks = [
        single_websocket_test(
            f"load_test_user_{i}", test_duration=30, messages_per_second=1
        )
        for i in range(10)
    ]

    websocket_results = await asyncio.gather(*websocket_tasks, return_exceptions=True)

    # Анализ результатов WebSocket
    connected_count = 0
    total_messages = 0
    total_responses = 0
    total_errors = 0
    all_response_times = []
    home_state_detections = 0

    for result in websocket_results:
        if isinstance(result, dict):
            if result["connected"]:
                connected_count += 1
            total_messages += result["messages_sent"]
            total_responses += result["messages_received"]
            total_errors += len(result["errors"])
            all_response_times.extend(result["response_times"])
            home_state_detections += result["home_state_detections"]

    print(f"   Подключений успешных: {connected_count}/10")
    print(f"   Сообщений отправлено: {total_messages}")
    print(f"   Ответов получено: {total_responses}")
    print(f"   Home State детекций: {home_state_detections}")

    if all_response_times:
        avg_ws_time = statistics.mean(all_response_times)
        print(f"   Среднее время ответа WebSocket: {avg_ws_time:.3f}s")

    if total_errors > 0:
        print(f"   ⚠️  WebSocket ошибки: {total_errors}")

    # 3. Health check стресс-тест
    print(f"\n💓 3. Health Endpoints Stress Test")
    health_start = time.time()

    async with aiohttp.ClientSession() as session:
        health_tasks = []
        for _ in range(100):
            health_tasks.extend(
                [
                    session.get("http://localhost:8000/health"),
                    session.get("http://localhost:8000/health/live"),
                    session.get("http://localhost:8000/health/ready"),
                ]
            )

        health_responses = await asyncio.gather(*health_tasks, return_exceptions=True)

    health_time = time.time() - health_start
    health_success = sum(
        1 for r in health_responses if hasattr(r, "status") and r.status in [200, 503]
    )

    print(f"   300 health requests за {health_time:.2f}s")
    print(f"   Успешных ответов: {health_success}/300")
    print(f"   RPS: {300/health_time:.1f}")

    print("\n" + "=" * 60)
    print("✅ Нагрузочное тестирование завершено!")

    # Общая оценка производительности
    overall_score = 0
    if http_results["successful_requests"] > 40:  # 80%+ success rate
        overall_score += 3
    if connected_count >= 8:  # 80%+ WebSocket connections
        overall_score += 3
    if health_success > 250:  # 80%+ health checks
        overall_score += 2
    if total_errors < 10:  # Low error count
        overall_score += 2

    print(f"\n🎯 Общая оценка производительности: {overall_score}/10")
    if overall_score >= 8:
        print("🟢 Отличная производительность!")
    elif overall_score >= 6:
        print("🟡 Хорошая производительность")
    else:
        print("🔴 Требуются улучшения производительности")


if __name__ == "__main__":
    try:
        asyncio.run(comprehensive_load_test())
    except KeyboardInterrupt:
        print("\n❌ Тестирование прервано пользователем")
    except Exception as e:
        print(f"\n💥 Ошибка тестирования: {e}")
