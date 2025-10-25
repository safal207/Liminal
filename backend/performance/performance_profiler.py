"""
LIMINAL Performance Profiler
Анализ производительности системы для выявления bottleneck'ов.
"""

import asyncio
import time
import statistics
import requests
from datetime import datetime
from typing import Dict, List, Optional
from dataclasses import dataclass
import json


@dataclass
class PerformanceMetric:
    """Метрика производительности."""
    endpoint: str
    avg_response_time: float
    min_response_time: float
    max_response_time: float
    p95_response_time: float
    success_rate: float
    errors: List[str]
    sample_size: int


class LIMINALPerformanceProfiler:
    """Профайлер производительности LIMINAL системы."""
    
    def __init__(self, base_url: str = "http://localhost:8000"):
        self.base_url = base_url
        self.results = {}
    
    async def run_performance_analysis(self):
        """Запускает полный анализ производительности."""
        print("LIMINAL PERFORMANCE ANALYSIS")
        print("=" * 50)
        print(f"Target: {self.base_url}")
        print(f"Start: {datetime.now().strftime('%H:%M:%S')}")
        print()
        
        # 1. Анализ основных endpoints
        await self._analyze_core_endpoints()
        
        # 2. Анализ Emotime производительности
        await self._analyze_emotime_performance()
        
        # 3. Анализ пропускной способности
        await self._analyze_throughput()
        
        # 4. Стресс-тест
        await self._run_stress_test()
        
        # 5. Сводка и рекомендации
        self._generate_recommendations()
        
        return self.results
    
    async def _analyze_core_endpoints(self):
        """Анализ производительности основных endpoints."""
        print("1. CORE ENDPOINTS ANALYSIS")
        print("-" * 30)
        
        endpoints = [
            "/health",
            "/metrics", 
            "/emotime/status",
            "/emotime/health"
        ]
        
        for endpoint in endpoints:
            metric = await self._measure_endpoint_performance(endpoint, samples=10)
            self.results[f"endpoint_{endpoint.replace('/', '_')}"] = metric
            
            status = "OK" if metric.avg_response_time < 1000 else "SLOW"
            print(f"   {endpoint}: {metric.avg_response_time:.0f}ms avg [{status}]")
            print(f"     Success rate: {metric.success_rate:.1f}%")
            print(f"     P95: {metric.p95_response_time:.0f}ms")
            
            if metric.errors:
                print(f"     Errors: {len(metric.errors)}")
    
    async def _analyze_emotime_performance(self):
        """Анализ производительности Emotime обработки."""
        print(f"\n2. EMOTIME PROCESSING PERFORMANCE")
        print("-" * 30)
        
        # Тест обработки текста разной длины
        test_cases = [
            {"text": "Short", "expected_time": 500},
            {"text": "Medium length emotional text with some complexity", "expected_time": 1000},
            {"text": " ".join(["Very long emotional text"] * 20), "expected_time": 2000},
        ]
        
        emotime_results = []
        
        for i, case in enumerate(test_cases):
            times = []
            errors = []
            
            print(f"   Testing case {i+1}: {len(case['text'])} chars")
            
            for _ in range(5):
                start_time = time.time()
                try:
                    response = requests.post(
                        f"{self.base_url}/emotime/text",
                        json={
                            "text": case["text"],
                            "user_id": "perf_test_user",
                            "session_id": "perf_session"
                        },
                        timeout=10
                    )
                    
                    elapsed = (time.time() - start_time) * 1000
                    times.append(elapsed)
                    
                    if response.status_code != 200:
                        errors.append(f"HTTP {response.status_code}")
                    else:
                        # Проверяем качество ответа
                        data = response.json()
                        confidence = data.get("confidence", 0)
                        if confidence == 0:
                            errors.append("Zero confidence")
                
                except Exception as e:
                    errors.append(str(e))
                    times.append(10000)  # 10 second penalty for errors
            
            if times:
                avg_time = statistics.mean(times)
                success_rate = ((5 - len(errors)) / 5) * 100
                
                status = "OK" if avg_time < case["expected_time"] else "SLOW"
                print(f"     Avg time: {avg_time:.0f}ms [{status}]")
                print(f"     Success rate: {success_rate:.0f}%")
                
                emotime_results.append({
                    "case": i+1,
                    "text_length": len(case["text"]),
                    "avg_time": avg_time,
                    "success_rate": success_rate,
                    "errors": errors
                })
        
        self.results["emotime_processing"] = emotime_results
    
    async def _analyze_throughput(self):
        """Анализ пропускной способности системы."""
        print(f"\n3. THROUGHPUT ANALYSIS")
        print("-" * 30)
        
        # Concurrent requests test
        concurrent_levels = [1, 5, 10, 20]
        
        for concurrent in concurrent_levels:
            print(f"   Testing {concurrent} concurrent requests...")
            
            start_time = time.time()
            success_count = 0
            error_count = 0
            
            # Create tasks for concurrent requests
            tasks = []
            for _ in range(concurrent):
                task = asyncio.create_task(self._make_concurrent_request())
                tasks.append(task)
            
            # Wait for all tasks to complete
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            # Count successes and failures
            for result in results:
                if isinstance(result, Exception):
                    error_count += 1
                elif result:
                    success_count += 1
                else:
                    error_count += 1
            
            elapsed_time = time.time() - start_time
            throughput = concurrent / elapsed_time
            
            print(f"     Throughput: {throughput:.1f} req/sec")
            print(f"     Success: {success_count}/{concurrent}")
            print(f"     Errors: {error_count}")
            
            self.results[f"throughput_{concurrent}"] = {
                "concurrent_requests": concurrent,
                "throughput": throughput,
                "success_rate": (success_count / concurrent) * 100,
                "total_time": elapsed_time
            }
    
    async def _run_stress_test(self):
        """Стресс-тест системы."""
        print(f"\n4. STRESS TEST")
        print("-" * 30)
        
        print("   Running 30-second stress test...")
        
        start_time = time.time()
        end_time = start_time + 30  # 30 seconds
        
        request_count = 0
        success_count = 0
        error_count = 0
        response_times = []
        
        while time.time() < end_time:
            try:
                req_start = time.time()
                response = requests.get(f"{self.base_url}/health", timeout=5)
                req_time = (time.time() - req_start) * 1000
                
                response_times.append(req_time)
                request_count += 1
                
                if response.status_code == 200:
                    success_count += 1
                else:
                    error_count += 1
                    
            except Exception:
                error_count += 1
                request_count += 1
                response_times.append(5000)  # 5 second penalty
            
            # Small delay to prevent overwhelming
            await asyncio.sleep(0.1)
        
        total_time = time.time() - start_time
        avg_throughput = request_count / total_time
        avg_response_time = statistics.mean(response_times) if response_times else 0
        
        print(f"     Duration: {total_time:.1f}s")
        print(f"     Total requests: {request_count}")
        print(f"     Average throughput: {avg_throughput:.1f} req/sec")
        print(f"     Success rate: {(success_count/request_count*100):.1f}%")
        print(f"     Avg response time: {avg_response_time:.0f}ms")
        
        self.results["stress_test"] = {
            "duration": total_time,
            "total_requests": request_count,
            "throughput": avg_throughput,
            "success_rate": (success_count / request_count) * 100,
            "avg_response_time": avg_response_time,
            "errors": error_count
        }
    
    async def _measure_endpoint_performance(self, endpoint: str, samples: int = 10) -> PerformanceMetric:
        """Измеряет производительность конкретного endpoint."""
        times = []
        errors = []
        
        for _ in range(samples):
            start_time = time.time()
            try:
                response = requests.get(f"{self.base_url}{endpoint}", timeout=10)
                elapsed = (time.time() - start_time) * 1000
                times.append(elapsed)
                
                if response.status_code >= 400:
                    errors.append(f"HTTP {response.status_code}")
            
            except Exception as e:
                errors.append(str(e))
                times.append(10000)  # 10 second penalty for timeout
        
        if times:
            avg_time = statistics.mean(times)
            min_time = min(times)
            max_time = max(times)
            p95_time = sorted(times)[int(len(times) * 0.95)] if len(times) >= 20 else max(times)
            success_rate = ((samples - len(errors)) / samples) * 100
        else:
            avg_time = min_time = max_time = p95_time = 0
            success_rate = 0
        
        return PerformanceMetric(
            endpoint=endpoint,
            avg_response_time=avg_time,
            min_response_time=min_time,
            max_response_time=max_time,
            p95_response_time=p95_time,
            success_rate=success_rate,
            errors=errors,
            sample_size=samples
        )
    
    async def _make_concurrent_request(self) -> bool:
        """Делает одиночный запрос для теста пропускной способности."""
        try:
            response = requests.get(f"{self.base_url}/health", timeout=5)
            return response.status_code == 200
        except:
            return False
    
    def _generate_recommendations(self):
        """Генерирует рекомендации по оптимизации."""
        print(f"\n5. PERFORMANCE RECOMMENDATIONS")
        print("-" * 30)
        
        recommendations = []
        
        # Analyze results and generate recommendations
        health_metric = self.results.get("endpoint__health")
        if health_metric and health_metric.avg_response_time > 1000:
            recommendations.append("Health endpoint is slow - check database connections and reduce startup checks")
        
        emotime_results = self.results.get("emotime_processing", [])
        slow_emotime = any(r["avg_time"] > 1500 for r in emotime_results)
        if slow_emotime:
            recommendations.append("Emotime processing is slow - implement caching and batch processing")
        
        # Check throughput
        throughput_10 = self.results.get("throughput_10", {})
        if throughput_10.get("throughput", 0) < 5:
            recommendations.append("Low throughput detected - consider async optimization and connection pooling")
        
        # Check stress test
        stress = self.results.get("stress_test", {})
        if stress.get("success_rate", 100) < 95:
            recommendations.append("High error rate under stress - improve error handling and resource limits")
        
        if not recommendations:
            recommendations.append("Performance is within acceptable ranges")
        
        for i, rec in enumerate(recommendations, 1):
            print(f"   {i}. {rec}")
        
        self.results["recommendations"] = recommendations
        
        print(f"\n   Analysis completed at {datetime.now().strftime('%H:%M:%S')}")


async def main():
    """Запуск анализа производительности."""
    profiler = LIMINALPerformanceProfiler()
    
    try:
        results = await profiler.run_performance_analysis()
        
        # Save results to file
        with open("performance_analysis.json", "w") as f:
            # Convert PerformanceMetric objects to dictionaries for JSON serialization
            serializable_results = {}
            for key, value in results.items():
                if isinstance(value, PerformanceMetric):
                    serializable_results[key] = {
                        "endpoint": value.endpoint,
                        "avg_response_time": value.avg_response_time,
                        "min_response_time": value.min_response_time,
                        "max_response_time": value.max_response_time,
                        "p95_response_time": value.p95_response_time,
                        "success_rate": value.success_rate,
                        "errors": value.errors,
                        "sample_size": value.sample_size
                    }
                else:
                    serializable_results[key] = value
            
            json.dump(serializable_results, f, indent=2)
        
        print(f"\nDetailed results saved to: performance_analysis.json")
        return True
        
    except Exception as e:
        print(f"Performance analysis failed: {e}")
        return False


if __name__ == "__main__":
    result = asyncio.run(main())
    exit(0 if result else 1)