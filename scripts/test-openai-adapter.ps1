# Resonance Liminal - Тест универсального OpenAI адаптера
# Скрипт для автоматизированного тестирования универсального OpenAI адаптера
# Выполняет тесты как с реальным API, так и в режиме мока

param(
    [switch]$MockOnly,     # Использовать только мок-режим
    [switch]$ShowResponse, # Показать полные ответы
    [switch]$Verbose       # Подробный вывод
)

# Цветной вывод
$Green = [System.ConsoleColor]::Green
$Cyan = [System.ConsoleColor]::Cyan 
$Yellow = [System.ConsoleColor]::Yellow
$Red = [System.ConsoleColor]::Red
$Magenta = [System.ConsoleColor]::Magenta

function Write-Header {
    param([string]$Message)
    Write-Host "`n$('=' * 70)" -ForegroundColor $Magenta
    Write-Host "  $Message" -ForegroundColor $Magenta
    Write-Host "$('=' * 70)" -ForegroundColor $Magenta
}

function Write-Section {
    param([string]$Message)
    Write-Host "`n--- $Message ---" -ForegroundColor $Cyan
}

function Write-Success {
    param([string]$Message)
    Write-Host "[+] $Message" -ForegroundColor $Green
}

function Write-Info {
    param([string]$Message) 
    Write-Host "[*] $Message" -ForegroundColor $Cyan
}

function Write-Warning {
    param([string]$Message)
    Write-Host "[!] $Message" -ForegroundColor $Yellow
}

function Write-Error {
    param([string]$Message)
    Write-Host "[-] $Message" -ForegroundColor $Red
}

function Get-MaskedKey {
    param([string]$Key)
    
    if ([string]::IsNullOrEmpty($Key) -or $Key.Length -lt 8) {
        return "отсутствует"
    }
    
    return "$($Key.Substring(0, 5))...$($Key.Substring($Key.Length - 4))"
}

# Загружаем переменные окружения
Write-Info "Загружаем переменные окружения из .env"
try {
    $envPath = "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/.env"
    if (Test-Path $envPath) {
        Get-Content $envPath | ForEach-Object {
            if ($_ -match "^\s*([^#=]+)=(.*)$") {
                $key = $Matches[1].Trim()
                $value = $Matches[2].Trim()
                if ($value -and $key) {
                    [Environment]::SetEnvironmentVariable($key, $value)
                    if ($Verbose) {
                        if ($key -match "API_KEY") {
                            Write-Info "Установлена переменная $key = $(Get-MaskedKey $value)"
                        } else {
                            Write-Info "Установлена переменная $key = $value"
                        }
                    }
                }
            }
        }
        Write-Success "Переменные окружения загружены"
    } else {
        Write-Warning "Файл .env не найден"
    }
} catch {
    Write-Error "Ошибка при загрузке переменных окружения: $_"
}

# Проверка API ключей
Write-Header "ПРОВЕРКА API КЛЮЧЕЙ"

$openaiKey = [Environment]::GetEnvironmentVariable("OPENAI_API_KEY")
$anthropicKey = [Environment]::GetEnvironmentVariable("ANTHROPIC_API_KEY")
$xaiKey = [Environment]::GetEnvironmentVariable("XAI_API_KEY")

Write-Info "OpenAI API ключ: $(Get-MaskedKey $openaiKey)"
Write-Info "Anthropic API ключ: $(Get-MaskedKey $anthropicKey)"
Write-Info "XAI API ключ: $(Get-MaskedKey $xaiKey)"

# Установка режима мока если нужно
if ($MockOnly) {
    Write-Warning "Устанавливаем режим MOCK_ONLY=true для тестирования"
    [Environment]::SetEnvironmentVariable("OPENAI_MOCK_ONLY", "true")
} else {
    [Environment]::SetEnvironmentVariable("OPENAI_MOCK_ONLY", "false")
}

# Запускаем тест универсального адаптера
Write-Header "ТЕСТИРОВАНИЕ УНИВЕРСАЛЬНОГО OPENAI АДАПТЕРА"

$testScript = @'
import os
import sys
import json
import asyncio
from pathlib import Path

# Добавляем директорию проекта в sys.path
sys.path.append(str(Path(__file__).parent.parent))

async def test_universal_adapter():
    """Тестируем универсальный OpenAI адаптер"""
    from backend.ml.openai_wrapper import llm_client, initialize_llm_client, LLMRequest
    print("[*] Модуль openai_wrapper успешно импортирован")
    
    # Инициализируем клиент
    success = await initialize_llm_client()
    if success:
        print("[+] Клиент инициализирован с реальным API")
    else:
        print("[!] Клиент инициализирован в мок-режиме")
    
    # Проверяем параметры клиента
    print(f"[*] API ключ: {llm_client.api_key[:5]}...{llm_client.api_key[-4:] if llm_client.api_key else 'нет'}")
    print(f"[*] Мок режим: {llm_client.mock_only}")
    print(f"[*] Кэширование: {llm_client.cache_enabled}")
    
    # Выполняем тестовый запрос для обнаружения аномалий
    request = LLMRequest(
        model="gpt-4",
        messages=[
            {"role": "system", "content": "Ты - аналитический AI для обнаружения аномалий в системах."},
            {"role": "user", "content": "Объясни аномалию: большое количество запросов (120 в минуту) с одного IP адреса."}
        ],
        max_tokens=500,
        temperature=0.2
    )
    
    # Выполняем запрос
    response = await llm_client.call(request)
    
    # Выводим информацию о результате
    print(f"[+] Запрос выполнен успешно")
    print(f"[*] Модель: {response.model}")
    print(f"[*] Мок-режим: {response.is_mock}")
    print(f"[*] Кэширован: {response.cached}")
    
    if response.usage:
        print(f"[*] Использовано токенов: {response.usage}")
    
    # Проверяем результат
    try:
        content = json.loads(response.content)
        print("[+] Получен структурированный JSON ответ")
        
        if "$SHOW_RESPONSE" == "True":
            print("===== ПОЛНЫЙ ОТВЕТ =====")
            print(json.dumps(content, indent=2, ensure_ascii=False))
            print("=======================")
    except json.JSONDecodeError:
        print("[!] Ответ не является валидным JSON")
        if "$SHOW_RESPONSE" == "True":
            print("===== ОТВЕТ =====")
            print(response.content[:300] + "..." if len(response.content) > 300 else response.content)
            print("================")
    
    # Проверяем кэширование
    cache_response = await llm_client.call(request)
    
    if cache_response.cached:
        print("[+] Кэширование работает корректно")
    else:
        print("[!] Кэширование не сработало")
    
    # Проверяем OpenAI Service
    try:
        from backend.ml.openai_service import OpenAIService
        
        print("[*] Тестирование OpenAIService")
        openai_service = OpenAIService()
        await openai_service.initialize()
        
        print("[+] OpenAIService инициализирован")
        health = await openai_service.health_check()
        print(f"[*] Health check: {health}")
    except Exception as e:
        print(f"[-] Ошибка при тестировании OpenAIService: {e}")

if __name__ == "__main__":
    asyncio.run(test_universal_adapter())
'@

# Замещаем переменную для отображения ответа
$testScript = $testScript -replace '\$SHOW_RESPONSE', $ShowResponse.ToString()

# Сохраняем временный файл
$tempScriptPath = "c:/Users/safal/OneDrive/Documente/GitHub/resonance-liminal/scripts/temp-test-adapter.py"
$testScript | Out-File -FilePath $tempScriptPath -Encoding utf8

# Запускаем тест
Write-Info "Запускаем тест универсального адаптера..."
try {
    python $tempScriptPath
    
    if ($LASTEXITCODE -eq 0) {
        Write-Success "Тест универсального адаптера завершен успешно"
    } else {
        Write-Error "Тест универсального адаптера завершен с ошибками (код: $LASTEXITCODE)"
    }
} catch {
    Write-Error "Ошибка при запуске теста: $_"
}

# Удаляем временный файл
if (Test-Path $tempScriptPath) {
    Remove-Item $tempScriptPath -Force
}

# Проверка Backend API
Write-Header "ПРОВЕРКА BACKEND API (ML/OPENAI)"

Write-Info "Проверяем доступность ML API endpoint'ов..."
$endpoints = @{
    "Health" = "http://localhost:8000/health"
    "ML Health" = "http://localhost:8000/ml/health"
    "OpenAI Health" = "http://localhost:8000/ml/openai/health"
}

$results = @()

foreach ($endpoint in $endpoints.GetEnumerator()) {
    Write-Section "Проверка $($endpoint.Key)"
    
    try {
        $response = Invoke-WebRequest -Uri $endpoint.Value -UseBasicParsing
        if ($response.StatusCode -eq 200) {
            Write-Success "$($endpoint.Key): $($response.StatusCode) OK"
            
            # Если JSON, показать содержимое
            if ($response.Headers["Content-Type"] -like "*json*") {
                $content = $response.Content | ConvertFrom-Json
                
                # Для разных эндпоинтов разная обработка
                switch -Wildcard ($endpoint.Key) {
                    "Health" {
                        if ($content.status -eq "ok") {
                            Write-Success "API статус: $($content.status)"
                        } else {
                            Write-Warning "API статус: $($content.status)"
                        }
                        
                        # Проверка Redis
                        if ($content.redis.status -eq "connected") {
                            Write-Success "Redis: $($content.redis.status)"
                        } else {
                            Write-Warning "Redis: $($content.redis.status)"
                        }
                    }
                    "ML*" {
                        if ($content.status -eq "ok" -or $content.status -eq "healthy") {
                            Write-Success "ML статус: $($content.status)"
                        } else {
                            Write-Warning "ML статус: $($content.status)"
                        }
                        
                        if ($content.models) {
                            Write-Info "Доступные модели: $($content.models.Count)"
                        }
                    }
                    "OpenAI*" {
                        if ($content.status -eq "ok" -or $content.status -eq "healthy") {
                            Write-Success "OpenAI статус: $($content.status)"
                        } else {
                            Write-Warning "OpenAI статус: $($content.status)"
                        }
                        
                        if ($content.mock_mode -eq $true) {
                            Write-Warning "OpenAI работает в режиме мока"
                        } else {
                            Write-Success "OpenAI использует реальный API"
                        }
                    }
                }
            }
            
            $results += @{
                Endpoint = $endpoint.Key
                Status = "SUCCESS"
            }
        } else {
            Write-Warning "$($endpoint.Key): $($response.StatusCode)"
            $results += @{
                Endpoint = $endpoint.Key
                Status = "WARNING"
            }
        }
    } catch {
        Write-Error "$($endpoint.Key): Недоступен - $_"
        $results += @{
            Endpoint = $endpoint.Key
            Status = "FAILED"
        }
    }
}

# Результаты проверок
Write-Header "РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ"

$results | ForEach-Object {
    $statusColor = switch($_.Status) {
        "SUCCESS" { $Green }
        "WARNING" { $Yellow }
        "FAILED" { $Red }
        default { $Cyan }
    }
    
    Write-Host ("  • {0,-15}: {1}" -f $_.Endpoint, $_.Status) -ForegroundColor $statusColor
}

# Итоговый статус
$successCount = ($results | Where-Object { $_.Status -eq "SUCCESS" }).Count
$totalCount = $results.Count

if ($successCount -eq $totalCount) {
    Write-Success "`nВсе тесты успешно пройдены!"
} elseif ($successCount -gt 0) {
    Write-Warning "`nЧасть тестов пройдена успешно ($successCount из $totalCount)"
} else {
    Write-Error "`nВсе тесты завершились с ошибками"
}

Write-Info "`nВ зависимости от среды могут быть недоступны некоторые API endpoints"
Write-Info "При наличии работы в мок-режиме это нормально"
